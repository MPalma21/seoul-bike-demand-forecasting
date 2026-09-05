suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ranger)
  library(readr)
  library(tidymodels)
}))

required_bike_columns <- c(
  "date", "rented_bike_count", "hour", "temperature_c", "humidity_pct",
  "wind_speed_ms", "visibility_10m", "dew_point_c", "solar_radiation_mj_m2",
  "rainfall_mm", "snowfall_cm", "season", "holiday", "functioning_day",
  "weekday", "is_weekend"
)

read_bike_snapshot <- function(path = here::here("data", "seoul_bike_snapshot.csv")) {
  if (!file.exists(path)) stop("Bike snapshot does not exist: ", path)
  data <- read_csv(path, show_col_types = FALSE, col_types = cols(date = col_date())) |>
    mutate(
      weekday = factor(
        weekday,
        levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      ),
      season = factor(season), holiday = factor(holiday),
      functioning_day = factor(functioning_day), is_weekend = factor(is_weekend)
    ) |>
    arrange(date, hour)
  validate_bike_data(data)
  data
}

validate_bike_data <- function(data) {
  missing_columns <- setdiff(required_bike_columns, names(data))
  if (length(missing_columns) > 0) {
    stop("Bike data is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  if (nrow(data) == 0 || anyNA(data[required_bike_columns])) {
    stop("Bike data must contain complete observations")
  }
  if (any(data$rented_bike_count < 0)) stop("Bike rental counts cannot be negative")
  if (any(!data$hour %in% 0:23)) stop("Bike hours must be integers between 0 and 23")
  if (any(data$humidity_pct < 0 | data$humidity_pct > 100)) {
    stop("Humidity must be between 0 and 100 percent")
  }
  nonnegative <- c(
    "wind_speed_ms", "visibility_10m", "solar_radiation_mj_m2",
    "rainfall_mm", "snowfall_cm"
  )
  if (any(vapply(data[nonnegative], function(x) any(x < 0), logical(1)))) {
    stop("Wind, visibility, radiation, rainfall and snowfall cannot be negative")
  }
  if (anyDuplicated(data[c("date", "hour")])) {
    stop("Date-hour combinations must be unique")
  }
  if (!all(as.character(data$functioning_day) %in% c("Yes", "No"))) {
    stop("Functioning day must contain only Yes or No")
  }
  invisible(TRUE)
}

bike_data_profile <- function(data) {
  tibble(
    observations = nrow(data), first_date = min(data$date), last_date = max(data$date),
    operating_days = n_distinct(data$date[data$functioning_day == "Yes"]),
    closure_hours = sum(data$functioning_day == "No"),
    average_hourly_demand = mean(data$rented_bike_count),
    peak_hourly_demand = max(data$rented_bike_count), missing_cells = sum(is.na(data))
  )
}

prepare_modeling_data <- function(data) {
  origin <- min(data$date)
  data |>
    filter(functioning_day == "Yes") |>
    transmute(
      date, rented_bike_count, hour = factor(hour, levels = 0:23),
      weekday = factor(weekday),
      month = factor(format(date, "%m"), levels = sprintf("%02d", 1:12)),
      season = factor(season), holiday = factor(holiday), is_weekend = factor(is_weekend),
      trend_day = as.numeric(date - origin), temperature_c, humidity_pct, wind_speed_ms,
      visibility_10m, dew_point_c, solar_radiation_mj_m2, rainfall_mm, snowfall_cm
    ) |>
    arrange(date, hour)
}

temporal_partition <- function(data, test_fraction = 0.20) {
  if (!is.numeric(test_fraction) || test_fraction <= 0 || test_fraction >= 0.5) {
    stop("test_fraction must be greater than 0 and smaller than 0.5")
  }
  dates <- sort(unique(data$date))
  test_days <- max(1L, ceiling(length(dates) * test_fraction))
  first_test_date <- dates[[length(dates) - test_days + 1L]]
  training_data <- data |> filter(date < first_test_date)
  testing_data <- data |> filter(date >= first_test_date)
  if (nrow(training_data) == 0 || nrow(testing_data) == 0) {
    stop("Temporal partition produced an empty training or test set")
  }
  if (max(training_data$date) >= min(testing_data$date)) {
    stop("Training dates must precede all final test dates")
  }
  list(
    training = training_data, testing = testing_data,
    first_test_date = first_test_date, test_days = test_days
  )
}

temporal_resamples <- function(training_data, folds = 4L, assessment_days = 28L, gap_days = 1L) {
  folds <- as.integer(folds)
  assessment_days <- as.integer(assessment_days)
  gap_days <- as.integer(gap_days)
  if (folds < 2L || assessment_days < 1L || gap_days < 0L) {
    stop("Use at least two folds, one assessment day and a nonnegative gap")
  }
  dates <- sort(unique(training_data$date))
  maximum_analysis_end <- length(dates) - assessment_days - gap_days
  minimum_analysis_end <- max(30L, floor(length(dates) * 0.45))
  if (maximum_analysis_end <= minimum_analysis_end) {
    stop("Not enough dates to create temporal resamples")
  }
  analysis_ends <- unique(as.integer(round(seq(
    minimum_analysis_end, maximum_analysis_end, length.out = folds
  ))))
  splits <- lapply(analysis_ends, function(end_position) {
    assessment_start <- end_position + gap_days + 1L
    assessment_end <- assessment_start + assessment_days - 1L
    rsample::make_splits(
      list(
        analysis = which(training_data$date %in% dates[seq_len(end_position)]),
        assessment = which(training_data$date %in% dates[assessment_start:assessment_end])
      ),
      training_data
    )
  })
  rsample::manual_rset(splits, paste0("Window", sprintf("%02d", seq_along(splits))))
}

calendar_baseline <- function(training_data, testing_data) {
  global_average <- mean(training_data$rented_bike_count)
  hourly_average <- training_data |>
    group_by(hour) |>
    summarise(hour_average = mean(rented_bike_count), .groups = "drop")
  calendar_average <- training_data |>
    group_by(hour, weekday) |>
    summarise(calendar_average = mean(rented_bike_count), .groups = "drop")
  testing_data |>
    left_join(calendar_average, by = c("hour", "weekday")) |>
    left_join(hourly_average, by = "hour") |>
    transmute(
      date, hour, rented_bike_count,
      .pred = coalesce(calendar_average, hour_average, global_average),
      model = "Calendar baseline"
    )
}

bike_recipe <- function(training_data) {
  recipe(rented_bike_count ~ ., data = training_data) |>
    update_role(date, new_role = "id") |>
    step_unknown(all_nominal_predictors()) |>
    step_novel(all_nominal_predictors()) |>
    step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
    step_zv(all_predictors()) |>
    step_lincomb(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors())
}

prediction_frame <- function(fit, testing_data, model_name) {
  bind_cols(
    testing_data |> select(date, hour, rented_bike_count),
    predict(fit, new_data = testing_data)
  ) |>
    mutate(.pred = pmax(0, .pred), model = model_name)
}

operational_metrics <- function(predictions, peak_threshold) {
  predictions |>
    group_by(model) |>
    group_modify(function(data, key) {
      peak <- data$rented_bike_count >= peak_threshold
      tibble(
        rmse = sqrt(mean((data$rented_bike_count - data$.pred)^2)),
        mae = mean(abs(data$rented_bike_count - data$.pred)),
        rsq = yardstick::rsq_trad_vec(data$rented_bike_count, data$.pred),
        wape_pct = 100 * sum(abs(data$rented_bike_count - data$.pred)) /
          sum(data$rented_bike_count),
        bias = mean(data$.pred - data$rented_bike_count),
        peak_mae = mean(abs(data$rented_bike_count[peak] - data$.pred[peak]))
      )
    }) |>
    ungroup() |>
    arrange(rmse)
}

fit_bike_models <- function(
  data, seed = 2026, folds = 4L, assessment_days = 28L,
  tune_size = 6L, trees = 300L, test_fraction = 0.20
) {
  model_data <- prepare_modeling_data(data)
  partition <- temporal_partition(model_data, test_fraction)
  training_data <- partition$training
  testing_data <- partition$testing
  resamples <- temporal_resamples(training_data, folds, assessment_days)
  recipe <- bike_recipe(training_data)
  tuning_metrics <- metric_set(rmse, mae)
  linear_workflow <- workflow() |>
    add_recipe(recipe) |>
    add_model(linear_reg() |> set_engine("lm"))
  linear_fit <- fit(linear_workflow, training_data)

  elastic_workflow <- workflow() |>
    add_recipe(recipe) |>
    add_model(linear_reg(penalty = tune(), mixture = tune()) |> set_engine("glmnet"))
  set.seed(seed + 1L)
  elastic_grid <- grid_space_filling(penalty(range = c(-5, 0)), mixture(), size = tune_size)
  elastic_results <- tune_grid(
    elastic_workflow, resamples = resamples, grid = elastic_grid, metrics = tuning_metrics,
    control = control_grid(allow_par = FALSE)
  )
  best_elastic <- select_best(elastic_results, metric = "rmse")
  elastic_fit <- elastic_workflow |>
    finalize_workflow(best_elastic) |>
    fit(training_data)

  forest_workflow <- workflow() |>
    add_recipe(recipe) |>
    add_model(
      rand_forest(mtry = tune(), min_n = tune(), trees = trees) |>
        set_engine(
          "ranger", importance = "permutation", num.threads = 1, seed = seed + 3L
        ) |>
        set_mode("regression")
    )
  set.seed(seed + 2L)
  forest_grid <- grid_space_filling(
    mtry(range = c(4L, 18L)), min_n(range = c(5L, 40L)), size = tune_size
  )
  forest_results <- tune_grid(
    forest_workflow, resamples = resamples, grid = forest_grid, metrics = tuning_metrics,
    control = control_grid(allow_par = FALSE)
  )
  best_forest <- select_best(forest_results, metric = "rmse")
  set.seed(seed + 3L)
  forest_fit <- forest_workflow |>
    finalize_workflow(best_forest) |>
    fit(training_data)

  predictions <- bind_rows(
    calendar_baseline(training_data, testing_data),
    prediction_frame(linear_fit, testing_data, "Linear regression"),
    prediction_frame(elastic_fit, testing_data, "Elastic Net"),
    prediction_frame(forest_fit, testing_data, "Random forest")
  )
  peak_threshold <- as.numeric(quantile(
    training_data$rented_bike_count, probs = 0.90, names = FALSE
  ))
  metrics <- operational_metrics(predictions, peak_threshold)
  importance_vector <- forest_fit |>
    extract_fit_engine() |>
    purrr::pluck("variable.importance")
  importance <- tibble(
    variable = names(importance_vector), importance = as.numeric(importance_vector)
  ) |>
    arrange(desc(importance)) |>
    slice_head(n = 12)

  list(
    partition = partition, resamples = resamples, metrics = metrics,
    predictions = predictions, importance = importance,
    best_elastic = best_elastic, best_forest = best_forest,
    tuning = list(elastic = elastic_results, forest = forest_results),
    fits = list(linear = linear_fit, elastic = elastic_fit, forest = forest_fit),
    peak_threshold = peak_threshold
  )
}

temporal_window_summary <- function(results) {
  purrr::map2_dfr(results$resamples$splits, results$resamples$id, function(split, id) {
    analysis_data <- analysis(split)
    assessment_data <- assessment(split)
    tibble(
      window = id, training_start = min(analysis_data$date),
      training_end = max(analysis_data$date),
      validation_start = min(assessment_data$date),
      validation_end = max(assessment_data$date),
      training_rows = nrow(analysis_data), validation_rows = nrow(assessment_data)
    )
  })
}

best_model_name <- function(results) {
  results$metrics$model[[which.min(results$metrics$rmse)]]
}

plot_demand_heatmap <- function(data, language = c("en", "es")) {
  language <- match.arg(language)
  labels <- if (language == "en") {
    c(title = "Commute peaks define the operating rhythm", x = "Hour", y = "Weekday", fill = "Average rentals")
  } else {
    c(title = "Los picos de viaje definen el ritmo operativo", x = "Hora", y = "Día", fill = "Alquileres promedio")
  }
  plot_data <- data |>
    filter(functioning_day == "Yes") |>
    group_by(weekday, hour) |>
    summarise(average_demand = mean(rented_bike_count), .groups = "drop")
  ggplot(plot_data, aes(hour, weekday, fill = average_demand)) +
    geom_tile() +
    scale_fill_viridis_c(option = "C") +
    scale_x_continuous(breaks = seq(0, 23, 3)) +
    labs(title = labels[["title"]], x = labels[["x"]], y = labels[["y"]], fill = labels[["fill"]]) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())
}

plot_model_metrics <- function(metrics, language = c("en", "es")) {
  language <- match.arg(language)
  title <- if (language == "en") "Final holdout error by model" else "Error final por modelo"
  x_label <- if (language == "en") "RMSE (bikes)" else "RMSE (bicicletas)"
  ggplot(metrics, aes(rmse, reorder(model, -rmse), fill = model)) +
    geom_col(show.legend = FALSE, width = 0.68) +
    geom_text(aes(label = round(rmse, 1)), hjust = -0.15, size = 3.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
    scale_fill_viridis_d(end = 0.85) +
    labs(title = title, x = x_label, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
}

plot_holdout_series <- function(results, days = 7L, language = c("en", "es")) {
  language <- match.arg(language)
  model_name <- best_model_name(results)
  predictions <- results$predictions |>
    filter(model == .env$model_name) |>
    mutate(timestamp = as.POSIXct(date) + as.numeric(as.character(hour)) * 3600) |>
    filter(date >= max(date) - as.integer(days) + 1L) |>
    select(timestamp, observed = rented_bike_count, predicted = .pred) |>
    tidyr::pivot_longer(c(observed, predicted), names_to = "series", values_to = "bikes")
  labels <- if (language == "en") {
    c(title = paste("Observed and predicted demand —", model_name), y = "Bikes")
  } else {
    c(title = paste("Demanda observada y predicha —", model_name), y = "Bicicletas")
  }
  ggplot(predictions, aes(timestamp, bikes, color = series)) +
    geom_line(linewidth = 0.75) +
    scale_color_manual(values = c(observed = "#264653", predicted = "#E76F51")) +
    labs(title = labels[["title"]], x = NULL, y = labels[["y"]], color = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
}

plot_peak_residuals <- function(results, language = c("en", "es")) {
  language <- match.arg(language)
  model_name <- best_model_name(results)
  diagnostics <- results$predictions |>
    filter(model == .env$model_name) |>
    mutate(
      hour = as.integer(as.character(hour)),
      residual = rented_bike_count - .pred,
      peak = rented_bike_count >= results$peak_threshold
    )
  labels <- if (language == "en") {
    c(title = "Peak-hour errors remain the hardest problem", x = "Hour", y = "Observed minus predicted", color = "Peak demand")
  } else {
    c(title = "Los errores en horas pico siguen siendo el mayor reto", x = "Hora", y = "Observado menos predicho", color = "Demanda pico")
  }
  ggplot(diagnostics, aes(hour, residual, color = peak)) +
    geom_hline(yintercept = 0, color = "#6C757D") +
    geom_jitter(width = 0.18, alpha = 0.25) +
    geom_smooth(se = FALSE, color = "#E76F51") +
    scale_color_manual(values = c(`FALSE` = "#457B9D", `TRUE` = "#D62828")) +
    scale_x_continuous(breaks = seq(0, 23, 3)) +
    labs(title = labels[["title"]], x = labels[["x"]], y = labels[["y"]], color = labels[["color"]]) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
}

plot_variable_importance <- function(data, language = c("en", "es")) {
  language <- match.arg(language)
  labels <- if (language == "en") {
    c(title = "Random forest permutation importance", x = "Importance", y = "")
  } else {
    c(title = "Importancia por permutación del random forest", x = "Importancia", y = "")
  }
  ggplot(data, aes(importance, reorder(variable, importance))) +
    geom_col(fill = "#2A9D8F") +
    labs(title = labels[["title"]], x = labels[["x"]], y = labels[["y"]]) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
}
