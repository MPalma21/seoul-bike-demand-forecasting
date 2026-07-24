suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidymodels)
}))

read_bike_snapshot <- function(path = here::here("data", "seoul_bike_snapshot.csv")) {
  data <- read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(date = col_date())
  ) |>
    mutate(
      weekday = factor(weekday),
      season = factor(season),
      holiday = factor(holiday),
      functioning_day = factor(functioning_day),
      is_weekend = factor(is_weekend)
    ) |>
    arrange(date, hour)
  validate_bike_data(data)
  data
}

validate_bike_data <- function(data) {
  required <- c(
    "date", "rented_bike_count", "hour", "temperature_c", "humidity_pct",
    "wind_speed_ms", "visibility_10m", "dew_point_c", "solar_radiation_mj_m2",
    "rainfall_mm", "snowfall_cm", "season", "holiday", "functioning_day",
    "weekday", "is_weekend"
  )
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0) {
    stop("Bike data is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  if (any(data$rented_bike_count < 0, na.rm = TRUE)) {
    stop("Bike rental counts cannot be negative")
  }
  if (anyDuplicated(data[c("date", "hour")])) {
    stop("Date-hour combinations must be unique")
  }
  invisible(TRUE)
}

bike_metrics <- function(predictions, model_name) {
  tibble(
    model = model_name,
    .metric = c("rmse", "mae", "rsq"),
    .estimate = c(
      rmse_vec(predictions$rented_bike_count, predictions$.pred),
      mae_vec(predictions$rented_bike_count, predictions$.pred),
      rsq_trad_vec(predictions$rented_bike_count, predictions$.pred)
    )
  )
}

fit_bike_models <- function(
  data,
  seed = 2026,
  folds = 3L,
  tune_size = 6L,
  trees = 300L
) {
  modeling_data <- data |>
    filter(functioning_day == "Yes") |>
    select(-functioning_day)

  set.seed(seed)
  split <- initial_time_split(modeling_data, prop = 0.8)
  training_data <- training(split)
  testing_data <- testing(split)
  resamples <- vfold_cv(training_data, v = folds)

  baseline_predictions <- testing_data |>
    transmute(rented_bike_count, .pred = mean(training_data$rented_bike_count))

  model_recipe <- recipe(
    rented_bike_count ~ hour + temperature_c + humidity_pct + wind_speed_ms +
      visibility_10m + dew_point_c + solar_radiation_mj_m2 + rainfall_mm +
      snowfall_cm + season + holiday + weekday,
    data = training_data
  ) |>
    step_unknown(all_nominal_predictors()) |>
    step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors())

  linear_workflow <- workflow() |>
    add_recipe(model_recipe) |>
    add_model(linear_reg() |> set_engine("lm"))
  linear_fit <- fit(linear_workflow, training_data)
  linear_predictions <- bind_cols(
    testing_data |> select(rented_bike_count),
    predict(linear_fit, testing_data)
  )

  elastic_workflow <- workflow() |>
    add_recipe(model_recipe) |>
    add_model(linear_reg(penalty = tune(), mixture = tune()) |> set_engine("glmnet"))
  elastic_grid <- grid_space_filling(
    penalty(range = c(-5, 1)),
    mixture(),
    size = tune_size
  )
  elastic_results <- tune_grid(
    elastic_workflow,
    resamples = resamples,
    grid = elastic_grid,
    metrics = metric_set(rmse, mae, rsq_trad)
  )
  best_elastic <- select_best(elastic_results, metric = "rmse")
  elastic_fit <- elastic_workflow |>
    finalize_workflow(best_elastic) |>
    fit(training_data)
  elastic_predictions <- bind_cols(
    testing_data |> select(rented_bike_count),
    predict(elastic_fit, testing_data)
  )

  forest_workflow <- workflow() |>
    add_recipe(model_recipe) |>
    add_model(
      rand_forest(mtry = tune(), min_n = tune(), trees = trees) |>
        set_engine("ranger", importance = "permutation") |>
        set_mode("regression")
    )
  forest_grid <- grid_space_filling(
    mtry(range = c(3L, 12L)),
    min_n(range = c(5L, 35L)),
    size = tune_size
  )
  forest_results <- tune_grid(
    forest_workflow,
    resamples = resamples,
    grid = forest_grid,
    metrics = metric_set(rmse, mae, rsq_trad)
  )
  best_forest <- select_best(forest_results, metric = "rmse")
  forest_fit <- forest_workflow |>
    finalize_workflow(best_forest) |>
    fit(training_data)
  forest_predictions <- bind_cols(
    testing_data |> select(rented_bike_count),
    predict(forest_fit, testing_data)
  )

  importance <- forest_fit |>
    extract_fit_parsnip() |>
    purrr::pluck("fit", "variable.importance")
  importance <- tibble(
    variable = names(importance),
    importance = as.numeric(importance)
  ) |>
    arrange(desc(importance)) |>
    slice_head(n = 12)

  metrics <- bind_rows(
    bike_metrics(baseline_predictions, "Mean baseline"),
    bike_metrics(linear_predictions, "Linear regression"),
    bike_metrics(elastic_predictions, "Elastic Net"),
    bike_metrics(forest_predictions, "Random forest")
  ) |>
    arrange(.metric, .estimate)

  list(
    split = split,
    metrics = metrics,
    importance = importance,
    best_elastic = best_elastic,
    best_forest = best_forest
  )
}

plot_hourly_demand <- function(data, language = c("en", "es")) {
  language <- match.arg(language)
  labels <- if (language == "en") {
    c(title = "Demand changes by hour and season", x = "Hour", y = "Average rented bikes", color = "Season")
  } else {
    c(title = "La demanda cambia según la hora y la estación", x = "Hora", y = "Bicicletas alquiladas en promedio", color = "Estación")
  }
  plot_data <- data |>
    filter(functioning_day == "Yes") |>
    group_by(hour, season) |>
    summarise(average_demand = mean(rented_bike_count), .groups = "drop")

  ggplot(plot_data, aes(hour, average_demand, color = season)) +
    geom_line(linewidth = 1) +
    scale_color_viridis_d(end = 0.85) +
    scale_x_continuous(breaks = seq(0, 23, 3)) +
    labs(
      title = labels[["title"]], x = labels[["x"]], y = labels[["y"]],
      color = labels[["color"]]
    ) +
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
