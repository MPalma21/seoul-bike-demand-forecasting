source(here::here("R", "analysis.R"))

test_that("the committed snapshot is complete and internally consistent", {
  bikes <- read_bike_snapshot()
  profile <- bike_data_profile(bikes)
  expect_equal(nrow(bikes), 8760)
  expect_equal(profile$first_date, as.Date("2017-12-01"))
  expect_equal(profile$last_date, as.Date("2018-11-30"))
  expect_equal(profile$missing_cells, 0)
  expect_false(anyDuplicated(bikes[c("date", "hour")]) > 0)
  expect_true(all(bikes$rented_bike_count >= 0))
  checksums <- readr::read_csv(here::here("data", "checksums.csv"), show_col_types = FALSE)
  expect_identical(
    digest::digest(here::here(checksums$file[[1]]), algo = "sha256", file = TRUE),
    checksums$sha256[[1]]
  )
})

test_that("validation rejects malformed operational data", {
  bikes <- read_bike_snapshot()
  negative <- bikes
  negative$rented_bike_count[[1]] <- -1
  expect_error(validate_bike_data(negative), "cannot be negative")
  invalid_hour <- bikes
  invalid_hour$hour[[1]] <- 24
  expect_error(validate_bike_data(invalid_hour), "between 0 and 23")
  invalid_humidity <- bikes
  invalid_humidity$humidity_pct[[1]] <- 101
  expect_error(validate_bike_data(invalid_humidity), "between 0 and 100")
  duplicate <- bind_rows(bikes, bikes[1, ])
  expect_error(validate_bike_data(duplicate), "must be unique")
})

test_that("the final holdout contains only future dates", {
  partition <- read_bike_snapshot() |>
    prepare_modeling_data() |>
    temporal_partition(test_fraction = 0.20)
  expect_gt(nrow(partition$training), 0)
  expect_gt(nrow(partition$testing), 0)
  expect_lt(max(partition$training$date), min(partition$testing$date))
  expect_equal(min(partition$testing$date), partition$first_test_date)
})

test_that("resampling windows expand without temporal leakage", {
  training <- read_bike_snapshot() |>
    prepare_modeling_data() |>
    temporal_partition() |>
    purrr::pluck("training")
  windows <- temporal_resamples(training, folds = 3, assessment_days = 14)
  summary <- temporal_window_summary(list(resamples = windows))
  expect_equal(nrow(summary), 3)
  expect_true(all(summary$training_end < summary$validation_start))
  expect_true(all(diff(summary$training_rows) > 0))
  expect_true(all(summary$validation_rows > 0))
})

test_that("model comparison returns actionable holdout diagnostics", {
  bikes <- read_bike_snapshot() |> slice_head(n = 2400)
  results <- fit_bike_models(
    bikes, folds = 2, assessment_days = 7, tune_size = 2, trees = 50
  )
  expect_setequal(
    results$metrics$model,
    c("Calendar baseline", "Linear regression", "Elastic Net", "Random forest")
  )
  expect_true(all(c("rmse", "mae", "rsq", "wape_pct", "bias", "peak_mae") %in%
    names(results$metrics)))
  expect_true(all(is.finite(as.matrix(results$metrics[-1]))))
  expect_true(all(results$predictions$.pred >= 0))
  expect_true(nrow(results$importance) > 0)
  expect_true(all(c("penalty", "mixture") %in% names(results$best_elastic)))
  expect_true(all(c("mtry", "min_n") %in% names(results$best_forest)))
  expect_lt(max(results$partition$training$date), min(results$partition$testing$date))
})

