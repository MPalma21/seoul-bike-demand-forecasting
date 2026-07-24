source(here::here("R", "analysis.R"))

test_that("bike snapshot preserves hourly counts and semantic variables", {
  bikes <- read_bike_snapshot()
  expect_equal(nrow(bikes), 8760)
  expect_true(all(bikes$rented_bike_count >= 0))
  expect_false(anyDuplicated(bikes[c("date", "hour")]) > 0)
  expect_s3_class(bikes$season, "factor")
})

test_that("model comparison returns holdout metrics and importance", {
  bikes <- read_bike_snapshot() |>
    slice_head(n = 1800)
  results <- fit_bike_models(
    bikes,
    folds = 2,
    tune_size = 3,
    trees = 50
  )
  expect_setequal(
    unique(results$metrics$model),
    c("Mean baseline", "Linear regression", "Elastic Net", "Random forest")
  )
  expect_setequal(unique(results$metrics$.metric), c("rmse", "mae", "rsq"))
  expect_true(all(is.finite(results$metrics$.estimate)))
  expect_true(nrow(results$importance) > 0)
  expect_true(nrow(testing(results$split)) > 0)
})

test_that("negative rental counts are rejected", {
  bikes <- read_bike_snapshot()
  bikes$rented_bike_count[[1]] <- -1
  expect_error(validate_bike_data(bikes), "cannot be negative")
})

