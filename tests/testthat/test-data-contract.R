test_that("the committed snapshot is complete and internally consistent", {
  bikes <- readr::read_csv(
    here::here("data", "seoul_bike_snapshot.csv"),
    show_col_types = FALSE
  )

  expect_equal(nrow(bikes), 8760)
  expect_equal(ncol(bikes), 16)
  expect_equal(min(bikes$date), as.Date("2017-12-01"))
  expect_equal(max(bikes$date), as.Date("2018-11-30"))
  expect_equal(sum(is.na(bikes)), 0)
  expect_true(all(bikes$hour %in% 0:23))
  expect_true(all(bikes$rented_bike_count >= 0))
  expect_false(anyDuplicated(bikes[c("date", "hour")]) > 0)

  checksums <- readr::read_csv(
    here::here("data", "checksums.csv"),
    show_col_types = FALSE
  )
  expect_identical(
    digest::digest(here::here(checksums$file[[1]]), algo = "sha256", file = TRUE),
    checksums$sha256[[1]]
  )
})

test_that("the chronological holdout is strictly in the future", {
  bikes <- readr::read_csv(
    here::here("data", "seoul_bike_snapshot.csv"),
    show_col_types = FALSE
  )
  ordered_dates <- sort(unique(bikes$date))
  first_test_date <- ordered_dates[[floor(length(ordered_dates) * 0.80) + 1]]
  training <- dplyr::filter(bikes, date < first_test_date)
  testing <- dplyr::filter(bikes, date >= first_test_date)

  expect_gt(nrow(training), 0)
  expect_gt(nrow(testing), 0)
  expect_lt(max(training$date), min(testing$date))
})
