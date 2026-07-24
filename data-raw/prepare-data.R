suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source_url <- "https://archive.ics.uci.edu/static/public/560/seoul+bike+sharing+demand.zip"
archive <- tempfile(fileext = ".zip")
extract_dir <- tempfile()
dir.create(extract_dir)
download.file(source_url, archive, mode = "wb", quiet = TRUE)
unzip(archive, exdir = extract_dir)

source_file <- list.files(
  extract_dir,
  pattern = "SeoulBikeData[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(source_file) != 1) {
  stop("Expected exactly one SeoulBikeData.csv file")
}

raw <- read.csv(
  source_file,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)
expected_columns <- c(
  "Date", "Rented Bike Count", "Hour", "Temperature(°C)", "Humidity(%)",
  "Wind speed (m/s)", "Visibility (10m)", "Dew point temperature(°C)",
  "Solar Radiation (MJ/m2)", "Rainfall(mm)", "Snowfall (cm)", "Seasons",
  "Holiday", "Functioning Day"
)
if (!identical(names(raw), expected_columns)) {
  stop("The UCI Seoul Bike schema has changed")
}

names(raw) <- c(
  "date", "rented_bike_count", "hour", "temperature_c", "humidity_pct",
  "wind_speed_ms", "visibility_10m", "dew_point_c", "solar_radiation_mj_m2",
  "rainfall_mm", "snowfall_cm", "season", "holiday", "functioning_day"
)

bikes <- as_tibble(raw) |>
  mutate(
    date = as.Date(date, format = "%d/%m/%Y"),
    weekday = factor(weekdays(date), levels = weekdays(as.Date("2026-07-20") + 0:6)),
    is_weekend = weekday %in% c("Saturday", "Sunday")
  ) |>
  arrange(date, hour)

if (
  nrow(bikes) != 8760 ||
  any(is.na(bikes$date)) ||
  any(bikes$rented_bike_count < 0) ||
  anyDuplicated(bikes[c("date", "hour")])
) {
  stop("Prepared Seoul Bike data failed validation")
}

dir.create("data", showWarnings = FALSE, recursive = TRUE)
write_csv(bikes, file.path("data", "seoul_bike_snapshot.csv"), na = "")
writeLines(
  c(
    paste("snapshot_date:", Sys.Date()),
    paste("source:", source_url),
    "doi: https://doi.org/10.24432/C5F62R",
    "license: CC BY 4.0"
  ),
  file.path("data", "SOURCES.txt")
)

