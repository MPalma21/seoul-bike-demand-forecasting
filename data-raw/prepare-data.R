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

raw <- read_delim(
  source_file,
  delim = ",",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE,
  trim_ws = TRUE
)
expected_columns <- c(
  "Date", "Rented Bike Count", "Hour", "Temperature", "Humidity(%)",
  "Wind speed (m/s)", "Visibility (10m)", "Dew point temperature",
  "Solar Radiation (MJ/m2)", "Rainfall(mm)", "Snowfall (cm)", "Seasons",
  "Holiday", "Functioning Day"
)
observed_columns <- names(raw)
observed_columns[[4]] <- if (grepl("^Temperature\\(.*C\\)$", observed_columns[[4]])) {
  "Temperature"
} else observed_columns[[4]]
observed_columns[[8]] <- if (grepl("^Dew point temperature\\(.*C\\)$", observed_columns[[8]])) {
  "Dew point temperature"
} else observed_columns[[8]]
if (!identical(observed_columns, expected_columns)) {
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
    weekday_number = as.POSIXlt(date)$wday,
    weekday = factor(
      c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[weekday_number + 1L],
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    is_weekend = weekday %in% c("Saturday", "Sunday")
  ) |>
  select(-weekday_number) |>
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
snapshot_path <- file.path("data", "seoul_bike_snapshot.csv")
temporary_snapshot <- tempfile(tmpdir = "data", fileext = ".csv")
write_csv(bikes, temporary_snapshot, na = "")
if (file.exists(snapshot_path)) invisible(file.remove(snapshot_path))
if (!file.rename(temporary_snapshot, snapshot_path)) stop("Could not atomically replace snapshot")
writeLines(
  c(
    paste("snapshot_date:", Sys.Date()),
    paste("source:", source_url),
    "doi: https://doi.org/10.24432/C5F62R",
    "license: CC BY 4.0"
  ),
  file.path("data", "SOURCES.txt")
)

checksum <- digest::digest(snapshot_path, algo = "sha256", file = TRUE)
write_csv(
  tibble(file = snapshot_path, sha256 = checksum, rows = nrow(bikes), columns = ncol(bikes)),
  file.path("data", "checksums.csv")
)

