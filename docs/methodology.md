# Methodology

## Objective

Estimate hourly rentals on operating days to support capacity planning and eventual bicycle rebalancing. The target is `rented_bike_count`; inputs are calendar and contemporaneous weather measurements.

## Leakage controls

1. Sort observations by date and hour.
2. Reserve the latest 20% of complete dates for final evaluation.
3. Tune Elastic Net and random forest on four expanding windows inside the training period.
4. Place a one-day gap before each 28-day validation window.
5. Fit finalized workflows on all pre-holdout observations and evaluate once on the future holdout.

This design mirrors deployment more closely than random cross-validation. Because the dataset contains observed rather than forecast weather, reported performance is conditional on weather inputs being available at prediction time.

## Features and preprocessing

The model uses hour, weekday, month, season, holiday, weekend, elapsed day, temperature, humidity, wind, visibility, dew point, solar radiation, rainfall, and snowfall. Categorical predictors receive unknown/novel handling and dummy encoding. Zero-variance and linearly dependent columns are removed; numeric predictors are normalized.

## Candidates

| Candidate | Role |
|---|---|
| Weekday-hour calendar average | Operationally meaningful benchmark |
| Linear regression | Transparent additive reference |
| Elastic Net | Regularized linear alternative |
| Random forest | Nonlinear interactions and thresholds |

Tuning minimizes resampled RMSE. The final table reports RMSE, MAE, traditional R², WAPE, signed bias, and MAE on observations above the training-set 90th percentile.

## Reproducibility

The source archive is downloaded from UCI, checked against its expected schema, transformed atomically, and written with provenance and SHA-256 checksums. `renv.lock`, automated tests, Quarto rendering, and the Connect manifest pin the analytical and deployment contract.

