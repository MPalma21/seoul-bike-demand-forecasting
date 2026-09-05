# Model card

## Intended use

Planning signal for hourly aggregate bicycle demand on functioning days in Seoul. Appropriate for scenario analysis, staffing discussions, and as one input to a richer station-level optimization system.

## Not intended for

- Autonomous bicycle dispatch or station-level inventory decisions
- Safety-critical or individual-level decisions
- Forecasting closures, new geographies, or structural changes without retraining
- Causal claims about weather or calendar effects

## Training and evaluation

One year of hourly UCI observations (December 2017–November 2018). The latest 20% of dates forms the final holdout. Hyperparameters and the XGBoost calibration are estimated using expanding temporal validation with a one-day gap. The published site calculates the current metrics from the committed snapshot.

## Key risks

- Observed weather gives an optimistic proxy for a real forecast-weather pipeline.
- Aggregate counts hide station shortages and trip flows.
- A single year may not represent long-term trend or rare events.
- Peak demand is harder to estimate and systematic under-forecasting can create stockouts.

## Monitoring recommendation

Track RMSE, WAPE, bias, and peak MAE weekly and by season. Alert on sustained bias, abrupt feature drift, or error outside operational tolerance. Compare every retrain with the calendar baseline and retain the previous model until the replacement passes future-period evaluation.

