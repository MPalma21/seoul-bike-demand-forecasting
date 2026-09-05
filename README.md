# Seoul Bike Demand Forecasting

[![Validate](https://github.com/MPalma21/seoul-bike-demand-forecasting/actions/workflows/validate.yml/badge.svg)](https://github.com/MPalma21/seoul-bike-demand-forecasting/actions/workflows/validate.yml)
[![R 4.6.1](https://img.shields.io/badge/R-4.6.1-276DC3.svg)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-bilingual-39729E.svg)](https://quarto.org/)
[![License: MIT](https://img.shields.io/badge/code-MIT-green.svg)](LICENSE)

An end-to-end, bilingual forecasting case study for Seoul's public bicycle demand. The project turns an hourly UCI snapshot into an operational model comparison with leakage-safe temporal validation, business-facing error metrics, diagnostics, tests, and a deployable Quarto site.

## What makes the analysis credible

- The final 20% of complete dates is reserved as a strictly future holdout.
- Hyperparameters use expanding training windows followed by 28-day validation periods and a one-day gap.
- A weekday-hour benchmark keeps model gains honest.
- RMSE, MAE, R², WAPE, bias, and peak-demand MAE expose different operating risks.
- Predictions are constrained to nonnegative counts and closures are handled outside the demand model.
- The committed dataset is validated and checksummed for reproducibility.

## Modeling flow

```text
UCI source → validated snapshot → calendar features → future holdout
                                         └→ expanding resampling → tuning
future holdout + finalized models → operational metrics → diagnostics → site
```

Candidates: calendar baseline, linear regression, Elastic Net, and random forest. The interactive narrative reports the current winning model and interprets remaining peak-hour risk.

## Reproduce locally

Requirements: R 4.6.1 and Quarto 1.8 or newer.

```bash
Rscript -e "install.packages('renv'); renv::restore()"
Rscript tests/testthat.R
quarto render
```

To rebuild the governed snapshot from UCI:

```bash
Rscript data-raw/prepare-data.R
```

## Repository map

| Path | Purpose |
|---|---|
| `R/analysis.R` | Validation, feature engineering, temporal resampling, models, metrics, and charts |
| `data/` | Versioned modeling snapshot, provenance, and checksums |
| `data-raw/` | Repeatable UCI ingestion and validation |
| `index.qmd`, `es/index.qmd` | Visible-code English and Spanish case studies |
| `docs/` | Methodology, model card, and data dictionary |
| `tests/` | Data-contract and modeling tests |
| `manifest.json` | Posit Connect Cloud deployment manifest |

## Data and responsible use

Source: [UCI Seoul Bike Sharing Demand](https://doi.org/10.24432/C5F62R), licensed CC BY 4.0. The dataset covers one year and lacks station-level inventory, routes, events, pricing, and forecast-weather inputs. This repository is a portfolio-grade forecasting demonstration, not a production dispatch system.

Code is available under the [MIT License](LICENSE). Dataset attribution and license remain governed by UCI's terms.
