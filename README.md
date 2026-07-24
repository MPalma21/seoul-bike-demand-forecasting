# Seoul Bike Demand Prediction / Predicción de demanda de bicicletas en Seúl

Bilingual Quarto machine-learning project using a temporal holdout to compare a mean baseline, linear regression, Elastic Net, and random forest. Metrics remain interpretable in rented bicycles.

Proyecto bilingüe de aprendizaje automático con holdout temporal, regresión lineal, Elastic Net y random forest. Las métricas se conservan en bicicletas alquiladas.

## Reproduce / Reproducir

```bash
Rscript data-raw/prepare-data.R
Rscript -e "renv::restore(); testthat::test_dir('tests/testthat')"
quarto render
```

## Sources / Fuentes

- UCI Seoul Bike Sharing Demand: https://doi.org/10.24432/C5F62R
- License: CC BY 4.0
- Original publication: https://rpubs.com/MPalmaR19/1306629
- GitHub: https://github.com/MPalma21/seoul-bike-demand-prediction
- Posit Connect Cloud: added after deployment.

