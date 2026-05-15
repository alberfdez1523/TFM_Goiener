# TFM GoiEner: pipeline de datos, modelos e informes

Este proyecto procesa lecturas horarias seudonimizadas de GoiEner, las convierte a Parquet, enriquece los datos con clima, genera features residenciales, segmenta hogares por patrones de consumo y entrena modelos de forecasting diario. Los informes finales se renderizan con Quarto desde los `.qmd` de `qmd/`.

## Requisitos

- R 4.5.x con los paquetes indicados en `.lintr` y usados por los scripts: `arrow`, `duckdb`, `DBI`, `dplyr`, `tidyr`, `fs`, `glue`, `lubridate`, `timetk`, `ggplot2`, `ranger`, `xgboost`, `cluster`, `fpc`, `bench`, `testthat`, entre otros.
- Quarto 1.8.x o compatible.
- Entradas locales:
  - `data/raw/imputed_goiener_v7.tar.zst`
  - `data/raw/metadata.csv`
- Variable `AEMET_API_KEY` en `.Renviron` o en el entorno antes de ejecutar el paso climático.

En esta máquina `Rscript` no está en el PATH de PowerShell. Usa la ruta explícita:

```powershell
$RSCRIPT = "C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
```

## Pipeline

1. `R/00_extract_raw.R`
   Extrae `data/raw/imputed_goiener_v7.tar.zst` en `data/extracted/imputed_goiener_v7/`. Este paso ahora contiene la lógica completa de extracción y no delega en scripts de raíz.

2. `R/01_csv_to_parquet.R`
   Convierte los CSV horarios y `metadata.csv` a Parquet usando DuckDB. Genera `data/parquet/year=*/`, `daily_consumption.parquet`, `monthly_consumption.parquet`, perfiles horarios preagregados y `metadata.parquet`. Usa `_config.R` para rutas y configuración de DuckDB.

3. `R/02_data_quality.R`
   Calcula cobertura temporal, gaps, días incompletos, anomalías y resúmenes de calidad. Escribe tablas en `outputs/tables/`.

4. `R/03_climate/03a_build_climate_dataset.R` y `R/03b_augment_climate.R`
   Descargan y preparan datos AEMET por estación/provincia, calculan HDD/CDD y variables climáticas derivadas. El paso 03 cachea cada estación en `data/parquet/climate/aemet_cache/` y escribe auditoría de descargas en `outputs/tables/climate_download_audit.csv`.

5. `R/03_climate/03c_impute_climate.R`
   Completa la rejilla diaria provincia-estación con `timetk`, imputa solo variables climáticas exógenas, recalcula derivadas y guarda auditorías de imputación. No imputa consumo ni objetivos de forecasting.

6. `R/03_climate/03d_merge_climate.R`
   Une consumo diario, metadata, calendario y clima en `data/parquet/features/daily_with_climate.parquet`.

7. `R/04_features/04a_legacy_features.R`
   Construye features por usuario para clustering: consumo, forma horaria, estacionalidad, ratios, cobertura, calidad y sensibilidad climática. Escribe `user_features.parquet`.

8. `R/05_clustering/05a_pool.R`
   Segmenta hogares residenciales con un enfoque determinista más clustering robusto, fija la semilla desde `_config.R`, exporta modelos, métricas, figuras, `cluster_sensitivity.csv` y `user_clusters.parquet`.

9. `R/06_forecasting/06a_targets.R`
   Entrena baselines, Random Forest, XGBoost con tuning reproducible, intervalos conformales, validación rolling-origin y modelos por cluster. Exporta `forecast_tuning_results.csv`, `forecast_error_slices.csv` y `model_evidence_summary.csv` para trazabilidad metodológica.

10. `R/07_benchmark/07_benchmark.R`
   Compara lectura CSV, Parquet y DuckDB, y resume tiempos y tamaños en disco.

## Ejecución

Ejecuta el pipeline completo desde la raíz del proyecto:

```powershell
$RSCRIPT = "C:\Program Files\R\R-4.5.1\bin\Rscript.exe"

& $RSCRIPT R\00_extract_raw.R 2>&1 | Tee-Object outputs\_log_R00.txt
& $RSCRIPT R\01_csv_to_parquet.R 2>&1 | Tee-Object outputs\_log_R01.txt
& $RSCRIPT R\02_data_quality.R 2>&1 | Tee-Object outputs\_log_R02.txt
& $RSCRIPT R\03_climate\03a_build_climate_dataset.R 2>&1 | Tee-Object outputs\_log_R03.txt
& $RSCRIPT R\03b_augment_climate.R 2>&1 | Tee-Object outputs\_log_R03b.txt
& $RSCRIPT R\03_climate\03c_impute_climate.R 2>&1 | Tee-Object outputs\_log_R03c.txt
& $RSCRIPT R\03_climate\03d_merge_climate.R 2>&1 | Tee-Object outputs\_log_R04.txt
& $RSCRIPT R\04_features\04a_legacy_features.R 2>&1 | Tee-Object outputs\_log_R05.txt
& $RSCRIPT R\05_clustering\05a_pool.R 2>&1 | Tee-Object outputs\_log_R06.txt
& $RSCRIPT R\06_forecasting\06a_targets.R 2>&1 | Tee-Object outputs\_log_R07.txt
& $RSCRIPT R\07_benchmark\07_benchmark.R 2>&1 | Tee-Object outputs\_log_R08.txt
```

Renderiza los informes:

```powershell
quarto render qmd\01_eda.qmd
quarto render qmd\02_data_quality.qmd
quarto render qmd\03_climate_integration.qmd
quarto render qmd\04_clustering.qmd
quarto render qmd\05_forecasting.qmd
quarto render qmd\06_benchmark.qmd
quarto render qmd\07_etica_privacidad.qmd
```

## Validación

```powershell
quarto check
& $RSCRIPT -e "testthat::test_dir('tests/testthat')"
```

Los informes HTML se generan junto a sus fuentes en `qmd/`. Las tablas, figuras, modelos y logs operativos se escriben en `outputs/`.

## Organización

- `_config.R`: rutas, parámetros, semillas, paleta, funciones de formato y conexión DuckDB.
- `R/`: pipeline ejecutable y numerado.
- `qmd/`: informes Quarto.
- `tests/`: tests de configuración y split temporal.
- `data/`: datos crudos, extraídos y Parquet locales.
- `outputs/`: artefactos reproducibles del análisis.

Se eliminaron duplicados de raíz y documentación textual fragmentada para que la fuente de verdad sea el pipeline numerado más este README.
