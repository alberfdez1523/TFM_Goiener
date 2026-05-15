# TFM GoiEner: código, datos y ejecución

Este repositorio contiene el código usado para el TFM sobre consumo eléctrico horario de GoiEner. El flujo parte de lecturas horarias seudonimizadas, las convierte a Parquet, cruza clima de AEMET, construye variables por usuario, segmenta hogares residenciales y entrena modelos de predicción de demanda. Al final genera tablas, figuras, modelos, logs e informes HTML con Quarto.

El proyecto no descarga los datos de GoiEner por sí solo. Primero hay que dejar los ficheros de entrada en `data/raw/`. Después se ejecuta el orquestador `R/99_orchestrator.R` o, si interesa revisar una parte concreta, cada fase por separado.

## Dataset que hay que descargar

El pipeline usa dos archivos de entrada, ambos colocados en `data/raw/`:

```text
data/raw/imputed_goiener_v7.tar.zst
data/raw/metadata.csv
```

`imputed_goiener_v7.tar.zst` contiene los CSV horarios imputados que después extrae `R/00_extract/00_extract_raw.R`. `metadata.csv` contiene los metadatos contractuales y geográficos que usa `R/01_ingest/01_csv_to_parquet.R` para cruzar tarifas, potencias contratadas, provincias y fechas de contrato.

El dataset público de referencia es:

- Nombre: `GoiEner smart meters data`
- Autores: Carlos Quesada Granja, Cruz Enrique Borges Hernández, Leire Astigarraga y Chris Merveille
- DOI: `10.5281/zenodo.7362094`
- URL del registro: https://zenodo.org/records/7362094
- Licencia indicada en Zenodo: Creative Commons Attribution 4.0 International

En este proyecto no se dejan los ficheros imputados de Zenodo como tres archivos separados (`imp-pre.tzst`, `imp-in.tzst`, `imp-post.tzst`). Se trabaja con el comprimido consolidado `imputed_goiener_v7.tar.zst`, ya preparado con ese nombre para que el paso 00 pueda extraerlo directamente. El archivo no se guarda en Git por tamaño.

`metadata.csv` se descarga desde el mismo registro de Zenodo:

```text
https://zenodo.org/records/7362094/files/metadata.csv?download=1
```

Antes de ejecutar nada, revisa que la carpeta `data/raw/` tenga este aspecto:

```text
data/raw/
  imputed_goiener_v7.tar.zst
  metadata.csv
```

Hay otro registro de Zenodo, `GoiEner smart meters raw data`, con DOI `10.5281/zenodo.7859413` y URL https://zenodo.org/records/7859413. Ese registro contiene `GoiEner.zip` con ficheros SIMEL crudos. No es la entrada normal de este pipeline; sirve si quieres rehacer el preprocesado desde el origen bruto.

## Otros datos externos

El bloque climático usa AEMET OpenData mediante el paquete `climaemet`. Hace falta una clave de API de AEMET:

- Alta de usuario AEMET OpenData: https://opendata.aemet.es/centrodedescargas/altaUsuario

Guarda la clave en un fichero `.Renviron` en la raíz del proyecto:

```text
AEMET_API_KEY=tu_clave_de_aemet
```

No subas ese fichero al repositorio. La clave queda fuera del código y los scripts la leen con `Sys.getenv("AEMET_API_KEY")`.

Los CSV de contexto que ya están en `data/external/` (`province_context.csv` y `reference_matrix.csv`) forman parte del repositorio de trabajo. No hay que descargarlos de Zenodo.

## Requisitos

El proyecto se ha ejecutado con R 4.5.x y Quarto 1.8.x. En Windows, si `Rscript` no está en el PATH, usa la ruta completa:

```powershell
$RSCRIPT = "C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
```

Si `Rscript` sí está disponible en consola:

```powershell
$RSCRIPT = "Rscript"
```

Paquetes principales de R:

```r
install.packages(c(
  "archive", "arrow", "bench", "climaemet", "cluster", "DBI", "dbscan",
  "dplyr", "duckdb", "fpc", "fs", "ggplot2", "glue", "here",
  "lubridate", "mclust", "purrr", "ranger", "readr", "scales",
  "stringr", "testthat", "tibble", "tidyr", "timetk", "xgboost"
))
```

Comprueba Quarto antes de renderizar informes:

```powershell
quarto check
```

## Estructura del repositorio

```text
TFM_codigo/
  _config.R                  # rutas, parámetros, semillas y utilidades comunes
  R/
    00_extract/              # extracción del comprimido horario
    01_ingest/               # conversión CSV -> Parquet y agregados base
    02_quality/              # calidad del dato, cobertura y anomalías
    03_climate/              # descarga, imputación y cruce climático
    04_features/             # variables por usuario y sensibilidad climática
    05_clustering/           # matrices, modelos, validación y lectura de segmentos
    06_forecasting/          # targets, features temporales, modelos y evaluación
    07_benchmark/            # comparación de almacenamiento y consulta
    99_orchestrator.R        # ejecuta fases R y documentos Quarto
    _lib/                    # funciones compartidas
  qmd/                       # informes Quarto renderizados a HTML
  app/shiny/                 # panel Shiny opcional
  data/
    raw/                     # datos originales locales; no se versionan
    extracted/               # CSV extraídos del comprimido
    parquet/                 # datos transformados para análisis
    external/                # tablas pequeñas de contexto
  outputs/
    tables/                  # CSV de resultados y auditorías
    figures/                 # figuras usadas en informes y memoria
    models/                  # modelos entrenados y objetos auxiliares
    logs/                    # logs de ejecución del orquestador
  tests/testthat/            # pruebas de configuración y salidas esperadas
```

`_config.R` es la pieza común. Si cambias rutas, años, semillas, límites de memoria de DuckDB o cortes temporales de entrenamiento/validación/test, hazlo ahí y no dentro de cada script.

## Preparación paso a paso

Abre una terminal en la raíz del proyecto:

```powershell
Set-Location "C:\Users\alber\Desktop\TFM\TFM_codigo"
```

Crea las carpetas necesarias si estás empezando desde cero:

```powershell
New-Item -ItemType Directory -Force data\raw, data\external, outputs\logs | Out-Null
```

Coloca las entradas de GoiEner:

```text
data/raw/imputed_goiener_v7.tar.zst
data/raw/metadata.csv
```

Después crea `.Renviron` con `AEMET_API_KEY` si vas a ejecutar la fase climática. Sin esa clave, el paso 03 se detendrá al intentar descargar datos de AEMET.

## Ejecución completa

Para ejecutar scripts R e informes Quarto en orden:

```powershell
& $RSCRIPT R\99_orchestrator.R
```

Para ejecutar solo el pipeline R, sin renderizar HTML:

```powershell
& $RSCRIPT R\99_orchestrator.R --r-only
```

Para renderizar solo los informes Quarto, suponiendo que las tablas y figuras ya existen:

```powershell
& $RSCRIPT R\99_orchestrator.R --qmd-only
```

El orquestador escribe un log por script en `outputs/logs/` y una tabla de tiempos en:

```text
outputs/tables/pipeline_timings.csv
```

Si una fase falla, el orquestador se detiene. Para continuar y revisar todos los fallos al final:

```powershell
& $RSCRIPT R\99_orchestrator.R --keep-going
```

## Ejecución por fases

El orquestador permite lanzar una fase concreta. Ejemplos:

```powershell
& $RSCRIPT R\99_orchestrator.R 00 --r-only
& $RSCRIPT R\99_orchestrator.R 03 --r-only
& $RSCRIPT R\99_orchestrator.R 05 --r-only
& $RSCRIPT R\99_orchestrator.R qmd05 --qmd-only
```

Resumen de fases:

| Fase | Scripts | Qué produce |
| --- | --- | --- |
| 00 | `R/00_extract/00_extract_raw.R` | Extrae los CSV horarios en `data/extracted/imputed_goiener_v7/`. |
| 01 | `R/01_ingest/01_csv_to_parquet.R` | Crea Parquet horario particionado por año, consumo diario/mensual, perfiles base y `metadata.parquet`. |
| 02 | `R/02_quality/02_data_quality.R` | Calcula cobertura, huecos, días incompletos, outliers y tablas de calidad. |
| 03 | `R/03_climate/03a` a `03d` | Descarga AEMET, imputa clima, calcula HDD/CDD y une clima con consumo diario. |
| 04 | `R/04_features/04a`, `04b` | Genera variables por usuario, contexto provincial y sensibilidad climática. |
| 05 | `R/05_clustering/05a` a `05h` | Construye matrices, entrena modelos de clustering, valida estabilidad e interpreta segmentos. |
| 06 | `R/06_forecasting/06a` a `06f` | Prepara targets, entrena modelos diarios/horarios/por cluster y evalúa error e intervalos. |
| 07 | `R/07_benchmark/07_benchmark.R` | Compara CSV, Parquet y DuckDB en tamaño, tiempo y memoria. |

Documentos Quarto:

| ID | Documento | Tema |
| --- | --- | --- |
| qmd01 | `qmd/01_eda.qmd` | Exploración inicial. |
| qmd02 | `qmd/02_data_quality.qmd` | Calidad del dato. |
| qmd03 | `qmd/03_climate_integration.qmd` | Integración climática. |
| qmd04 | `qmd/04_clustering.qmd` | Segmentación de hogares. |
| qmd05 | `qmd/05_forecasting.qmd` | Predicción de demanda. |
| qmd06 | `qmd/06_benchmark.qmd` | Benchmark técnico. |
| qmd07 | `qmd/07_etica_privacidad.qmd` | Ética y privacidad. |

## Qué revisar después de ejecutar

Los artefactos principales quedan en estas rutas:

```text
data/parquet/year=*/
data/parquet/daily_consumption.parquet
data/parquet/features/daily_with_climate.parquet
data/parquet/features/user_features.parquet
data/parquet/features/user_clusters.parquet
outputs/tables/
outputs/figures/
outputs/models/
outputs/logs/
qmd/*.html
```

Para comprobar que las pruebas básicas pasan:

```powershell
& $RSCRIPT tests\testthat.R
```

También puedes ejecutar `testthat` directamente:

```powershell
& $RSCRIPT -e "testthat::test_dir('tests/testthat')"
```

## Uso del panel Shiny

El panel de consulta está en `app/shiny/`. Úsalo después de generar `data/parquet/` y `outputs/`:

```powershell
& $RSCRIPT -e "shiny::runApp('app/shiny')"
```

Si la app arranca pero muestra paneles vacíos, casi siempre falta una tabla de `outputs/tables/` o una figura de `outputs/figures/`. Ejecuta primero `R/99_orchestrator.R --r-only`.

## Reglas prácticas para no romper la reproducción

- No subas `data/raw/`, `data/extracted/`, `data/parquet/`, `data/goiener.duckdb` ni claves API.
- No cambies nombres de columnas en tablas intermedias sin revisar los QMD y la app Shiny.
- Si repites una fase avanzada, asegúrate de que las fases anteriores generaron sus Parquet y CSV.
- Los resultados de forecasting dependen de los cortes temporales definidos en `_config.R`.
- Los datos son seudonimizados, pero siguen describiendo hábitos de consumo. Trabaja con agregados, clusters o cartera; evita publicar series de usuarios individuales.

## Cita del dataset

Para citar el dataset usado como fuente principal:

```text
Quesada Granja, C., Borges Hernández, C. E., Astigarraga, L., & Merveille, C. (2022).
GoiEner smart meters data (Version 1) [Data set]. Zenodo.
https://doi.org/10.5281/zenodo.7362094
```

Para el registro bruto, si lo usas:

```text
Quesada Granja, C., Borges Hernández, C. E., Astigarraga, L., & Merveille, C. (2023).
GoiEner smart meters raw data (Version 4) [Data set]. Zenodo.
https://doi.org/10.5281/zenodo.7859413
```