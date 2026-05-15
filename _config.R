# ==============================================================================
# GoiEner TFM - Configuracion centralizada del proyecto
# ==============================================================================
#
# Este archivo contiene rutas, parametros, paletas de colores, funciones
# de formato y utilidades compartidas por todos los scripts y QMDs del proyecto.
#
# Uso:
#   source(here::here("_config.R"))
# ==============================================================================

library(here)
library(fs)

options(encoding = "UTF-8", useFancyQuotes = FALSE)

# ==============================================================================
# Rutas del proyecto
# ==============================================================================

PROJ_DIR      <- here::here()
DATA_DIR      <- path(PROJ_DIR, "data")
RAW_DIR       <- path(DATA_DIR, "raw")
EXTERNAL_DIR  <- path(DATA_DIR, "external")
EXTRACTED_DIR <- path(DATA_DIR, "extracted", "imputed_goiener_v7")
PARQUET_DIR   <- path(DATA_DIR, "parquet")
CLIMATE_DIR   <- path(PARQUET_DIR, "climate")
FEATURES_DIR  <- path(PARQUET_DIR, "features")
DB_PATH       <- path(DATA_DIR, "goiener.duckdb")
DUCKDB_TEMP_DIR <- path(DATA_DIR, "duckdb_tmp")

# Rutas de salida
OUTPUT_DIR    <- path(PROJ_DIR, "outputs")
FIG_DIR       <- path(OUTPUT_DIR, "figures")
TABLE_DIR     <- path(OUTPUT_DIR, "tables")
MODEL_DIR     <- path(OUTPUT_DIR, "models")

# Asegurar que existen todos los directorios de salida
for (d in c(FIG_DIR, TABLE_DIR, MODEL_DIR, EXTERNAL_DIR, CLIMATE_DIR, FEATURES_DIR,
            DUCKDB_TEMP_DIR)) {
  if (!dir_exists(d)) dir_create(d, recurse = TRUE)
}

# Rutas a ficheros Parquet principales
HOURLY_GLOB              <- path(PARQUET_DIR, "year=*", "*.parquet") |> path_abs() |> path_norm()
DAILY_PARQUET            <- path(PARQUET_DIR, "daily_consumption.parquet")
MONTHLY_PARQUET          <- path(PARQUET_DIR, "monthly_consumption.parquet")
METADATA_PARQUET         <- path(PARQUET_DIR, "metadata.parquet")
USER_HOURLY_PROFILE      <- path(PARQUET_DIR, "user_hourly_profile.parquet")
USER_DOW_TYPE_PROFILE    <- path(PARQUET_DIR, "user_dow_type_profile.parquet")
USER_SEASON_PROFILE      <- path(PARQUET_DIR, "user_season_profile.parquet")
USER_WEEKDAY_PROFILE     <- path(PARQUET_DIR, "user_weekday_profile.parquet")
USER_REGION_MAP          <- path(PARQUET_DIR, "user_region_map.parquet")
PROVINCE_CONTEXT_CSV     <- path(EXTERNAL_DIR, "province_context.csv")
REFERENCE_MATRIX_CSV     <- path(EXTERNAL_DIR, "reference_matrix.csv")
MODEL_SCOPE_AUDIT_CSV    <- path(TABLE_DIR, "model_scope_audit.csv")

# Ficheros de clima y features
STATION_MAPPING_PARQUET  <- path(CLIMATE_DIR, "station_mapping.parquet")
DAILY_CLIMATE_PARQUET    <- path(CLIMATE_DIR, "daily_climate.parquet")
DAILY_CLIMATE_IMPUTED_PARQUET <- path(CLIMATE_DIR, "daily_climate_imputed.parquet")
AEMET_CACHE_DIR          <- path(CLIMATE_DIR, "aemet_cache")
DAILY_WITH_CLIMATE       <- path(FEATURES_DIR, "daily_with_climate.parquet")
USER_FEATURES_PARQUET    <- path(FEATURES_DIR, "user_features.parquet")
USER_CLUSTERS_PARQUET    <- path(FEATURES_DIR, "user_clusters.parquet")
CLIMATE_IMPUTATION_SUMMARY_CSV <- path(TABLE_DIR, "climate_imputation_summary.csv")
CLIMATE_IMPUTATION_BY_DATE_CSV <- path(TABLE_DIR, "climate_imputation_by_province_date.csv")

# Salidas V2 (nuevo pipeline modular)
USER_FEATURES_V2_PARQUET <- path(FEATURES_DIR, "user_features_v2.parquet")
USER_CLUSTERS_V2_PARQUET <- path(FEATURES_DIR, "user_clusters_v2.parquet")
PORTFOLIO_HOURLY_PARQUET <- path(FEATURES_DIR, "portfolio_hourly.parquet")
PORTFOLIO_DAILY_PARQUET  <- path(FEATURES_DIR, "portfolio_daily.parquet")
CLUSTER_DAILY_PARQUET    <- path(FEATURES_DIR, "cluster_daily.parquet")

if (!dir_exists(AEMET_CACHE_DIR)) dir_create(AEMET_CACHE_DIR, recurse = TRUE)

# ==============================================================================
# Parametros del proyecto
# ==============================================================================

# Rango temporal del dataset
YEAR_MIN <- 2014L
YEAR_MAX <- 2024L

# Umbrales para HDD/CDD (estandar europeo)
HDD_BASE <- 15.0
CDD_BASE <- 22.0

# Split temporal para forecasting
TRAIN_END   <- as.Date("2021-12-31")
VAL_START   <- as.Date("2022-01-01")
VAL_END     <- as.Date("2022-12-31")
TEST_START  <- as.Date("2023-01-01")
TEST_END    <- as.Date("2023-12-31")

# Clustering
MAX_K <- 10L  # Maximo numero de clusters a evaluar
CLUSTER_K_RANGE <- 2:6
CLUSTER_SAMPLE_SIZE <- 5000L
CLUSTER_SENSITIVITY_SAMPLE_SIZE <- 4000L
CLUSTER_KMEANS_NSTART <- 25L
CLUSTER_STABILITY_B <- 30L
CLUSTER_STABILITY_CANDIDATES <- 4L
CLUSTER_MIN_PCT_PER_CLUSTER <- 10.0
CLUSTER_MAX_PCT_PER_CLUSTER <- 75.0
CLUSTER_SCORE_WEIGHTS <- c(
  silhouette = 0.25,
  calinski_harabasz = 0.15,
  dunn = 0.15,
  balance = 0.45
)
CLUSTER_ENABLE_HOME_DAY_OPTIONAL <- FALSE
CLUSTER_CNAE_MIN_N <- 20L

# === Clustering V2 (R/05_clustering/*) ===
# Criterios de seleccion basados en calidad real, no en balance forzado.
CLUSTER_MIN_SILHOUETTE <- 0.15
CLUSTER_MIN_JACCARD    <- 0.75
CLUSTER_MIN_PCT_V2     <- 3.0
CLUSTER_MAX_PCT_V2     <- 55.0
CLUSTER_BOOTSTRAP_B    <- 30L
CLUSTER_ALGORITHMS     <- c("kmeans", "pam", "ward", "gmm", "hdbscan",
                            "fpca_kmeans")
CLUSTER_K_RANGE_V2     <- 2:7
CLUSTER_HDBSCAN_MINPTS <- c(50L, 100L, 200L)
# Umbral para segmentar vivienda habitual vs no habitual (segunda residencia /
# autoconsumo). Por debajo de este consumo medio diario los usuarios se
# tratan como segmento descriptivo "no_habitual" en lugar de clusterizarse.
MIN_DAILY_KWH_CLUSTER  <- 1.5

# === Forecasting V2 (R/06_forecasting/*) ===
# Datos disponibles 2015-12-31 -> 2024-01-31 (1 mes solo de 2024).
# Splits ajustados para tener un test set de ~7 meses con estacionalidad
# completa (verano 2023, invierno 2023/24, enero 2024).
TEST_END_V2   <- as.Date("2024-01-31")
TRAIN_END_V2  <- as.Date("2022-12-31")
VAL_START_V2  <- as.Date("2023-01-01")
VAL_END_V2    <- as.Date("2023-06-30")
TEST_START_V2 <- as.Date("2023-07-01")

# Precio medio OMIE EUR/MWh para metrica de impacto (configurable).
OMIE_AVG_PRICE_EUR_MWH <- 95
FORECAST_HOURLY_LAGS  <- c(1L, 24L, 48L, 168L, 336L)
FORECAST_DAILY_LAGS   <- c(1L, 2L, 7L, 14L, 28L, 365L)
FORECAST_HORIZON_HOURLY <- 24L
FORECAST_HORIZON_DAILY  <- 7L

# Rutas nuevas
R_LIB_DIR <- path(PROJ_DIR, "R", "_lib")

# Reproducibilidad: semilla global del proyecto
SEED <- 42L

# Filtro residencial estricto (TFM se centra en hogares)
# Tarifa 2.0TD = domÃ©stica. 3.0TD/6.1TD = comercial/industrial.
RESIDENTIAL_TARIFFS <- c("2.0TD", "2.0A", "2.0DHA", "2.0DHS",
                        "2.1A", "2.1DHA", "2.1DHS")
# Filtro permisivo (anexo): tarifa NA + p1_kw <= 10 kW
RESIDENTIAL_P1KW_MAX <- 15.0   # Umbral conservador para 2.0TD

# Calidad: lectura horaria sospechosa (kWh/h) para filtros de outliers
OUTLIER_KWH_HOUR  <- 100.0     # Pico horario inverosÃ­mil para residencial
OUTLIER_KWH_DAY   <- 200.0     # Consumo diario inverosÃ­mil para residencial
MIN_ACTIVE_DAYS   <- 180L      # MÃ­nimo de dÃ­as con dato para entrar a clustering

# Forecasting: cross-validation rolling-origin
CV_FOLDS          <- 5L        # NÃºmero de ventanas de validaciÃ³n
CV_HORIZON        <- 30L       # Horizonte (dÃ­as) por ventana
FORECAST_XGB_ENABLE_TUNING <- TRUE
FORECAST_XGB_MAX_GRID_ROWS <- 8L
FORECAST_XGB_GRID <- expand.grid(
  eta = c(0.03, 0.05),
  max_depth = c(3L, 5L),
  min_child_weight = c(3, 6),
  subsample = 0.85,
  colsample_bytree = 0.85,
  KEEP.OUT.ATTRS = FALSE
)
FORECAST_XGB_NROUNDS <- 1200L
FORECAST_XGB_EARLY_STOPPING <- 50L
FORECAST_INTERVAL_MIN_COVERAGE <- 85
FORECAST_CLUSTER_MIN_COVERAGE <- 85
FORECAST_AGG_INTERVAL_SAFETY_FACTOR <- 1.10
FORECAST_CLUSTER_INTERVAL_SAFETY_FACTOR <- 1.15
FORECAST_RULE_CLUSTER_INTERVAL_SAFETY_FACTOR <- 4.00

# DuckDB
DUCKDB_MEMORY_LIMIT <- "10GB"
DUCKDB_THREADS      <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
DUCKDB_PRESERVE_INSERTION_ORDER <- FALSE
DUCKDB_ENABLE_OBJECT_CACHE <- TRUE

# ==============================================================================
# Paleta de colores unificada
# ==============================================================================

PAL_MAIN   <- "#2C6E91"   # Azul oscuro: lineas y barras principales
PAL_ACCENT <- "#E8734A"   # Naranja: color de acento
PAL_FILL   <- "#B8D8E8"   # Azul claro: rellenos de fondo
PAL_SEASON <- c(
  "Invierno"   = "#3B82C4",
  "Primavera"  = "#5EBD72",
  "Verano"     = "#F5A623",
  "Otono"      = "#D35F5F"
)
PAL_CLUSTER <- c(
  "#2C6E91", "#E8734A", "#5EBD72", "#D35F5F",
  "#9B59B6", "#F5A623", "#1ABC9C", "#E74C3C",
  "#3498DB", "#2ECC71"
)

# ==============================================================================
# Tema ggplot2 personalizado
# ==============================================================================

theme_goiener <- function(base_size = 13) {
  `%+replace%` <- ggplot2::`%+replace%`
  ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.15),
                                               margin = ggplot2::margin(b = 6)),
      plot.subtitle    = ggplot2::element_text(color = "grey40", size = ggplot2::rel(0.9),
                                               margin = ggplot2::margin(b = 10)),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )
}

# ==============================================================================
# Funciones de formato para texto interpretativo
# ==============================================================================

fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}

fmt_int <- function(x) {
  format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  paste0(fmt_num(100 * x, digits = digits), "%")
}

# Tabla uniforme para informes Quarto.
# Evita que knitr::kable() se imprima como texto pipe dentro de chunks asis.
report_table <- function(x, ..., digits = getOption("digits"), escape = TRUE) {
  table_format <- if (requireNamespace("knitr", quietly = TRUE) &&
                      knitr::is_html_output()) {
    "html"
  } else {
    "pipe"
  }

  args <- c(
    list(
      x = x,
      format = table_format,
      digits = digits,
      escape = escape
    ),
    list(...)
  )

  if (identical(table_format, "html")) {
    args$table.attr <- paste(
      'class="table table-sm table-striped table-hover goiener-table"',
      'style="width:auto; font-size:0.92rem;"'
    )
  }

  do.call(knitr::kable, args)
}

# ==============================================================================
# Sistema de medicion de tiempos
# ==============================================================================

.timings_env <- new.env(parent = emptyenv())
.timings_env$timings <- list()

tic <- function(label) {
  .timings_env$timings[[label]] <- list(start = proc.time()["elapsed"])
}

toc <- function(label) {
  elapsed <- proc.time()["elapsed"] - .timings_env$timings[[label]]$start
  .timings_env$timings[[label]]$elapsed <- elapsed
  message(sprintf("[TIMING] %s: %.1f s", label, elapsed))
}

get_timings <- function() {
  .timings_env$timings
}

message("[CONFIG] Configuracion del proyecto cargada.")

# ==============================================================================
# Funcion para deduplicar metadata
# ==============================================================================
# Centralizada aqui para que scripts y QMDs usen la misma logica:
# - Priorizar filas con tarifa conocida
# - De esas, quedarse con la fecha_alta mas reciente
# - Extraer cod_provincia de los 2 primeros digitos del codigo postal

deduplicate_metadata <- function(meta_df) {
  meta_df |>
    dplyr::rename(user_id = cups) |>
    dplyr::mutate(
      cod_provincia = ifelse(
        is.na(codigo_postal),
        NA_character_,
        substr(sprintf("%05d", as.integer(codigo_postal)), 1, 2)
      ),
      tarifa_clean = ifelse(
        is.na(tarifa_atr) | tarifa_atr == "",
        "Desconocida",
        tarifa_atr
      )
    ) |>
    dplyr::arrange(user_id, !is.na(tarifa_atr), dplyr::desc(fecha_alta)) |>
    dplyr::group_by(user_id) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()
}

# ==============================================================================
# Nombres de provincias espanolas (para mapeos)
# ==============================================================================

PROVINCIA_NOMBRES <- c(
  "01" = "Alava",       "02" = "Albacete",    "03" = "Alicante",
  "04" = "Almeria",     "05" = "Avila",       "06" = "Badajoz",
  "07" = "Baleares",    "08" = "Barcelona",   "09" = "Burgos",
  "10" = "Caceres",     "11" = "Cadiz",       "12" = "Castellon",
  "13" = "Ciudad Real", "14" = "Cordoba",     "15" = "A Coruna",
  "16" = "Cuenca",      "17" = "Girona",      "18" = "Granada",
  "19" = "Guadalajara", "20" = "Gipuzkoa",    "21" = "Huelva",
  "22" = "Huesca",      "23" = "Jaen",        "24" = "Leon",
  "25" = "Lleida",      "26" = "La Rioja",    "27" = "Lugo",
  "28" = "Madrid",      "29" = "Malaga",      "30" = "Murcia",
  "31" = "Navarra",     "32" = "Ourense",     "33" = "Asturias",
  "34" = "Palencia",    "35" = "Las Palmas",  "36" = "Pontevedra",
  "37" = "Salamanca",   "38" = "S.C.Tenerife","39" = "Cantabria",
  "40" = "Segovia",     "41" = "Sevilla",     "42" = "Soria",
  "43" = "Tarragona",   "44" = "Teruel",      "45" = "Toledo",
  "46" = "Valencia",    "47" = "Valladolid",  "48" = "Bizkaia",
  "49" = "Zamora",      "50" = "Zaragoza",    "51" = "Ceuta",
  "52" = "Melilla"
)

# ==============================================================================
# Alcance de modelado del TFM
# ==============================================================================

# El EDA mantiene contexto global, pero clima, feature engineering, clustering y
# forecasting se focalizan en las provincias con mayor masa estadistica.
MODEL_SCOPE_NAME <- "top5_provincias_mayor_masa_usuarios"
FOCUS_PROVINCES <- c("20", "48", "31", "01", "28")
FOCUS_PROVINCE_NAMES <- unname(PROVINCIA_NOMBRES[FOCUS_PROVINCES])

normalize_province_code <- function(x) {
  x_chr <- trimws(as.character(x))
  out <- ifelse(
    !is.na(x_chr) & grepl("^[0-9]+$", x_chr),
    sprintf("%02d", as.integer(x_chr)),
    x_chr
  )
  out[out %in% c("", "NA")] <- NA_character_
  out
}

focus_provinces_sql <- function() {
  paste0("'", FOCUS_PROVINCES, "'", collapse = ", ")
}

focus_province_filter_sql <- function(column = "cod_provincia") {
  sprintf("%s IN (%s)", column, focus_provinces_sql())
}

write_model_scope_audit <- function(province_counts,
                                    output_path = MODEL_SCOPE_AUDIT_CSV) {
  audit <- as.data.frame(province_counts, stringsAsFactors = FALSE)
  names(audit) <- sub("^n$", "n_usuarios", names(audit))
  if (!all(c("cod_provincia", "n_usuarios") %in% names(audit))) {
    stop("province_counts debe contener cod_provincia y n_usuarios.")
  }

  audit$cod_provincia <- normalize_province_code(audit$cod_provincia)
  audit$n_usuarios <- as.integer(audit$n_usuarios)
  audit <- audit[!is.na(audit$cod_provincia), , drop = FALSE]
  audit <- audit[order(-audit$n_usuarios, audit$cod_provincia), , drop = FALSE]

  total_users <- sum(audit$n_usuarios, na.rm = TRUE)
  audit$in_model_scope <- audit$cod_provincia %in% FOCUS_PROVINCES
  scope_user_total <- sum(audit$n_usuarios[audit$in_model_scope], na.rm = TRUE)
  excluded_users <- total_users - scope_user_total

  audit$scope_name <- MODEL_SCOPE_NAME
  audit$provincia <- unname(PROVINCIA_NOMBRES[audit$cod_provincia])
  audit$pct_total_users <- round(100 * audit$n_usuarios / total_users, 2)
  audit$scope_user_total <- scope_user_total
  audit$total_users <- total_users
  audit$excluded_users <- excluded_users
  audit$scope_pct_total <- round(100 * scope_user_total / total_users, 2)

  audit <- audit[, c(
    "scope_name", "cod_provincia", "provincia", "in_model_scope",
    "n_usuarios", "pct_total_users", "scope_user_total", "total_users",
    "excluded_users", "scope_pct_total"
  )]

  utils::write.csv(audit, output_path, row.names = FALSE)
  audit
}

# ==============================================================================
# Funcion para conectar a DuckDB con configuracion estandar
# ==============================================================================

connect_duckdb <- function(dbdir = ":memory:", read_only = FALSE) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir, read_only = read_only)
  DBI::dbExecute(con, sprintf("SET memory_limit = '%s'", DUCKDB_MEMORY_LIMIT))
  DBI::dbExecute(con, sprintf("SET threads = %d", DUCKDB_THREADS))
  DBI::dbExecute(con, sprintf("SET temp_directory = '%s'", path_norm(DUCKDB_TEMP_DIR)))
  DBI::dbExecute(con, sprintf(
    "SET preserve_insertion_order = %s",
    tolower(as.character(DUCKDB_PRESERVE_INSERTION_ORDER))
  ))
  DBI::dbExecute(con, sprintf(
    "SET enable_object_cache = %s",
    tolower(as.character(DUCKDB_ENABLE_OBJECT_CACHE))
  ))
  con
}

# ==============================================================================
# Silenciar warnings "no visible binding" del linter (NSE de tidyverse).
# Estas variables son nombres de columnas o pronombres usados en filter/mutate/
# aes() que el anÃ¡lisis estÃ¡tico no resuelve.
# ==============================================================================

utils::globalVariables(c(
  # Pronombres y variables de NSE
  ".", ".data", ".env",
  # Identificadores y columnas crudas
  "user_id", "cups", "fecha", "fecha_alta", "fecha_baja", "hora",
  "codigo_postal", "cod_provincia", "tarifa_atr", "tarifa_clean",
  "cnae", "p1_kw", "p2_kw", "p3_kw", "p4_kw", "p5_kw", "p6_kw",
  "kWh", "kwh", "consumo", "indicativo", "provincia", "year", "month", "day",
  "fecha_inicio", "fecha_fin",
  # Features
  "mean_daily_kWh", "log_mean_daily_kWh", "median_daily_kWh", "cv_daily",
  "ratio_night_day", "ratio_weekend_weekday", "peak_hour",
  "seasonal_amplitude_norm", "seasonal_amp_norm",
  "active_days", "n_days", "n_obs", "is_residential_strict",
  "is_residential_permissive", "has_sustained_extreme",
  "corr_hdd", "corr_cdd", "norm_kWh", "hour_label", "hour", "h", "v",
  "zero_day_rate", "low_day_rate", "max_month_share", "monthly_entropy",
  "summer_winter_ratio", "peak_share", "flat_share", "valley_share",
  "peak_to_valley_ratio",
  "morning_kWh_share", "afternoon_kWh_share", "evening_kWh_share",
  "night_kWh_share", "ratio_morning_afternoon", "ratio_evening_morning",
  "ratio_night_morning", "holiday_ratio", "bridge_ratio",
  "weekday_peak_hour", "weekend_peak_hour", "weekday_weekend_peak_shift",
  "low_consumption_spell_rate", "possible_intermittent_home",
  "proxy_autoconsumption_second_home",
  "ccaa", "coastal_flag", "density_bucket", "climate_zone",
  "goiener_core_region",
  # Clima
  "tmed", "tmax", "tmin", "prec", "hrMedia", "thi",
  "is_heatwave", "is_coldwave", "amplitud_termica",
  "hdd", "cdd", "season", "is_holiday", "is_bridge_day",
  "is_easter_window", "days_to_holiday", "daylight_hours",
  # Clustering
  "cluster", "cluster_kmeans", "cluster_pam", "cluster_hclust",
  "cluster_gmm", "cluster_hdbscan", "dist_to_centroid", "p95_dist",
  "is_anomalous", "metric", "metrica", "value", "valor",
  "rank_consenso", "silhouette_avg", "davies_bouldin",
  "calinski_harabasz", "dunn", "gap", "gap_se", "wss",
  "balance_entropy", "min_cluster_pct", "preprocessing",
  "selected_current_pipeline",
  "cnae_clean", "cnae_division", "cnae_section", "cnae_section_label",
  "cnae_business_family", "cnae_known", "known_users_cluster",
  "n_cnae_known", "n_cnae_unknown", "coverage_pct", "pct_cluster",
  "pct_cluster_all", "pct_global", "pct_point_diff", "enrichment_ratio",
  "support_ok", "global_support_ok", "is_interpretable", "rank_enrichment",
  "business_question", "behavioral_signal", "cnae_signal", "goiener_action",
  "forecasting_or_flexibility_link", "caveat",
  "question_id", "reference_theme", "reference_rows", "reference_basis",
  "smart_meter_rationale", "repo_evidence", "applicable_output",
  "conclusion_rule", "decision_scope", "question_caveat",
  "evidence_strength", "is_primary_question", "cluster_evidence",
  "recommended_conclusion", "n_anomalous", "anomaly_rate_pct",
  "has_forecast_alert", "has_supported_cnae", "has_cnae_enrichment",
  "strength_order",
  # Forecasting
  "actual", "pred_naive", "pred_naive365", "pred_arima",
  "pred_rf", "pred_xgb", "pred_xgb_q05", "pred_xgb_q95",
  "model", "mae", "rmse", "mape", "mase",
  "feature", "importance", "Gain", "Feature",
  "season_label", "dow_label", "residual", "abs_residual",
  "qhat_mean_user_kWh_season", "n_calibration_season",
  "slice_type", "slice_value", "evidence_item", "grid_id",
  # Otros
  "n", "pct", "tipo", "n_usuarios", "pct_usuarios", "paso"
))

