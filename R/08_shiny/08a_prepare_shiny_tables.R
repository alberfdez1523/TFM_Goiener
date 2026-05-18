# ==============================================================================
# Preparacion de tablas ligeras para Shiny
# ==============================================================================
#
# Genera resumenes EDA pequenos para que la app desplegada no tenga que abrir
# Parquet grandes en shinyapps.io.
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(fs)
  library(glue)
  library(readr)
  library(here)
})

source(here::here("_config.R"))

required_files <- c(DAILY_PARQUET, USER_HOURLY_PROFILE, METADATA_PARQUET)
missing_files <- required_files[!fs::file_exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Faltan Parquet necesarios para preparar Shiny: ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

daily_path <- fs::path_abs(DAILY_PARQUET) |> fs::path_norm()
hourly_profile_path <- fs::path_abs(USER_HOURLY_PROFILE) |> fs::path_norm()
metadata_path <- fs::path_abs(METADATA_PARQUET) |> fs::path_norm()

summary <- DBI::dbGetQuery(con, glue::glue("
  SELECT
    COUNT(*) AS dias_usuario_completos,
    COUNT(DISTINCT user_id) AS usuarios,
    MIN(date) AS fecha_min,
    MAX(date) AS fecha_max,
    AVG(daily_kWh) AS media_kWh,
    MEDIAN(daily_kWh) AS mediana_kWh,
    QUANTILE_CONT(daily_kWh, 0.90) AS p90_kWh,
    QUANTILE_CONT(daily_kWh, 0.99) AS p99_kWh,
    SUM(daily_kWh) AS kWh_total
  FROM read_parquet('{daily_path}')
  WHERE user_id IS NOT NULL AND user_id <> ''
    AND daily_kWh > 0
    AND hours_recorded = 24
"))

monthly <- DBI::dbGetQuery(con, glue::glue("
  SELECT
    DATE_TRUNC('month', date)::DATE AS month,
    AVG(daily_kWh) AS mean_daily_kWh,
    COUNT(DISTINCT user_id) AS n_users
  FROM read_parquet('{daily_path}')
  WHERE user_id IS NOT NULL AND user_id <> ''
    AND daily_kWh > 0
    AND hours_recorded = 24
  GROUP BY 1
  ORDER BY 1
"))

hourly <- DBI::dbGetQuery(con, glue::glue("
  SELECT hour, AVG(mean_kWh_user) AS mean_kWh
  FROM read_parquet('{hourly_profile_path}')
  GROUP BY 1
  ORDER BY 1
"))

tariff <- DBI::dbGetQuery(con, glue::glue("
  SELECT COALESCE(tarifa_atr, 'Desconocida') AS tarifa, COUNT(*) AS n
  FROM read_parquet('{metadata_path}')
  GROUP BY 1
  ORDER BY n DESC
  LIMIT 8
"))

readr::write_csv(summary, fs::path(TABLE_DIR, "shiny_eda_summary.csv"))
readr::write_csv(monthly, fs::path(TABLE_DIR, "shiny_eda_monthly.csv"))
readr::write_csv(hourly, fs::path(TABLE_DIR, "shiny_eda_hourly.csv"))
readr::write_csv(tariff, fs::path(TABLE_DIR, "shiny_eda_tariff.csv"))

message("[SHINY] Tablas ligeras escritas en outputs/tables/shiny_eda_*.csv")
