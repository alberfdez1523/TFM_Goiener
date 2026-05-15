#!/usr/bin/env Rscript
# ==============================================================================
# R/06_forecasting/06b_features_temporal.R
#
# Anade lags, Fourier intradia/intrasemana, indicadores de calendario y
# clima diferida. Escribe parquets enriquecidos:
#   features/portfolio_daily_fe.parquet
#   features/portfolio_hourly_fe.parquet
#   features/cluster_daily_fe.parquet
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(lubridate); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 07b: Features temporales")
t0 <- proc.time(); set.seed(SEED)

add_calendar <- function(df, date_col = "date") {
  d <- df[[date_col]]
  df |> mutate(
    dow = as.integer(wday(d, week_start = 1)),
    month = as.integer(month(d)),
    week = as.integer(isoweek(d)),
    doy = as.integer(yday(d)),
    is_weekend = as.integer(dow %in% c(6L, 7L)),
    is_friday = as.integer(dow == 5L),
    is_monday = as.integer(dow == 1L),
    sin_doy = sin(2 * pi * doy / 365.25),
    cos_doy = cos(2 * pi * doy / 365.25),
    sin_doy2 = sin(4 * pi * doy / 365.25),
    cos_doy2 = cos(4 * pi * doy / 365.25),
    sin_dow = sin(2 * pi * dow / 7),
    cos_dow = cos(2 * pi * dow / 7),
    is_summer = as.integer(month %in% 6:8),
    is_winter = as.integer(month %in% c(12, 1, 2))
  )
}

add_lags_daily <- function(df, target = "kWh_total", lags = FORECAST_DAILY_LAGS) {
  for (lg in lags) {
    df[[paste0("lag", lg)]] <- dplyr::lag(df[[target]], lg)
  }
  df |> mutate(
    roll7  = zoo::rollmeanr(.data[[target]], k = 7,  fill = NA),
    roll14 = zoo::rollmeanr(.data[[target]], k = 14, fill = NA),
    roll28 = zoo::rollmeanr(.data[[target]], k = 28, fill = NA),
    roll7_sd  = zoo::rollapplyr(.data[[target]], width = 7, FUN = sd, fill = NA),
    roll28_sd = zoo::rollapplyr(.data[[target]], width = 28, FUN = sd, fill = NA),
    # YoY change features
    yoy_diff = .data[[target]] - dplyr::lag(.data[[target]], 365),
    # Climate lags
    hdd_lag1 = if ("hdd_mean" %in% names(df)) dplyr::lag(hdd_mean, 1) else NA_real_,
    cdd_lag1 = if ("cdd_mean" %in% names(df)) dplyr::lag(cdd_mean, 1) else NA_real_,
    hdd_roll7 = if ("hdd_mean" %in% names(df))
                  zoo::rollmeanr(hdd_mean, k = 7, fill = NA) else NA_real_,
    cdd_roll7 = if ("cdd_mean" %in% names(df))
                  zoo::rollmeanr(cdd_mean, k = 7, fill = NA) else NA_real_
  )
}

# 1. Daily portfolio
message("[1/3] Daily portfolio FE...")
pd <- read_parquet_safe(PORTFOLIO_DAILY_PARQUET, "portfolio_daily")
pd <- pd |> arrange(date) |> add_calendar()
suppressPackageStartupMessages(library(zoo))
pd <- pd |> add_lags_daily()
arrow::write_parquet(pd, path(FEATURES_DIR, "portfolio_daily_fe.parquet"))
message(sprintf("  filas: %s", fmt_int(nrow(pd))))

# 2. Cluster daily
message("[2/3] Cluster daily FE...")
cd <- read_parquet_safe(CLUSTER_DAILY_PARQUET, "cluster_daily")
cd <- cd |> arrange(cluster, date) |>
  group_by(cluster) |>
  do(add_calendar(.) |> add_lags_daily()) |>
  ungroup()
arrow::write_parquet(cd, path(FEATURES_DIR, "cluster_daily_fe.parquet"))
message(sprintf("  filas: %s, clusters: %d",
                fmt_int(nrow(cd)), length(unique(cd$cluster))))

# 3. Hourly portfolio
message("[3/3] Hourly portfolio FE...")
ph <- read_parquet_safe(PORTFOLIO_HOURLY_PARQUET, "portfolio_hourly")
ph <- ph |> arrange(datetime) |>
  mutate(
    date = as.Date(datetime),
    hour = as.integer(hour(datetime)),
    dow  = as.integer(wday(datetime, week_start = 1)),
    month = as.integer(month(datetime)),
    is_weekend = as.integer(dow %in% c(6L, 7L)),
    sin_h = sin(2 * pi * hour / 24),
    cos_h = cos(2 * pi * hour / 24),
    sin_h2 = sin(4 * pi * hour / 24),
    cos_h2 = cos(4 * pi * hour / 24),
    sin_dow = sin(2 * pi * dow / 7),
    cos_dow = cos(2 * pi * dow / 7),
    sin_doy = sin(2 * pi * yday(date) / 365.25),
    cos_doy = cos(2 * pi * yday(date) / 365.25)
  )
for (lg in FORECAST_HOURLY_LAGS) {
  ph[[paste0("lag", lg)]] <- dplyr::lag(ph$kWh_total, lg)
}
ph$roll24    <- zoo::rollmeanr(ph$kWh_total, k = 24,  fill = NA)
ph$roll168   <- zoo::rollmeanr(ph$kWh_total, k = 168, fill = NA)
ph$roll24_sd <- zoo::rollapplyr(ph$kWh_total, width = 24, FUN = sd, fill = NA)
# Diferencias respecto al mismo dia/hora pasados (estabilizan tendencias).
ph$diff_lag24  <- ph$kWh_total - dplyr::lag(ph$kWh_total, 24)
ph$diff_lag168 <- ph$kWh_total - dplyr::lag(ph$kWh_total, 168)
# Join HDD/CDD diario
ph <- ph |> left_join(
  pd |> select(date, hdd_mean, cdd_mean, tmed_mean, any_holiday),
  by = "date"
)
arrow::write_parquet(ph, path(FEATURES_DIR, "portfolio_hourly_fe.parquet"))
message(sprintf("  filas: %s", fmt_int(nrow(ph))))

message(sprintf("07b en %.1f s", (proc.time() - t0)[["elapsed"]]))
