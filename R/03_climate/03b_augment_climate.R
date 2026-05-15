#!/usr/bin/env Rscript
# ==============================================================================
# Utilidad local de recuperacion/augmentation climatica.
#
# Este script NO es una fase principal del pipeline actual: R/03_climate/03a_build_climate_dataset.R
# ya genera THI, is_heatwave e is_coldwave. Se conserva como herramienta
# reproducible para reparar o completar un daily_climate.parquet creado con una
# version antigua, sin volver a llamar a la API de AEMET.
#
# Augmenta data/parquet/climate/daily_climate.parquet con THI, is_heatwave,
# is_coldwave cuando esas columnas faltan.
#
# Se aplican las mismas formulas que R/03_climate/03a_build_climate_dataset.R:
#   - THI = tmed - (0.55 - 0.0055 * hrMedia) * (tmed - 14.5)
#   - is_heatwave: 3 dias consecutivos con tmax >= P95(verano JJA-S) por estacion
#   - is_coldwave: 3 dias consecutivos con tmin <= P05(DJF) por estacion
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(lubridate); library(fs)
})
source(here::here("_config.R"))

message("Utilidad 03b: recuperacion/local augmentation de daily_climate.parquet; no sustituye a R/03.")

stopifnot(file_exists(DAILY_CLIMATE_PARQUET))

clima <- read_parquet(DAILY_CLIMATE_PARQUET) |>
  mutate(fecha = as.Date(fecha))

if ("thi" %in% names(clima) && "is_heatwave" %in% names(clima) &&
    "is_coldwave" %in% names(clima)) {
  message("daily_climate ya tiene THI y olas. Nada que hacer.")
  quit(status = 0)
}

clima <- clima |>
  mutate(thi = ifelse(!is.na(tmed) & !is.na(hrMedia),
                      tmed - (0.55 - 0.0055 * hrMedia) * (tmed - 14.5),
                      NA_real_))

# Olas de calor: P95 de tmax en verano (Jun-Sep) por estacion
heat_thresh <- clima |>
  filter(month(fecha) %in% 6:9, !is.na(tmax)) |>
  group_by(indicativo) |>
  summarise(p95_tmax = quantile(tmax, 0.95, na.rm = TRUE), .groups = "drop")

# Olas de frio: P05 de tmin en invierno (DJF) por estacion
cold_thresh <- clima |>
  filter(month(fecha) %in% c(12, 1, 2), !is.na(tmin)) |>
  group_by(indicativo) |>
  summarise(p05_tmin = quantile(tmin, 0.05, na.rm = TRUE), .groups = "drop")

clima_aug <- clima |>
  left_join(heat_thresh, by = "indicativo") |>
  left_join(cold_thresh, by = "indicativo") |>
  arrange(indicativo, fecha) |>
  group_by(indicativo) |>
  mutate(
    hot = !is.na(tmax) & !is.na(p95_tmax) & tmax >= p95_tmax,
    cold = !is.na(tmin) & !is.na(p05_tmin) & tmin <= p05_tmin,
    hot_run = hot & lag(hot, 1, default = FALSE) & lag(hot, 2, default = FALSE),
    cold_run = cold & lag(cold, 1, default = FALSE) & lag(cold, 2, default = FALSE),
    is_heatwave = hot_run | lead(hot_run, 1, default = FALSE) |
                  lead(hot_run, 2, default = FALSE),
    is_coldwave = cold_run | lead(cold_run, 1, default = FALSE) |
                  lead(cold_run, 2, default = FALSE)
  ) |>
  ungroup() |>
  select(-hot, -cold, -hot_run, -cold_run, -p95_tmax, -p05_tmin)

# Resumen
n_heat <- sum(clima_aug$is_heatwave, na.rm = TRUE)
n_cold <- sum(clima_aug$is_coldwave, na.rm = TRUE)
message(sprintf("  Filas: %d | THI no-NA: %d | dias ola calor: %d | dias ola frio: %d",
                nrow(clima_aug), sum(!is.na(clima_aug$thi)), n_heat, n_cold))

write_parquet(clima_aug, DAILY_CLIMATE_PARQUET, compression = "zstd",
              compression_level = 9L)
message("OK: daily_climate.parquet augmentado in-place.")

