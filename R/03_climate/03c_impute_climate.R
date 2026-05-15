#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 03c: Imputacion climatica con timetk
# ==============================================================================
#
# Completa la capa diaria AEMET por provincia-estacion-fecha y rellena solo
# variables climaticas exogenas. La capa AEMET original se conserva intacta.
#
# Inputs:
#   data/parquet/climate/daily_climate.parquet
#   data/parquet/climate/station_mapping.parquet
#
# Outputs:
#   data/parquet/climate/daily_climate_imputed.parquet
#   outputs/tables/climate_imputation_summary.csv
#   outputs/tables/climate_imputation_by_province_date.csv
#
# Uso:
#   Rscript R/03_climate/03c_impute_climate.R
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(fs)
})

if (!requireNamespace("timetk", quietly = TRUE)) {
  stop(
    "Falta el paquete timetk. Instala con install.packages('timetk') ",
    "antes de ejecutar R/03_climate/03c_impute_climate.R."
  )
}

source(here::here("_config.R"))

message(strrep("=", 60))
message("PASO 03c: Imputacion climatica con timetk")
message(strrep("=", 60))

t0_total <- proc.time()

stopifnot(
  "Falta daily_climate.parquet; ejecuta R/03_climate/03a_build_climate_dataset.R" =
    file_exists(DAILY_CLIMATE_PARQUET),
  "Falta station_mapping.parquet; ejecuta R/03_climate/03a_build_climate_dataset.R" =
    file_exists(STATION_MAPPING_PARQUET)
)

base_vars <- c("tmed", "tmax", "tmin", "prec", "hrMedia", "sol", "velmedia")
flag_cols <- paste0(base_vars, "_imputed")
start_date <- as.Date(sprintf("%d-01-01", YEAR_MIN))
end_date <- as.Date(sprintf("%d-12-31", YEAR_MAX))
expected_dates <- seq(start_date, end_date, by = "day")

longest_true_run <- function(x) {
  x <- as.logical(x)
  x[is.na(x)] <- FALSE
  if (!any(x)) return(0L)
  runs <- rle(x)
  max(runs$lengths[runs$values])
}

impute_vec_timetk <- function(x, variable, cod_provincia, indicativo) {
  x <- as.numeric(x)
  if (all(is.na(x))) {
    stop(sprintf(
      "La variable %s esta completamente vacia para provincia %s, estacion %s.",
      variable, cod_provincia, indicativo
    ))
  }
  if (!any(is.na(x))) return(x)

  out <- timetk::ts_impute_vec(x, period = 365)
  if (any(is.na(out))) {
    stop(sprintf(
      "timetk::ts_impute_vec dejo NA en %s para provincia %s, estacion %s.",
      variable, cod_provincia, indicativo
    ))
  }
  as.numeric(out)
}

pad_and_impute_station <- function(station_df) {
  cod <- unique(station_df$cod_provincia)
  station_indicativo <- unique(station_df$indicativo)
  if (length(cod) != 1 || length(station_indicativo) != 1) {
    stop("Cada grupo de imputacion debe contener una unica provincia y estacion.")
  }

  padded <- station_df |>
    arrange(.data$fecha) |>
    timetk::pad_by_time(
      .date_var = fecha,
      .by = "day",
      .start_date = start_date,
      .end_date = end_date
    ) |>
    mutate(
      cod_provincia = cod,
      indicativo = station_indicativo,
      aemet_row_present = tidyr::replace_na(.data$aemet_row_present, FALSE),
      climate_row_imputed = !.data$aemet_row_present
    )

  for (var in base_vars) {
    padded[[paste0(var, "_imputed")]] <- is.na(padded[[var]])
    padded[[var]] <- impute_vec_timetk(
      padded[[var]], var, cod_provincia = cod, indicativo = station_indicativo
    )
  }

  temp_inconsistent <- with(
    padded,
    !is.na(tmin) & !is.na(tmed) & !is.na(tmax) & (tmin > tmed | tmed > tmax)
  )

  temp_low <- pmin(padded$tmin, padded$tmed, padded$tmax)
  temp_high <- pmax(padded$tmin, padded$tmed, padded$tmax)
  padded$tmin <- temp_low
  padded$tmax <- temp_high
  padded$tmed <- pmin(pmax(padded$tmed, padded$tmin), padded$tmax)

  padded |>
    mutate(
      prec = pmax(.data$prec, 0),
      sol = pmax(.data$sol, 0),
      velmedia = pmax(.data$velmedia, 0),
      hrMedia = pmin(pmax(.data$hrMedia, 0), 100),
      temperature_order_corrected = temp_inconsistent,
      n_climate_values_imputed = rowSums(across(all_of(flag_cols))),
      climate_any_imputed = .data$n_climate_values_imputed > 0
    )
}

message("\n[1/5] Cargando capa AEMET original...")
climate_raw <- read_parquet(DAILY_CLIMATE_PARQUET) |>
  mutate(
    cod_provincia = normalize_province_code(.data$cod_provincia),
    indicativo = as.character(.data$indicativo),
    fecha = as.Date(.data$fecha),
    aemet_row_present = TRUE
  ) |>
  filter(.data$cod_provincia %in% FOCUS_PROVINCES)

missing_cols <- setdiff(c("cod_provincia", "indicativo", "fecha", base_vars), names(climate_raw))
if (length(missing_cols) > 0) {
  stop("daily_climate.parquet no contiene columnas requeridas: ",
       paste(missing_cols, collapse = ", "))
}

station_mapping <- read_parquet(STATION_MAPPING_PARQUET) |>
  mutate(
    cod_provincia = normalize_province_code(.data$cod_provincia),
    indicativo = as.character(.data$indicativo),
    provincia = unname(PROVINCIA_NOMBRES[as.character(.data$cod_provincia)])
  ) |>
  filter(.data$cod_provincia %in% FOCUS_PROVINCES)

target_stations <- station_mapping |>
  select("cod_provincia", "indicativo", "station_name", "provincia", "n_usuarios") |>
  distinct()

observed_stations <- climate_raw |>
  distinct(.data$cod_provincia, .data$indicativo)

missing_station_data <- anti_join(
  target_stations,
  observed_stations,
  by = c("cod_provincia", "indicativo")
)
if (nrow(missing_station_data) > 0) {
  stop("Hay estaciones mapeadas sin datos climaticos: ",
       paste(missing_station_data$indicativo, collapse = ", "))
}

message(sprintf(
  "  Filas AEMET: %s | estaciones: %s | rango objetivo: %s a %s",
  fmt_int(nrow(climate_raw)), fmt_int(nrow(observed_stations)), start_date, end_date
))

message("\n[2/5] Completando rejilla diaria con timetk::pad_by_time...")
station_groups <- split(
  climate_raw,
  paste(climate_raw$cod_provincia, climate_raw$indicativo, sep = "__")
)

climate_imputed <- bind_rows(lapply(station_groups, pad_and_impute_station)) |>
  arrange(.data$cod_provincia, .data$fecha)

expected_rows <- length(expected_dates) * nrow(observed_stations)
if (nrow(climate_imputed) != expected_rows) {
  stop(sprintf(
    "La capa imputada tiene %s filas; se esperaban %s.",
    fmt_int(nrow(climate_imputed)), fmt_int(expected_rows)
  ))
}

message(sprintf(
  "  Filas tras padding: %s | filas creadas: %s",
  fmt_int(nrow(climate_imputed)), fmt_int(sum(climate_imputed$climate_row_imputed))
))

message("\n[3/5] Recalculando derivadas climaticas...")
climate_imputed <- climate_imputed |>
  mutate(
    hdd = pmax(0, HDD_BASE - .data$tmed),
    cdd = pmax(0, .data$tmed - CDD_BASE),
    amplitud_termica = .data$tmax - .data$tmin,
    thi = .data$tmed - (0.55 - 0.0055 * .data$hrMedia) * (.data$tmed - 14.5),
    month = month(.data$fecha),
    year = year(.data$fecha),
    season = case_when(
      .data$month %in% c(12, 1, 2) ~ "Invierno",
      .data$month %in% c(3, 4, 5) ~ "Primavera",
      .data$month %in% c(6, 7, 8) ~ "Verano",
      TRUE ~ "Otono"
    )
  ) |>
  group_by(.data$cod_provincia, .data$indicativo) |>
  mutate(
    is_heatwave = .data$tmax >= quantile(.data$tmax, 0.95, na.rm = TRUE),
    is_coldwave = .data$tmin <= quantile(.data$tmin, 0.05, na.rm = TRUE)
  ) |>
  ungroup()

remaining_na <- climate_imputed |>
  summarise(across(all_of(c(base_vars, "hdd", "cdd")), ~sum(is.na(.x))))
if (sum(unlist(remaining_na)) > 0) {
  print(remaining_na)
  stop("La capa imputada conserva NA en variables climaticas criticas.")
}

message("\n[4/5] Construyendo auditorias de imputacion...")
climate_with_lookup <- climate_imputed |>
  left_join(target_stations, by = c("cod_provincia", "indicativo")) |>
  mutate(provincia = ifelse(is.na(.data$provincia),
                            unname(PROVINCIA_NOMBRES[as.character(.data$cod_provincia)]),
                            .data$provincia))

summary_rows <- vector("list", length(station_groups) * length(base_vars))
row_id <- 1L
for (station_key in names(station_groups)) {
  parts <- strsplit(station_key, "__", fixed = TRUE)[[1]]
  cod <- parts[[1]]
  indicativo <- parts[[2]]
  df <- climate_with_lookup |>
    filter(.data$cod_provincia == cod, .data$indicativo == indicativo)

  for (var in base_vars) {
    flag <- df[[paste0(var, "_imputed")]]
    n_missing_before <- sum(flag, na.rm = TRUE)
    n_missing_after <- sum(is.na(df[[var]]))
    summary_rows[[row_id]] <- tibble::tibble(
      cod_provincia = cod,
      provincia = unique(df$provincia)[1],
      indicativo = indicativo,
      station_name = unique(df$station_name)[1],
      n_usuarios = unique(df$n_usuarios)[1],
      variable = var,
      n_rows = nrow(df),
      n_missing_before = n_missing_before,
      pct_missing_before = round(100 * n_missing_before / nrow(df), 2),
      n_imputed = sum(flag & !is.na(df[[var]]), na.rm = TRUE),
      n_missing_after = n_missing_after,
      longest_gap_days = longest_true_run(flag),
      method = "timetk::pad_by_time + timetk::ts_impute_vec(period=365)",
      status = case_when(
        n_missing_after > 0 ~ "fail_missing_after",
        n_missing_before == 0 ~ "ok_no_imputation",
        n_missing_before / nrow(df) <= 0.01 ~ "imputed_low_missingness",
        n_missing_before / nrow(df) <= 0.05 ~ "imputed_moderate_missingness",
        TRUE ~ "imputed_high_missingness"
      )
    )
    row_id <- row_id + 1L
  }
}

imputation_summary <- bind_rows(summary_rows) |>
  arrange(desc(.data$n_imputed), .data$cod_provincia, .data$variable)

variables_imputed <- apply(
  as.data.frame(climate_with_lookup[, flag_cols]),
  1,
  function(row) paste(base_vars[as.logical(row)], collapse = ",")
)

imputation_by_date <- climate_with_lookup |>
  mutate(variables_imputed = variables_imputed) |>
  filter(.data$climate_any_imputed | .data$climate_row_imputed) |>
  transmute(
    cod_provincia,
    provincia,
    indicativo,
    station_name,
    fecha,
    climate_row_imputed,
    climate_any_imputed,
    n_climate_values_imputed,
    variables_imputed,
    tmed,
    tmax,
    tmin,
    prec,
    hrMedia,
    sol,
    velmedia,
    hdd,
    cdd
  ) |>
  arrange(.data$cod_provincia, .data$fecha)

write.csv(imputation_summary, CLIMATE_IMPUTATION_SUMMARY_CSV, row.names = FALSE)
write.csv(imputation_by_date, CLIMATE_IMPUTATION_BY_DATE_CSV, row.names = FALSE)

message(sprintf(
  "  Celdas climaticas imputadas: %s | filas con alguna imputacion: %s",
  fmt_int(sum(climate_imputed$n_climate_values_imputed)),
  fmt_int(sum(climate_imputed$climate_any_imputed))
))
message("  Auditoria resumen: ", CLIMATE_IMPUTATION_SUMMARY_CSV)
message("  Auditoria por fecha: ", CLIMATE_IMPUTATION_BY_DATE_CSV)

message("\n[5/5] Guardando capa climatica imputada...")
climate_output <- climate_imputed |>
  select(
    "cod_provincia",
    "indicativo",
    "fecha",
    all_of(base_vars),
    "hdd",
    "cdd",
    "amplitud_termica",
    "thi",
    "month",
    "year",
    "season",
    "is_heatwave",
    "is_coldwave",
    "climate_row_imputed",
    "climate_any_imputed",
    "n_climate_values_imputed",
    all_of(flag_cols),
    "temperature_order_corrected"
  ) |>
  arrange(.data$cod_provincia, .data$fecha)

write_parquet(
  climate_output,
  DAILY_CLIMATE_IMPUTED_PARQUET,
  compression = "zstd",
  compression_level = 9L
)

message("Guardado: ", DAILY_CLIMATE_IMPUTED_PARQUET,
        " (", fmt_int(nrow(climate_output)), " registros)")

elapsed <- (proc.time() - t0_total)[["elapsed"]]
message(sprintf("\nPaso 03c completado en %.1f s.", elapsed))
