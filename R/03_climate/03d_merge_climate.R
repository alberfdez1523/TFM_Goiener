#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 04: Union de consumo y clima
# ==============================================================================
#
# Cruza los datos diarios de consumo con las variables climaticas descargadas
# de AEMET, usando la provincia como clave de enlace.
#
# Ademas, enriquece con variables de calendario:
#   - Dia de la semana, fin de semana
#   - Festivos nacionales y autonomicos/locales aproximados para el alcance
#     top 5: Euskadi, Navarra y Madrid
#   - Semana del anio (ISO)
#   - Horas de luz (calculadas con suncalc)
#
# Inputs:
#   data/parquet/daily_consumption.parquet
#   data/parquet/metadata.parquet
#   data/parquet/climate/station_mapping.parquet
#   data/parquet/climate/daily_climate_imputed.parquet
#
# Outputs:
#   data/parquet/features/daily_with_climate.parquet
#   outputs/tables/climate_merge_quality.csv
#   outputs/tables/climate_missing_by_province_date.csv
#   outputs/tables/climate_merge_imputation_audit.csv
#
# Dependencias:
#   DBI, duckdb, dplyr, arrow, lubridate, glue, fs, suncalc, timeDate
#
# Uso:
#   Rscript R/03_climate/03d_merge_climate.R
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(arrow)
  library(lubridate)
  library(glue)
  library(fs)
})

source(here::here("_config.R"))

message("=" |> strrep(60))
message("PASO 04: Union de consumo y clima")
message("=" |> strrep(60))

t0_total <- proc.time()

# --- Verificaciones ---
stopifnot(
  "Falta daily_consumption.parquet" = file_exists(DAILY_PARQUET),
  "Falta metadata.parquet" = file_exists(METADATA_PARQUET),
  "Falta daily_climate_imputed.parquet (ejecuta R/03_climate/03c_impute_climate.R)" =
    file_exists(DAILY_CLIMATE_IMPUTED_PARQUET),
  "Falta station_mapping.parquet" = file_exists(STATION_MAPPING_PARQUET)
)

# ==============================================================================
# 1. Cargar datos
# ==============================================================================
message("\n[1/5] Cargando datos...")

con <- connect_duckdb()

daily_abs    <- path_abs(DAILY_PARQUET) |> path_norm()
metadata_abs <- path_abs(METADATA_PARQUET) |> path_norm()
climate_abs  <- path_abs(DAILY_CLIMATE_IMPUTED_PARQUET) |> path_norm()
mapping_abs  <- path_abs(STATION_MAPPING_PARQUET) |> path_norm()
focus_filter_sql <- focus_province_filter_sql("cod_provincia")
message(sprintf(
  "  Alcance modelado: %s (%s)",
  MODEL_SCOPE_NAME, paste(FOCUS_PROVINCES, collapse = ", ")
))

# Contar registros
n_daily   <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM read_parquet('{daily_abs}')"))$n
n_climate <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM read_parquet('{climate_abs}')"))$n
message(sprintf("  Registros diarios consumo: %s", fmt_int(n_daily)))
message(sprintf("  Registros climaticos: %s", fmt_int(n_climate)))

# ==============================================================================
# 2. Construir tabla de festivos
# ==============================================================================
message("\n[2/5] Construyendo calendario de festivos...")

# Festivos nacionales de Espana (fuentes oficiales BOE): solo dias que son
# festivo en todo el territorio nacional. Los autonomicos/locales se guardan en
# columnas separadas para no mezclar alcance estatal con senales regionales.
festivos_nacionales <- as.Date(c(
  # 2014
  "2014-01-01", "2014-01-06", "2014-04-18", "2014-05-01",
  "2014-08-15", "2014-10-12", "2014-11-01", "2014-12-06",
  "2014-12-08", "2014-12-25",
  # 2015
  "2015-01-01", "2015-01-06", "2015-04-03", "2015-05-01",
  "2015-08-15", "2015-10-12", "2015-11-02", "2015-12-07",
  "2015-12-08", "2015-12-25",
  # 2016
  "2016-01-01", "2016-01-06", "2016-03-25", "2016-05-02",
  "2016-08-15", "2016-10-12", "2016-11-01", "2016-12-06",
  "2016-12-08", "2016-12-26",
  # 2017
  "2017-01-06", "2017-04-14", "2017-05-01", "2017-08-15",
  "2017-10-12", "2017-11-01", "2017-12-06", "2017-12-08",
  "2017-12-25",
  # 2018
  "2018-01-01", "2018-01-06", "2018-03-30", "2018-05-01",
  "2018-08-15", "2018-10-12", "2018-11-01", "2018-12-06",
  "2018-12-08", "2018-12-25",
  # 2019
  "2019-01-01", "2019-04-19", "2019-05-01", "2019-08-15",
  "2019-10-12", "2019-11-01", "2019-12-06", "2019-12-25",
  # 2020
  "2020-01-01", "2020-01-06", "2020-04-10", "2020-05-01",
  "2020-08-15", "2020-10-12", "2020-12-08", "2020-12-25",
  # 2021
  "2021-01-01", "2021-01-06", "2021-04-02", "2021-05-01",
  "2021-08-16", "2021-10-12", "2021-11-01", "2021-12-06",
  "2021-12-08", "2021-12-25",
  # 2022
  "2022-01-01", "2022-01-06", "2022-04-15", "2022-08-15",
  "2022-10-12", "2022-11-01", "2022-12-06", "2022-12-08",
  "2022-12-26",
  # 2023
  "2023-01-06", "2023-04-07", "2023-05-01", "2023-08-15",
  "2023-10-12", "2023-11-01", "2023-12-06", "2023-12-08",
  "2023-12-25",
  # 2024
  "2024-01-01", "2024-01-06", "2024-03-29", "2024-05-01",
  "2024-08-15", "2024-10-12", "2024-11-01", "2024-12-06",
  "2024-12-25"
))

calendar_dates <- seq(as.Date(paste0(YEAR_MIN, "-01-01")),
                      as.Date(paste0(YEAR_MAX, "-12-31")),
                      by = "day")

easter_dates <- as.Date(character())
easter_window_dates <- as.Date(character())
if (requireNamespace("timeDate", quietly = TRUE)) {
  easter_dates <- as.Date(timeDate::Easter(YEAR_MIN:YEAR_MAX))
  easter_window_dates <- sort(unique(unlist(lapply(
    easter_dates,
    function(d) seq(d - 3, d + 1, by = "day")
  ))))
}

fixed_dates <- function(month_value, day_value) {
  as.Date(sprintf("%d-%02d-%02d", YEAR_MIN:YEAR_MAX, month_value, day_value))
}

holiday_rows <- function(codes, dates, label) {
  if (length(dates) == 0) {
    return(tibble::tibble(
      cod_provincia = character(), fecha = as.Date(character()),
      holiday_label = character()
    ))
  }
  expand.grid(
    cod_provincia = codes,
    fecha = unique(as.Date(dates)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) |>
    as_tibble() |>
    mutate(holiday_label = label)
}

regional_local_holidays <- bind_rows(
  holiday_rows(
    c("01", "20", "48"),
    c(easter_dates - 3, easter_dates + 1, fixed_dates(7, 25)),
    "Euskadi: Jueves Santo, Lunes de Pascua o Santiago"
  ),
  holiday_rows(
    "31",
    c(easter_dates - 3, easter_dates + 1, fixed_dates(12, 3)),
    "Navarra: Jueves Santo, Lunes de Pascua o San Francisco Javier"
  ),
  holiday_rows(
    "28",
    c(fixed_dates(5, 2), fixed_dates(5, 15), fixed_dates(11, 9)),
    "Madrid: Comunidad de Madrid, San Isidro o Almudena"
  )
) |>
  distinct(.data$cod_provincia, .data$fecha, .keep_all = TRUE)

calendar_base <- expand.grid(
  cod_provincia = FOCUS_PROVINCES,
  fecha = calendar_dates,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) |>
  as_tibble() |>
  mutate(
    cod_provincia = as.character(.data$cod_provincia),
    fecha = as.Date(.data$fecha),
    dow = lubridate::wday(.data$fecha, week_start = 1)
  )

national_calendar <- tibble::tibble(
  fecha = festivos_nacionales,
  is_holiday_national = TRUE
)

regional_calendar <- regional_local_holidays |>
  group_by(.data$cod_provincia, .data$fecha) |>
  summarise(
    holiday_label = paste(sort(unique(.data$holiday_label)), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(is_holiday_regional_local = TRUE)

calendar_df <- calendar_base |>
  left_join(national_calendar, by = "fecha") |>
  left_join(regional_calendar, by = c("cod_provincia", "fecha")) |>
  mutate(
    is_holiday_national = coalesce(.data$is_holiday_national, FALSE),
    is_holiday_regional_local = coalesce(.data$is_holiday_regional_local, FALSE),
    is_holiday = .data$is_holiday_national | .data$is_holiday_regional_local,
    holiday_scope = case_when(
      .data$is_holiday_national & .data$is_holiday_regional_local ~ "nacional_y_autonomico_local",
      .data$is_holiday_national ~ "nacional",
      .data$is_holiday_regional_local ~ "autonomico_local",
      TRUE ~ "ninguno"
    ),
    holiday_label = case_when(
      .data$is_holiday_national & .data$is_holiday_regional_local ~ paste("Nacional", .data$holiday_label, sep = "; "),
      .data$is_holiday_national ~ "Nacional",
      .data$is_holiday_regional_local ~ .data$holiday_label,
      TRUE ~ ""
    ),
    is_easter_window = .data$fecha %in% easter_window_dates
  ) |>
  group_by(.data$cod_provincia) |>
  arrange(.data$fecha, .by_group = TRUE) |>
  group_modify(function(.x, .y) {
    holiday_dates <- .x$fecha[.x$is_holiday]
    .x$days_to_holiday <- pmin(vapply(
      .x$fecha,
      function(d) min(abs(as.integer(d - holiday_dates)), na.rm = TRUE),
      numeric(1)
    ), 30)
    .x
  }) |>
  mutate(
    prev_is_holiday = lag(.data$is_holiday, default = FALSE),
    next_is_holiday = lead(.data$is_holiday, default = FALSE),
    is_bridge_day = .data$dow <= 5 & !.data$is_holiday &
      (.data$prev_is_holiday | .data$next_is_holiday)
  ) |>
  ungroup() |>
  select(
    cod_provincia, fecha, is_holiday, is_holiday_national,
    is_holiday_regional_local, holiday_scope, holiday_label,
    is_bridge_day, is_easter_window, days_to_holiday
  )

dbWriteTable(con, "calendar_features", calendar_df, overwrite = TRUE)

# ==============================================================================
# 3. Calcular horas de luz por provincia y dia
# ==============================================================================
message("\n[3/5] Calculando horas de luz...")

# Coordenadas aproximadas de las capitales de provincia
# (usadas como referencia para calcular sunrise/sunset)
province_coords <- tibble::tribble(
  ~cod_provincia, ~lat,   ~lon,
  "01",           42.85,  -2.67,   # Vitoria
  "20",           43.32,  -1.98,   # San Sebastian
  "48",           43.26,  -2.93,   # Bilbao
  "31",           42.82,  -1.64,   # Pamplona
  "28",           40.42,  -3.70,   # Madrid
  "08",           41.39,   2.17,   # Barcelona
  "41",           37.39,  -5.98,   # Sevilla
  "46",           39.47,  -0.38,   # Valencia
  "29",           36.72,  -4.42,   # Malaga
  "33",           43.36,  -5.85,   # Oviedo
  "39",           43.46,  -3.80,   # Santander
  "26",           42.47,  -2.45,   # Logrono
  "50",           41.65,  -0.89,   # Zaragoza
  "47",           41.65,  -4.72,   # Valladolid
  "15",           43.37,  -8.40,   # A Coruna
  "36",           42.43,  -8.64,   # Pontevedra
  "18",           37.18,  -3.60,   # Granada
  "11",           36.53,  -6.29,   # Cadiz
  "30",           37.98,  -1.13,   # Murcia
  "03",           38.35,  -0.48,   # Alicante
  "09",           42.34,  -3.70,   # Burgos
  "37",           40.97,  -5.66    # Salamanca
)

# Generar todas las combinaciones provincia x fecha
# y calcular horas de luz con suncalc
if (requireNamespace("suncalc", quietly = TRUE)) {
  library(suncalc)

  # Rango de fechas del dataset
  all_dates <- seq(as.Date(paste0(YEAR_MIN, "-01-01")),
                   as.Date(paste0(YEAR_MAX, "-12-31")),
                   by = "day")

  # Solo para las provincias en nuestro mapeo
  mapping_df <- arrow::read_parquet(STATION_MAPPING_PARQUET)
  provinces_with_climate <- unique(mapping_df$cod_provincia)

  coords_needed <- province_coords |>
    filter(cod_provincia %in% provinces_with_climate)

  # Calcular en bloques por provincia para no saturar memoria
  sunlight_list <- list()

  for (i in seq_len(nrow(coords_needed))) {
    prov <- coords_needed$cod_provincia[i]
    lat <- coords_needed$lat[i]
    lon <- coords_needed$lon[i]

    sun_times <- getSunlightTimes(
      date = all_dates,
      lat = lat,
      lon = lon,
      keep = c("sunrise", "sunset")
    )

    sun_times <- sun_times |>
      mutate(
        daylight_hours = as.numeric(difftime(sunset, sunrise, units = "hours")),
        cod_provincia = prov
      ) |>
      select(cod_provincia, fecha = date, daylight_hours)

    sunlight_list[[i]] <- sun_times
  }

  sunlight_df <- bind_rows(sunlight_list)
  message(sprintf("  Horas de luz calculadas: %s registros para %d provincias",
                  fmt_int(nrow(sunlight_df)), nrow(coords_needed)))

  dbWriteTable(con, "sunlight", sunlight_df, overwrite = TRUE)

} else {
  message("  AVISO: paquete suncalc no disponible. Se omiten horas de luz.")
  # Crear tabla vacia para que el JOIN no falle
  dbExecute(con, "CREATE TABLE sunlight (cod_provincia VARCHAR, fecha DATE, daylight_hours FLOAT)")
}

# ==============================================================================
# 4. JOIN: consumo + metadata + clima + festivos + horas de luz
# ==============================================================================
message("\n[4/5] Realizando el cruce consumo + clima + calendario...")

output_abs <- path_abs(DAILY_WITH_CLIMATE) |> path_norm()

# Subir mapeo de estaciones a DuckDB
mapping_df <- arrow::read_parquet(STATION_MAPPING_PARQUET)
dbWriteTable(con, "station_mapping", mapping_df, overwrite = TRUE)

query_merge <- glue("
COPY (
  WITH
  -- Deduplicar metadata: un registro por usuario
  meta_dedup AS (
    SELECT user_id, cod_provincia FROM (
      SELECT
        cups AS user_id,
        SUBSTR(LPAD(CAST(codigo_postal AS VARCHAR), 5, '0'), 1, 2) AS cod_provincia,
        ROW_NUMBER() OVER (
          PARTITION BY cups
          ORDER BY
            CASE WHEN tarifa_atr IS NOT NULL AND tarifa_atr <> '' THEN 0 ELSE 1 END,
            fecha_alta DESC NULLS LAST
        ) AS rn
      FROM read_parquet('{metadata_abs}')
    ) sub
    WHERE rn = 1
      AND cod_provincia IS NOT NULL AND cod_provincia <> ''
      AND {focus_filter_sql}
  ),

  -- Consumo diario con provincia del usuario
  consumo AS (
    SELECT
      d.user_id,
      d.date,
      d.daily_kWh,
      d.mean_hourly_kWh,
      d.sd_hourly_kWh,
      d.hours_recorded,
      m.cod_provincia
    FROM read_parquet('{daily_abs}') d
    INNER JOIN meta_dedup m ON d.user_id = m.user_id
    WHERE d.user_id IS NOT NULL AND d.user_id <> ''
      AND d.daily_kWh IS NOT NULL AND d.daily_kWh >= 0
      AND d.hours_recorded = 24
  ),

  -- Clima diario con cod_provincia
  clima AS (
    SELECT
      c.cod_provincia,
      c.fecha,
      c.tmed,
      c.tmax,
      c.tmin,
      c.prec,
      c.hrMedia,
      c.sol,
      c.velmedia,
      c.hdd,
      c.cdd,
      c.amplitud_termica,
      c.thi,
      c.is_heatwave,
      c.is_coldwave,
      c.climate_row_imputed,
      c.climate_any_imputed,
      c.n_climate_values_imputed,
      c.tmed_imputed,
      c.tmax_imputed,
      c.tmin_imputed,
      c.prec_imputed,
      c.hrMedia_imputed,
      c.sol_imputed,
      c.velmedia_imputed,
      c.temperature_order_corrected
    FROM read_parquet('{climate_abs}') c
  )

  SELECT
    co.user_id,
    co.date,
    co.daily_kWh,
    co.mean_hourly_kWh,
    co.sd_hourly_kWh,
    co.hours_recorded,
    co.cod_provincia,

    -- Variables climaticas
    cl.tmed,
    cl.tmax,
    cl.tmin,
    cl.prec,
    cl.hrMedia AS hr_media,
    cl.sol,
    cl.velmedia,
    cl.hdd,
    cl.cdd,
    cl.amplitud_termica,
    cl.thi,
    cl.is_heatwave,
    cl.is_coldwave,
    COALESCE(cl.climate_row_imputed, FALSE) AS climate_row_imputed,
    COALESCE(cl.climate_any_imputed, FALSE) AS climate_any_imputed,
    COALESCE(cl.n_climate_values_imputed, 0) AS n_climate_values_imputed,
    COALESCE(cl.tmed_imputed, FALSE) AS tmed_imputed,
    COALESCE(cl.tmax_imputed, FALSE) AS tmax_imputed,
    COALESCE(cl.tmin_imputed, FALSE) AS tmin_imputed,
    COALESCE(cl.prec_imputed, FALSE) AS prec_imputed,
    COALESCE(cl.hrMedia_imputed, FALSE) AS hr_media_imputed,
    COALESCE(cl.sol_imputed, FALSE) AS sol_imputed,
    COALESCE(cl.velmedia_imputed, FALSE) AS velmedia_imputed,
    COALESCE(cl.temperature_order_corrected, FALSE) AS temperature_order_corrected,

    -- Horas de luz
    sl.daylight_hours,

    -- Variables de calendario
    EXTRACT('isodow' FROM co.date)::INTEGER AS dow,
    WEEK(co.date)::INTEGER AS week_of_year,
    MONTH(co.date) AS month,
    YEAR(co.date) AS year,
    CASE WHEN EXTRACT('isodow' FROM co.date) IN (6, 7) THEN TRUE ELSE FALSE END AS is_weekend,
    COALESCE(cal.is_holiday, FALSE) AS is_holiday,
    COALESCE(cal.is_holiday_national, FALSE) AS is_holiday_national,
    COALESCE(cal.is_holiday_regional_local, FALSE) AS is_holiday_regional_local,
    COALESCE(cal.holiday_scope, 'ninguno') AS holiday_scope,
    COALESCE(cal.holiday_label, '') AS holiday_label,
    COALESCE(cal.is_bridge_day, FALSE) AS is_bridge_day,
    COALESCE(cal.is_easter_window, FALSE) AS is_easter_window,
    COALESCE(cal.days_to_holiday, 30) AS days_to_holiday,
    CASE
      WHEN MONTH(co.date) IN (12, 1, 2) THEN 'Invierno'
      WHEN MONTH(co.date) IN (3, 4, 5)  THEN 'Primavera'
      WHEN MONTH(co.date) IN (6, 7, 8)  THEN 'Verano'
      ELSE 'Otono'
    END AS season

  FROM consumo co
  LEFT JOIN clima cl
    ON co.cod_provincia = cl.cod_provincia
    AND co.date = cl.fecha
  LEFT JOIN sunlight sl
    ON co.cod_provincia = sl.cod_provincia
    AND co.date = sl.fecha
  LEFT JOIN calendar_features cal
    ON co.cod_provincia = cal.cod_provincia
    AND co.date = cal.fecha
)
TO '{output_abs}'
(
  FORMAT PARQUET,
  CODEC 'ZSTD',
  COMPRESSION_LEVEL 9,
  ROW_GROUP_SIZE 500000,
  OVERWRITE TRUE
);
")

message("  Ejecutando JOIN...")
t0_merge <- proc.time()
dbExecute(con, query_merge)
elapsed_merge <- (proc.time() - t0_merge)["elapsed"]

# ==============================================================================
# 5. Validacion
# ==============================================================================
message("\n[5/5] Validando resultado...")

output_summary <- dbGetQuery(con, glue("
  SELECT
    COUNT(*) AS n_registros,
    COUNT(DISTINCT user_id) AS n_usuarios,
    MIN(date) AS fecha_min,
    MAX(date) AS fecha_max,
    ROUND(AVG(daily_kWh), 2) AS media_daily_kWh,
    SUM(CASE WHEN tmed IS NOT NULL THEN 1 ELSE 0 END) AS n_con_clima,
    SUM(CASE WHEN tmed IS NULL THEN 1 ELSE 0 END) AS n_sin_clima,
    SUM(CASE WHEN climate_any_imputed THEN 1 ELSE 0 END) AS n_con_clima_imputado,
    SUM(CASE WHEN is_holiday THEN 1 ELSE 0 END) AS n_festivos,
    SUM(CASE WHEN is_holiday_national THEN 1 ELSE 0 END) AS n_festivos_nacionales,
    SUM(CASE WHEN is_holiday_regional_local THEN 1 ELSE 0 END) AS n_festivos_autonomicos_locales,
    SUM(CASE WHEN daylight_hours IS NOT NULL THEN 1 ELSE 0 END) AS n_con_luz
  FROM read_parquet('{output_abs}')
"))

merge_quality_by_province <- dbGetQuery(con, glue("
  SELECT
    cod_provincia,
    COUNT(*) AS n_registros,
    COUNT(DISTINCT user_id) AS n_usuarios,
    SUM(CASE WHEN tmed IS NOT NULL THEN 1 ELSE 0 END) AS n_con_clima,
    SUM(CASE WHEN tmed IS NULL THEN 1 ELSE 0 END) AS n_sin_clima,
    ROUND(100.0 * SUM(CASE WHEN tmed IS NULL THEN 1 ELSE 0 END) / COUNT(*), 2)
      AS pct_sin_clima,
    SUM(CASE WHEN climate_any_imputed THEN 1 ELSE 0 END) AS n_con_clima_imputado,
    ROUND(100.0 * SUM(CASE WHEN climate_any_imputed THEN 1 ELSE 0 END) / COUNT(*), 2)
      AS pct_clima_imputado,
    MIN(date) AS fecha_min,
    MAX(date) AS fecha_max
  FROM read_parquet('{output_abs}')
  GROUP BY cod_provincia
  ORDER BY pct_sin_clima DESC, n_registros DESC
"))

merge_quality_by_province <- merge_quality_by_province |>
  mutate(
    provincia = unname(PROVINCIA_NOMBRES[as.character(cod_provincia)]),
    climate_merge_status = case_when(
      pct_sin_clima == 0 ~ "ok",
      pct_sin_clima <= 1 ~ "missingness_bajo",
      pct_sin_clima <= 5 ~ "revisar_cobertura",
      TRUE ~ "fallback_ERA5_o_estacion_alternativa"
    )
  ) |>
  relocate(provincia, .after = cod_provincia)

write.csv(merge_quality_by_province,
          path(TABLE_DIR, "climate_merge_quality.csv"),
          row.names = FALSE)

merge_imputation_audit <- dbGetQuery(con, glue("
  SELECT
    cod_provincia,
    COUNT(*) AS n_registros,
    COUNT(DISTINCT user_id) AS n_usuarios,
    SUM(CASE WHEN climate_any_imputed THEN 1 ELSE 0 END) AS n_registros_clima_imputado,
    COUNT(DISTINCT CASE WHEN climate_any_imputed THEN user_id ELSE NULL END)
      AS n_usuarios_con_clima_imputado,
    COUNT(DISTINCT CASE WHEN climate_any_imputed THEN date ELSE NULL END)
      AS n_dias_con_clima_imputado,
    ROUND(100.0 * SUM(CASE WHEN climate_any_imputed THEN 1 ELSE 0 END) / COUNT(*), 2)
      AS pct_registros_clima_imputado,
    MIN(CASE WHEN climate_any_imputed THEN date ELSE NULL END) AS fecha_min_imputada,
    MAX(CASE WHEN climate_any_imputed THEN date ELSE NULL END) AS fecha_max_imputada,
    MAX(n_climate_values_imputed) AS max_valores_imputados_registro
  FROM read_parquet('{output_abs}')
  GROUP BY cod_provincia
  ORDER BY pct_registros_clima_imputado DESC, n_registros DESC
")) |>
  mutate(
    provincia = unname(PROVINCIA_NOMBRES[as.character(.data$cod_provincia)])
  ) |>
  relocate(provincia, .after = cod_provincia)

write.csv(merge_imputation_audit,
          path(TABLE_DIR, "climate_merge_imputation_audit.csv"),
          row.names = FALSE)

missing_climate_by_province_date <- dbGetQuery(con, glue("
  SELECT
    out.cod_provincia,
    out.date AS fecha,
    COUNT(*) AS n_registros_sin_clima,
    COUNT(DISTINCT out.user_id) AS n_usuarios_sin_clima,
    CASE
      WHEN MIN(cl.fecha) IS NULL THEN 'sin_fila_climatica_provincia_fecha'
      WHEN SUM(CASE WHEN cl.tmed IS NULL THEN 1 ELSE 0 END) > 0 THEN 'tmed_missing_en_capa_climatica_imputada'
      ELSE 'sin_clima_en_merge'
    END AS missing_reason
  FROM read_parquet('{output_abs}') out
  LEFT JOIN read_parquet('{climate_abs}') cl
    ON out.cod_provincia = cl.cod_provincia
    AND out.date = cl.fecha
  WHERE out.tmed IS NULL
  GROUP BY out.cod_provincia, out.date
  ORDER BY out.cod_provincia, out.date
")) |>
  mutate(
    provincia = unname(PROVINCIA_NOMBRES[as.character(.data$cod_provincia)])
  ) |>
  relocate(provincia, .after = cod_provincia)

write.csv(missing_climate_by_province_date,
          path(TABLE_DIR, "climate_missing_by_province_date.csv"),
          row.names = FALSE)

message(sprintf("  Registros totales: %s", fmt_int(output_summary$n_registros)))
message(sprintf("  Usuarios: %s", fmt_int(output_summary$n_usuarios)))
message(sprintf("  Con datos climaticos: %s (%s%%)",
                fmt_int(output_summary$n_con_clima),
                fmt_num(100 * output_summary$n_con_clima / output_summary$n_registros)))
message(sprintf("  Sin datos climaticos: %s", fmt_int(output_summary$n_sin_clima)))
message(sprintf("  Con clima imputado: %s (%s%%)",
                fmt_int(output_summary$n_con_clima_imputado),
                fmt_num(100 * output_summary$n_con_clima_imputado / output_summary$n_registros)))
message(sprintf("  Registros en festivo: %s", fmt_int(output_summary$n_festivos)))
message(sprintf("    Nacionales: %s | Autonomicos/locales: %s",
                fmt_int(output_summary$n_festivos_nacionales),
                fmt_int(output_summary$n_festivos_autonomicos_locales)))
message(sprintf("  Calidad merge clima exportada a: %s",
                path(TABLE_DIR, "climate_merge_quality.csv")))
message(sprintf("  Auditoria merge-imputacion exportada a: %s",
                path(TABLE_DIR, "climate_merge_imputation_audit.csv")))
message(sprintf("  Missing clima por provincia-fecha exportado a: %s",
                path(TABLE_DIR, "climate_missing_by_province_date.csv")))

DBI::dbDisconnect(con, shutdown = TRUE)

elapsed_total <- (proc.time() - t0_total)["elapsed"]
message(sprintf("\nMerge completado en %.1f s. JOIN en %.1f s.", elapsed_total, elapsed_merge))
message(sprintf("Fichero generado: %s", DAILY_WITH_CLIMATE))

