#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 05: Feature Engineering
# ==============================================================================
#
# Construye un dataset de features a nivel de usuario para alimentar
# los modelos de clustering y como resumen analitico.
#
# Features generadas:
#   A) Perfil horario normalizado (24 features): forma del consumo intradiario
#   B) Ratios de comportamiento: noche/dia, fin de semana/laborable
#   C) Variabilidad: CV diario, amplitud estacional
#   D) Nivel: media diaria, total, dias activos
#   E) Sensibilidad climatica: correlacion consumo ~ HDD, consumo ~ CDD
#   F) Metadata contractual: tarifa, potencia, provincia
#   G) Contexto territorial agregado por provincia, sin datos personales
#   H) Calendario: festivos nacionales frente a autonomicos/locales
#
# Inputs:
#   data/parquet/user_hourly_profile.parquet
#   data/parquet/user_dow_type_profile.parquet
#   data/parquet/user_season_profile.parquet
#   data/parquet/daily_consumption.parquet
#   data/parquet/metadata.parquet
#   data/parquet/features/daily_with_climate.parquet (si existe)
#
# Outputs:
#   data/parquet/features/user_features.parquet
#   outputs/tables/feature_summary.csv
#   outputs/tables/feature_family_dictionary.csv
#   outputs/tables/feature_family_groups.csv
#   outputs/tables/context_feature_audit.csv
#
# Uso:
#   Rscript R/04_features/04a_legacy_features.R
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(glue)
  library(fs)
})

source(here::here("_config.R"))

message("=" |> strrep(60))
message("PASO 05: Feature Engineering")
message("=" |> strrep(60))

t0_total <- proc.time()

stopifnot(
  "Falta daily_consumption.parquet" = file_exists(DAILY_PARQUET),
  "Falta user_hourly_profile.parquet" = file_exists(USER_HOURLY_PROFILE),
  "Falta metadata.parquet" = file_exists(METADATA_PARQUET),
  "Falta data/external/province_context.csv" = file_exists(PROVINCE_CONTEXT_CSV)
)

has_climate <- file_exists(DAILY_WITH_CLIMATE)
if (!has_climate) {
  message("  AVISO: No existe daily_with_climate.parquet. Features climaticas no disponibles.")
}

con <- connect_duckdb()

build_feature_family_dictionary <- function(feature_names) {
  rules <- tibble::tribble(
    ~pattern, ~family, ~purpose, ~source, ~privacy_level,
    "^(user_id|cod_provincia)$", "Identificacion y alcance",
    "Clave tecnica y provincia de modelado; no se usa para definir distancia de clustering.",
    "metadata.parquet", "pseudonymous_internal_id",
    "^norm_h[0-9]{2}$", "Perfil horario normalizado",
    "Forma intradiaria del consumo, normalizada por la media del usuario.",
    "user_hourly_profile.parquet", "pseudonymous_consumption_aggregate",
    "^(ratio_night_day|ratio_weekend_weekday|morning_kWh_share|afternoon_kWh_share|evening_kWh_share|night_kWh_share|ratio_morning_afternoon|ratio_evening_morning|ratio_night_morning|weekday_peak_hour|weekend_peak_hour|weekday_weekend_peak_shift|peak_hour|seasonal_amplitude|seasonal_amplitude_norm)$",
    "Ratios de comportamiento", "Patrones relativos de uso horario, semanal y estacional.",
    "perfiles agregados de consumo", "pseudonymous_consumption_aggregate",
    "^(mean_daily_kWh|sd_daily_kWh|median_daily_kWh|p90_daily_kWh|total_kWh|active_days|first_date|last_date|span_days|zero_day_rate|low_day_rate|cv_daily|log_mean_daily_kWh)$",
    "Nivel, cobertura y variabilidad", "Magnitud, dispersion y continuidad temporal de la serie diaria.",
    "daily_consumption.parquet", "pseudonymous_consumption_aggregate",
    "^(months_observed|max_month_share|monthly_entropy|summer_winter_ratio|summer_share|winter_share)$",
    "Comportamiento mensual", "Concentracion mensual y contraste verano-invierno.",
    "daily_consumption.parquet", "pseudonymous_consumption_aggregate",
    "^(valley_kWh_week|flat_kWh_week|peak_kWh_week|valley_share|flat_share|peak_share|peak_to_valley_ratio)$",
    "Periodos tarifarios 2.0TD", "Distribucion del consumo reconstruida por periodos valle, llano y punta.",
    "user_dow_type_profile.parquet", "pseudonymous_consumption_aggregate",
    "^(holiday_ratio|national_holiday_ratio|regional_local_holiday_ratio|bridge_ratio|holiday_nonholiday_ratio|national_holiday_nonholiday_ratio|regional_local_holiday_nonholiday_ratio|holiday_regional_to_national_ratio|weekend_weekday_daily_ratio|low_consumption_spell_rate)$",
    "Calendario y festivos", "Frecuencia y respuesta relativa ante fines de semana, puentes y festivos por ambito.",
    "daily_with_climate.parquet", "public_calendar_plus_consumption_aggregate",
    "^(tarifa_clean|p1_kw|cnae|is_residential_strict|is_residential_permissive)$",
    "Metadata contractual", "Senales contractuales usadas para filtrar y describir uso residencial.",
    "metadata.parquet", "contract_metadata_minimized",
    "^(ccaa|coastal_flag|density_bucket|climate_zone|goiener_core_region)$",
    "Contexto territorial agregado", "Variables provinciales externas, agregadas y justificables; no personales.",
    "data/external/province_context.csv", "provincial_aggregate_no_personal_data",
    "^(n_extreme_hourly|pct_extreme_hourly|has_sustained_extreme)$",
    "Calidad y outliers", "Control de lecturas extremas sostenidas para interpretar la matriz.",
    "parquet horario", "pseudonymous_quality_aggregate",
    "^(corr_hdd|corr_cdd|corr_tmed|mean_hdd|mean_cdd)$",
    "Sensibilidad climatica", "Relacion exploratoria entre consumo diario y demanda termica.",
    "daily_with_climate.parquet", "public_weather_plus_consumption_aggregate",
    "^(possible_intermittent_home|proxy_autoconsumption_second_home)$",
    "Proxies exploratorios", "Indicadores derivados para revision analitica, no etiquetas verificadas.",
    "features derivadas", "pseudonymous_consumption_aggregate"
  )

  dictionary <- tibble::tibble(
    feature = feature_names,
    family = "Sin clasificar",
    purpose = "Revisar manualmente si se incorpora una nueva feature.",
    source = "desconocido",
    privacy_level = "review_required"
  )

  for (i in seq_len(nrow(rules))) {
    matched <- grepl(rules$pattern[i], dictionary$feature) &
      dictionary$family == "Sin clasificar"
    dictionary$family[matched] <- rules$family[i]
    dictionary$purpose[matched] <- rules$purpose[i]
    dictionary$source[matched] <- rules$source[i]
    dictionary$privacy_level[matched] <- rules$privacy_level[i]
  }

  dictionary
}

hourly_profile_abs <- path_abs(USER_HOURLY_PROFILE) |> path_norm()
dow_profile_abs    <- path_abs(USER_DOW_TYPE_PROFILE) |> path_norm()
season_profile_abs <- path_abs(USER_SEASON_PROFILE) |> path_norm()
daily_abs          <- path_abs(DAILY_PARQUET) |> path_norm()
metadata_abs       <- path_abs(METADATA_PARQUET) |> path_norm()
climate_daily_abs  <- if (has_climate) path_abs(DAILY_WITH_CLIMATE) |> path_norm() else ""
focus_filter_sql <- focus_province_filter_sql("cod_provincia")
province_context_raw <- read.csv(
  PROVINCE_CONTEXT_CSV,
  colClasses = c(cod_provincia = "character"),
  stringsAsFactors = FALSE
)

forbidden_external_cols <- c(
  "cups", "user_id", "dni", "nif", "nie", "nombre", "apellido",
  "email", "telefono", "phone", "direccion", "address", "renta",
  "income", "household", "vivienda"
)
external_personal_cols <- intersect(tolower(names(province_context_raw)),
                                    forbidden_external_cols)
if (length(external_personal_cols) > 0) {
  stop(sprintf(
    "province_context.csv contiene posibles identificadores personales: %s",
    paste(external_personal_cols, collapse = ", ")
  ))
}

province_context <- province_context_raw |>
  mutate(
    cod_provincia = sprintf("%02s", as.character(cod_provincia)),
    coastal_flag = as.logical(coastal_flag),
    goiener_core_region = as.logical(goiener_core_region)
  ) |>
  distinct(cod_provincia, .keep_all = TRUE)

dbExecute(con, glue("
  CREATE TEMP TABLE model_users AS
  SELECT user_id, cod_provincia
  FROM (
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
    AND user_id IS NOT NULL AND user_id <> ''
    AND cod_provincia IS NOT NULL AND cod_provincia <> ''
    AND {focus_filter_sql}
"))

model_scope_counts <- dbGetQuery(con, "
  SELECT cod_provincia, COUNT(*) AS n_usuarios
  FROM model_users
  GROUP BY cod_provincia
  ORDER BY n_usuarios DESC
")
message(sprintf(
  "  Alcance modelado: %s usuarios en %s provincias (%s)",
  fmt_int(sum(model_scope_counts$n_usuarios)),
  fmt_int(nrow(model_scope_counts)),
  paste(FOCUS_PROVINCES, collapse = ", ")
))

# ==============================================================================
# 1. Perfil horario normalizado por usuario (24 features)
# ==============================================================================
message("\n[1/6] Perfil horario normalizado...")

hourly_wide <- dbGetQuery(con, glue("
  WITH user_total AS (
    SELECT user_id, AVG(mean_kWh_user) AS user_mean
    FROM read_parquet('{hourly_profile_abs}')
    WHERE user_id IN (SELECT user_id FROM model_users)
    GROUP BY user_id
  ),
  normalized AS (
    SELECT
      p.user_id,
      p.hour,
      -- Normalizar dividiendo por la media del usuario
      CASE WHEN ut.user_mean > 0 THEN p.mean_kWh_user / ut.user_mean ELSE 0 END AS norm_kWh
    FROM read_parquet('{hourly_profile_abs}') p
    INNER JOIN user_total ut ON p.user_id = ut.user_id
  )
  PIVOT normalized
  ON hour
  USING FIRST(norm_kWh)
  GROUP BY user_id
  ORDER BY user_id
"))

# Renombrar columnas pivot a nombres consistentes
hour_cols <- setdiff(names(hourly_wide), "user_id")
new_names <- sprintf("norm_h%02d", as.integer(hour_cols))
names(hourly_wide)[names(hourly_wide) %in% hour_cols] <- new_names

message(sprintf("  Usuarios con perfil horario: %s", fmt_int(nrow(hourly_wide))))

# ==============================================================================
# 2. Ratios de comportamiento
# ==============================================================================
message("\n[2/6] Ratios de comportamiento (noche/dia, fin de semana/laborable)...")

behavior_ratios <- dbGetQuery(con, glue("
  WITH
  hourly_focus AS (
    SELECT *
    FROM read_parquet('{hourly_profile_abs}')
    WHERE user_id IN (SELECT user_id FROM model_users)
  ),
  dow_focus AS (
    SELECT *
    FROM read_parquet('{dow_profile_abs}')
    WHERE user_id IN (SELECT user_id FROM model_users)
  ),
  season_focus AS (
    SELECT *
    FROM read_parquet('{season_profile_abs}')
    WHERE user_id IN (SELECT user_id FROM model_users)
  ),
  -- Ratio noche/dia: noche = horas 0-6, dia = horas 7-23
  night_day AS (
    SELECT
      user_id,
      AVG(CASE WHEN hour BETWEEN 0 AND 6 THEN mean_kWh_user END) AS mean_night,
      AVG(CASE WHEN hour BETWEEN 7 AND 23 THEN mean_kWh_user END) AS mean_day
    FROM hourly_focus
    GROUP BY user_id
  ),
  -- Ratio fin de semana / laborable
  dow_type AS (
    SELECT
      user_id,
      AVG(CASE WHEN tipo_dia = 'Fin de semana' THEN mean_kWh_user END) AS mean_weekend,
      AVG(CASE WHEN tipo_dia = 'Laborable' THEN mean_kWh_user END) AS mean_weekday
    FROM dow_focus
    GROUP BY user_id
  ),
  dayparts AS (
    SELECT
      user_id,
      SUM(CASE WHEN hour BETWEEN 6 AND 11 THEN mean_kWh_user ELSE 0 END) AS morning_kWh,
      SUM(CASE WHEN hour BETWEEN 12 AND 17 THEN mean_kWh_user ELSE 0 END) AS afternoon_kWh,
      SUM(CASE WHEN hour BETWEEN 18 AND 23 THEN mean_kWh_user ELSE 0 END) AS evening_kWh,
      SUM(CASE WHEN hour BETWEEN 0 AND 5 THEN mean_kWh_user ELSE 0 END) AS night_kWh,
      SUM(mean_kWh_user) AS total_profile_kWh
    FROM hourly_focus
    GROUP BY user_id
  ),
  dow_peak_ranked AS (
    SELECT
      user_id,
      tipo_dia,
      hour,
      ROW_NUMBER() OVER (
        PARTITION BY user_id, tipo_dia
        ORDER BY mean_kWh_user DESC, hour
      ) AS rn
    FROM dow_focus
    WHERE tipo_dia IN ('Laborable', 'Fin de semana')
  ),
  dow_peaks AS (
    SELECT
      user_id,
      MAX(CASE WHEN tipo_dia = 'Laborable' THEN hour END) AS weekday_peak_hour,
      MAX(CASE WHEN tipo_dia = 'Fin de semana' THEN hour END) AS weekend_peak_hour
    FROM dow_peak_ranked
    WHERE rn = 1
    GROUP BY user_id
  ),
  -- Hora pico por usuario
  peak AS (
    SELECT user_id, hour AS peak_hour
    FROM (
      SELECT user_id, hour,
             ROW_NUMBER() OVER (PARTITION BY user_id ORDER BY mean_kWh_user DESC) AS rn
      FROM hourly_focus
    ) sub
    WHERE rn = 1
  ),
  -- Amplitud estacional: max estacion - min estacion
  season_amp AS (
    SELECT
      user_id,
      MAX(season_mean) - MIN(season_mean) AS seasonal_amplitude,
      -- Normalizar por la media global del usuario
      CASE WHEN AVG(season_mean) > 0
        THEN (MAX(season_mean) - MIN(season_mean)) / AVG(season_mean)
      ELSE 0
      END AS seasonal_amplitude_norm
    FROM (
      SELECT user_id, season, AVG(mean_kWh_user) AS season_mean
      FROM season_focus
      GROUP BY user_id, season
    ) sub
    GROUP BY user_id
  )
  SELECT
    nd.user_id,
    CASE WHEN nd.mean_day > 0 THEN nd.mean_night / nd.mean_day ELSE NULL END AS ratio_night_day,
    CASE WHEN dt.mean_weekday > 0 THEN dt.mean_weekend / dt.mean_weekday ELSE NULL END AS ratio_weekend_weekday,
    dp.morning_kWh / NULLIF(dp.total_profile_kWh, 0) AS morning_kWh_share,
    dp.afternoon_kWh / NULLIF(dp.total_profile_kWh, 0) AS afternoon_kWh_share,
    dp.evening_kWh / NULLIF(dp.total_profile_kWh, 0) AS evening_kWh_share,
    dp.night_kWh / NULLIF(dp.total_profile_kWh, 0) AS night_kWh_share,
    dp.morning_kWh / NULLIF(dp.afternoon_kWh, 0) AS ratio_morning_afternoon,
    dp.evening_kWh / NULLIF(dp.morning_kWh, 0) AS ratio_evening_morning,
    dp.night_kWh / NULLIF(dp.morning_kWh, 0) AS ratio_night_morning,
    dpk.weekday_peak_hour,
    dpk.weekend_peak_hour,
    ABS(dpk.weekend_peak_hour - dpk.weekday_peak_hour) AS weekday_weekend_peak_shift,
    p.peak_hour,
    sa.seasonal_amplitude,
    sa.seasonal_amplitude_norm
  FROM night_day nd
  LEFT JOIN dow_type dt ON nd.user_id = dt.user_id
  LEFT JOIN dayparts dp ON nd.user_id = dp.user_id
  LEFT JOIN dow_peaks dpk ON nd.user_id = dpk.user_id
  LEFT JOIN peak p ON nd.user_id = p.user_id
  LEFT JOIN season_amp sa ON nd.user_id = sa.user_id
"))

message(sprintf("  Ratios calculados para %s usuarios", fmt_int(nrow(behavior_ratios))))

# ==============================================================================
# 3. Estadisticos diarios por usuario
# ==============================================================================
message("\n[3/6] Estadisticos diarios por usuario...")

daily_stats <- dbGetQuery(con, glue("
  SELECT
    d.user_id,
    AVG(d.daily_kWh) AS mean_daily_kWh,
    STDDEV_SAMP(d.daily_kWh) AS sd_daily_kWh,
    MEDIAN(d.daily_kWh) AS median_daily_kWh,
    QUANTILE_CONT(d.daily_kWh, 0.90) AS p90_daily_kWh,
    SUM(d.daily_kWh) AS total_kWh,
    COUNT(*) AS active_days,
    MIN(d.date) AS first_date,
    MAX(d.date) AS last_date,
    DATEDIFF('day', MIN(d.date), MAX(d.date)) AS span_days,
    AVG(CASE WHEN d.daily_kWh = 0 THEN 1.0 ELSE 0.0 END) AS zero_day_rate,
    AVG(CASE WHEN d.daily_kWh <= 1 THEN 1.0 ELSE 0.0 END) AS low_day_rate,
    -- CV diario
    CASE WHEN AVG(d.daily_kWh) > 0
      THEN STDDEV_SAMP(d.daily_kWh) / AVG(d.daily_kWh)
      ELSE NULL
    END AS cv_daily
  FROM read_parquet('{daily_abs}') d
  INNER JOIN model_users mu ON d.user_id = mu.user_id
  WHERE d.user_id IS NOT NULL AND d.user_id <> ''
    AND d.daily_kWh IS NOT NULL AND d.daily_kWh >= 0
    AND d.hours_recorded = 24
  GROUP BY d.user_id
"))

message(sprintf("  Estadisticos para %s usuarios", fmt_int(nrow(daily_stats))))

monthly_behavior <- dbGetQuery(con, glue("
  WITH monthly AS (
    SELECT
      user_id,
      YEAR(date) AS year,
      MONTH(date) AS month,
      SUM(daily_kWh) AS monthly_kWh
    FROM read_parquet('{daily_abs}')
    WHERE user_id IN (SELECT user_id FROM model_users)
      AND user_id IS NOT NULL AND user_id <> ''
      AND daily_kWh IS NOT NULL AND daily_kWh >= 0
      AND hours_recorded = 24
    GROUP BY user_id, YEAR(date), MONTH(date)
  ),
  totals AS (
    SELECT
      user_id,
      COUNT(*) AS months_observed,
      SUM(monthly_kWh) AS total_monthly_kWh,
      SUM(CASE WHEN month IN (6, 7, 8) THEN monthly_kWh ELSE 0 END) AS summer_kWh,
      SUM(CASE WHEN month IN (12, 1, 2) THEN monthly_kWh ELSE 0 END) AS winter_kWh
    FROM monthly
    GROUP BY user_id
  ),
  shares AS (
    SELECT
      m.user_id,
      m.month,
      t.months_observed,
      t.total_monthly_kWh,
      t.summer_kWh,
      t.winter_kWh,
      CASE WHEN t.total_monthly_kWh > 0
        THEN m.monthly_kWh / t.total_monthly_kWh
        ELSE NULL
      END AS month_share
    FROM monthly m
    INNER JOIN totals t ON m.user_id = t.user_id
  )
  SELECT
    user_id,
    MAX(months_observed) AS months_observed,
    MAX(month_share) AS max_month_share,
    CASE
      WHEN COUNT(*) FILTER (WHERE month_share > 0) > 1
      THEN -SUM(CASE WHEN month_share > 0 THEN month_share * LN(month_share) ELSE 0 END) /
           LN(COUNT(*) FILTER (WHERE month_share > 0))
      ELSE 0
    END AS monthly_entropy,
    MAX(summer_kWh) / NULLIF(MAX(winter_kWh), 0) AS summer_winter_ratio,
    MAX(summer_kWh) / NULLIF(MAX(total_monthly_kWh), 0) AS summer_share,
    MAX(winter_kWh) / NULLIF(MAX(total_monthly_kWh), 0) AS winter_share
  FROM shares
  GROUP BY user_id
"))

message(sprintf("  Comportamiento mensual para %s usuarios", fmt_int(nrow(monthly_behavior))))

tariff_period_features <- dbGetQuery(con, glue("
  WITH period_profile AS (
    SELECT
      user_id,
      CASE
        WHEN tipo_dia <> 'Laborable' THEN 'valle'
        WHEN hour BETWEEN 0 AND 7 THEN 'valle'
        WHEN hour IN (10, 11, 12, 13, 18, 19, 20, 21) THEN 'punta'
        WHEN hour IN (8, 9, 14, 15, 16, 17, 22, 23) THEN 'llano'
        ELSE NULL
      END AS period_20td,
      mean_kWh_user * CASE WHEN tipo_dia = 'Laborable' THEN 5 ELSE 2 END AS weighted_kWh
    FROM read_parquet('{dow_profile_abs}')
    WHERE user_id IN (SELECT user_id FROM model_users)
      AND user_id IS NOT NULL AND user_id <> ''
  ),
  period_sum AS (
    SELECT user_id, period_20td, SUM(weighted_kWh) AS period_kWh_week
    FROM period_profile
    WHERE period_20td IS NOT NULL
    GROUP BY user_id, period_20td
  ),
  wide AS (
    SELECT
      user_id,
      SUM(CASE WHEN period_20td = 'valle' THEN period_kWh_week ELSE 0 END) AS valley_kWh_week,
      SUM(CASE WHEN period_20td = 'llano' THEN period_kWh_week ELSE 0 END) AS flat_kWh_week,
      SUM(CASE WHEN period_20td = 'punta' THEN period_kWh_week ELSE 0 END) AS peak_kWh_week
    FROM period_sum
    GROUP BY user_id
  )
  SELECT
    user_id,
    valley_kWh_week,
    flat_kWh_week,
    peak_kWh_week,
    valley_kWh_week / NULLIF(valley_kWh_week + flat_kWh_week + peak_kWh_week, 0) AS valley_share,
    flat_kWh_week / NULLIF(valley_kWh_week + flat_kWh_week + peak_kWh_week, 0) AS flat_share,
    peak_kWh_week / NULLIF(valley_kWh_week + flat_kWh_week + peak_kWh_week, 0) AS peak_share,
    peak_kWh_week / NULLIF(valley_kWh_week, 0) AS peak_to_valley_ratio
  FROM wide
"))

message(sprintf("  Shares 2.0TD para %s usuarios", fmt_int(nrow(tariff_period_features))))

if (has_climate) {
  climate_daily_cols <- names(dbGetQuery(con, glue(
    "SELECT * FROM read_parquet('{climate_daily_abs}') LIMIT 0"
  )))
  has_holiday_split <- all(c("is_holiday_national", "is_holiday_regional_local") %in%
                             climate_daily_cols)
  holiday_national_sql <- if ("is_holiday_national" %in% climate_daily_cols) {
    "CASE WHEN is_holiday_national THEN 1 ELSE 0 END"
  } else {
    "CASE WHEN is_holiday THEN 1 ELSE 0 END"
  }
  holiday_regional_local_sql <- if ("is_holiday_regional_local" %in% climate_daily_cols) {
    "CASE WHEN is_holiday_regional_local THEN 1 ELSE 0 END"
  } else {
    "0"
  }

  message(sprintf(
    "  Desglose nacional/autonomico-local de festivos: %s",
    ifelse(has_holiday_split, "disponible", "no disponible; se usa fallback nacional")
  ))

  calendar_behavior <- dbGetQuery(con, glue("
    WITH daily AS (
      SELECT
        user_id,
        date,
        daily_kWh,
        CASE WHEN is_holiday THEN 1 ELSE 0 END AS is_holiday_i,
        {holiday_national_sql} AS is_holiday_national_i,
        {holiday_regional_local_sql} AS is_holiday_regional_local_i,
        CASE WHEN is_bridge_day THEN 1 ELSE 0 END AS is_bridge_day_i,
        CASE WHEN is_weekend THEN 1 ELSE 0 END AS is_weekend_i,
        CASE WHEN daily_kWh <= 1 THEN 1 ELSE 0 END AS is_low_i
      FROM read_parquet('{climate_daily_abs}')
      WHERE user_id IS NOT NULL AND user_id <> ''
        AND daily_kWh IS NOT NULL AND daily_kWh >= 0
        AND hours_recorded = 24
    ),
    marked AS (
      SELECT
        *,
        LAG(is_low_i) OVER (PARTITION BY user_id ORDER BY date) AS prev_low_i,
        LEAD(is_low_i) OVER (PARTITION BY user_id ORDER BY date) AS next_low_i
      FROM daily
    )
    SELECT
      user_id,
      AVG(is_holiday_i) AS holiday_ratio,
      AVG(is_holiday_national_i) AS national_holiday_ratio,
      AVG(is_holiday_regional_local_i) AS regional_local_holiday_ratio,
      AVG(is_bridge_day_i) AS bridge_ratio,
      AVG(CASE WHEN is_holiday_i = 1 THEN daily_kWh END) /
        NULLIF(AVG(CASE WHEN is_holiday_i = 0 THEN daily_kWh END), 0) AS holiday_nonholiday_ratio,
      AVG(CASE WHEN is_holiday_national_i = 1 THEN daily_kWh END) /
        NULLIF(AVG(CASE WHEN is_holiday_i = 0 THEN daily_kWh END), 0) AS national_holiday_nonholiday_ratio,
      AVG(CASE WHEN is_holiday_regional_local_i = 1 THEN daily_kWh END) /
        NULLIF(AVG(CASE WHEN is_holiday_i = 0 THEN daily_kWh END), 0) AS regional_local_holiday_nonholiday_ratio,
      AVG(is_holiday_regional_local_i) /
        NULLIF(AVG(is_holiday_national_i), 0) AS holiday_regional_to_national_ratio,
      AVG(CASE WHEN is_weekend_i = 1 THEN daily_kWh END) /
        NULLIF(AVG(CASE WHEN is_weekend_i = 0 THEN daily_kWh END), 0) AS weekend_weekday_daily_ratio,
      AVG(CASE
        WHEN is_low_i = 1 AND (prev_low_i = 1 OR next_low_i = 1) THEN 1.0
        ELSE 0.0
      END) AS low_consumption_spell_rate
    FROM marked
    GROUP BY user_id
  "))
} else {
  calendar_behavior <- tibble::tibble(
    user_id = character(0),
    holiday_ratio = numeric(0),
    national_holiday_ratio = numeric(0),
    regional_local_holiday_ratio = numeric(0),
    bridge_ratio = numeric(0),
    holiday_nonholiday_ratio = numeric(0),
    national_holiday_nonholiday_ratio = numeric(0),
    regional_local_holiday_nonholiday_ratio = numeric(0),
    holiday_regional_to_national_ratio = numeric(0),
    weekend_weekday_daily_ratio = numeric(0),
    low_consumption_spell_rate = numeric(0)
  )
}

message(sprintf("  Patrones calendario/rachas para %s usuarios",
                fmt_int(nrow(calendar_behavior))))

# ==============================================================================
# 4. Metadata contractual
# ==============================================================================
message("\n[4/6] Metadata contractual...")

residential_tariffs_sql <- paste(sprintf("'%s'", RESIDENTIAL_TARIFFS), collapse = ",")

metadata_features <- dbGetQuery(con, glue("
  SELECT
    user_id,
    cod_provincia,
    tarifa_clean,
    p1_kw,
    cnae,
    -- Flag residencial estricto: tarifa explÃ­citamente domÃ©stica (2.0*)
    (tarifa_clean IN ({residential_tariffs_sql})) AS is_residential_strict,
    -- Flag residencial permisivo (anexo): tarifa estricta o (NA + p1_kw <= umbral)
    (
      tarifa_clean IN ({residential_tariffs_sql})
      OR (tarifa_clean = 'Desconocida' AND p1_kw IS NOT NULL AND p1_kw <= {RESIDENTIAL_P1KW_MAX})
    ) AS is_residential_permissive
  FROM (
    SELECT
      cups AS user_id,
      SUBSTR(LPAD(CAST(codigo_postal AS VARCHAR), 5, '0'), 1, 2) AS cod_provincia,
      COALESCE(NULLIF(tarifa_atr, ''), 'Desconocida') AS tarifa_clean,
      p1_kw,
      CAST(cnae AS VARCHAR) AS cnae,
      ROW_NUMBER() OVER (
        PARTITION BY cups
        ORDER BY
          CASE WHEN tarifa_atr IS NOT NULL AND tarifa_atr <> '' THEN 0 ELSE 1 END,
          fecha_alta DESC NULLS LAST
      ) AS rn
    FROM read_parquet('{metadata_abs}')
  ) sub
  WHERE rn = 1
    AND {focus_filter_sql}
"))

message(sprintf("  Metadata para %s usuarios", fmt_int(nrow(metadata_features))))
message(sprintf("    -> Residenciales (estricto, 2.0*):     %s (%s%%)",
                fmt_int(sum(metadata_features$is_residential_strict, na.rm = TRUE)),
                fmt_num(100 * mean(metadata_features$is_residential_strict, na.rm = TRUE), 1)))
message(sprintf("    -> Residenciales (permisivo + p1<=%g): %s (%s%%)",
                RESIDENTIAL_P1KW_MAX,
                fmt_int(sum(metadata_features$is_residential_permissive, na.rm = TRUE)),
                fmt_num(100 * mean(metadata_features$is_residential_permissive, na.rm = TRUE), 1)))

# ==============================================================================
# 4.b Marcas de outliers extremos a nivel de usuario
# ==============================================================================
message("\n[4b/6] Detectando usuarios con lecturas extremas (>", OUTLIER_KWH_HOUR, " kWh/h)...")

# Contar lecturas horarias extremas por usuario en el dataset bruto
hourly_glob <- HOURLY_GLOB
extreme_hourly <- dbGetQuery(con, glue("
  SELECT
    user_id,
    COUNT(*) FILTER (WHERE kWh > {OUTLIER_KWH_HOUR}) AS n_extreme_hourly,
    COUNT(*) AS n_hourly_total
  FROM read_parquet('{hourly_glob}', hive_partitioning = true)
  WHERE user_id IN (SELECT user_id FROM model_users)
    AND user_id IS NOT NULL AND kWh IS NOT NULL
  GROUP BY user_id
"))

extreme_hourly <- extreme_hourly |>
  mutate(
    pct_extreme_hourly = ifelse(n_hourly_total > 0,
                                100 * n_extreme_hourly / n_hourly_total, 0),
    # Outlier sostenido: >0.1% de las lecturas o >50 ocurrencias
    has_sustained_extreme = (pct_extreme_hourly > 0.1) | (n_extreme_hourly > 50)
  )

n_with_extreme    <- sum(extreme_hourly$n_extreme_hourly > 0, na.rm = TRUE)
n_with_sustained  <- sum(extreme_hourly$has_sustained_extreme, na.rm = TRUE)

message(sprintf("  Usuarios con alguna lectura extrema:  %s",  fmt_int(n_with_extreme)))
message(sprintf("  Usuarios con outlier SOSTENIDO:        %s",  fmt_int(n_with_sustained)))

# ==============================================================================
# 5. Sensibilidad climatica (si existe el merge)
# ==============================================================================

if (has_climate) {
  message("\n[5/6] Sensibilidad climatica (correlacion consumo ~ HDD/CDD)...")

  climate_sensitivity <- dbGetQuery(con, glue("
    SELECT
      user_id,
      CORR(daily_kWh, hdd) AS corr_hdd,
      CORR(daily_kWh, cdd) AS corr_cdd,
      CORR(daily_kWh, tmed) AS corr_tmed,
      AVG(hdd) AS mean_hdd,
      AVG(cdd) AS mean_cdd
    FROM read_parquet('{climate_daily_abs}')
    WHERE tmed IS NOT NULL
    GROUP BY user_id
    HAVING COUNT(*) >= 90  -- Al menos 90 dias con dato climatico
  "))

  message(sprintf("  Sensibilidad climatica para %s usuarios", fmt_int(nrow(climate_sensitivity))))
} else {
  message("\n[5/6] Sensibilidad climatica: omitida (no hay datos climaticos)")
  climate_sensitivity <- tibble(user_id = character(0),
                                corr_hdd = numeric(0),
                                corr_cdd = numeric(0),
                                corr_tmed = numeric(0),
                                mean_hdd = numeric(0),
                                mean_cdd = numeric(0))
}

# ==============================================================================
# 6. Unir todo y exportar
# ==============================================================================
message("\n[6/6] Uniendo features y exportando...")

user_features <- hourly_wide |>
  inner_join(behavior_ratios, by = "user_id") |>
  inner_join(daily_stats, by = "user_id") |>
  left_join(monthly_behavior, by = "user_id") |>
  left_join(tariff_period_features, by = "user_id") |>
  left_join(calendar_behavior, by = "user_id") |>
  inner_join(metadata_features, by = "user_id") |>
  left_join(province_context, by = "cod_provincia") |>
  left_join(extreme_hourly |>
              select(user_id, n_extreme_hourly, pct_extreme_hourly,
                     has_sustained_extreme),
            by = "user_id") |>
  left_join(climate_sensitivity, by = "user_id")

# Imputar flags ausentes (usuarios sin lecturas registradas en el barrido horario)
user_features <- user_features |>
  mutate(
    n_extreme_hourly       = tidyr::replace_na(n_extreme_hourly, 0L),
    pct_extreme_hourly     = tidyr::replace_na(pct_extreme_hourly, 0),
    has_sustained_extreme  = tidyr::replace_na(has_sustained_extreme, FALSE),
    is_residential_strict     = tidyr::replace_na(is_residential_strict, FALSE),
    is_residential_permissive = tidyr::replace_na(is_residential_permissive, FALSE),
    holiday_ratio = tidyr::replace_na(holiday_ratio, 0),
    national_holiday_ratio = tidyr::replace_na(national_holiday_ratio, 0),
    regional_local_holiday_ratio = tidyr::replace_na(regional_local_holiday_ratio, 0),
    holiday_regional_to_national_ratio = tidyr::replace_na(holiday_regional_to_national_ratio, 0),
    bridge_ratio = tidyr::replace_na(bridge_ratio, 0),
    low_consumption_spell_rate = dplyr::coalesce(low_consumption_spell_rate, low_day_rate),
    coastal_flag = tidyr::replace_na(coastal_flag, FALSE),
    goiener_core_region = tidyr::replace_na(goiener_core_region, FALSE),
    ccaa = tidyr::replace_na(ccaa, "Sin contexto"),
    density_bucket = tidyr::replace_na(density_bucket, "unknown"),
    climate_zone = tidyr::replace_na(climate_zone, "unknown")
  )

# Transformaciones finales
user_features <- user_features |>
  mutate(
    log_mean_daily_kWh = log1p(mean_daily_kWh),
    possible_intermittent_home = low_day_rate >= 0.50 |
      zero_day_rate >= 0.20 |
      max_month_share >= 0.16,
    proxy_autoconsumption_second_home = low_day_rate >= 0.35 &
      morning_kWh_share >= 0.22 &
      peak_share <= 0.28
  )

if (any(!user_features$cod_provincia %in% FOCUS_PROVINCES)) {
  stop("user_features contiene provincias fuera del alcance de modelado.")
}

message(sprintf("  Features finales: %d usuarios x %d columnas",
                nrow(user_features), ncol(user_features)))

# Guardar
arrow::write_parquet(user_features, USER_FEATURES_BASE_PARQUET)
message(sprintf("  Guardado en: %s", USER_FEATURES_BASE_PARQUET))

# Exportar resumen para la memoria
feature_dictionary <- build_feature_family_dictionary(names(user_features))

feature_summary <- tibble::tibble(
  feature = names(user_features),
  tipo = sapply(user_features, class) |> sapply(\(x) x[1]),
  n_na = sapply(user_features, \(x) sum(is.na(x))),
  pct_na = sapply(user_features, \(x) round(100 * mean(is.na(x)), 1))
)|>
  left_join(feature_dictionary, by = "feature")

write.csv(feature_summary, path(TABLE_DIR, "feature_summary.csv"), row.names = FALSE)
write.csv(feature_dictionary,
          path(TABLE_DIR, "feature_family_dictionary.csv"),
          row.names = FALSE)

feature_family_groups <- feature_summary |>
  group_by(family, purpose, source, privacy_level) |>
  summarise(
    n_features = n(),
    feature_examples = {
      examples <- head(feature, 8)
      if (n() > 8) examples <- c(examples, "...")
      paste(examples, collapse = ", ")
    },
    n_features_with_na = sum(n_na > 0),
    max_pct_na = max(pct_na, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(family)

write.csv(feature_family_groups,
          path(TABLE_DIR, "feature_family_groups.csv"),
          row.names = FALSE)

if (any(feature_dictionary$family == "Sin clasificar")) {
  message("  AVISO: existen features sin familia asignada en feature_family_dictionary.csv")
}

context_cols <- c(
  "ccaa", "coastal_flag", "density_bucket", "climate_zone",
  "goiener_core_region"
)
context_justification <- tibble::tribble(
  ~feature, ~aggregation_level, ~justification,
  "ccaa", "provincia", "Control territorial agregado para interpretar diferencias regulatorias y geograficas.",
  "coastal_flag", "provincia", "Indicador fisico agregado para separar costa/interior sin localizar usuarios.",
  "density_bucket", "provincia", "Bucket demografico provincial, usado solo como contexto macro.",
  "climate_zone", "provincia", "Zona climatica provincial para explicar heterogeneidad meteorologica residual.",
  "goiener_core_region", "provincia", "Marca agregada del nucleo territorial historico de GoiEner."
)
n_feature_users <- nrow(user_features)
context_feature_audit <- context_justification |>
  mutate(
    n_users = n_feature_users,
    n_missing = vapply(.data$feature, function(col) {
    sum(is.na(user_features[[col]]) |
          user_features[[col]] %in% c("", "Sin contexto", "unknown"))
    }, numeric(1)),
    pct_missing = round(100 * .data$n_missing / .data$n_users, 2),
    source = "data/external/province_context.csv",
    privacy_level = "provincial_aggregate_no_personal_data",
    includes_personal_data = FALSE
  ) |>
  select(feature, n_users, n_missing, pct_missing, source,
         aggregation_level, justification, privacy_level,
         includes_personal_data)
write.csv(context_feature_audit,
          path(TABLE_DIR, "context_feature_audit.csv"),
          row.names = FALSE)

DBI::dbDisconnect(con, shutdown = TRUE)

elapsed_total <- (proc.time() - t0_total)["elapsed"]
message(sprintf("\nPaso 05 completado en %.1f s.", elapsed_total))

