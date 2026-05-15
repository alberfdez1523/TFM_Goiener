#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 03: Construccion del dataset climatico AEMET
# ==============================================================================
#
# Esta version recupera el flujo antiguo que funcionaba en el proyecto:
#   - usa el paquete climaemet,
#   - cubre solo las cinco provincias con mayor masa estadistica del TFM,
#   - asigna una estacion AEMET representativa por provincia y documenta
#     la razon de seleccion,
#   - descarga la serie diaria completa por estacion,
#   - guarda cache por estacion para poder reanudar.
#
# Salidas:
#   data/parquet/climate/daily_climate.parquet
#   data/parquet/climate/station_mapping.parquet
#   outputs/tables/climate_quality_summary.csv
#   outputs/tables/climate_station_audit.csv
#   outputs/tables/climate_download_audit.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(climaemet)
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(arrow)
  library(lubridate)
  library(fs)
  library(glue)
})

source(here::here("_config.R"))

message(strrep("=", 60))
message("PASO 03: Construccion del dataset climatico (AEMET)")
message(strrep("=", 60))

t0_total <- proc.time()
STATION_CACHE_DIR <- path(AEMET_CACHE_DIR, "station_daily")
dir_create(STATION_CACHE_DIR, recurse = TRUE)

aemet_key <- Sys.getenv("AEMET_API_KEY")
if (!nzchar(aemet_key) && file_exists(path(PROJ_DIR, ".Renviron"))) {
  readRenviron(path(PROJ_DIR, ".Renviron"))
  aemet_key <- Sys.getenv("AEMET_API_KEY")
}
if (!nzchar(aemet_key)) {
  stop("Falta AEMET_API_KEY en .Renviron o en el entorno.")
}
invisible(capture.output({
  invisible(capture.output({
    climaemet::aemet_api_key(aemet_key, overwrite = TRUE, install = FALSE)
  }, type = "message"))
}, type = "output"))
message("[OK] API key de AEMET detectada.")

read_province_counts <- function() {
  con <- connect_duckdb()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  metadata_abs <- path_abs(METADATA_PARQUET) |> path_norm()
  dbGetQuery(con, glue("
    WITH meta_dedup AS (
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
    )
    SELECT cod_provincia, COUNT(*) AS n_usuarios
    FROM meta_dedup
    WHERE rn = 1
      AND cod_provincia IS NOT NULL
      AND cod_provincia <> ''
    GROUP BY cod_provincia
    ORDER BY n_usuarios DESC
  "))
}

manual_station_mapping <- function() {
  # Las estaciones se fijan manualmente para que el cruce sea reproducible.
  # En el alcance principal se priorizan estaciones AEMET con serie diaria larga,
  # variables completas y localizacion representativa de la provincia usada en
  # el modelado, aceptando que una unica estacion no captura todos los microclimas.
  mapping <- tibble::tribble(
    ~cod_provincia, ~indicativo, ~station_name,
    "01", "9091O", "Vitoria/Gasteiz aeropuerto",
    "20", "1024E", "San Sebastian/Igueldo",
    "48", "1082",  "Bilbao aeropuerto",
    "31", "9263D", "Pamplona/Noain aeropuerto",
    "28", "3129",  "Madrid/Retiro",
    "08", "0076",  "Barcelona aeropuerto",
    "41", "5783",  "Sevilla aeropuerto",
    "46", "8416",  "Valencia aeropuerto",
    "29", "6155A", "Malaga aeropuerto",
    "33", "1249I", "Oviedo",
    "39", "1111",  "Santander aeropuerto",
    "26", "9170",  "Logrono/Agoncillo",
    "50", "9434",  "Zaragoza aeropuerto",
    "47", "2422",  "Valladolid aeropuerto",
    "15", "1387",  "A Coruna",
    "36", "1484C", "Pontevedra",
    "18", "5530E", "Granada aeropuerto",
    "11", "5973",  "Cadiz",
    "30", "7178I", "Murcia",
    "03", "8025",  "Alicante aeropuerto",
    "09", "2331",  "Burgos/Villafria",
    "37", "2867",  "Salamanca aeropuerto",
    "21", "4560Y", "Alajar",
    "42", "9352A", "Almazul",
    "19", "3209Y", "Brihuega",
    "06", "4452",  "Badajoz aeropuerto",
    "22", "9898",  "Huesca aeropuerto",
    "10", "4411C", "Alcuescar",
    "12", "8492X", "Atzeneta del Maestrat",
    "45", "3362Y", "Castillo de Bayuela",
    "02", "8178D", "Albacete",
    "13", "4210Y", "Abenojar",
    "24", "2661B", "Leon aeropuerto",
    "27", "1505",  "Lugo aeropuerto",
    "04", "6325O", "Almeria aeropuerto",
    "40", "2140A", "Aldeanueva de Serrezuela",
    "43", "0016A", "Reus aeropuerto",
    "34", "2243A", "Aguilar de Campoo",
    "49", "2966D", "Alcanices",
    "05", "2456B", "Arevalo",
    "14", "5402",  "Cordoba aeropuerto",
    "23", "5406X", "Alcala la Real",
    "16", "4070Y", "Abia de Obispalia",
    "32", "2969U", "A Gudina",
    "25", "9650X", "Artesa de Segre"
  )

  mapping |>
    mutate(
      station_lat = case_when(
        cod_provincia == "01" ~ 42.8828,
        cod_provincia == "20" ~ 43.3067,
        cod_provincia == "48" ~ 43.3011,
        cod_provincia == "31" ~ 42.7761,
        cod_provincia == "28" ~ 40.4119,
        TRUE ~ NA_real_
      ),
      station_lon = case_when(
        cod_provincia == "01" ~ -2.7244,
        cod_provincia == "20" ~ -2.0392,
        cod_provincia == "48" ~ -2.9106,
        cod_provincia == "31" ~ -1.6463,
        cod_provincia == "28" ~ -3.6781,
        TRUE ~ NA_real_
      ),
      selection_reason = case_when(
        cod_provincia == "01" ~ "Aeropuerto de Vitoria-Gasteiz: serie diaria larga, estacion principal de Alava y referencia estable para el interior vasco.",
        cod_provincia == "20" ~ "Igueldo: observatorio costero historico de Gipuzkoa, buena continuidad y representativo del nucleo GoiEner guipuzcoano.",
        cod_provincia == "48" ~ "Aeropuerto de Bilbao: estacion principal de Bizkaia, cobertura diaria amplia y proximidad al area metropolitana con mayor masa de usuarios.",
        cod_provincia == "31" ~ "Pamplona-Noain: estacion aeroportuaria de referencia para Navarra, serie completa y ubicacion central para el alcance provincial.",
        cod_provincia == "28" ~ "Madrid-Retiro: observatorio urbano de referencia, adecuado para captar clima residencial de Madrid capital frente a estaciones perifericas.",
        TRUE ~ "Estacion manual de respaldo fuera del alcance principal top 5."
      )
    )
}

parse_aemet_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- as.character(x)
  x[x %in% c("", "Ip", "Acum", "Varias")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

normalise_station_data <- function(raw, cod_provincia, indicativo) {
  raw <- as_tibble(raw)

  tibble::tibble(
    cod_provincia = cod_provincia,
    indicativo = as.character(ifelse(
      is.na(safe_col(raw, "indicativo")),
      indicativo,
      safe_col(raw, "indicativo")
    )),
    fecha = as.Date(safe_col(raw, "fecha")),
    tmed = parse_aemet_num(safe_col(raw, "tmed")),
    tmax = parse_aemet_num(safe_col(raw, "tmax")),
    tmin = parse_aemet_num(safe_col(raw, "tmin")),
    prec = parse_aemet_num(safe_col(raw, "prec")),
    hrMedia = parse_aemet_num(safe_col(raw, "hrMedia")),
    sol = parse_aemet_num(safe_col(raw, "sol")),
    velmedia = parse_aemet_num(safe_col(raw, "velmedia"))
  ) |>
    filter(!is.na(.data$fecha)) |>
    distinct(.data$fecha, .keep_all = TRUE) |>
    arrange(.data$fecha) |>
    mutate(
      hdd = pmax(0, HDD_BASE - .data$tmed),
      cdd = pmax(0, .data$tmed - CDD_BASE),
      amplitud_termica = .data$tmax - .data$tmin,
      thi = if_else(
        !is.na(.data$tmed) & !is.na(.data$hrMedia),
        .data$tmed - (0.55 - 0.0055 * .data$hrMedia) * (.data$tmed - 14.5),
        NA_real_
      )
    )
}

download_station <- function(station_row, idx, total) {
  cod <- station_row$cod_provincia
  indicativo <- station_row$indicativo
  name <- station_row$station_name
  cache_file <- path(STATION_CACHE_DIR, sprintf("aemet_station_%s_%s.parquet", cod, indicativo))

  if (file_exists(cache_file)) {
    message(sprintf("  [%s/%s] Cache %s (%s, prov %s)", idx, total, name, indicativo, cod))
    return(read_parquet(cache_file))
  }

  message(sprintf("  [%s/%s] Descargando %s (%s, prov %s)...", idx, total, name, indicativo, cod))
  raw <- NULL
  api_error <- NULL
  invisible(capture.output({
    invisible(capture.output({
      raw <- tryCatch(
        climaemet::aemet_daily_clim(
          station = indicativo,
          start = as.Date(sprintf("%d-01-01", YEAR_MIN)),
          end = as.Date(sprintf("%d-12-31", YEAR_MAX)),
          verbose = FALSE,
          return_sf = FALSE,
          extract_metadata = FALSE,
          progress = FALSE
        ),
        error = function(e) {
          api_error <<- e
          NULL
        }
      )
    }, type = "message"))
  }, type = "output"))

  if (!is.null(api_error)) {
    message(sprintf("  [WARN] %s (%s, prov %s): %s", name, indicativo, cod, api_error$message))
    return(tibble::tibble())
  }

  if (is.null(raw) || nrow(raw) == 0) {
    return(tibble::tibble())
  }

  climate <- normalise_station_data(raw, cod, indicativo)
  if (nrow(climate) == 0 || all(is.na(climate$tmed))) {
    message(sprintf("  [WARN] %s (%s, prov %s): sin tmed util.", name, indicativo, cod))
    return(tibble::tibble())
  }

  write_parquet(climate, cache_file, compression = "zstd", compression_level = 9L)
  climate
}

message("\n[1/5] Determinando provincias con usuarios...")
province_counts <- read_province_counts()
print(head(province_counts, 10))

scope_audit <- write_model_scope_audit(province_counts)
message(sprintf(
  "  Alcance de modelado: %s provincias (%s usuarios de %s; %s%%)",
  paste(FOCUS_PROVINCE_NAMES, collapse = ", "),
  fmt_int(unique(scope_audit$scope_user_total)[1]),
  fmt_int(unique(scope_audit$total_users)[1]),
  fmt_num(unique(scope_audit$scope_pct_total)[1])
))
message("  Auditoria de alcance guardada en: ", MODEL_SCOPE_AUDIT_CSV)

target_provinces <- province_counts |>
  mutate(cod_provincia = normalize_province_code(.data$cod_provincia)) |>
  filter(.data$cod_provincia %in% FOCUS_PROVINCES)
message(sprintf(
  "  Provincias climaticas a cubrir: %s",
  fmt_int(nrow(target_provinces))
))

message("\n[2/5] Mapeando estaciones AEMET a provincias...")
station_mapping <- manual_station_mapping() |>
  semi_join(target_provinces, by = "cod_provincia") |>
  left_join(target_provinces, by = "cod_provincia") |>
  mutate(
    provincia = unname(PROVINCIA_NOMBRES[as.character(.data$cod_provincia)]),
    mapping_method = "manual_documented_representative_station"
  ) |>
  arrange(desc(.data$n_usuarios), .data$cod_provincia)

missing_mapping <- setdiff(target_provinces$cod_provincia, station_mapping$cod_provincia)
if (length(missing_mapping) > 0) {
  warning("Provincias sin estacion mapeada: ", paste(missing_mapping, collapse = ", "))
}

message(sprintf(
  "  Estaciones mapeadas: %s para %s provincias",
  fmt_int(nrow(station_mapping)), fmt_int(n_distinct(station_mapping$cod_provincia))
))

arrow::write_parquet(station_mapping, STATION_MAPPING_PARQUET,
                     compression = "zstd", compression_level = 9L)
message("  Mapeo guardado en: ", STATION_MAPPING_PARQUET)

message("\n[3/5] Descargando datos climaticos de AEMET...")
message("  (climaemet gestiona internamente rate limiting, retries y rangos)")

climate_list <- lapply(seq_len(nrow(station_mapping)), function(i) {
  download_station(station_mapping[i, ], i, nrow(station_mapping))
})

daily_climate <- bind_rows(climate_list)
if (nrow(daily_climate) == 0) {
  stop("No se pudo descargar ningun dato climatico.")
}

daily_climate <- daily_climate |>
  mutate(
    month = month(.data$fecha),
    year = year(.data$fecha),
    season = case_when(
      .data$month %in% c(12, 1, 2) ~ "Invierno",
      .data$month %in% c(3, 4, 5) ~ "Primavera",
      .data$month %in% c(6, 7, 8) ~ "Verano",
      TRUE ~ "Otono"
    )
  ) |>
  group_by(.data$cod_provincia) |>
  mutate(
    is_heatwave = .data$tmax >= quantile(.data$tmax, 0.95, na.rm = TRUE),
    is_coldwave = .data$tmin <= quantile(.data$tmin, 0.05, na.rm = TRUE)
  ) |>
  ungroup() |>
  select(
    cod_provincia, indicativo, fecha, tmed, tmax, tmin, prec, hrMedia,
    sol, velmedia, hdd, cdd, amplitud_termica, thi, month, year,
    season, is_heatwave, is_coldwave
  ) |>
  arrange(.data$cod_provincia, .data$fecha)

message("\n[4/5] Guardando capa climatica...")
arrow::write_parquet(daily_climate, DAILY_CLIMATE_PARQUET,
                     compression = "zstd", compression_level = 9L)

station_audit <- daily_climate |>
  group_by(.data$cod_provincia, .data$indicativo) |>
  summarise(
    climate_date_min = min(.data$fecha, na.rm = TRUE),
    climate_date_max = max(.data$fecha, na.rm = TRUE),
    n_climate_days = n_distinct(.data$fecha),
    pct_tmed_missing = round(100 * mean(is.na(.data$tmed)), 2),
    pct_prec_missing = round(100 * mean(is.na(.data$prec)), 2),
    pct_sol_missing = round(100 * mean(is.na(.data$sol)), 2),
    .groups = "drop"
  ) |>
  left_join(station_mapping, by = c("cod_provincia", "indicativo")) |>
  mutate(
    station_status = case_when(
      .data$pct_tmed_missing <= 1 ~ "ok",
      .data$pct_tmed_missing <= 5 ~ "revisar_missingness",
      TRUE ~ "cobertura_debil"
    )
  ) |>
  arrange(desc(coalesce(.data$n_usuarios, 0)), .data$cod_provincia)

write.csv(station_audit, path(TABLE_DIR, "climate_station_audit.csv"), row.names = FALSE)

quality_summary <- tibble::tibble(
  metric = c(
    "records", "stations", "provinces", "date_min", "date_max",
    "pct_tmed_missing", "pct_prec_missing", "pct_sol_missing",
    "mean_tmed", "mean_hdd", "mean_cdd"
  ),
  value = c(
    nrow(daily_climate),
    n_distinct(daily_climate$indicativo),
    n_distinct(daily_climate$cod_provincia),
    as.character(min(daily_climate$fecha, na.rm = TRUE)),
    as.character(max(daily_climate$fecha, na.rm = TRUE)),
    round(100 * mean(is.na(daily_climate$tmed)), 2),
    round(100 * mean(is.na(daily_climate$prec)), 2),
    round(100 * mean(is.na(daily_climate$sol)), 2),
    round(mean(daily_climate$tmed, na.rm = TRUE), 2),
    round(mean(daily_climate$hdd, na.rm = TRUE), 2),
    round(mean(daily_climate$cdd, na.rm = TRUE), 2)
  )
)

download_audit <- tibble::tibble(
  source = "climaemet_aemet_daily_clim",
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  model_scope = MODEL_SCOPE_NAME,
  focus_provinces = paste(FOCUS_PROVINCES, collapse = ","),
  requested_provinces = nrow(station_mapping),
  downloaded_provinces = n_distinct(daily_climate$cod_provincia),
  station_cache_files = length(dir_ls(STATION_CACHE_DIR, glob = "*.parquet", fail = FALSE)),
  note = "Flujo focalizado: una estacion representativa por cada provincia top 5 usando climaemet."
)

write.csv(quality_summary, path(TABLE_DIR, "climate_quality_summary.csv"), row.names = FALSE)
write.csv(download_audit, path(TABLE_DIR, "climate_download_audit.csv"), row.names = FALSE)

message("\n[5/5] Resumen de calidad climatica:")
print(as.data.frame(t(quality_summary$value)))
print(quality_summary)
message("Guardado: ", DAILY_CLIMATE_PARQUET, " (", fmt_int(nrow(daily_climate)), " registros)")

elapsed <- (proc.time() - t0_total)[["elapsed"]]
message(sprintf("\nPaso 03 completado en %.1f s.", elapsed))
