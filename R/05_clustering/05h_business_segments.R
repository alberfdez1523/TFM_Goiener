#!/usr/bin/env Rscript
# ==============================================================================
# R/05_clustering/05h_business_segments.R
#
# Mapping cluster -> finalidad de negocio Goiener + tabla resumen ejecutiva.
# Outputs:
#   outputs/tables/cluster_business_mapping_v2.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 05h: Mapping a finalidades de negocio")
t0 <- proc.time()

profiles <- read.csv(path(TABLE_DIR, "cluster_profiles_v2.csv"))
poverty  <- read.csv(path(TABLE_DIR, "cluster_poverty_proxy_v2.csv"))
context  <- read.csv(path(TABLE_DIR, "cluster_socioeconomic_v2.csv"))

joined <- profiles |>
  left_join(poverty,  by = c("cluster_label", "n")) |>
  left_join(context,  by = c("cluster_label", "n"))

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

num <- function(row, name, default = NA_real_) {
  if (!name %in% names(row)) return(default)
  row[[name]][1] %||% default
}

txt <- function(row, name, default = NA_character_) {
  if (!name %in% names(row)) return(default)
  row[[name]][1] %||% default
}

pct <- function(x) sprintf("%.1f%%", 100 * x)

segment <- function(finalidad, perfil, accion, evidencia, prioridad) {
  data.frame(
    finalidad = finalidad,
    perfil_cluster = perfil,
    accion_recomendada = accion,
    evidencia_clave = evidencia,
    prioridad = prioridad,
    stringsAsFactors = FALSE
  )
}

# Heuristic mapping based on the actual aggregate profile of each cluster.
# The order matters: strongest business signals are assigned first.
classify <- function(row) {
  label <- txt(row, "cluster_label", "")
  median_kwh <- num(row, "median_daily_kWh")
  mean_kwh <- num(row, "mean_daily_kWh")
  peak_share <- num(row, "peak_share", 0)
  valley_share <- num(row, "valley_share", 0)
  night_share <- num(row, "night_kWh_share", 0)
  afternoon_share <- num(row, "afternoon_kWh_share", 0)
  ratio_weekend <- num(row, "ratio_weekend_weekday", 1)
  seasonal_amp <- num(row, "seasonal_amplitude_norm", 0)
  low_day_rate <- num(row, "low_day_rate", 0)
  zero_day_rate <- num(row, "zero_day_rate", 0)
  beta_hdd <- num(row, "beta_hdd", 0)
  beta_cdd <- num(row, "beta_cdd", 0)
  r2_joint <- num(row, "r2_joint", 0)
  pct_high <- num(row, "pct_high_risk", 0)
  median_p1 <- num(row, "median_p1_kw")

  if (label %in% c("noise", "anomalias")) {
    return(segment(
      "anomalias_operativas",
      "Consumos fuera de patron",
      "Revision individual de contrato, equipo de medida y eventos excepcionales antes de incluir en campanas masivas.",
      "Cluster marcado como ruido/anomalia por el modelo.",
      "Muy alta"
    ))
  }

  if (label == "no_habitual" || (!is.na(median_kwh) && median_kwh < 1.2 && low_day_rate >= 0.55)) {
    return(segment(
      "vivienda_no_habitual",
      "Segunda residencia, baja ocupacion o contrato casi inactivo",
      "Tarifa y potencia minima ajustada; alerta de reactivacion subita y revision de contratos con riesgo social alto.",
      sprintf("Mediana %.2f kWh/d, dias bajos %s, dias cero %s, riesgo alto %.1f%%.",
              median_kwh, pct(low_day_rate), pct(zero_day_rate), pct_high),
      "Alta"
    ))
  }

  if (!is.na(median_kwh) && median_kwh >= 15 && valley_share >= 0.60 && night_share >= 0.35) {
    return(segment(
      "consumo_nocturno_intensivo",
      "Hogares de alto consumo desplazado a valle/noche",
      "Oferta indexada valle y auditoria de cargas nocturnas: acumuladores, termo, VE o calefaccion; ajustar potencia nocturna y seguimiento de coste.",
      sprintf("Mediana %.1f kWh/d, valle %s, noche %s, beta_HDD %.2f.",
              median_kwh, pct(valley_share), pct(night_share), beta_hdd),
      "Alta"
    ))
  }

  if (beta_hdd >= 1.0 && r2_joint >= 0.20 && seasonal_amp >= 0.75) {
    return(segment(
      "climatizacion_invernal",
      "Residencial con demanda fuertemente explicada por frio",
      "Campanas preventivas de invierno: asesoramiento de calefaccion electrica, aislamiento, autoconsumo/almacenamiento y forecast sensible a HDD.",
      sprintf("beta_HDD %.2f, R2 clima %.2f, amplitud estacional %.2f, mediana %.1f kWh/d.",
              beta_hdd, r2_joint, seasonal_amp, median_kwh),
      "Alta"
    ))
  }

  if (peak_share >= 0.42 && valley_share <= 0.30 && ratio_weekend <= 0.70) {
    return(segment(
      "flexibilidad_punta_laboral",
      "Consumo concentrado en horas punta y dias laborables",
      "Programa de respuesta a la demanda: desplazar cargas de mediodia/tarde, optimizar potencia contratada y priorizar comunidad energetica/autoconsumo.",
      sprintf("Punta %s, valle %s, tarde %s, fin_semana/laborable %.2f, P1 mediana %.1f kW.",
              pct(peak_share), pct(valley_share), pct(afternoon_share), ratio_weekend, median_p1),
      "Alta"
    ))
  }

  if (seasonal_amp >= 0.85 && low_day_rate >= 0.25 && !is.na(median_kwh) && median_kwh < 4) {
    return(segment(
      "uso_estacional_bajo",
      "Baja ocupacion con estacionalidad acusada",
      "Segmento de segunda residencia activa: potencia/tarifa ajustada, avisos de consumo fuera de temporada y comunicacion de autoconsumo compartido si es zona costera.",
      sprintf("Mediana %.1f kWh/d, amplitud %.2f, dias bajos %s, dias cero %s.",
              median_kwh, seasonal_amp, pct(low_day_rate), pct(zero_day_rate)),
      "Media"
    ))
  }

  if (pct_high >= 15 && !is.na(median_kwh) && median_kwh < 7 && beta_hdd < 0.2) {
    return(segment(
      "residencial_eficiencia_social",
      "Cartera residencial estable de bajo consumo con riesgo social moderado",
      "Acompanamiento de eficiencia y factura: deteccion de hogares vulnerables, recomendaciones de habitos y oferta 2.0TD sin sobredimensionar potencia.",
      sprintf("Mediana %.1f kWh/d, riesgo alto %.1f%%, valle %s, beta_HDD %.2f.",
              median_kwh, pct_high, pct(valley_share), beta_hdd),
      "Media"
    ))
  }

  if (night_share <= 0.16 && seasonal_amp <= 0.45 && !is.na(median_kwh) && median_kwh < 7) {
    return(segment(
      "cartera_residencial_base",
      "Perfil residencial masivo, estable y predecible",
      "Base para forecasting operativo y campanas generales: tarifa 2.0TD estandar, comunicacion de autoconsumo y control de desviaciones agregadas.",
      sprintf("Representa %.1f%% de la cartera, mediana %.1f kWh/d, noche %s, amplitud %.2f.",
              num(row, "pct", 0), median_kwh, pct(night_share), seasonal_amp),
      "Media"
    ))
  }

  segment(
    "cartera_residencial_mixta",
    "Perfil mixto sin senal dominante unica",
    "Forecasting OMIE y oferta estandar 2.0TD; revisar subsegmentos por potencia y sensibilidad climatica antes de campanas especificas.",
    sprintf("Media %.1f kWh/d, mediana %.1f kWh/d, pico %s, valle %s, beta_HDD %.2f, beta_CDD %.2f.",
            mean_kwh, median_kwh, pct(peak_share), pct(valley_share), beta_hdd, beta_cdd),
    "Media"
  )
}

classification <- bind_rows(lapply(seq_len(nrow(joined)), function(i) classify(joined[i, ])))

mapping <- bind_cols(joined, classification) |>
  select(cluster_label, n, pct,
         finalidad, perfil_cluster, accion_recomendada, evidencia_clave, prioridad,
         median_daily_kWh, mean_daily_kWh, pct_high_risk,
         peak_share, valley_share, night_kWh_share, afternoon_kWh_share,
         beta_hdd, beta_cdd, r2_joint, seasonal_amplitude_norm,
         low_day_rate, zero_day_rate,
         median_p1_kw, pct_goiener_core, pct_coastal)

write_csv_audit(mapping, "cluster_business_mapping_v2.csv")
print(mapping)

message(sprintf("05h en %.1f s", (proc.time() - t0)[["elapsed"]]))
