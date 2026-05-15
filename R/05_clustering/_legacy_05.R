#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 06: Segmentacion de perfiles de consumo
# ==============================================================================
#
# Objetivo TFM:
#   Obtener una segmentacion interpretable de hogares a partir de perfiles de
#   carga, siguiendo la practica habitual en smart meters: normalizar forma,
#   validar k con silhouette/balance y describir los clusters con variables de
#   consumo, calendario, tarifa y clima.
#
# Criterio metodologico:
#   - C0 se reserva para usuarios claramente nocturnos/valle por regla de negocio.
#   - El resto se segmenta con K-Means y CLARA/PAM sobre features de forma.
#   - Se prioriza que los clusters sean defendibles en la memoria: no residuales,
#     no dominados por un solo grupo y con etiquetas interpretables.
#
# Outputs:
#   data/parquet/features/user_clusters.parquet
#   outputs/tables/cluster_*.csv
#   outputs/tables/cluster_hourly_profile_bands.csv
#   outputs/tables/cluster_representative_profiles.csv
#   outputs/tables/cluster_top_separators_natural_language.csv
#   outputs/tables/cluster_cnae_*.csv
#   outputs/tables/cluster_business_interpretation.csv
#   outputs/tables/cluster_business_question_catalog.csv
#   outputs/tables/cluster_business_question_assessment.csv
#   outputs/tables/cluster_forecasting_calibration_assessment.csv
#   outputs/figures/06_*.png
#
# Uso:
#   Rscript R/05_clustering/05a_pool.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(cluster)
  library(fpc)
  library(ggplot2)
  library(fs)
})

source(here::here("_config.R"))

message(strrep("=", 70))
message("PASO 06: Segmentacion de perfiles de consumo")
message(strrep("=", 70))

t0_total <- proc.time()
set.seed(SEED)

stopifnot(
  "Falta user_features.parquet; ejecuta R/04_features/04a_legacy_features.R" =
    file_exists(USER_FEATURES_PARQUET)
)

min_pct_required <- CLUSTER_MIN_PCT_PER_CLUSTER
max_pct_allowed <- if (exists("CLUSTER_MAX_PCT_PER_CLUSTER")) {
  CLUSTER_MAX_PCT_PER_CLUSTER
} else {
  100
}
score_weights <- if (exists("CLUSTER_SCORE_WEIGHTS")) {
  CLUSTER_SCORE_WEIGHTS
} else {
  c(silhouette = 0.25, calinski_harabasz = 0.15, dunn = 0.15, balance = 0.45)
}
cnae_min_n <- if (exists("CLUSTER_CNAE_MIN_N")) CLUSTER_CNAE_MIN_N else 20L

is_true <- function(x) {
  out <- as.logical(x)
  out[is.na(out)] <- FALSE
  out
}

balance_entropy <- function(cluster_id) {
  p <- as.numeric(table(cluster_id)) / length(cluster_id)
  p <- p[p > 0]
  if (length(p) <= 1) return(0)
  -sum(p * log(p)) / log(length(p))
}

cluster_min_pct <- function(cluster_id) {
  100 * min(table(cluster_id)) / length(cluster_id)
}

cluster_max_pct <- function(cluster_id) {
  100 * max(table(cluster_id)) / length(cluster_id)
}

winsorize <- function(x, lo = 0.01, hi = 0.99) {
  q <- quantile(x, c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, q[[1]]), q[[2]])
}

robust_scale <- function(x) {
  med <- median(x, na.rm = TRUE)
  scale <- mad(x, na.rm = TRUE)
  if (!is.finite(scale) || scale == 0) scale <- sd(x, na.rm = TRUE)
  if (!is.finite(scale) || scale == 0) scale <- 1
  (x - med) / scale
}

feature_label_es <- function(feature) {
  labels <- c(
    ratio_night_day = "ratio noche/dia",
    ratio_weekend_weekday = "ratio fin de semana/laborable",
    cv_daily = "variabilidad diaria",
    seasonal_amplitude_norm = "amplitud estacional normalizada",
    zero_day_rate = "frecuencia de dias sin consumo",
    low_day_rate = "frecuencia de dias de bajo consumo",
    max_month_share = "concentracion en el mes dominante",
    monthly_entropy = "regularidad mensual",
    summer_winter_ratio = "ratio verano/invierno",
    peak_share = "peso del periodo punta",
    flat_share = "peso del periodo llano",
    valley_share = "peso del periodo valle",
    peak_to_valley_ratio = "ratio punta/valle",
    corr_hdd = "sensibilidad a frio (HDD)",
    corr_cdd = "sensibilidad a calor (CDD)",
    morning_kWh_share = "peso de la manana",
    afternoon_kWh_share = "peso de la tarde",
    evening_kWh_share = "peso de la noche temprana",
    night_kWh_share = "peso de la madrugada",
    ratio_morning_afternoon = "ratio manana/tarde",
    ratio_evening_morning = "ratio noche temprana/manana",
    ratio_night_morning = "ratio madrugada/manana",
    holiday_ratio = "frecuencia de festivos",
    bridge_ratio = "frecuencia de puentes",
    weekday_weekend_peak_shift = "desplazamiento del pico laboral-fin de semana",
    low_consumption_spell_rate = "rachas de bajo consumo",
    possible_intermittent_home = "proxy de vivienda intermitente",
    proxy_autoconsumption_second_home = "proxy exploratorio de autoconsumo/segunda residencia",
    coastal_flag = "provincia costera",
    goiener_core_region = "region nucleo GoiEner"
  )
  out <- unname(labels[feature])
  out[is.na(out)] <- gsub("_", " ", feature[is.na(out)])
  out
}

separator_text <- function(feature_label, standardized_difference,
                           cluster_mean, global_mean) {
  direction <- ifelse(standardized_difference >= 0,
                      "por encima", "por debajo")
  sprintf(
    "%s esta %s de la media global: %.3f frente a %.3f.",
    feature_label, direction, cluster_mean, global_mean
  )
}

prep_matrix <- function(df, cols) {
  out <- df |> select(all_of(cols)) |> as.data.frame()
  for (nm in names(out)) {
    x <- as.numeric(out[[nm]])
    med <- median(x, na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    x[is.na(x)] <- med
    out[[nm]] <- robust_scale(winsorize(x))
  }
  as.matrix(out)
}

context_matrix <- function(df) {
  context_cols <- intersect(
    c("ccaa", "coastal_flag", "density_bucket", "climate_zone", "goiener_core_region"),
    names(df)
  )
  if (length(context_cols) == 0) return(NULL)

  context_df <- df |> select(all_of(context_cols)) |> as.data.frame()
  for (nm in names(context_df)) {
    if (is.logical(context_df[[nm]])) context_df[[nm]] <- as.integer(context_df[[nm]])
    if (is.character(context_df[[nm]])) {
      context_df[[nm]][is.na(context_df[[nm]]) | context_df[[nm]] == ""] <- "unknown"
      context_df[[nm]] <- factor(context_df[[nm]])
    }
  }
  mm <- model.matrix(~ . - 1, data = context_df)
  if (ncol(mm) == 0) return(NULL)
  mm
}

clean_cnae_code <- function(x) {
  raw <- trimws(as.character(x))
  raw_upper <- toupper(ifelse(is.na(raw), "", raw))
  is_unknown <- is.na(raw) | raw == "" |
    raw_upper %in% c("NA", "NAN", "NULL", "DESCONOCIDO", "DESCONOCIDA")
  raw[is_unknown] <- NA_character_
  raw <- sub("\\.0+$", "", raw)

  digits <- gsub("[^0-9]", "", raw)
  out <- rep(NA_character_, length(raw))
  valid <- !is.na(digits) & nchar(digits) > 0 & nchar(digits) <= 4
  numeric_code <- suppressWarnings(as.integer(digits[valid]))
  out[valid] <- ifelse(!is.na(numeric_code) & numeric_code > 0,
                       sprintf("%04d", numeric_code), NA_character_)
  out
}

cnae_section_from_code <- function(code) {
  division <- suppressWarnings(as.integer(substr(code, 1, 2)))
  dplyr::case_when(
    is.na(division) ~ NA_character_,
    dplyr::between(division, 1L, 3L) ~ "A",
    dplyr::between(division, 5L, 9L) ~ "B",
    dplyr::between(division, 10L, 33L) ~ "C",
    division == 35L ~ "D",
    dplyr::between(division, 36L, 39L) ~ "E",
    dplyr::between(division, 41L, 43L) ~ "F",
    dplyr::between(division, 45L, 47L) ~ "G",
    dplyr::between(division, 49L, 53L) ~ "H",
    dplyr::between(division, 55L, 56L) ~ "I",
    dplyr::between(division, 58L, 63L) ~ "J",
    dplyr::between(division, 64L, 66L) ~ "K",
    division == 68L ~ "L",
    dplyr::between(division, 69L, 75L) ~ "M",
    dplyr::between(division, 77L, 82L) ~ "N",
    division == 84L ~ "O",
    division == 85L ~ "P",
    dplyr::between(division, 86L, 88L) ~ "Q",
    dplyr::between(division, 90L, 93L) ~ "R",
    dplyr::between(division, 94L, 96L) ~ "S",
    dplyr::between(division, 97L, 98L) ~ "T",
    division == 99L ~ "U",
    TRUE ~ NA_character_
  )
}

cnae_section_label <- function(section) {
  labels <- c(
    A = "A - Agricultura, ganaderia, silvicultura y pesca",
    B = "B - Industrias extractivas",
    C = "C - Industria manufacturera",
    D = "D - Energia electrica, gas y vapor",
    E = "E - Agua, saneamiento y residuos",
    F = "F - Construccion",
    G = "G - Comercio y reparacion de vehiculos",
    H = "H - Transporte y almacenamiento",
    I = "I - Hosteleria",
    J = "J - Informacion y comunicaciones",
    K = "K - Actividades financieras y seguros",
    L = "L - Actividades inmobiliarias",
    M = "M - Actividades profesionales y tecnicas",
    N = "N - Actividades administrativas y auxiliares",
    O = "O - Administracion publica y defensa",
    P = "P - Educacion",
    Q = "Q - Sanidad y servicios sociales",
    R = "R - Actividades artisticas y recreativas",
    S = "S - Otros servicios",
    T = "T - Hogares y actividades domesticas",
    U = "U - Organismos extraterritoriales"
  )
  out <- unname(labels[section])
  out[is.na(out)] <- "Desconocido"
  out
}

cnae_business_family <- function(section) {
  dplyr::case_when(
    is.na(section) ~ "Desconocido",
    section == "T" ~ "Hogares/uso domestico",
    section %in% c("G", "H", "J", "K", "L", "M", "N", "R", "S") ~ "Comercio y servicios",
    section == "I" ~ "Hosteleria y turismo",
    section %in% c("O", "P", "Q") ~ "Servicios publicos y sociales",
    section %in% c("C", "D", "E", "F") ~ "Industria, energia y construccion",
    section %in% c("A", "B") ~ "Primario y extractivo",
    TRUE ~ "Otros/no clasificado"
  )
}

behavioral_business_question <- function(cluster, mean_ratio_night,
                                         mean_zero_day_rate, mean_low_day_rate,
                                         mean_peak_share, mean_valley_share) {
  dplyr::case_when(
    cluster == 0 | mean_ratio_night >= 1.4 ~
      "Como aprovechar el consumo nocturno/valle sin convertirlo en una etiqueta individual de hogar?",
    mean_zero_day_rate >= 0.10 | mean_low_day_rate >= 0.35 ~
      "Que usuarios con consumo intermitente requieren seguimiento diferenciado, acompanamiento o revision de calidad de dato?",
    mean_peak_share >= 0.30 ~
      "Que palancas reducen exposicion a punta y mejoran eficiencia en el segmento con pico de tarde?",
    mean_valley_share >= 0.45 ~
      "Que segmento puede sostener estrategias valle moderadas y mensajes de eficiencia sin intervencion intensiva?",
    TRUE ~
      "Que acciones comerciales u operativas son prudentes cuando el patron electrico es reconocible pero CNAE no diferencia fuerte?"
  )
}

behavioral_signal_text <- function(cluster, mean_ratio_night,
                                   mean_zero_day_rate, mean_low_day_rate,
                                   mean_peak_share, mean_valley_share) {
  dplyr::case_when(
    cluster == 0 | mean_ratio_night >= 1.4 ~
      "Consumo claramente desplazado hacia noche/valle.",
    mean_zero_day_rate >= 0.10 | mean_low_day_rate >= 0.35 ~
      "Consumo variable o intermitente, con mas dias de bajo consumo.",
    mean_peak_share >= 0.30 ~
      "Mayor peso relativo del periodo punta y pico vespertino.",
    mean_valley_share >= 0.45 ~
      "Perfil con valle relevante y menor tension punta-valle.",
    TRUE ~
      "Perfil residencial estable con diferencias moderadas de forma."
  )
}

goiener_action_text <- function(cluster, mean_ratio_night,
                                mean_zero_day_rate, mean_low_day_rate,
                                mean_peak_share, mean_valley_share) {
  dplyr::case_when(
    cluster == 0 | mean_ratio_night >= 1.4 ~
      "Priorizar seguimiento de flexibilidad valle, dimensionando ofertas con cautela por su calibracion especifica.",
    mean_zero_day_rate >= 0.10 | mean_low_day_rate >= 0.35 ~
      "Separar comunicaciones de eficiencia y revisar si hay estacionalidad, segunda residencia, autoconsumo o incidencias de medida.",
    mean_peak_share >= 0.30 ~
      "Orientar acciones de desplazamiento fuera de punta, eficiencia vespertina y simulaciones tarifarias 2.0TD.",
    mean_valley_share >= 0.45 ~
      "Mantener acciones de eficiencia de baja friccion y evaluar incentivos valle solo donde el ahorro sea claro.",
    TRUE ~
      "Usar el cluster como control operativo y comparador de cartera antes de personalizar acciones."
  )
}

reference_basis_text <- function(reference_rows, reference_matrix) {
  refs <- trimws(unlist(strsplit(reference_rows, ";"), use.names = FALSE))
  refs <- refs[nzchar(refs)]
  if (length(refs) == 0) return(NA_character_)

  if (!is.data.frame(reference_matrix) || nrow(reference_matrix) == 0 ||
      !all(c("referencia", "idea_metodologica") %in% names(reference_matrix))) {
    return(paste(refs, collapse = " | "))
  }

  idx <- match(refs, reference_matrix$referencia)
  basis <- ifelse(
    is.na(idx),
    refs,
    sprintf(
      "%s: %s",
      reference_matrix$referencia[idx],
      reference_matrix$idea_metodologica[idx]
    )
  )
  paste(basis, collapse = " | ")
}

build_business_question_catalog <- function(reference_matrix) {
  catalog <- tibble::tribble(
    ~question_id, ~business_question, ~reference_theme, ~reference_rows,
    ~smart_meter_rationale, ~repo_evidence, ~applicable_output,
    ~conclusion_rule, ~decision_scope, ~caveat,
    "BQ_FLEXIBILITY",
    "Que clusters conviene priorizar para eficiencia, flexibilidad o desplazamiento punta-valle?",
    "Load profiling y analitica descriptiva",
    "Review smart meter analytics; Clustering load profiles",
    "La literatura usa perfiles de carga para separar patrones horarios accionables antes de pasar a prediccion o decision comercial.",
    "cluster_profiles.csv; flexibility_opportunity.csv; cluster_top_separators_natural_language.csv",
    "cluster_business_question_assessment.csv; flexibility_opportunity.csv",
    "Concluir solo cuando haya tamano suficiente, senal punta/valle clara y una accion agregada medible.",
    "Priorizacion de campanas agregadas de eficiencia y flexibilidad.",
    "No observa equipamientos ni disponibilidad real del hogar para desplazar consumo.",
    "BQ_PEAK_TARIFF",
    "Que clusters tienen mayor exposicion a punta 2.0TD y podrian beneficiarse de simulaciones tarifarias o consejos horarios?",
    "Load profiling aplicado a tarifas y eficiencia",
    "Review smart meter analytics; Clustering load profiles",
    "Las franjas horarias normalizadas permiten traducir un patron tecnico a exposicion economica agregada sin inferir causas personales.",
    "cluster_profiles.csv; flexibility_opportunity.csv",
    "cluster_business_question_assessment.csv",
    "Combinar peso de punta, peso relativo del cluster y cautela sobre causas no observadas.",
    "Diseno de mensajes horarios, simulaciones 2.0TD y seguimiento de respuesta.",
    "No debe presentarse como recomendacion individual sin validar contrato, potencia y restricciones reales.",
    "BQ_INTERMITTENT_QUALITY",
    "Que clusters muestran consumo intermitente que exige separar oportunidad comercial de revision de calidad o uso estacional?",
    "Calidad de dato, cobertura y sesgo de dataset publico",
    "GoiEner Scientific Data; Review smart meter analytics; Smart grid public datasets",
    "Los smart meters combinan analitica descriptiva con controles de calidad; la intermitencia puede ser patron real o artefacto de medida/cobertura.",
    "cluster_profiles.csv; anomalous_users.csv; data_quality_summary.csv",
    "cluster_business_question_assessment.csv; anomalous_users.csv",
    "Concluir como segmento de seguimiento si hay dias cero/bajo consumo y revisar evidencia antes de convertirlo en oferta.",
    "Separacion entre campanas comerciales, segunda residencia/autoconsumo hipotetico y revision de calidad.",
    "No identifica la causa de la intermitencia; vivienda, autoconsumo o error de medida no estan observados directamente.",
    "BQ_FORECASTING_SEGMENT",
    "En que clusters el forecasting requiere calibracion o intervalos especificos antes de usarse operativamente?",
    "Validacion temporal, escenario operativo e incertidumbre",
    "Forecasting smart meters; Forecasting operativo vs ex-post; Probabilistic forecasting",
    "Las referencias de forecasting recomiendan validacion temporal, comparacion con baselines y cobertura de intervalos, no solo error puntual.",
    "cluster_forecasting_calibration_assessment.csv; forecast_cluster_interval_calibration.csv; forecast_interval_alerts.csv",
    "cluster_business_question_assessment.csv; cluster_forecasting_calibration_assessment.csv",
    "Citar el escenario operativo cuando se hable de uso real y exigir cobertura de intervalos suficiente por cluster.",
    "Planificacion operativa, seguimiento de riesgo predictivo y calibracion por segmento.",
    "Un buen WAPE no elimina infracobertura local ni sesgos estacionales de clusters pequenos.",
    "BQ_RECONCILIATION",
    "Aporta valor predecir por cluster frente al total directo para explicar demanda agregada y decisiones operativas?",
    "ETL reproducible y comparacion de metricas de forecasting",
    "London AMI case study; Forecasting smart meters; Forecasting operativo vs ex-post",
    "Los estudios AMI suelen comparar aproximaciones agregadas y segmentadas con metricas homogeneas para justificar complejidad adicional.",
    "forecast_reconciliation_metrics.csv; forecast_bottomup_scope.csv; forecast_metrics_by_cluster.csv",
    "cluster_business_question_assessment.csv; forecast_reconciliation_metrics.csv",
    "Aceptar la segmentacion predictiva solo si mejora o explica el total sin perder trazabilidad frente a baselines simples.",
    "Decision sobre usar prediccion total directa, bottom-up por cluster o ambas como control.",
    "La utilidad del bottom-up puede ser explicativa aunque no siempre mejore el error agregado.",
    "BQ_CNAE_CONTEXT",
    "El CNAE ayuda a contextualizar diferencias agregadas sin convertirlas en inferencias socioeconomicas individuales?",
    "Metadatos limitados, privacidad y cobertura documental",
    "GoiEner Scientific Data; Smart grid public datasets; Review smart meter analytics",
    "Las referencias piden documentar metadatos, sesgos y privacidad; el CNAE puede describir cartera pero no explicar hogares.",
    "cluster_cnae_coverage.csv; cluster_cnae_enrichment.csv; cluster_business_interpretation.csv",
    "cluster_business_question_assessment.csv; cluster_business_interpretation.csv",
    "Usar CNAE solo si hay cobertura y soporte minimo; formularlo como contexto agregado, no como causa del consumo.",
    "Contextualizacion de cartera, transparencia de metadatos y limites socioeconomicos.",
    "CNAE contractual no equivale a renta, ocupacion, vivienda ni vulnerabilidad.",
    "BQ_ANOMALY_REVIEW",
    "Que perfiles deben revisarse como atipicos antes de generalizar campanas o metricas del cluster?",
    "Calidad de datos y reproducibilidad de perfiles",
    "Review smart meter analytics; Smart grid public datasets; Clustering load profiles",
    "La calidad del dato y los perfiles representativos deben acompanar cualquier lectura de cluster para no confundir outliers con segmentos.",
    "anomalous_users.csv; cluster_representative_profiles.csv; cluster_hourly_profile_bands.csv",
    "cluster_business_question_assessment.csv; anomalous_users.csv",
    "Revisar distancia al centroide y perfiles representativos antes de extrapolar conclusiones.",
    "Control de calidad, explicabilidad interna y seleccion de casos para auditoria agregada.",
    "Un usuario atipico no implica fraude ni conducta concreta; solo indica distancia a la forma media del cluster.",
    "BQ_REPRODUCIBILITY_SCOPE",
    "Hasta donde se pueden generalizar los clusters de esta cartera GoiEner?",
    "Cobertura, sesgo geografico y pipeline reproducible",
    "GoiEner Scientific Data; Smart grid public datasets; London AMI case study",
    "Las referencias sobre datasets publicos y AMI insisten en declarar cobertura, sesgo, ETL y limitaciones antes de generalizar.",
    "model_scope_audit.csv; cluster_filter_summary.csv; cluster_validation.csv; cluster_stability.csv",
    "cluster_business_question_assessment.csv; cluster_filter_summary.csv",
    "Limitar la conclusion a la cartera y provincias modeladas, con estabilidad y balance como condiciones de credibilidad.",
    "Gobernanza del resultado, alcance de memoria y defensa metodologica.",
    "No extrapolar a todos los hogares espanoles ni a segmentos socioeconomicos no observados."
  )

  catalog |>
    mutate(
      reference_basis = vapply(
        reference_rows,
        reference_basis_text,
        character(1),
        reference_matrix = reference_matrix
      )
    ) |>
    select(question_id, business_question, reference_theme, reference_rows,
           reference_basis, smart_meter_rationale, repo_evidence,
           applicable_output, conclusion_rule, decision_scope, caveat)
}

fmt_share_pct <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.1f%%", 100 * x))
}

fmt_raw_pct <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.1f%%", x))
}

fmt_plain_num <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", sprintf(paste0("%.", digits, "f"), x))
}

build_business_question_assessment <- function(cluster_question_base,
                                               question_catalog,
                                               cnae_min_n) {
  question_lookup <- question_catalog |>
    select(question_id, business_question, reference_theme, decision_scope,
           caveat) |>
    rename(question_caveat = caveat)

  tidyr::crossing(cluster_question_base, question_lookup) |>
    mutate(
      n_interval_alerts = tidyr::replace_na(n_interval_alerts, 0L),
      needs_special_forecasting_calibration = tidyr::replace_na(
        needs_special_forecasting_calibration,
        FALSE
      ),
      has_forecast_alert = needs_special_forecasting_calibration |
        (!is.na(coverage_test) & coverage_test < 90) |
        n_interval_alerts > 0,
      has_supported_cnae = !is.na(n_cnae_known) & n_cnae_known >= cnae_min_n &
        !is.na(coverage_pct) & coverage_pct >= 80,
      has_cnae_enrichment = !is.na(top_enriched_cnae_ratio) &
        top_enriched_cnae_ratio >= 1.20,
      evidence_strength = case_when(
        question_id == "BQ_FLEXIBILITY" &
          (cluster == 0 | mean_peak_share >= 0.30 | mean_valley_share >= 0.45) ~ "Alta",
        question_id == "BQ_PEAK_TARIFF" & mean_peak_share >= 0.30 ~ "Alta",
        question_id == "BQ_INTERMITTENT_QUALITY" &
          (mean_zero_day_rate >= 0.10 | mean_low_day_rate >= 0.35) ~ "Alta",
        question_id == "BQ_FORECASTING_SEGMENT" & has_forecast_alert ~ "Alta",
        question_id == "BQ_CNAE_CONTEXT" & has_cnae_enrichment ~ "Alta",
        question_id == "BQ_ANOMALY_REVIEW" & anomaly_rate_pct >= 4 ~ "Alta",
        question_id == "BQ_RECONCILIATION" ~ "Media",
        question_id == "BQ_CNAE_CONTEXT" & has_supported_cnae ~ "Media",
        question_id == "BQ_REPRODUCIBILITY_SCOPE" ~ "Contextual",
        TRUE ~ "Contextual"
      ),
      cluster_evidence = case_when(
        question_id %in% c("BQ_FLEXIBILITY", "BQ_PEAK_TARIFF") ~ sprintf(
          "punta=%s; valle=%s; ratio noche/dia=%s.",
          fmt_share_pct(mean_peak_share),
          fmt_share_pct(mean_valley_share),
          fmt_plain_num(mean_ratio_night, 2)
        ),
        question_id == "BQ_INTERMITTENT_QUALITY" ~ sprintf(
          "dias cero=%s; dias bajo consumo=%s; cv diario=%s.",
          fmt_share_pct(mean_zero_day_rate),
          fmt_share_pct(mean_low_day_rate),
          fmt_plain_num(mean_cv, 2)
        ),
        question_id == "BQ_FORECASTING_SEGMENT" ~ sprintf(
          "WAPE=%s; cobertura intervalo=%s; alertas=%s; calibracion especial=%s.",
          fmt_raw_pct(WAPE),
          fmt_raw_pct(coverage_test),
          n_interval_alerts,
          ifelse(needs_special_forecasting_calibration, "si", "no")
        ),
        question_id == "BQ_RECONCILIATION" ~ sprintf(
          "cluster=%s usuarios (%s); WAPE por cluster=%s.",
          n_usuarios,
          fmt_raw_pct(pct_usuarios),
          fmt_raw_pct(WAPE)
        ),
        question_id == "BQ_CNAE_CONTEXT" ~ sprintf(
          "cobertura CNAE=%s; dominante=%s (%s); enriquecimiento=%s.",
          fmt_raw_pct(coverage_pct),
          top_cnae_section_label,
          fmt_raw_pct(top_cnae_section_share_pct),
          fmt_plain_num(top_enriched_cnae_ratio, 2)
        ),
        question_id == "BQ_ANOMALY_REVIEW" ~ sprintf(
          "usuarios anomalos=%s (%s); distancia al centroide auditada por cluster.",
          n_anomalous,
          fmt_raw_pct(anomaly_rate_pct)
        ),
        question_id == "BQ_REPRODUCIBILITY_SCOPE" ~ sprintf(
          "cluster=%s usuarios (%s); estabilidad y balance se documentan en salidas del paso 06.",
          n_usuarios,
          fmt_raw_pct(pct_usuarios)
        ),
        TRUE ~ "Evidencia no disponible."
      ),
      recommended_conclusion = case_when(
        question_id == "BQ_FLEXIBILITY" & evidence_strength == "Alta" ~
          "Priorizar como candidato a acciones agregadas de flexibilidad o eficiencia, midiendo respuesta posterior.",
        question_id == "BQ_PEAK_TARIFF" & evidence_strength == "Alta" ~
          "Probar mensajes horarios o simulaciones 2.0TD enfocadas en reducir exposicion a punta.",
        question_id == "BQ_INTERMITTENT_QUALITY" & evidence_strength == "Alta" ~
          "Tratar como segmento de seguimiento: primero calidad/estacionalidad, despues accion comercial si procede.",
        question_id == "BQ_FORECASTING_SEGMENT" & evidence_strength == "Alta" ~
          "Usar prediccion por cluster solo con calibracion especifica e intervalos visibles.",
        question_id == "BQ_RECONCILIATION" ~
          "Comparar el valor explicativo del bottom-up por cluster frente al total directo antes de anadir complejidad operativa.",
        question_id == "BQ_CNAE_CONTEXT" & evidence_strength %in% c("Alta", "Media") ~
          "Usar CNAE para describir composicion agregada y reforzar limites, no para explicar comportamiento individual.",
        question_id == "BQ_ANOMALY_REVIEW" & evidence_strength == "Alta" ~
          "Revisar perfiles representativos y usuarios anomalos antes de extrapolar el mensaje del cluster.",
        question_id == "BQ_REPRODUCIBILITY_SCOPE" ~
          "Presentar la conclusion como valida para la cartera y provincias modeladas, con sesgo geografico declarado.",
        TRUE ~
          "Mantener la pregunta como contexto secundario; no priorizar una accion especifica con la evidencia actual."
      ),
      is_primary_question = evidence_strength == "Alta",
      strength_order = match(evidence_strength, c("Alta", "Media", "Contextual"))
    ) |>
    transmute(
      cluster, cluster_label, question_id, business_question,
      reference_theme, evidence_strength, is_primary_question,
      cluster_evidence, recommended_conclusion, decision_scope,
      question_caveat, n_usuarios, pct_usuarios, strength_order
    ) |>
    arrange(cluster, strength_order, question_id) |>
    select(-strength_order)
}

evaluate_solution <- function(cluster_id, dist_sample, sample_idx) {
  sample_clusters <- cluster_id[sample_idx]
  sil <- cluster::silhouette(sample_clusters, dist_sample)
  stats <- fpc::cluster.stats(dist_sample, sample_clusters,
                              silhouette = FALSE, G2 = FALSE, G3 = FALSE)
  tibble::tibble(
    silhouette_avg = mean(sil[, "sil_width"], na.rm = TRUE),
    davies_bouldin = stats$sindex,
    calinski_harabasz = stats$ch,
    dunn = stats$dunn,
    balance_entropy = balance_entropy(cluster_id),
    min_cluster_pct = cluster_min_pct(cluster_id),
    max_cluster_pct = cluster_max_pct(cluster_id)
  )
}

evaluate_sensitivity_grid <- function(X, preprocessing, description) {
  n <- nrow(X)
  if (n < 100) return(tibble::tibble())
  set.seed(SEED + ncol(X))
  idx <- sample(seq_len(n), min(CLUSTER_SENSITIVITY_SAMPLE_SIZE, n))
  Xs <- X[idx, , drop = FALSE]
  dist_s <- dist(Xs)
  rows <- list()

  for (k in CLUSTER_K_RANGE) {
    set.seed(SEED + 700 + k)
    km <- kmeans(Xs, centers = k, nstart = CLUSTER_KMEANS_NSTART,
                 iter.max = 200)
    rows[[length(rows) + 1]] <- evaluate_solution(
      km$cluster, dist_s, seq_len(nrow(Xs))
    ) |>
      mutate(algo = "kmeans", k = k, .before = 1)

    set.seed(SEED + 900 + k)
    pm <- pam(Xs, k = k, metric = "manhattan")
    rows[[length(rows) + 1]] <- evaluate_solution(
      pm$clustering, dist_s, seq_len(nrow(Xs))
    ) |>
      mutate(algo = "pam", k = k, .before = 1)
  }

  bind_rows(rows) |>
    mutate(
      preprocessing = preprocessing,
      description = description,
      n_sample = nrow(Xs),
      n_features = ncol(Xs),
      passes_min_pct = min_cluster_pct >= min_pct_required,
      passes_max_pct = max_cluster_pct <= max_pct_allowed,
      selected_current_pipeline = FALSE,
      .before = algo
    )
}

message("\n[1/7] Cargando y filtrando hogares residenciales...")
features_all <- arrow::read_parquet(USER_FEATURES_PARQUET)
if (!"cod_provincia" %in% names(features_all)) {
  stop("user_features.parquet no contiene cod_provincia.")
}
if (any(!features_all$cod_provincia %in% FOCUS_PROVINCES)) {
  stop("user_features.parquet contiene provincias fuera del alcance top 5.")
}
message(sprintf(
  "  Alcance modelado: %s usuarios en provincias %s",
  fmt_int(nrow(features_all)), paste(sort(unique(features_all$cod_provincia)), collapse = ", ")
))

mask_residential <- is_true(features_all$is_residential_strict)
mask_clean <- !is_true(features_all$has_sustained_extreme)
mask_coverage <- !is.na(features_all$active_days) &
  features_all$active_days >= MIN_ACTIVE_DAYS &
  !is.na(features_all$mean_daily_kWh) &
  features_all$mean_daily_kWh > 0

features <- features_all[mask_residential & mask_clean & mask_coverage, ]
if (nrow(features) < 500) stop("Pool residencial insuficiente para clustering.")

filter_summary <- tibble::tibble(
  paso = c(
    "Total features",
    "Tarifa residencial 2.0*",
    "Sin outlier horario sostenido",
    sprintf(">= %d dias activos", MIN_ACTIVE_DAYS),
    "Pool residencial modelable"
  ),
  n_usuarios = c(
    nrow(features_all),
    sum(mask_residential),
    sum(mask_residential & mask_clean),
    sum(mask_residential & mask_clean & mask_coverage),
    nrow(features)
  ),
  pct_sobre_total = round(100 * n_usuarios / nrow(features_all), 2)
)
write.csv(filter_summary, path(TABLE_DIR, "cluster_filter_summary.csv"),
          row.names = FALSE)
print(filter_summary)

message("\n[2/7] Separando patron nocturno/valle claro...")
mask_night <- (!is.na(features$ratio_night_day) & features$ratio_night_day > 1.5) |
  (!is.na(features$peak_hour) & features$peak_hour %in% 0:6) |
  (!is.na(features$valley_share) & features$valley_share >= 0.62)

features$cluster <- ifelse(mask_night, 0L, NA_integer_)
stage_b_idx <- which(!mask_night)
stage_b <- features[stage_b_idx, ]

message(sprintf("  C0 nocturno/valle: %s usuarios (%.1f%%)",
                fmt_int(sum(mask_night)), 100 * mean(mask_night)))
message(sprintf("  Fase B: %s usuarios", fmt_int(nrow(stage_b))))

message("\n[3/7] Preparando matriz de forma y comportamiento...")
hour_cols <- grep("^norm_h\\d{2}$", names(features), value = TRUE)
behaviour_cols_core <- c(
  "ratio_night_day", "ratio_weekend_weekday",
  "cv_daily", "seasonal_amplitude_norm",
  "zero_day_rate", "low_day_rate",
  "max_month_share", "monthly_entropy",
  "summer_winter_ratio",
  "peak_share", "flat_share", "valley_share", "peak_to_valley_ratio",
  "corr_hdd", "corr_cdd"
) |>
  intersect(names(features))
behaviour_cols_extra <- c(
  "morning_kWh_share", "afternoon_kWh_share", "evening_kWh_share",
  "night_kWh_share", "ratio_morning_afternoon", "ratio_evening_morning",
  "ratio_night_morning", "holiday_ratio", "bridge_ratio",
  "weekday_weekend_peak_shift", "low_consumption_spell_rate",
  "possible_intermittent_home", "proxy_autoconsumption_second_home"
) |>
  intersect(names(features))
behaviour_cols <- c(behaviour_cols_core, behaviour_cols_extra)

feature_cols <- c(hour_cols, behaviour_cols_core)
feature_cols_enhanced <- c(hour_cols, behaviour_cols)
X_pool <- prep_matrix(stage_b, feature_cols)

sample_n <- min(CLUSTER_SAMPLE_SIZE, nrow(X_pool))
sample_idx <- sample(seq_len(nrow(X_pool)), sample_n)
dist_sample <- dist(X_pool[sample_idx, , drop = FALSE])

message(sprintf("  Matriz fase B: %s usuarios x %s features",
                fmt_int(nrow(X_pool)), fmt_int(ncol(X_pool))))

message("\n[4/7] Evaluando K-Means y CLARA/PAM...")
k_values <- CLUSTER_K_RANGE
models <- list()
eval_rows <- list()

for (k in k_values) {
  set.seed(SEED + k)
  km <- kmeans(X_pool, centers = k, nstart = CLUSTER_KMEANS_NSTART,
               iter.max = 200)
  models[[paste0("kmeans_", k)]] <- km
  eval_rows[[length(eval_rows) + 1]] <- evaluate_solution(
    km$cluster, dist_sample, sample_idx
  ) |>
    mutate(algo = "kmeans", k = k, wss = km$tot.withinss, .before = 1)

  set.seed(SEED + 100 + k)
  cl <- if (nrow(X_pool) > 8000) {
    clara(X_pool, k = k, sampsize = min(5000, nrow(X_pool)),
          samples = 12, pamLike = TRUE, metric = "manhattan")
  } else {
    pam(X_pool, k = k, metric = "manhattan")
  }
  models[[paste0("pam_", k)]] <- cl
  eval_rows[[length(eval_rows) + 1]] <- evaluate_solution(
    cl$clustering, dist_sample, sample_idx
  ) |>
    mutate(algo = "pam", k = k, wss = NA_real_, .before = 1)
}

eval_results <- bind_rows(eval_rows) |>
  mutate(
    passes_min_pct = min_cluster_pct >= min_pct_required,
    passes_max_pct = max_cluster_pct <= max_pct_allowed,
    passes_size_constraints = passes_min_pct & passes_max_pct
  )

candidates <- eval_results |> filter(passes_size_constraints)
if (nrow(candidates) == 0) {
  warning("No hay candidatos que cumplan min/max; se prioriza max_pct < 50 para evitar colapso.")
  candidates <- eval_results |> filter(max_cluster_pct < 50)
}
if (nrow(candidates) == 0) {
  warning("No hay candidatos con max_pct < 50; se relaja solo min_pct.")
  candidates <- eval_results |> filter(passes_max_pct)
}
if (nrow(candidates) == 0) {
  warning("No hay candidatos con balance aceptable; se usa el ranking completo.")
  candidates <- eval_results
}

candidates <- candidates |>
  mutate(
    r_sil = rank(-silhouette_avg, ties.method = "min"),
    r_ch = rank(-calinski_harabasz, ties.method = "min"),
    r_dunn = rank(-dunn, ties.method = "min"),
    r_bal = rank(-balance_entropy, ties.method = "min"),
    score = score_weights[["silhouette"]] * r_sil +
      score_weights[["calinski_harabasz"]] * r_ch +
      score_weights[["dunn"]] * r_dunn +
      score_weights[["balance"]] * r_bal
  ) |>
  arrange(score)

best <- candidates |> slice(1)
best_key <- paste0(best$algo, "_", best$k)
best_model <- models[[best_key]]

eval_results <- eval_results |>
  left_join(candidates |> select(algo, k, r_sil, r_ch, r_dunn, r_bal, score),
            by = c("algo", "k")) |>
  mutate(selected = algo == best$algo & k == best$k)

write.csv(eval_results, path(TABLE_DIR, "cluster_validation.csv"),
          row.names = FALSE)

message(sprintf(
  "  Seleccion: %s k=%d | silhouette=%.3f | balance=%.3f | max=%.1f%%",
  best$algo, best$k, best$silhouette_avg, best$balance_entropy,
  best$max_cluster_pct
))

message("\n[5/7] Estabilidad bootstrap de la solucion elegida...")
cb_method <- if (best$algo == "kmeans") fpc::kmeansCBI else fpc::claraCBI
stability <- tryCatch(
  fpc::clusterboot(
    data = X_pool[sample_idx, , drop = FALSE],
    B = CLUSTER_STABILITY_B,
    bootmethod = "boot",
    clustermethod = cb_method,
    k = best$k,
    seed = SEED,
    count = FALSE
  ),
  error = function(e) {
    message("  Estabilidad omitida: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(stability)) {
  stability_df <- tibble::tibble(
    cluster = seq_along(stability$bootmean),
    jaccard_mean = round(stability$bootmean, 3),
    n_dissolved = stability$bootbrd,
    n_recovered = stability$bootrecover
  )
} else {
  stability_df <- tibble::tibble(
    cluster = seq_len(best$k),
    jaccard_mean = NA_real_,
    n_dissolved = NA_integer_,
    n_recovered = NA_integer_
  )
}
write.csv(stability_df, path(TABLE_DIR, "cluster_stability.csv"),
          row.names = FALSE)

message("\n[6/7] Asignando clusters y generando perfiles...")
stage_b_cluster <- if (best$algo == "kmeans") {
  best_model$cluster
} else {
  best_model$clustering
}

features$cluster[stage_b_idx] <- as.integer(stage_b_cluster)

stage_summary <- features[stage_b_idx, ] |>
  group_by(cluster) |>
  summarise(
    mean_zero_day_rate = mean(zero_day_rate, na.rm = TRUE),
    mean_peak_share = mean(peak_share, na.rm = TRUE),
    mean_valley_share = mean(valley_share, na.rm = TRUE),
    median_peak_hour = median(peak_hour, na.rm = TRUE),
    .groups = "drop"
  )

intermittent_cluster <- stage_summary |>
  slice_max(mean_zero_day_rate, n = 1, with_ties = FALSE) |>
  pull(cluster)
remaining <- stage_summary |> filter(cluster != intermittent_cluster)
peak_cluster <- remaining |>
  slice_max(mean_peak_share, n = 1, with_ties = FALSE) |>
  pull(cluster)
valley_cluster <- remaining |>
  filter(cluster != peak_cluster) |>
  slice_max(mean_valley_share, n = 1, with_ties = FALSE) |>
  pull(cluster)

label_parts <- list(
  tibble::tibble(cluster = 0L, cluster_label_base = "Nocturno/valle")
)
if (length(intermittent_cluster) == 1) {
  label_parts[[length(label_parts) + 1]] <- tibble::tibble(
    cluster = intermittent_cluster,
    cluster_label_base = "Variable/intermitente"
  )
}
if (length(peak_cluster) == 1) {
  label_parts[[length(label_parts) + 1]] <- tibble::tibble(
    cluster = peak_cluster,
    cluster_label_base = "Pico tarde"
  )
}
if (length(valley_cluster) == 1) {
  label_parts[[length(label_parts) + 1]] <- tibble::tibble(
    cluster = valley_cluster,
    cluster_label_base = "Valle moderado"
  )
}

label_map <- bind_rows(label_parts) |>
  distinct(cluster, .keep_all = TRUE)

remaining_unlabeled <- setdiff(sort(unique(features$cluster)), label_map$cluster)
if (length(remaining_unlabeled) > 0) {
  label_map <- bind_rows(
    label_map,
    tibble::tibble(
      cluster = remaining_unlabeled,
      cluster_label_base = paste("Perfil", seq_along(remaining_unlabeled))
    )
  )
}

label_map <- label_map |>
  arrange(cluster) |>
  mutate(
    cluster_label = paste0("C", cluster, " - ", cluster_label_base),
    cluster_stage = ifelse(cluster == 0,
                           "A_nocturno_valle_regla",
                           "B_perfiles_clara_kmeans"),
    cluster_method = ifelse(cluster == 0,
                            "regla_ratio_noche_valle",
                            best$algo)
  ) |>
  select(cluster, cluster_label, cluster_stage, cluster_method)

features <- features |>
  left_join(label_map, by = "cluster")

cluster_profiles <- features |>
  group_by(cluster) |>
  summarise(
    n_usuarios = n(),
    pct_usuarios = round(100 * n() / nrow(features), 1),
    median_daily_kWh = round(median(mean_daily_kWh, na.rm = TRUE), 2),
    mean_daily_kWh = round(mean(mean_daily_kWh, na.rm = TRUE), 2),
    mean_cv = round(mean(cv_daily, na.rm = TRUE), 3),
    mean_ratio_night = round(mean(ratio_night_day, na.rm = TRUE), 3),
    mean_ratio_weekend = round(mean(ratio_weekend_weekday, na.rm = TRUE), 3),
    mean_morning_share = round(mean(morning_kWh_share, na.rm = TRUE), 3),
    mean_afternoon_share = round(mean(afternoon_kWh_share, na.rm = TRUE), 3),
    mean_evening_share = round(mean(evening_kWh_share, na.rm = TRUE), 3),
    mean_holiday_ratio = round(mean(holiday_ratio, na.rm = TRUE), 3),
    mean_low_spell_rate = round(mean(low_consumption_spell_rate, na.rm = TRUE), 3),
    median_peak_hour = median(peak_hour, na.rm = TRUE),
    median_peak_shift = median(weekday_weekend_peak_shift, na.rm = TRUE),
    mean_seasonal_amp = round(mean(seasonal_amplitude_norm, na.rm = TRUE), 3),
    mean_zero_day_rate = round(mean(zero_day_rate, na.rm = TRUE), 3),
    mean_low_day_rate = round(mean(low_day_rate, na.rm = TRUE), 3),
    mean_max_month_share = round(mean(max_month_share, na.rm = TRUE), 3),
    mean_monthly_entropy = round(mean(monthly_entropy, na.rm = TRUE), 3),
    mean_peak_share = round(mean(peak_share, na.rm = TRUE), 3),
    mean_valley_share = round(mean(valley_share, na.rm = TRUE), 3),
    mean_corr_hdd = round(mean(corr_hdd, na.rm = TRUE), 3),
    mean_corr_cdd = round(mean(corr_cdd, na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  left_join(label_map, by = "cluster") |>
  relocate(cluster_label, cluster_stage, cluster_method, .after = cluster) |>
  arrange(cluster)

cluster_balance <- features |>
  count(cluster, name = "n_usuarios") |>
  mutate(
    pct_total = round(100 * n_usuarios / sum(n_usuarios), 2),
    is_stage_b = cluster != 0,
    pct_stage_b = ifelse(is_stage_b,
                         round(100 * n_usuarios / sum(n_usuarios[is_stage_b]), 2),
                         NA_real_),
    min_pct_constraint = min_pct_required,
    max_pct_constraint = max_pct_allowed
  ) |>
  arrange(cluster)

write.csv(cluster_profiles, path(TABLE_DIR, "cluster_profiles.csv"),
          row.names = FALSE)
write.csv(label_map, path(TABLE_DIR, "cluster_labels.csv"), row.names = FALSE)
write.csv(cluster_balance, path(TABLE_DIR, "cluster_balance_diagnostics.csv"),
          row.names = FALSE)

feature_contribution_cols <- intersect(
  c(
    behaviour_cols,
    "coastal_flag", "goiener_core_region"
  ),
  names(features)
)
cluster_feature_contribution <- bind_rows(lapply(feature_contribution_cols, function(col) {
  x <- features[[col]]
  if (is.logical(x)) x <- as.integer(x)
  if (!is.numeric(x)) return(NULL)
  global_mean <- mean(x, na.rm = TRUE)
  global_sd <- sd(x, na.rm = TRUE)
  if (!is.finite(global_sd) || global_sd == 0) global_sd <- 1

  features |>
    mutate(.feature_value = as.numeric(.data[[col]])) |>
    group_by(cluster, cluster_label) |>
    summarise(
      cluster_mean = mean(.feature_value, na.rm = TRUE),
      global_mean = global_mean,
      standardized_difference = (cluster_mean - global_mean) / global_sd,
      .groups = "drop"
    ) |>
    mutate(feature = col, .before = cluster)
})) |>
  mutate(
    abs_standardized_difference = abs(standardized_difference)
  ) |>
  group_by(cluster) |>
  arrange(desc(abs_standardized_difference), .by_group = TRUE) |>
  mutate(rank_abs = row_number()) |>
  ungroup() |>
  mutate(across(where(is.numeric), ~round(., 4)))

write.csv(cluster_feature_contribution,
          path(TABLE_DIR, "cluster_feature_contribution.csv"),
          row.names = FALSE)

cluster_top_separators <- cluster_feature_contribution |>
  filter(rank_abs <= 3) |>
  mutate(
    feature_label = feature_label_es(.data$feature),
    direction = ifelse(.data$standardized_difference >= 0,
                       "por encima", "por debajo")
  ) |>
  mutate(
    natural_language = separator_text(
      feature_label,
      .data$standardized_difference,
      .data$cluster_mean,
      .data$global_mean
    )
  ) |>
  arrange(cluster, rank_abs) |>
  select(cluster, cluster_label, rank_abs, feature, feature_label,
         direction, cluster_mean, global_mean,
         standardized_difference, natural_language)

write.csv(cluster_top_separators,
          path(TABLE_DIR, "cluster_top_separators_natural_language.csv"),
          row.names = FALSE)

context_mm <- context_matrix(stage_b)
X_pool_enhanced <- prep_matrix(stage_b, feature_cols_enhanced)
X_pool_context <- if (!is.null(context_mm)) {
  cbind(X_pool_enhanced, context_mm * 0.35)
} else {
  X_pool_enhanced
}

cluster_sensitivity <- bind_rows(
  evaluate_sensitivity_grid(
    X_pool,
    "shape_behavior_core",
    "Perfil horario normalizado + intermitencia + estacionalidad + periodos 2.0TD + clima."
  ),
  evaluate_sensitivity_grid(
    X_pool_enhanced,
    "shape_behavior_enhanced",
    "Matriz core mas patrones de calendario, franjas manana/tarde/noche y proxies no personales."
  ),
  evaluate_sensitivity_grid(
    X_pool_context,
    "shape_behavior_context",
    "Matriz enhanced con contexto territorial provincial agregado ponderado."
  )
) |>
  mutate(
    selected_current_pipeline = preprocessing == "shape_behavior_core" &
      algo == best$algo & k == best$k
  ) |>
  select(preprocessing, description, algo, k, n_sample, n_features,
         silhouette_avg, davies_bouldin, calinski_harabasz, dunn,
         balance_entropy, min_cluster_pct, max_cluster_pct,
         passes_min_pct, passes_max_pct, selected_current_pipeline)
write.csv(cluster_sensitivity, path(TABLE_DIR, "cluster_sensitivity.csv"),
          row.names = FALSE)

message("\n[7/7] Figuras, anomalias y export final...")
hourly_profiles <- features |>
  select(cluster, all_of(hour_cols)) |>
  pivot_longer(all_of(hour_cols), names_to = "hour_label", values_to = "norm_kWh") |>
  mutate(hour = as.integer(gsub("norm_h", "", hour_label))) |>
  group_by(cluster, hour) |>
  summarise(mean_norm_kWh = mean(norm_kWh, na.rm = TRUE), .groups = "drop")

hourly_profile_bands <- features |>
  select(cluster, cluster_label, all_of(hour_cols)) |>
  pivot_longer(all_of(hour_cols), names_to = "hour_label", values_to = "norm_kWh") |>
  mutate(hour = as.integer(gsub("norm_h", "", hour_label))) |>
  group_by(cluster, cluster_label, hour) |>
  summarise(
    n_users = n(),
    mean_norm_kWh = mean(norm_kWh, na.rm = TRUE),
    median_norm_kWh = median(norm_kWh, na.rm = TRUE),
    p25_norm_kWh = quantile(norm_kWh, 0.25, na.rm = TRUE),
    p75_norm_kWh = quantile(norm_kWh, 0.75, na.rm = TRUE),
    p10_norm_kWh = quantile(norm_kWh, 0.10, na.rm = TRUE),
    p90_norm_kWh = quantile(norm_kWh, 0.90, na.rm = TRUE),
    sd_norm_kWh = sd(norm_kWh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~round(., 4))) |>
  arrange(cluster, hour)

write.csv(hourly_profile_bands,
          path(TABLE_DIR, "cluster_hourly_profile_bands.csv"),
          row.names = FALSE)

n_clusters <- n_distinct(features$cluster)

p_hourly <- ggplot(hourly_profiles,
                   aes(hour, mean_norm_kWh, color = factor(cluster))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.2) +
  scale_x_continuous(breaks = 0:23) +
  scale_color_manual(values = PAL_CLUSTER[seq_len(n_clusters)]) +
  labs(
    title = "Perfil horario normalizado por cluster",
    subtitle = "Segmentacion residencial GoiEner: forma, intermitencia y periodos 2.0TD",
    x = "Hora del dia", y = "Consumo normalizado", color = "Cluster"
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "06_cluster_hourly_profiles.png"), p_hourly,
       width = 12, height = 6, dpi = 300)

p_hourly_bands <- ggplot(hourly_profile_bands,
                         aes(hour, mean_norm_kWh,
                             color = factor(cluster),
                             fill = factor(cluster))) +
  geom_ribbon(aes(ymin = p25_norm_kWh, ymax = p75_norm_kWh),
              alpha = 0.18, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~cluster_label, ncol = 2) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  scale_color_manual(values = PAL_CLUSTER[seq_len(n_clusters)]) +
  scale_fill_manual(values = PAL_CLUSTER[seq_len(n_clusters)]) +
  labs(
    title = "Curvas horarias medias con banda de variabilidad",
    subtitle = "Linea = media normalizada; banda = percentiles 25-75 dentro de cada cluster",
    x = "Hora del dia", y = "Consumo normalizado", color = "Cluster", fill = "Cluster"
  ) +
  theme_goiener() +
  theme(legend.position = "none")
ggsave(path(FIG_DIR, "06_cluster_hourly_profiles_bands.png"), p_hourly_bands,
       width = 12, height = 8, dpi = 300)

p_sizes <- cluster_profiles |>
  ggplot(aes(x = "", y = pct_usuarios, fill = factor(cluster))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("C%s\n%.1f%%", cluster, pct_usuarios)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  coord_flip() +
  scale_fill_manual(values = PAL_CLUSTER[seq_len(n_clusters)]) +
  labs(
    title = "Distribucion de hogares por cluster",
    x = NULL, y = "% usuarios", fill = "Cluster"
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "06_cluster_sizes.png"), p_sizes,
       width = 12, height = 3.5, dpi = 300)

p_metrics <- eval_results |>
  select(algo, k, silhouette_avg, balance_entropy, min_cluster_pct, max_cluster_pct) |>
  pivot_longer(-c(algo, k), names_to = "metric", values_to = "value") |>
  ggplot(aes(k, value, color = algo, shape = algo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = best$k, linetype = "dashed", color = PAL_ACCENT) +
  facet_wrap(~metric, scales = "free_y") +
  scale_x_continuous(breaks = k_values) +
  scale_color_manual(values = c(kmeans = PAL_MAIN, pam = PAL_ACCENT)) +
  labs(
    title = "Seleccion de k: calidad y balance",
    subtitle = sprintf("Seleccion: %s k=%d", best$algo, best$k),
    x = "k", y = NULL, color = "Algoritmo", shape = "Algoritmo"
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "06_k_selection_metrics.png"), p_metrics,
       width = 12, height = 7, dpi = 300)

X_all <- prep_matrix(features, feature_cols)
pca <- prcomp(X_all, center = FALSE, scale. = FALSE)
pca_df <- as.data.frame(pca$x[, 1:2])
names(pca_df) <- c("PC1", "PC2")
pca_df$cluster <- factor(features$cluster)
plot_idx <- sample(seq_len(nrow(pca_df)), min(8000, nrow(pca_df)))

p_pca <- ggplot(pca_df[plot_idx, ], aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.4, size = 0.8) +
  scale_color_manual(values = PAL_CLUSTER[seq_len(n_clusters)]) +
  labs(
    title = "Proyeccion PCA de perfiles de consumo",
    subtitle = sprintf("PC1 %.1f%% | PC2 %.1f%%",
                       100 * summary(pca)$importance[2, 1],
                       100 * summary(pca)$importance[2, 2]),
    x = "PC1", y = "PC2", color = "Cluster"
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "06_pca_clusters.png"), p_pca,
       width = 10, height = 7, dpi = 300)
saveRDS(pca, path(MODEL_DIR, "cluster_pca.rds"))

p_box <- ggplot(features, aes(factor(cluster), mean_daily_kWh,
                              fill = factor(cluster))) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2) +
  scale_y_log10(labels = scales::label_number()) +
  scale_fill_manual(values = PAL_CLUSTER[seq_len(n_clusters)]) +
  labs(
    title = "Consumo medio diario por cluster",
    x = "Cluster", y = "kWh medio diario (escala log)"
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "06_cluster_boxplot.png"), p_box,
       width = 10, height = 5.5, dpi = 300)

centroids <- features[stage_b_idx, ] |>
  mutate(stage_b_cluster = stage_b_cluster) |>
  group_by(stage_b_cluster) |>
  summarise(across(all_of(feature_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop")

cluster_levels <- sort(unique(stage_b_cluster))
centroid_matrix <- t(sapply(cluster_levels, function(cl) {
  colMeans(X_pool[stage_b_cluster == cl, , drop = FALSE])
}))
rownames(centroid_matrix) <- cluster_levels

dist_to_centroid <- vapply(seq_len(nrow(X_pool)), function(i) {
  cl <- as.character(stage_b_cluster[[i]])
  sqrt(sum((X_pool[i, ] - centroid_matrix[cl, ])^2))
}, numeric(1))

features$dist_to_centroid <- NA_real_
features$dist_to_centroid[stage_b_idx] <- dist_to_centroid
thresholds <- features |>
  filter(!is.na(dist_to_centroid)) |>
  group_by(cluster) |>
  summarise(p95_dist = quantile(dist_to_centroid, 0.95, na.rm = TRUE),
            .groups = "drop")
features <- features |>
  left_join(thresholds, by = "cluster") |>
  mutate(is_anomalous = !is.na(dist_to_centroid) & dist_to_centroid > p95_dist)

anomalous_users <- features |>
  filter(is_anomalous) |>
  select(user_id, cluster, cluster_label, dist_to_centroid,
         mean_daily_kWh, cv_daily, zero_day_rate, peak_share, valley_share) |>
  arrange(desc(dist_to_centroid))
write.csv(anomalous_users, path(TABLE_DIR, "anomalous_users.csv"),
          row.names = FALSE)

representative_matrix <- prep_matrix(features, hour_cols)
representative_clusters <- features$cluster
representative_levels <- sort(unique(representative_clusters))
representative_centroids <- t(sapply(representative_levels, function(cl) {
  colMeans(representative_matrix[representative_clusters == cl, , drop = FALSE])
}))
rownames(representative_centroids) <- representative_levels

representative_distance <- vapply(seq_len(nrow(representative_matrix)), function(i) {
  cl <- as.character(representative_clusters[[i]])
  sqrt(sum((representative_matrix[i, ] - representative_centroids[cl, ])^2))
}, numeric(1))

representative_source <- features |>
  mutate(.representative_distance = representative_distance) |>
  group_by(cluster, cluster_label) |>
  arrange(.representative_distance, .by_group = TRUE) |>
  slice_head(n = 3) |>
  mutate(
    representative_rank = row_number(),
    anonymous_profile_id = sprintf("C%s-P%02d", cluster, representative_rank)
  ) |>
  ungroup()

representative_profiles <- representative_source |>
  transmute(
    cluster,
    cluster_label,
    anonymous_profile_id,
    representative_rank,
    normalized_shape_distance = round(.representative_distance, 4),
    mean_daily_kWh = round(mean_daily_kWh, 2),
    ratio_night_day = round(ratio_night_day, 3),
    peak_hour,
    valley_share = round(valley_share, 3),
    peak_share = round(peak_share, 3),
    zero_day_rate = round(zero_day_rate, 3),
    low_day_rate = round(low_day_rate, 3)
  ) |>
  arrange(cluster, representative_rank)

write.csv(representative_profiles,
          path(TABLE_DIR, "cluster_representative_profiles.csv"),
          row.names = FALSE)

representative_hourly_profiles <- representative_source |>
  select(cluster, cluster_label, anonymous_profile_id, all_of(hour_cols)) |>
  pivot_longer(all_of(hour_cols), names_to = "hour_label", values_to = "norm_kWh") |>
  mutate(hour = as.integer(gsub("norm_h", "", hour_label))) |>
  select(cluster, cluster_label, anonymous_profile_id, hour, norm_kWh) |>
  arrange(cluster, anonymous_profile_id, hour)

write.csv(representative_hourly_profiles,
          path(TABLE_DIR, "cluster_representative_hourly_profiles.csv"),
          row.names = FALSE)

cluster_output <- features |>
  select(user_id, cluster, cluster_label, cluster_stage, cluster_method,
         dist_to_centroid, is_anomalous)

user_clusters_final <- features_all |>
  left_join(cluster_output, by = "user_id")

if (!"cnae" %in% names(user_clusters_final)) {
  user_clusters_final$cnae <- NA_character_
}

user_clusters_final <- user_clusters_final |>
  mutate(
    cnae_clean = clean_cnae_code(.data$cnae),
    cnae_division = suppressWarnings(as.integer(substr(.data$cnae_clean, 1, 2))),
    cnae_section = cnae_section_from_code(.data$cnae_clean),
    cnae_section_label = cnae_section_label(.data$cnae_section),
    cnae_business_family = cnae_business_family(.data$cnae_section),
    cnae_known = !is.na(.data$cnae_clean) & !is.na(.data$cnae_section)
  )

arrow::write_parquet(user_clusters_final, USER_CLUSTERS_PARQUET)
saveRDS(best_model, path(MODEL_DIR, paste0("cluster_", best$algo, "_final.rds")))

flexibility <- features |>
  group_by(cluster, cluster_label) |>
  summarise(
    n_users = n(),
    mean_peak_share = round(mean(peak_share, na.rm = TRUE), 3),
    mean_flat_share = round(mean(flat_share, na.rm = TRUE), 3),
    mean_valley_share = round(mean(valley_share, na.rm = TRUE), 3),
    median_peak_to_valley_ratio = round(median(peak_to_valley_ratio, na.rm = TRUE), 2),
    interpretation = case_when(
      mean_valley_share >= 0.5 ~ "Alta presencia de consumo valle",
      mean_peak_share >= 0.3 ~ "Potencial de desplazamiento fuera de punta",
      TRUE ~ "Perfil sin oportunidad clara de flexibilidad"
    ),
    .groups = "drop"
  ) |>
  arrange(cluster)
write.csv(flexibility, path(TABLE_DIR, "flexibility_opportunity.csv"),
          row.names = FALSE)

message("  Generando lectura CNAE post-hoc por cluster...")
cnae_cluster_base <- user_clusters_final |>
  filter(!is.na(.data$cluster)) |>
  select(user_id, cluster, cluster_label, tarifa_clean, p1_kw,
         cnae, cnae_clean, cnae_division, cnae_section,
         cnae_section_label, cnae_business_family, cnae_known,
         mean_daily_kWh, ratio_night_day, peak_share, valley_share,
         zero_day_rate, low_day_rate)

cluster_cnae_coverage <- cnae_cluster_base |>
  group_by(cluster, cluster_label) |>
  summarise(
    n_users = n(),
    n_cnae_known = sum(.data$cnae_known, na.rm = TRUE),
    n_cnae_unknown = n_users - n_cnae_known,
    coverage_pct = round(100 * n_cnae_known / n_users, 2),
    min_support_n = cnae_min_n,
    .groups = "drop"
  )

known_cnae <- cnae_cluster_base |>
  filter(.data$cnae_known)

if (nrow(known_cnae) > 0) {
  cluster_known_totals <- known_cnae |>
    count(cluster, cluster_label, name = "known_users_cluster")

  global_section_totals <- known_cnae |>
    count(cnae_section, cnae_section_label, cnae_business_family,
          name = "n_users_global") |>
    mutate(pct_global = 100 * n_users_global / sum(n_users_global))

  cluster_cnae_section_distribution <- known_cnae |>
    count(cluster, cluster_label, cnae_section, cnae_section_label,
          cnae_business_family, name = "n_users") |>
    left_join(cluster_known_totals, by = c("cluster", "cluster_label")) |>
    left_join(
      cluster_cnae_coverage |> select(cluster, n_users_cluster = n_users),
      by = "cluster"
    ) |>
    left_join(
      global_section_totals,
      by = c("cnae_section", "cnae_section_label", "cnae_business_family")
    ) |>
    mutate(
      pct_cluster = 100 * n_users / known_users_cluster,
      pct_cluster_all = 100 * n_users / n_users_cluster,
      pct_point_diff = pct_cluster - pct_global,
      enrichment_ratio = ifelse(pct_global > 0, pct_cluster / pct_global, NA_real_),
      support_ok = n_users >= cnae_min_n
    ) |>
    mutate(
      across(c(pct_cluster, pct_cluster_all, pct_global, pct_point_diff),
             ~round(., 2)),
      enrichment_ratio = round(enrichment_ratio, 3)
    ) |>
    arrange(cluster, desc(n_users), cnae_section)

  cluster_cnae_enrichment <- cluster_cnae_section_distribution |>
    mutate(
      global_support_ok = n_users_global >= cnae_min_n,
      is_interpretable = support_ok & global_support_ok &
        !is.na(enrichment_ratio) & is.finite(enrichment_ratio)
    ) |>
    group_by(cluster) |>
    mutate(
      rank_enrichment = as.integer(rank(
        ifelse(is_interpretable, -enrichment_ratio, NA_real_),
        ties.method = "first", na.last = "keep"
      ))
    ) |>
    ungroup() |>
    arrange(cluster, rank_enrichment, desc(n_users), cnae_section)

  cluster_cnae_division_distribution <- known_cnae |>
    count(cluster, cluster_label, cnae_division, cnae_section,
          cnae_section_label, cnae_business_family, name = "n_users") |>
    left_join(cluster_known_totals, by = c("cluster", "cluster_label")) |>
    mutate(
      cnae_division_label = sprintf("%02d", cnae_division),
      pct_cluster = round(100 * n_users / known_users_cluster, 2),
      support_ok = n_users >= cnae_min_n
    ) |>
    group_by(cluster) |>
    arrange(desc(n_users), cnae_division, .by_group = TRUE) |>
    mutate(rank_division = row_number()) |>
    ungroup() |>
    filter(rank_division <= 10 | support_ok) |>
    arrange(cluster, rank_division)

  top_cnae_section <- cluster_cnae_section_distribution |>
    group_by(cluster) |>
    arrange(desc(n_users), cnae_section, .by_group = TRUE) |>
    slice_head(n = 1) |>
    ungroup() |>
    transmute(
      cluster,
      top_cnae_section = cnae_section,
      top_cnae_section_label = cnae_section_label,
      top_cnae_business_family = cnae_business_family,
      top_cnae_section_share_pct = pct_cluster
    )

  cluster_cnae_coverage <- cluster_cnae_coverage |>
    left_join(top_cnae_section, by = "cluster") |>
    mutate(
      top_cnae_section = tidyr::replace_na(top_cnae_section, "Desconocido"),
      top_cnae_section_label = tidyr::replace_na(top_cnae_section_label, "Sin CNAE conocido"),
      top_cnae_business_family = tidyr::replace_na(top_cnae_business_family, "Desconocido"),
      top_cnae_section_share_pct = tidyr::replace_na(top_cnae_section_share_pct, 0)
    ) |>
    arrange(cluster)
} else {
  cluster_cnae_section_distribution <- tibble::tibble()
  cluster_cnae_enrichment <- tibble::tibble()
  cluster_cnae_division_distribution <- tibble::tibble()
  cluster_cnae_coverage <- cluster_cnae_coverage |>
    mutate(
      top_cnae_section = "Desconocido",
      top_cnae_section_label = "Sin CNAE conocido",
      top_cnae_business_family = "Desconocido",
      top_cnae_section_share_pct = 0
    ) |>
    arrange(cluster)
}

write.csv(cluster_cnae_coverage,
          path(TABLE_DIR, "cluster_cnae_coverage.csv"), row.names = FALSE)
write.csv(cluster_cnae_section_distribution,
          path(TABLE_DIR, "cluster_cnae_section_distribution.csv"), row.names = FALSE)
write.csv(cluster_cnae_enrichment,
          path(TABLE_DIR, "cluster_cnae_enrichment.csv"), row.names = FALSE)
write.csv(cluster_cnae_division_distribution,
          path(TABLE_DIR, "cluster_cnae_division_distribution.csv"), row.names = FALSE)

message(sprintf(
  "    -> CNAE conocido en %s de %s usuarios clusterizados (%s%%).",
  fmt_int(sum(cnae_cluster_base$cnae_known, na.rm = TRUE)),
  fmt_int(nrow(cnae_cluster_base)),
  fmt_num(100 * mean(cnae_cluster_base$cnae_known, na.rm = TRUE), 1)
))

forecast_calibration_assessment <- cluster_profiles |>
  select(cluster, cluster_label, n_usuarios, pct_usuarios, mean_cv,
         mean_ratio_night, mean_valley_share, mean_zero_day_rate,
         mean_low_day_rate)

forecast_metrics_path <- path(TABLE_DIR, "forecast_metrics_by_cluster.csv")
if (file_exists(forecast_metrics_path)) {
  forecast_metrics_by_cluster <- read.csv(forecast_metrics_path,
                                          stringsAsFactors = FALSE) |>
    select(cluster, MAE, RMSE, MAPE, WAPE, SMAPE)
  forecast_calibration_assessment <- forecast_calibration_assessment |>
    left_join(forecast_metrics_by_cluster, by = "cluster")
}

cluster_interval_path <- path(TABLE_DIR, "forecast_cluster_interval_calibration.csv")
if (file_exists(cluster_interval_path)) {
  interval_calibration <- read.csv(cluster_interval_path,
                                   stringsAsFactors = FALSE) |>
    filter(calibration_group == "cluster") |>
    select(cluster, qhat, qhat_raw, safety_factor, coverage_val,
           coverage_test, n_test)
  forecast_calibration_assessment <- forecast_calibration_assessment |>
    left_join(interval_calibration, by = "cluster")
}

interval_alerts_path <- path(TABLE_DIR, "forecast_interval_alerts.csv")
if (file_exists(interval_alerts_path)) {
  interval_alerts <- read.csv(interval_alerts_path,
                              stringsAsFactors = FALSE)
  if (nrow(interval_alerts) > 0) {
    alert_summary <- interval_alerts |>
      group_by(cluster) |>
      summarise(
        n_interval_alerts = n(),
        has_cluster_or_season_alert = any(alert_level %in% c("cluster_global", "cluster_season")),
        worst_interval_coverage = min(empirical_coverage, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    alert_summary <- tibble::tibble(
      cluster = integer(0),
      n_interval_alerts = integer(0),
      has_cluster_or_season_alert = logical(0),
      worst_interval_coverage = numeric(0)
    )
  }
  forecast_calibration_assessment <- forecast_calibration_assessment |>
    left_join(alert_summary, by = "cluster")
}

standard_interval_factor <- if (exists("FORECAST_CLUSTER_INTERVAL_SAFETY_FACTOR")) {
  max(FORECAST_CLUSTER_INTERVAL_SAFETY_FACTOR, 1.50)
} else {
  1.50
}
rule_interval_factor <- if (exists("FORECAST_RULE_CLUSTER_INTERVAL_SAFETY_FACTOR")) {
  max(FORECAST_RULE_CLUSTER_INTERVAL_SAFETY_FACTOR, 1.50)
} else {
  1.50
}

for (col in c("MAE", "RMSE", "MAPE", "WAPE", "SMAPE", "qhat", "qhat_raw",
              "safety_factor", "coverage_val", "coverage_test", "n_test",
              "n_interval_alerts", "worst_interval_coverage")) {
  if (!col %in% names(forecast_calibration_assessment)) {
    forecast_calibration_assessment[[col]] <- NA_real_
  }
}
if (!"has_cluster_or_season_alert" %in% names(forecast_calibration_assessment)) {
  forecast_calibration_assessment$has_cluster_or_season_alert <- FALSE
}

forecast_calibration_assessment <- forecast_calibration_assessment |>
  mutate(
    n_interval_alerts = tidyr::replace_na(n_interval_alerts, 0L),
    has_cluster_or_season_alert = tidyr::replace_na(has_cluster_or_season_alert, FALSE),
    uses_rule_interval_factor = cluster == 0 &
      !is.na(safety_factor) & safety_factor >= rule_interval_factor,
    needs_special_forecasting_calibration = cluster == 0 & (
      uses_rule_interval_factor |
        (!is.na(coverage_test) & coverage_test < 90) |
        has_cluster_or_season_alert
    ),
    recommendation = case_when(
      cluster == 0 & needs_special_forecasting_calibration ~
        "Mantener calibracion especial de C0 y revisar sesgo estacional, especialmente verano.",
      cluster == 0 ~
        "Mantener monitorizacion especifica de C0 aunque no haya alerta fuerte.",
      !is.na(coverage_test) & coverage_test < 90 ~
        "Calibracion estandar con seguimiento: cobertura inferior al objetivo.",
      TRUE ~ "Calibracion estandar suficiente con seguimiento rutinario."
    ),
    evidence = sprintf(
      "WAPE=%s%%; cobertura intervalo=%s%%; factor=%s; alertas=%s.",
      ifelse(is.na(WAPE), "NA", sprintf("%.2f", WAPE)),
      ifelse(is.na(coverage_test), "NA", sprintf("%.2f", coverage_test)),
      ifelse(is.na(safety_factor), "NA", sprintf("%.2f", safety_factor)),
      n_interval_alerts
    ),
    standard_interval_factor = standard_interval_factor,
    rule_interval_factor = rule_interval_factor
  ) |>
  arrange(cluster)

write.csv(forecast_calibration_assessment,
          path(TABLE_DIR, "cluster_forecasting_calibration_assessment.csv"),
          row.names = FALSE)

message("  Generando catalogo de preguntas empresariales respaldadas por referencias...")
reference_matrix <- if (file_exists(REFERENCE_MATRIX_CSV)) {
  read.csv(REFERENCE_MATRIX_CSV, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  tibble::tibble()
}

cluster_business_question_catalog <- build_business_question_catalog(reference_matrix)
write.csv(cluster_business_question_catalog,
          path(TABLE_DIR, "cluster_business_question_catalog.csv"),
          row.names = FALSE)

top_enriched_cnae <- if (nrow(cluster_cnae_enrichment) > 0 &&
                         "is_interpretable" %in% names(cluster_cnae_enrichment)) {
  cluster_cnae_enrichment |>
    filter(.data$is_interpretable) |>
    group_by(cluster) |>
    arrange(rank_enrichment, desc(n_users), .by_group = TRUE) |>
    slice_head(n = 1) |>
    ungroup() |>
    transmute(
      cluster,
      top_enriched_cnae_section_label = cnae_section_label,
      top_enriched_cnae_ratio = enrichment_ratio,
      top_enriched_cnae_pct_cluster = pct_cluster
    )
} else {
  tibble::tibble(
    cluster = integer(0),
    top_enriched_cnae_section_label = character(0),
    top_enriched_cnae_ratio = numeric(0),
    top_enriched_cnae_pct_cluster = numeric(0)
  )
}

cluster_business_interpretation <- cluster_profiles |>
  select(cluster, cluster_label, n_usuarios, pct_usuarios,
         mean_ratio_night, mean_zero_day_rate, mean_low_day_rate,
         mean_peak_share, mean_valley_share) |>
  left_join(cluster_cnae_coverage |> select(-cluster_label), by = "cluster") |>
  left_join(top_enriched_cnae, by = "cluster") |>
  left_join(
    flexibility |>
      select(cluster, flexibility_interpretation = interpretation,
             mean_peak_share_flex = mean_peak_share,
             mean_valley_share_flex = mean_valley_share),
    by = "cluster"
  ) |>
  left_join(
    forecast_calibration_assessment |>
      select(cluster, WAPE, coverage_test,
             forecast_recommendation = recommendation),
    by = "cluster"
  ) |>
  mutate(
    flexibility_interpretation = tidyr::replace_na(
      flexibility_interpretation,
      "Sin lectura de flexibilidad disponible."
    ),
    forecast_recommendation = tidyr::replace_na(
      forecast_recommendation,
      "Sin evaluacion de forecasting disponible."
    ),
    business_question = behavioral_business_question(
      cluster, mean_ratio_night, mean_zero_day_rate, mean_low_day_rate,
      mean_peak_share, mean_valley_share
    ),
    behavioral_signal = behavioral_signal_text(
      cluster, mean_ratio_night, mean_zero_day_rate, mean_low_day_rate,
      mean_peak_share, mean_valley_share
    ),
    cnae_signal = case_when(
      is.na(n_cnae_known) | n_cnae_known == 0 ~
        "No hay CNAE conocido suficiente para describir este cluster.",
      top_cnae_section == "T" & top_cnae_section_share_pct >= 80 &
        !is.na(top_enriched_cnae_ratio) & top_enriched_cnae_ratio >= 1.20 ~ sprintf(
          "CNAE conocido en %.1f%%; domina %s (%.1f%% de CNAE conocidos). Hay una senal secundaria en %s (x%.2f), pero la lectura diferencial es debil frente al peso domestico.",
          coverage_pct,
          top_cnae_section_label,
          top_cnae_section_share_pct,
          top_enriched_cnae_section_label,
          top_enriched_cnae_ratio
        ),
      top_cnae_section == "T" & top_cnae_section_share_pct >= 80 ~ sprintf(
        "CNAE conocido en %.1f%%; domina %s (%.1f%% de CNAE conocidos), lo que valida el alcance residencial pero limita la lectura socioeconomica diferencial.",
        coverage_pct,
        top_cnae_section_label,
        top_cnae_section_share_pct
      ),
      !is.na(top_enriched_cnae_ratio) & top_enriched_cnae_ratio >= 1.20 ~ sprintf(
        "CNAE conocido en %.1f%%; %s esta sobrerrepresentado (%.1f%% del cluster; x%.2f frente al conjunto).",
        coverage_pct,
        top_enriched_cnae_section_label,
        top_enriched_cnae_pct_cluster,
        top_enriched_cnae_ratio
      ),
      TRUE ~ sprintf(
        "CNAE conocido en %.1f%%; categoria dominante: %s (%.1f%% de CNAE conocidos), sin enriquecimiento diferencial fuerte.",
        coverage_pct,
        top_cnae_section_label,
        top_cnae_section_share_pct
      )
    ),
    goiener_action = goiener_action_text(
      cluster, mean_ratio_night, mean_zero_day_rate, mean_low_day_rate,
      mean_peak_share, mean_valley_share
    ),
    forecasting_or_flexibility_link = sprintf(
      "%s Forecasting: %s",
      flexibility_interpretation,
      forecast_recommendation
    ),
    caveat = case_when(
      !is.na(n_cnae_known) & n_cnae_known < cnae_min_n ~ sprintf(
        "Soporte CNAE inferior a %s usuarios; lectura solo descriptiva.",
        cnae_min_n
      ),
      !is.na(coverage_pct) & coverage_pct < 90 ~
        "CNAE incompleto en una parte relevante del cluster; evitar conclusiones de composicion fina.",
      top_cnae_section == "T" & top_cnae_section_share_pct >= 80 ~
        "CNAE describe mayoritariamente hogares; no debe traducirse a renta, ocupacion ni tipo de vivienda.",
      TRUE ~
        "Lectura agregada: CNAE contextualiza la cartera, pero no explica causas individuales del consumo."
    )
  ) |>
  transmute(
    cluster, cluster_label, n_usuarios, pct_usuarios,
    business_question, behavioral_signal, cnae_signal, goiener_action,
    forecasting_or_flexibility_link, caveat,
    cnae_coverage_pct = coverage_pct,
    top_cnae_section_label,
    top_cnae_section_share_pct,
    top_enriched_cnae_section_label,
    top_enriched_cnae_ratio,
    min_support_n = cnae_min_n
  ) |>
  arrange(cluster)

write.csv(cluster_business_interpretation,
          path(TABLE_DIR, "cluster_business_interpretation.csv"),
          row.names = FALSE)

anomaly_summary <- features |>
  filter(!is.na(cluster)) |>
  group_by(cluster) |>
  summarise(
    n_anomalous = sum(is_true(is_anomalous)),
    anomaly_rate_pct = round(100 * mean(is_true(is_anomalous)), 2),
    .groups = "drop"
  )

cluster_question_base <- cluster_profiles |>
  select(cluster, cluster_label, n_usuarios, pct_usuarios, mean_cv,
         mean_ratio_night, mean_zero_day_rate, mean_low_day_rate,
         mean_peak_share, mean_valley_share) |>
  left_join(
    cluster_cnae_coverage |>
      select(cluster, n_cnae_known, coverage_pct, top_cnae_section_label,
             top_cnae_section_share_pct),
    by = "cluster"
  ) |>
  left_join(top_enriched_cnae, by = "cluster") |>
  left_join(
    forecast_calibration_assessment |>
      select(cluster, WAPE, coverage_test, n_interval_alerts,
             needs_special_forecasting_calibration),
    by = "cluster"
  ) |>
  left_join(anomaly_summary, by = "cluster") |>
  mutate(
    n_cnae_known = tidyr::replace_na(n_cnae_known, 0L),
    coverage_pct = tidyr::replace_na(coverage_pct, 0),
    top_cnae_section_label = tidyr::replace_na(
      top_cnae_section_label,
      "Sin CNAE conocido"
    ),
    top_cnae_section_share_pct = tidyr::replace_na(top_cnae_section_share_pct, 0),
    n_anomalous = tidyr::replace_na(n_anomalous, 0L),
    anomaly_rate_pct = tidyr::replace_na(anomaly_rate_pct, 0)
  )

cluster_business_question_assessment <- build_business_question_assessment(
  cluster_question_base,
  cluster_business_question_catalog,
  cnae_min_n
)
write.csv(cluster_business_question_assessment,
          path(TABLE_DIR, "cluster_business_question_assessment.csv"),
          row.names = FALSE)

print(cluster_profiles)

elapsed <- (proc.time() - t0_total)[["elapsed"]]
message(sprintf("\nPaso 06 completado en %.1f s.", elapsed))
