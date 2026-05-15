# ==============================================================================
# Dashboard Shiny — GoiEner TFM (Tribunal Edition)
# ==============================================================================
# Lanzamiento:  shiny::runApp("app/shiny")
#
# Secciones:
#   1. Inicio  — hero animado + KPIs globales
#   2. EDA — datos exploratorios clave
#   3. Clusters — comparativa global + análisis individual
#   4. Forecast — diario, horario y cluster en subapartados
#   5. Benchmarks — arquitectura de datos del capítulo 6
#   6. Conclusiones
#   7. Metodología
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(arrow)
  library(forcats)
  library(ggplot2)
  library(glue)
  library(scales)
  library(lubridate)
  library(fontawesome)
  library(shinyWidgets)
  library(shinycssloaders)
  library(fs)
  library(here)
})

# ---- Config / paths --------------------------------------------------------
source(here::here("_config.R"))

TBL <- function(name) fs::path(TABLE_DIR, paste0(name, ".csv"))
read_tbl <- function(name) {
  fp <- TBL(name)
  if (!file_exists(fp)) return(NULL)
  suppressWarnings(readr::read_csv(fp, show_col_types = FALSE))
}

empty_msg <- function(text) tibble::tibble(mensaje = text)

load_eda <- function() {
  if (!file_exists(DAILY_PARQUET)) {
    return(list(summary = NULL, monthly = NULL, hourly = NULL, tariff = NULL))
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  daily_path <- path_abs(DAILY_PARQUET) |> path_norm()
  hourly_profile_path <- path_abs(USER_HOURLY_PROFILE) |> path_norm()
  metadata_path <- path_abs(METADATA_PARQUET) |> path_norm()

  summary <- DBI::dbGetQuery(con, glue("
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

  monthly <- DBI::dbGetQuery(con, glue("
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

  hourly <- if (file_exists(USER_HOURLY_PROFILE)) {
    DBI::dbGetQuery(con, glue("
      SELECT hour, AVG(mean_kWh_user) AS mean_kWh
      FROM read_parquet('{hourly_profile_path}')
      GROUP BY 1
      ORDER BY 1
    "))
  } else {
    NULL
  }

  tariff <- if (file_exists(METADATA_PARQUET)) {
    DBI::dbGetQuery(con, glue("
      SELECT COALESCE(tarifa_atr, 'Desconocida') AS tarifa, COUNT(*) AS n
      FROM read_parquet('{metadata_path}')
      GROUP BY 1
      ORDER BY n DESC
      LIMIT 8
    "))
  } else {
    NULL
  }

  list(summary = summary, monthly = monthly, hourly = hourly, tariff = tariff)
}

detect_terminal_tail_cutoff <- function(df, date_col = "date", actual_col = "actual",
                                        lookback = 28, drop_ratio = 0.35) {
  if (is.null(df) || !all(c(date_col, actual_col) %in% names(df))) return(NULL)
  df <- df |>
    arrange(.data[[date_col]]) |>
    filter(!is.na(.data[[actual_col]]))
  if (nrow(df) <= lookback + 1) return(max(df[[date_col]], na.rm = TRUE))

  last_good_idx <- nrow(df)
  while (last_good_idx > lookback) {
    previous_values <- df[[actual_col]][(last_good_idx - lookback):(last_good_idx - 1)]
    local_baseline <- median(previous_values, na.rm = TRUE)
    current_value <- df[[actual_col]][last_good_idx]
    if (!is.finite(local_baseline) || current_value >= local_baseline * drop_ratio) break
    last_good_idx <- last_good_idx - 1
  }
  df[[date_col]][last_good_idx]
}

trim_forecast_tail <- function(data) {
  cutoff <- detect_terminal_tail_cutoff(data$fc_daily)
  if (is.null(cutoff) || is.na(cutoff)) return(data)

  data$forecast_display_cutoff <- cutoff
  if (!is.null(data$fc_daily)) data$fc_daily <- data$fc_daily |> filter(date <= cutoff)
  if (!is.null(data$fc_daily_intv)) data$fc_daily_intv <- data$fc_daily_intv |> filter(date <= cutoff)
  if (!is.null(data$fc_cluster)) data$fc_cluster <- data$fc_cluster |> filter(date <= cutoff)
  if (!is.null(data$fc_hourly)) data$fc_hourly <- data$fc_hourly |> filter(as.Date(datetime) <= cutoff)
  data
}

# ---- Carga perezosa (una sola vez por sesión de servidor) -------------------
.cache <- new.env(parent = emptyenv())
get_cached <- function(key, loader) {
  if (is.null(.cache[[key]])) .cache[[key]] <- loader()
  .cache[[key]]
}

load_all <- function() {
  trim_forecast_tail(list(
    eda               = load_eda(),
    cluster_profiles  = read_tbl("cluster_profiles"),
    cluster_socio     = read_tbl("cluster_socioeconomic"),
    cluster_business  = read_tbl("cluster_business_mapping"),
    cluster_top_sep   = read_tbl("cluster_top_separators"),
    cluster_leader    = read_tbl("cluster_leaderboard"),
    fc_daily          = read_tbl("forecast_daily_predictions"),
    fc_daily_intv     = read_tbl("forecast_daily_intervals"),
    fc_hourly         = read_tbl("forecast_hourly_predictions"),
    fc_cluster        = read_tbl("forecast_cluster_predictions"),
    fc_importance     = read_tbl("forecast_daily_xgb_importance"),
    fc_leader_master  = read_tbl("forecast_master_leaderboard"),
    fc_leader_daily   = read_tbl("forecast_leaderboard_daily"),
    fc_leader_hourly  = read_tbl("forecast_leaderboard_hourly"),
    fc_leader_cluster = read_tbl("forecast_leaderboard_cluster"),
    fc_slices         = read_tbl("forecast_error_slices"),
    fc_business       = read_tbl("forecast_hourly_business_impact"),
    feat_climate      = read_tbl("feature_climate_sensitivity_summary"),
    benchmark_results = read_tbl("benchmark_results"),
    benchmark_disk    = read_tbl("benchmark_disk_size"),
    benchmark_env     = read_tbl("benchmark_environment"),
    user_clusters     = if (file_exists(USER_CLUSTERS_PARQUET)) arrow::read_parquet(USER_CLUSTERS_PARQUET) else NULL
  ))
}

# ---- Tema ------------------------------------------------------------------
theme_goi <- bs_theme(
  version = 5,
  bg = "#0F1419",
  fg = "#E6EDF3",
  primary = "#00B894",
  secondary = "#6C5CE7",
  success = "#00B894",
  info = "#74B9FF",
  warning = "#FDCB6E",
  danger = "#E17055",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale = 0.95
)

# ---- Plotly: layout oscuro común -------------------------------------------
plotly_dark <- function(p) {
  p |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(color = "#E6EDF3", family = "Inter"),
      xaxis = list(gridcolor = "rgba(255,255,255,0.06)", zerolinecolor = "rgba(255,255,255,0.1)"),
      yaxis = list(gridcolor = "rgba(255,255,255,0.06)", zerolinecolor = "rgba(255,255,255,0.1)"),
      legend = list(bgcolor = "rgba(0,0,0,0)", bordercolor = "rgba(255,255,255,0)"),
      hoverlabel = list(bgcolor = "#232B36", font = list(color = "#E6EDF3"))
    ) |>
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove =
                     c("zoom2d","pan2d","select2d","lasso2d","autoScale2d","toggleSpikelines"))
}

GOI_PALETTE <- c("#00B894","#6C5CE7","#FDCB6E","#E17055","#74B9FF","#FD79A8","#55EFC4","#A29BFE")

# ---- Helpers UI ------------------------------------------------------------
kpi_tile <- function(label, value_id, icon_name = "bolt", sub = NULL) {
  div(class = "kpi",
      tags$i(class = paste0("kpi-icon fa-solid fa-", icon_name)),
      div(class = "kpi-label", label),
      div(class = "kpi-value", textOutput(value_id, inline = TRUE)),
      if (!is.null(sub)) div(class = "kpi-sub", sub)
  )
}

section_header <- function(title, sub = NULL) {
  tagList(
    div(class = "section-title", title),
    if (!is.null(sub)) div(class = "section-sub", sub)
  )
}

# ============================================================================
# UI
# ============================================================================
ui <- page_navbar(
  title = tagList(
    tags$i(class = "fa-solid fa-bolt", style = "margin-right:0.5rem;color:#00B894;"),
    "GoiEner · TFM"
  ),
  id = "main_nav",
  theme = theme_goi,
  bg = "#0F1419",
  inverse = TRUE,
  fillable = FALSE,

  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$link(rel = "stylesheet",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    )
  ),

  # ===================== TAB: Inicio =====================
  nav_panel(
    title = tagList(fa_i("house"), " Inicio"),
    div(class = "hero",
        div(class = "orb orb-1"),
        div(class = "orb orb-2"),
        h1("Hogares, clima y consumo energético"),
        p(class = "lead",
          "Trabajo Fin de Máster · Segmentación, sensibilidad climática y forecasting ",
          "operativo sobre la cartera GoiEner (2014-2024)."),
        div(class = "badges",
            span(class = "badge", fa_i("database"), " 25 700+ usuarios"),
            span(class = "badge", fa_i("cloud-sun-rain"), " AEMET clima diario"),
            span(class = "badge", fa_i("chart-line"), " XGBoost · LightGBM · Stack"),
            span(class = "badge", fa_i("layer-group"), " K-Means PCA"),
            span(class = "badge", fa_i("shield-halved"), " Intervalos conformales")
        )
    ),

    layout_column_wrap(
      width = 1/4,
      kpi_tile("Usuarios analizados",  "kpi_users",    "users",        "tarifa 2.0 con clima"),
      kpi_tile("Clusters seleccionados","kpi_clusters", "layer-group",  "K-Means sobre PCA"),
      kpi_tile("β_HDD mediano",         "kpi_beta_hdd", "snowflake",    "kWh/día por °C frío"),
      kpi_tile("WAPE diario",           "kpi_wape",     "bullseye",     "mejor modelo del leaderboard")
    ),

    br(),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(fa_i("chart-area"), " Predicción diaria vs. real (test set)"),
        withSpinner(plotlyOutput("home_forecast_plot", height = "360px"),
                    color = "#00B894", type = 8)
      ),
      card(
        card_header(fa_i("trophy"), " Top modelos diarios (WAPE)"),
        withSpinner(plotlyOutput("home_leaderboard_plot", height = "360px"),
                    color = "#00B894", type = 8)
      )
    ),

    br(),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(fa_i("circle-nodes"), " Distribución de hogares por cluster"),
        withSpinner(plotlyOutput("home_cluster_donut", height = "340px"),
                    color = "#00B894", type = 8)
      ),
      card(
        card_header(fa_i("temperature-half"), " Sensibilidad climática por cluster"),
        withSpinner(plotlyOutput("home_climate_scatter", height = "340px"),
                    color = "#00B894", type = 8)
      )
    )
  ),

  # ===================== TAB: EDA =====================
  nav_panel(
    title = tagList(fa_i("magnifying-glass-chart"), " EDA"),
    section_header("Datos exploratorios clave",
                   "Cobertura, distribución diaria, perfil horario y tarifas antes de modelar."),

    layout_column_wrap(
      width = 1/4,
      kpi_tile("Días-usuario completos", "eda_kpi_records", "calendar-check", "hours_recorded = 24"),
      kpi_tile("Usuarios EDA", "eda_kpi_users", "users", "user_id válido"),
      kpi_tile("Mediana diaria", "eda_kpi_median", "bolt", "kWh por usuario"),
      kpi_tile("Percentil 99", "eda_kpi_p99", "triangle-exclamation", "cola de consumo")
    ),

    br(),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(fa_i("calendar-days"), " Consumo medio mensual"),
        withSpinner(plotlyOutput("eda_monthly_plot", height = "360px"),
                    color = "#00B894", type = 8)
      ),
      card(
        card_header(fa_i("clock"), " Perfil horario medio"),
        withSpinner(plotlyOutput("eda_hourly_plot", height = "360px"),
                    color = "#00B894", type = 8)
      )
    ),
    br(),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header(fa_i("file-contract"), " Tarifas principales"),
        withSpinner(plotlyOutput("eda_tariff_plot", height = "340px"),
                    color = "#00B894", type = 8)
      ),
      card(
        card_header(fa_i("table"), " Resumen EDA"),
        withSpinner(DTOutput("eda_summary_dt"), color = "#00B894", type = 8)
      )
    )
  ),

  # ===================== TAB: Clusters =====================
  nav_panel(
    title = tagList(fa_i("layer-group"), " Clusters"),
    section_header("Segmentación de hogares",
                   "Patrones de consumo + sensibilidad climática (PCA + K-Means)."),

    navset_tab(
      nav_panel(
        tagList(fa_i("chart-column"), " Comparativa global"),
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Tamaño relativo de todos los clusters"),
               withSpinner(plotlyOutput("cluster_overview_size_plot", height = "360px"),
                           color = "#00B894", type = 8)),
          card(card_header("Reparto horario medio por cluster"),
               withSpinner(plotlyOutput("cluster_overview_shape_plot", height = "360px"),
                           color = "#00B894", type = 8))
        ),
        br(),
        card(card_header("Sensibilidad climática y consumo medio"),
             withSpinner(plotlyOutput("cluster_overview_climate_plot", height = "420px"),
                         color = "#00B894", type = 8))
      ),
      nav_panel(
        tagList(fa_i("user-gear"), " Análisis individual"),
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 280,
            uiOutput("cluster_picker"),
            hr(),
            helpText(tags$small(
              fa_i("circle-info"), " Selecciona un cluster para ver perfil horario, ",
              "sensibilidad climática, separadores y recomendación."))
          ),
          navset_tab(
            nav_panel(
              tagList(fa_i("clock"), " Perfil horario"),
              br(),
              layout_columns(
                col_widths = c(8, 4),
                card(card_header("Distribución horaria del consumo (shares)"),
                     withSpinner(plotlyOutput("cluster_hourly_plot", height = "380px"),
                                 color = "#00B894", type = 8)),
                card(card_header("Indicadores"),
                     withSpinner(uiOutput("cluster_indicators"), color = "#00B894", type = 8))
              )
            ),
            nav_panel(
              tagList(fa_i("temperature-half"), " Sensibilidad climática"),
              br(),
              card(card_header("β_HDD vs β_CDD (todos los clusters; resaltado el seleccionado)"),
                   withSpinner(plotlyOutput("cluster_climate_plot", height = "440px"),
                               color = "#00B894", type = 8))
            ),
            nav_panel(
              tagList(fa_i("ranking-star"), " Top separadores"),
              br(),
              card(card_header("Variables que más distinguen el cluster del resto (z-score)"),
                   withSpinner(plotlyOutput("cluster_separators_plot", height = "440px"),
                               color = "#00B894", type = 8))
            ),
            nav_panel(
              tagList(fa_i("briefcase"), " Recomendación comercial"),
              br(),
              uiOutput("cluster_business_card")
            ),
            nav_panel(
              tagList(fa_i("table"), " Tabla completa"),
              br(),
              card(card_header("Perfiles de cluster (cluster_profiles)"),
                   withSpinner(DTOutput("cluster_table"), color = "#00B894", type = 8))
            )
          )
        )
      )
    )
  ),

  # ===================== TAB: Forecast =====================
  nav_panel(
    title = tagList(fa_i("chart-line"), " Forecast"),
    section_header("Predicción energética",
                   "Diario, horario y por cluster dentro del mismo apartado."),

    navset_tab(
      nav_panel(
        tagList(fa_i("calendar-day"), " Diario"),
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 280,
            uiOutput("daily_date_range"),
            checkboxGroupInput(
              "daily_models", "Modelos a mostrar:",
              choices  = c("XGBoost" = "xgb", "LightGBM" = "lgbm",
                           "Stacking" = "stack", "Ensemble" = "ensemble",
                           "Random Forest" = "rf", "ARIMA" = "arimax",
                           "ETS" = "ets", "S-naïve 7d" = "snaive7"),
              selected = c("xgb", "ensemble")
            ),
            materialSwitch("daily_show_band", "Banda conformal 90%",
                           value = TRUE, status = "success"),
            hr(),
            helpText(tags$small(fa_i("circle-info"),
                                " La banda gris es el intervalo conformalizado al 90%."))
          ),
          navset_tab(
            nav_panel(
              tagList(fa_i("chart-area"), " Predicción"),
              br(),
              card(card_header("Predicción vs. real"),
                   withSpinner(plotlyOutput("daily_pred_plot", height = "460px"),
                               color = "#00B894", type = 8))
            ),
            nav_panel(
              tagList(fa_i("ranking-star"), " Leaderboard"),
              br(),
              card(card_header("Métricas diarias"),
                   withSpinner(DTOutput("daily_leader_dt"), color = "#00B894", type = 8))
            ),
            nav_panel(
              tagList(fa_i("seedling"), " Importancia"),
              br(),
              card(card_header("XGBoost — importancia (Gain)"),
                   withSpinner(plotlyOutput("daily_importance_plot", height = "520px"),
                               color = "#00B894", type = 8))
            ),
            nav_panel(
              tagList(fa_i("scissors"), " Slices de error"),
              br(),
              card(card_header("Error por estación / segmento"),
                   withSpinner(plotlyOutput("daily_slices_plot", height = "420px"),
                               color = "#00B894", type = 8))
            )
          )
        )
      ),
      nav_panel(
        tagList(fa_i("clock-rotate-left"), " Horario"),
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 280,
            uiOutput("hourly_date_picker"),
            checkboxGroupInput(
              "hourly_models", "Modelos:",
              choices  = c("XGBoost" = "xgb", "LightGBM" = "lgbm",
                           "Stacking" = "stack",
                           "S-naïve 24h" = "snaive24",
                           "S-naïve 168h" = "snaive168"),
              selected = c("xgb", "stack")
            ),
            materialSwitch("hourly_show_band", "Banda cuantílica XGB",
                           value = TRUE, status = "success")
          ),
          navset_tab(
            nav_panel(
              tagList(fa_i("chart-area"), " Curva horaria"),
              br(),
              card(card_header("Consumo horario — predicción vs. real"),
                   withSpinner(plotlyOutput("hourly_plot", height = "460px"),
                               color = "#00B894", type = 8))
            ),
            nav_panel(
              tagList(fa_i("ranking-star"), " Leaderboard horario"),
              br(),
              card(card_header("Métricas horarias"),
                   withSpinner(DTOutput("hourly_leader_dt"), color = "#00B894", type = 8))
            ),
            nav_panel(
              tagList(fa_i("euro-sign"), " Impacto económico"),
              br(),
              card(card_header("Desviación económica estimada por modelo"),
                   withSpinner(plotlyOutput("hourly_business_plot", height = "420px"),
                               color = "#00B894", type = 8))
            )
          )
        )
      ),
      nav_panel(
        tagList(fa_i("diagram-project"), " Por cluster"),
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 280,
            uiOutput("cluster_fc_picker")
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(card_header("Predicción vs. real por cluster"),
                 withSpinner(plotlyOutput("cluster_fc_plot", height = "460px"),
                             color = "#00B894", type = 8)),
            card(card_header("Métricas"),
                 withSpinner(DTOutput("cluster_fc_dt"), color = "#00B894", type = 8))
          )
        )
      ),
      nav_panel(
        tagList(fa_i("ranking-star"), " Leaderboard maestro"),
        br(),
        card(card_header(fa_i("table"), " Modelos por target"),
             withSpinner(DTOutput("master_leader_dt"), color = "#00B894", type = 8)),
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("WAPE por target"),
               withSpinner(plotlyOutput("master_wape_plot", height = "380px"),
                           color = "#00B894", type = 8)),
          card(card_header("MAE por target"),
               withSpinner(plotlyOutput("master_mae_plot", height = "380px"),
                           color = "#00B894", type = 8))
        )
      )
    )
  ),

  # ===================== TAB: Benchmarks =====================
  nav_panel(
    title = tagList(fa_i("trophy"), " Benchmarks"),
    section_header("Benchmark de arquitectura",
                   "Resultados del capítulo 6: CSV, Parquet, DuckDB y tamaño en disco."),

    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Tiempo por experimento y método"),
           withSpinner(plotlyOutput("benchmark_time_plot", height = "420px"),
                       color = "#00B894", type = 8)),
      card(card_header("Tamaño en disco por formato"),
           withSpinner(plotlyOutput("benchmark_disk_plot", height = "420px"),
                       color = "#00B894", type = 8))
    ),
    br(),
    card(card_header(fa_i("table"), " Resultados detallados"),
         withSpinner(DTOutput("benchmark_dt"), color = "#00B894", type = 8)),
    br(),
    card(card_header(fa_i("computer"), " Entorno de ejecución"),
         withSpinner(DTOutput("benchmark_env_dt"), color = "#00B894", type = 8))
  ),

  # ===================== TAB: Conclusiones =====================
  nav_panel(
    title = tagList(fa_i("clipboard-check"), " Conclusiones"),
    section_header("Conclusiones del TFM",
                   "Resumen de los puntos que sostienen la memoria y la app."),
    uiOutput("conclusions_ui")
  ),

  # ===================== TAB: Metodología =====================
  nav_panel(
    title = tagList(fa_i("book"), " Metodología"),
    section_header("Metodología y reproducibilidad"),

    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(fa_i("database"), " Datos"),
        div(class = "p-3",
            tags$ul(
              tags$li(tags$b("Fuente:"), " GoiEner imputed v7 (parquet, 2014-2024)."),
              tags$li(tags$b("Universo:"), " hogares residenciales 2.0 con foco en cinco provincias con soporte suficiente."),
              tags$li(tags$b("Granularidad:"), " horaria → agregado diario por usuario."),
              tags$li(tags$b("Clima:"), " estación AEMET trazable por provincia e imputación exógena por fecha."),
              tags$li(tags$b("Splits temporales:"),
                      " train ≤ 2022-12-31 · val 2023-01..06 · test 2023-07..2024-01.")
            )
        )
      ),
      card(
        card_header(fa_i("microchip"), " Modelado"),
        div(class = "p-3",
            tags$ul(
              tags$li(tags$b("Features:"), " consumo, calendario, HDD/CDD, amplitud térmica, festivos, ",
                      "potencia contratada y sensibilidad climática por usuario."),
              tags$li(tags$b("Clustering:"), " matriz de forma + PCA + K-Means; ",
                      "selección por silhouette + Jaccard + balance."),
              tags$li(tags$b("Forecast:"), " naïve, S-naïve, ETS, ARIMAX, RF, XGBoost, ",
                      "LightGBM, GLMNet, stacking."),
              tags$li(tags$b("Cuantiles:"), " XGB objective='reg:quantileerror' (q05, q95)."),
              tags$li(tags$b("Conformal:"), " split-conformal calibrado en validación."),
              tags$li(tags$b("Reproducibilidad:"), " SEED=42, orquestador con subprocesos.")
            )
        )
      )
    ),
    br(),
    card(
      card_header(fa_i("diagram-project"), " Pipeline"),
      div(class = "p-3",
          tags$ol(
            tags$li("00_extract_raw / 01_csv_to_parquet — ingesta y formato columnar."),
            tags$li("02_data_quality — auditoría y selección de hogares."),
            tags$li("03a-d_climate — descarga AEMET, augment, imputación y merge consumo-clima."),
            tags$li("04a-b_features — feature engineering por usuario y sensibilidad climática."),
            tags$li("05a-h_clustering — pool, matriz, modelos, validación, selección, ",
                    "interpretación, socioeconómico, mapping comercial."),
            tags$li("06a-f_forecasting — targets, features temporales, modelos diarios / horarios / cluster, evaluación."),
            tags$li("07_benchmark — rendimiento CSV/Parquet/DuckDB, disco y entorno."),
            tags$li("99_orchestrator — ejecuta scripts R y QMD con logs por artefacto.")
          )
      )
    ),
    div(class = "footer",
        tags$p(strong("GoiEner TFM"),
               " · construido con R · ", fa_i("heart"), " ",
               format(Sys.Date(), "%Y"), " · ",
               tags$a(href = "#", style = "color:#00B894;", "shiny + bslib + plotly")))
  )
)


# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  D <- get_cached("all", load_all)

  # ---------- KPIs (Inicio) ------------------------------------------------
  output$kpi_users <- renderText({
    if (!is.null(D$user_clusters)) format(nrow(D$user_clusters), big.mark = " ")
    else if (!is.null(D$cluster_profiles)) format(sum(D$cluster_profiles$n), big.mark = " ")
    else "n/d"
  })
  output$kpi_clusters <- renderText({
    if (!is.null(D$cluster_profiles)) as.character(nrow(D$cluster_profiles)) else "n/d"
  })
  output$kpi_beta_hdd <- renderText({
    if (!is.null(D$feat_climate)) {
      v <- D$feat_climate$value[D$feat_climate$variable == "beta_hdd_mediana"]
      if (length(v)) sprintf("%.3f", v) else "n/d"
    } else "n/d"
  })
  output$kpi_wape <- renderText({
    if (!is.null(D$fc_leader_daily)) {
      best <- D$fc_leader_daily |>
        filter(!is.na(WAPE)) |>
        slice_min(WAPE, n = 1, with_ties = FALSE)
      if (nrow(best)) sprintf("%.2f%%", best$WAPE[1]) else "n/d"
    } else "n/d"
  })

  # ---------- EDA ----------------------------------------------------------
  output$eda_kpi_records <- renderText({
    s <- D$eda$summary
    if (!is.null(s) && nrow(s)) format(round(s$dias_usuario_completos[1]), big.mark = " ") else "n/d"
  })

  output$eda_kpi_users <- renderText({
    s <- D$eda$summary
    if (!is.null(s) && nrow(s)) format(round(s$usuarios[1]), big.mark = " ") else "n/d"
  })

  output$eda_kpi_median <- renderText({
    s <- D$eda$summary
    if (!is.null(s) && nrow(s)) sprintf("%.2f", s$mediana_kWh[1]) else "n/d"
  })

  output$eda_kpi_p99 <- renderText({
    s <- D$eda$summary
    if (!is.null(s) && nrow(s)) sprintf("%.1f", s$p99_kWh[1]) else "n/d"
  })

  output$eda_monthly_plot <- renderPlotly({
    df <- D$eda$monthly
    validate(need(!is.null(df) && nrow(df) > 0, "No hay resumen mensual EDA disponible."))
    df$month <- as.Date(df$month)
    p <- plot_ly(df, x = ~month) |>
      add_lines(y = ~mean_daily_kWh, name = "kWh medio diario",
                line = list(color = "#00B894", width = 2)) |>
      add_lines(y = ~n_users / max(n_users, na.rm = TRUE) * max(mean_daily_kWh, na.rm = TRUE),
                name = "Usuarios activos (esc.)",
                line = list(color = "#6C5CE7", width = 1.6, dash = "dot")) |>
      plotly::layout(xaxis = list(title = ""),
                     yaxis = list(title = "kWh medio diario"),
                     hovermode = "x unified")
    plotly_dark(p)
  })

  output$eda_hourly_plot <- renderPlotly({
    df <- D$eda$hourly
    validate(need(!is.null(df) && nrow(df) > 0, "No hay perfil horario preagregado disponible."))
    p <- plot_ly(df, x = ~hour, y = ~mean_kWh, type = "scatter", mode = "lines+markers",
                 line = list(color = "#FDCB6E", width = 2.2),
                 marker = list(color = "#FDCB6E", size = 6),
                 hovertemplate = "%{x}:00<br>%{y:.3f} kWh<extra></extra>") |>
      plotly::layout(xaxis = list(title = "Hora", tickmode = "linear", dtick = 2),
                     yaxis = list(title = "kWh medio"))
    plotly_dark(p)
  })

  output$eda_tariff_plot <- renderPlotly({
    df <- D$eda$tariff
    validate(need(!is.null(df) && nrow(df) > 0, "No hay metadata de tarifas disponible."))
    df <- df |> mutate(tarifa = factor(tarifa, levels = rev(tarifa)))
    p <- plot_ly(df, x = ~n, y = ~tarifa, type = "bar", orientation = "h",
                 marker = list(color = "#74B9FF"),
                 text = ~format(n, big.mark = " "),
                 textposition = "outside",
                 hovertemplate = "%{y}<br>%{x} contratos<extra></extra>") |>
      plotly::layout(xaxis = list(title = "Contratos"), yaxis = list(title = ""))
    plotly_dark(p)
  })

  output$eda_summary_dt <- renderDT({
    s <- D$eda$summary
    if (is.null(s) || !nrow(s)) s <- empty_msg("No hay resumen EDA disponible.")
    DT::datatable(s, class = "stripe hover compact nowrap",
                  options = list(dom = "t", scrollX = TRUE),
                  rownames = FALSE) |>
      formatRound(columns = which(sapply(s, is.numeric)), digits = 2)
  })

  # ---------- Home: forecast plot ------------------------------------------
  output$home_forecast_plot <- renderPlotly({
    df <- D$fc_daily; req(df)
    p <- plot_ly(df, x = ~date) |>
      add_ribbons(ymin = ~conformal_lo, ymax = ~conformal_hi,
                  line = list(color = "rgba(0,184,148,0)"),
                  fillcolor = "rgba(0,184,148,0.18)",
                  name = "Conformal 90%") |>
      add_lines(y = ~actual, name = "Real",
                line = list(color = "#E6EDF3", width = 2)) |>
      add_lines(y = ~xgb, name = "XGBoost",
                line = list(color = "#00B894", width = 2.5)) |>
      add_lines(y = ~ensemble, name = "Ensemble",
                line = list(color = "#6C5CE7", width = 2, dash = "dot")) |>
      plotly::layout(
        xaxis = list(title = ""), yaxis = list(title = "kWh / día"),
        hovermode = "x unified")
    plotly_dark(p)
  })

  output$home_leaderboard_plot <- renderPlotly({
    df <- D$fc_leader_daily; req(df)
    df <- df |> arrange(WAPE) |> head(8)
    df$model <- factor(df$model, levels = rev(df$model))
    p <- plot_ly(df, x = ~WAPE, y = ~model, type = "bar", orientation = "h",
                 marker = list(color = ~WAPE,
                               colorscale = list(c(0, "#00B894"), c(1, "#E17055")),
                               line = list(color = "rgba(255,255,255,0.15)", width = 1)),
                 text = ~sprintf("%.2f%%", WAPE),
                 textposition = "outside",
                 hovertemplate = "%{y}<br>WAPE = %{x:.2f}%<extra></extra>") |>
      plotly::layout(xaxis = list(title = "WAPE (%)"),
                     yaxis = list(title = ""))
    plotly_dark(p)
  })

  output$home_cluster_donut <- renderPlotly({
    df <- D$cluster_profiles; req(df)
    pal <- rep(GOI_PALETTE, length.out = nrow(df))
    p <- plot_ly(df, labels = ~cluster_label, values = ~n,
                 type = "pie", hole = 0.55,
                 marker = list(colors = pal,
                               line = list(color = "#0F1419", width = 2)),
                 textinfo = "label+percent",
                 hovertemplate = "%{label}<br>n = %{value}<br>%{percent}<extra></extra>")
    plotly_dark(p) |>
      plotly::layout(showlegend = FALSE,
                     annotations = list(list(text = paste0("<b>", nrow(df), "</b><br>clusters"),
                                             showarrow = FALSE,
                                             font = list(size = 18, color = "#E6EDF3"))))
  })

  output$home_climate_scatter <- renderPlotly({
    df <- D$cluster_profiles; req(df)
    p <- plot_ly(df, x = ~beta_hdd, y = ~beta_cdd,
                 type = "scatter", mode = "markers+text",
                 text = ~cluster_label, textposition = "top center",
                 textfont = list(color = "#E6EDF3", size = 11),
                 marker = list(size = ~sqrt(n) * 1.4 + 8,
                               color = ~mean_daily_kWh,
                               colorscale = list(c(0, "#00B894"), c(1, "#6C5CE7")),
                               showscale = TRUE,
                               colorbar = list(title = "kWh/día", thickness = 12, len = 0.6),
                               line = list(color = "rgba(255,255,255,0.4)", width = 1)),
                 hovertemplate = "<b>%{text}</b><br>β_HDD=%{x:.3f}<br>β_CDD=%{y:.3f}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "β_HDD (sensibilidad frío)"),
                     yaxis = list(title = "β_CDD (sensibilidad calor)"))
    plotly_dark(p)
  })

  # ---------- Cluster tab --------------------------------------------------
  output$cluster_overview_size_plot <- renderPlotly({
    df <- D$cluster_profiles; req(df)
    df <- df |> arrange(desc(n)) |> mutate(cluster_label = factor(cluster_label, levels = rev(cluster_label)))
    p <- plot_ly(df, x = ~n, y = ~cluster_label, type = "bar", orientation = "h",
                 marker = list(color = ~pct,
                               colorscale = list(c(0, "#6C5CE7"), c(1, "#00B894")),
                               line = list(color = "rgba(255,255,255,0.15)", width = 1)),
                 text = ~sprintf("%s (%.1f%%)", format(n, big.mark = " "), pct),
                 textposition = "outside",
                 hovertemplate = "%{y}<br>n=%{x}<br>%{customdata:.1f}%<extra></extra>",
                 customdata = ~pct) |>
      plotly::layout(xaxis = list(title = "Hogares"), yaxis = list(title = ""))
    plotly_dark(p)
  })

  output$cluster_overview_shape_plot <- renderPlotly({
    df <- D$cluster_profiles; req(df)
    needed <- c("cluster_label", "morning_kWh_share", "afternoon_kWh_share",
                "evening_kWh_share", "night_kWh_share")
    validate(need(all(needed %in% names(df)), "Faltan shares horarios en cluster_profiles."))
    shares <- df |>
      select(all_of(needed)) |>
      pivot_longer(-cluster_label, names_to = "franja", values_to = "share") |>
      mutate(
        franja = recode(franja,
          morning_kWh_share = "Mañana",
          afternoon_kWh_share = "Tarde",
          evening_kWh_share = "Noche",
          night_kWh_share = "Madrugada"
        )
      )
    p <- plot_ly(shares, x = ~cluster_label, y = ~share, color = ~franja,
                 colors = c("#FDCB6E", "#E17055", "#74B9FF", "#6C5CE7"),
                 type = "bar",
                 customdata = ~franja,
                 hovertemplate = "%{x}<br>%{customdata}: %{y:.1%}<extra></extra>") |>
      plotly::layout(barmode = "stack",
                     xaxis = list(title = ""),
                     yaxis = list(title = "Share diario", tickformat = ".0%"))
    plotly_dark(p)
  })

  output$cluster_overview_climate_plot <- renderPlotly({
    df <- D$cluster_profiles; req(df)
    p <- plot_ly(df, x = ~beta_hdd, y = ~beta_cdd,
                 type = "scatter", mode = "markers+text",
                 text = ~cluster_label, textposition = "top center",
                 marker = list(size = ~sqrt(n) * 1.4 + 8,
                               color = ~mean_daily_kWh,
                               colorscale = list(c(0, "#00B894"), c(1, "#E17055")),
                               showscale = TRUE,
                               colorbar = list(title = "kWh/día", thickness = 12),
                               line = list(color = "rgba(255,255,255,0.45)", width = 1)),
                 hovertemplate = "<b>%{text}</b><br>β_HDD=%{x:.3f}<br>β_CDD=%{y:.3f}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "β_HDD"),
                     yaxis = list(title = "β_CDD"))
    plotly_dark(p)
  })

  output$cluster_picker <- renderUI({
    df <- D$cluster_profiles; req(df)
    pickerInput("sel_cluster", "Cluster:", choices = df$cluster_label,
                selected = df$cluster_label[1],
                options = list(style = "btn-success"))
  })

  output$cluster_hourly_plot <- renderPlotly({
    df <- D$cluster_profiles; req(df, input$sel_cluster)
    row <- df[df$cluster_label == input$sel_cluster, ]; req(nrow(row) > 0)
    shares <- data.frame(
      franja = factor(c("Mañana (06-12)", "Tarde (12-18)", "Noche (18-00)", "Madrugada (00-06)"),
                      levels = c("Madrugada (00-06)", "Mañana (06-12)",
                                 "Tarde (12-18)", "Noche (18-00)")),
      share  = c(row$morning_kWh_share, row$afternoon_kWh_share,
                 row$evening_kWh_share, row$night_kWh_share)
    )
    pal <- c("#6C5CE7","#FDCB6E","#E17055","#00B894")
    p <- plot_ly(shares, x = ~franja, y = ~share, type = "bar",
                 marker = list(color = pal,
                               line = list(color = "rgba(255,255,255,0.15)", width = 1)),
                 text = ~sprintf("%.1f%%", share*100),
                 textposition = "outside",
                 hovertemplate = "%{x}<br>share = %{y:.1%}<extra></extra>") |>
      plotly::layout(yaxis = list(title = "Share del consumo diario",
                                  tickformat = ".0%"),
                     xaxis = list(title = ""))
    plotly_dark(p)
  })

  output$cluster_indicators <- renderUI({
    df <- D$cluster_profiles; req(df, input$sel_cluster)
    row <- df[df$cluster_label == input$sel_cluster, ]; req(nrow(row) > 0)
    socio <- D$cluster_socio
    s_row <- if (!is.null(socio)) socio[socio$cluster_label == input$sel_cluster, ] else NULL

    metric <- function(lbl, val, icon = "circle") {
      div(style = "padding:0.6rem 0; border-bottom:1px solid rgba(255,255,255,0.06);",
          div(style = "display:flex;justify-content:space-between;align-items:center;",
              span(fa_i(icon, fill = "#00B894"), " ", tags$small(lbl)),
              span(style = "font-weight:700;color:#E6EDF3;", val)))
    }
    tagList(
      metric("Hogares (n)", format(row$n, big.mark = " "), "users"),
      metric("Porcentaje", sprintf("%.2f%%", row$pct), "percent"),
      metric("Consumo medio diario", sprintf("%.2f kWh", row$mean_daily_kWh), "bolt"),
      metric("Hora pico", paste0(row$peak_hour, "h"), "clock"),
      metric("Ratio noche/día", sprintf("%.2f", row$ratio_night_day), "moon"),
      metric("β_HDD", sprintf("%.3f", row$beta_hdd), "snowflake"),
      metric("β_CDD", sprintf("%.3f", row$beta_cdd), "sun"),
      metric("R² climático", sprintf("%.2f", row$r2_joint), "chart-line"),
      if (!is.null(s_row) && nrow(s_row))
        metric("Potencia mediana (kW)", sprintf("%.2f", s_row$median_p1_kw), "plug")
    )
  })

  output$cluster_climate_plot <- renderPlotly({
    df <- D$cluster_profiles; req(df, input$sel_cluster)
    df$is_sel <- df$cluster_label == input$sel_cluster
    p <- plot_ly() |>
      add_trace(data = df[!df$is_sel, ],
                x = ~beta_hdd, y = ~beta_cdd, text = ~cluster_label,
                type = "scatter", mode = "markers+text",
                textposition = "top center",
                textfont = list(color = "#8B949E", size = 11),
                marker = list(size = ~sqrt(n) * 1.3 + 6,
                              color = "rgba(140,148,165,0.4)",
                              line = list(color = "rgba(255,255,255,0.2)", width = 1)),
                name = "Otros",
                hovertemplate = "<b>%{text}</b><br>β_HDD=%{x:.3f}<br>β_CDD=%{y:.3f}<extra></extra>") |>
      add_trace(data = df[df$is_sel, ],
                x = ~beta_hdd, y = ~beta_cdd, text = ~cluster_label,
                type = "scatter", mode = "markers+text",
                textposition = "top center",
                textfont = list(color = "#00B894", size = 14),
                marker = list(size = ~sqrt(n) * 1.6 + 12,
                              color = "#00B894",
                              line = list(color = "white", width = 2)),
                name = "Seleccionado",
                hovertemplate = "<b>%{text}</b><br>β_HDD=%{x:.3f}<br>β_CDD=%{y:.3f}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "β_HDD (sensibilidad al frío)"),
                     yaxis = list(title = "β_CDD (sensibilidad al calor)"))
    plotly_dark(p)
  })

  output$cluster_separators_plot <- renderPlotly({
    df <- D$cluster_top_sep; req(df, input$sel_cluster)
    sub <- df |> filter(cluster_label == input$sel_cluster) |>
      arrange(desc(abs(std_diff))) |> head(12) |>
      mutate(feature = factor(feature, levels = rev(feature)),
             color  = ifelse(std_diff > 0, "#00B894", "#E17055"))
    req(nrow(sub) > 0)
    p <- plot_ly(sub, x = ~std_diff, y = ~feature, type = "bar", orientation = "h",
                 marker = list(color = ~color,
                               line = list(color = "rgba(255,255,255,0.15)", width = 1)),
                 text = ~sprintf("%+.2f", std_diff),
                 textposition = "outside",
                 hovertemplate = "%{y}<br>z = %{x:.2f}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "Diferencia estandarizada vs. global"),
                     yaxis = list(title = ""))
    plotly_dark(p)
  })

  output$cluster_business_card <- renderUI({
    df <- D$cluster_business; req(df, input$sel_cluster)
    row <- df[df$cluster_label == input$sel_cluster, ]; req(nrow(row) > 0)
    field <- function(name, default = "") {
      if (!name %in% names(row)) return(default)
      value <- row[[name]][1]
      if (is.na(value) || !nzchar(as.character(value))) default else as.character(value)
    }
    profile <- field("perfil_cluster", "Perfil operativo del cluster")
    evidence <- field("evidencia_clave")
    priority <- field("prioridad")
    card(
      card_header(fa_i("briefcase"), " ", row$cluster_label,
                  span(class = "chip", style = "margin-left:1rem;",
                       row$finalidad),
                  if (nzchar(priority)) span(class = "chip", style = "margin-left:0.5rem;background:rgba(253,203,110,0.16);color:#FDCB6E;",
                                             paste("Prioridad", priority))),
      div(style = "padding:1.5rem;",
          h4(style = "color:#E6EDF3;", fa_i("fingerprint"), " Perfil"),
          p(style = "font-size:1.02rem;line-height:1.6;color:#C9D1D9;", profile),
          if (nzchar(evidence)) div(
            style = "padding:0.9rem 1rem;margin:1rem 0;border-left:4px solid #00B894;background:rgba(0,184,148,0.08);border-radius:8px;",
            tags$small(style = "color:#8B949E;text-transform:uppercase;letter-spacing:0.08em;", "Evidencia"),
            div(style = "margin-top:0.25rem;", evidence)
          ),
          h4(style = "color:#00B894;",
             fa_i("lightbulb"), " Acción recomendada"),
          p(style = "font-size:1.05rem;line-height:1.6;",
            row$accion_recomendada),
          hr(style = "border-color:rgba(255,255,255,0.08);"),
          layout_column_wrap(
            width = 1/4,
            div(class = "kpi-label", "Hogares"),
            div(class = "kpi-label", "Consumo diario"),
            div(class = "kpi-label", "% alto riesgo"),
            div(class = "kpi-label", "Pico share"),
            div(class = "kpi-value", style = "font-size:1.6rem;",
                format(row$n, big.mark = " ")),
            div(class = "kpi-value", style = "font-size:1.6rem;",
                sprintf("%.1f kWh", row$median_daily_kWh)),
            div(class = "kpi-value", style = "font-size:1.6rem;",
                sprintf("%.1f%%", row$pct_high_risk)),
            div(class = "kpi-value", style = "font-size:1.6rem;",
                sprintf("%.1f%%", row$peak_share * 100))
          )
      )
    )
  })

  output$cluster_table <- renderDT({
    df <- D$cluster_profiles; req(df)
    DT::datatable(
      df,
      class = "stripe hover compact nowrap",
      options = list(
        pageLength = 12, scrollX = TRUE,
        dom = 'frtip',
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    ) |>
      formatRound(columns = which(sapply(df, is.numeric)), digits = 3)
  })

  # ---------- Forecast diario ---------------------------------------------
  output$daily_date_range <- renderUI({
    df <- D$fc_daily; req(df)
    sliderInput("daily_dates", "Rango de fechas:",
                min = min(df$date), max = max(df$date),
                value = c(min(df$date), max(df$date)),
                timeFormat = "%Y-%m-%d", step = 7)
  })

  output$daily_pred_plot <- renderPlotly({
    df <- D$fc_daily; req(df, input$daily_dates)
    df <- df |> filter(date >= input$daily_dates[1], date <= input$daily_dates[2])

    p <- plot_ly(df, x = ~date)
    if (isTRUE(input$daily_show_band) && all(c("conformal_lo","conformal_hi") %in% names(df))) {
      p <- p |> add_ribbons(
        ymin = ~conformal_lo, ymax = ~conformal_hi,
        line = list(color = "rgba(0,184,148,0)"),
        fillcolor = "rgba(255,255,255,0.10)",
        name = "Conformal 90%",
        hovertemplate = "[%{y:.0f}]<extra>conformal</extra>"
      )
    }
    p <- p |> add_lines(y = ~actual, name = "Real",
                        line = list(color = "#E6EDF3", width = 2.5))

    colors_map <- c(xgb="#00B894", lgbm="#FDCB6E", stack="#6C5CE7",
                    ensemble="#74B9FF", rf="#FD79A8", arimax="#E17055",
                    ets="#A29BFE", snaive7="#55EFC4")
    for (m in input$daily_models) {
      if (m %in% names(df)) {
        p <- p |> add_lines(
          y = df[[m]], name = m,
          line = list(color = colors_map[[m]] %||% "#888", width = 2,
                      dash = if (m %in% c("ensemble","stack")) "dot" else "solid")
        )
      }
    }
    plotly_dark(p) |>
      plotly::layout(yaxis = list(title = "kWh / día"),
                     xaxis = list(title = ""),
                     hovermode = "x unified")
  })

  output$daily_leader_dt <- renderDT({
    df <- D$fc_leader_daily; req(df)
    DT::datatable(df, class = "stripe hover compact",
                  options = list(pageLength = 15, dom = 'frtip'),
                  rownames = FALSE) |>
      formatRound(columns = which(sapply(df, is.numeric)), digits = 3)
  })

  output$daily_importance_plot <- renderPlotly({
    df <- D$fc_importance; req(df)
    df <- df |> arrange(desc(Gain)) |> head(20) |>
      mutate(Feature = factor(Feature, levels = rev(Feature)))
    p <- plot_ly(df, x = ~Gain, y = ~Feature, type = "bar", orientation = "h",
                 marker = list(color = ~Gain,
                               colorscale = list(c(0, "#1A2028"), c(1, "#00B894")),
                               line = list(color = "rgba(255,255,255,0.15)", width = 1)),
                 hovertemplate = "%{y}<br>Gain = %{x:.4f}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "Gain"),
                     yaxis = list(title = ""))
    plotly_dark(p)
  })

  output$daily_slices_plot <- renderPlotly({
    df <- D$fc_slices; req(df)
    df <- df |> arrange(desc(WAPE))
    df$slice <- factor(df$slice, levels = df$slice)
    p <- plot_ly(df, x = ~slice, y = ~WAPE, type = "bar",
                 marker = list(color = ~WAPE,
                               colorscale = list(c(0, "#00B894"), c(1, "#E17055")),
                               line = list(color = "rgba(255,255,255,0.15)", width = 1)),
                 text = ~sprintf("%.2f%%", WAPE),
                 textposition = "outside",
                 hovertemplate = "%{x}<br>WAPE = %{y:.2f}%<br>MAE = %{customdata:.0f}<extra></extra>",
                 customdata = ~MAE) |>
      plotly::layout(xaxis = list(title = ""),
                     yaxis = list(title = "WAPE (%)"))
    plotly_dark(p)
  })

  # ---------- Forecast horario --------------------------------------------
  output$hourly_date_picker <- renderUI({
    df <- D$fc_hourly; req(df)
    days <- unique(as.Date(df$datetime))
    sliderInput("hourly_dates", "Rango de fechas:",
                min = min(days), max = max(days),
                value = c(min(days), min(days) + 7),
                timeFormat = "%Y-%m-%d", step = 1)
  })

  output$hourly_plot <- renderPlotly({
    df <- D$fc_hourly; req(df, input$hourly_dates)
    df <- df |> filter(as.Date(datetime) >= input$hourly_dates[1],
                       as.Date(datetime) <= input$hourly_dates[2])
    p <- plot_ly(df, x = ~datetime)
    if (isTRUE(input$hourly_show_band) && all(c("xgb_q05","xgb_q95") %in% names(df))) {
      p <- p |> add_ribbons(
        ymin = ~xgb_q05, ymax = ~xgb_q95,
        line = list(color = "rgba(0,184,148,0)"),
        fillcolor = "rgba(0,184,148,0.15)",
        name = "XGB q05-q95"
      )
    }
    p <- p |> add_lines(y = ~actual, name = "Real",
                        line = list(color = "#E6EDF3", width = 2))
    colors_map <- c(xgb="#00B894", lgbm="#FDCB6E", stack="#6C5CE7",
                    snaive24="#A29BFE", snaive168="#FD79A8")
    for (m in input$hourly_models) {
      if (m %in% names(df)) {
        p <- p |> add_lines(y = df[[m]], name = m,
                            line = list(color = colors_map[[m]] %||% "#888", width = 1.8))
      }
    }
    plotly_dark(p) |>
      plotly::layout(yaxis = list(title = "kWh / hora"),
                     xaxis = list(title = ""),
                     hovermode = "x unified")
  })

  output$hourly_leader_dt <- renderDT({
    df <- D$fc_leader_hourly; req(df)
    DT::datatable(df, class = "stripe hover compact",
                  options = list(pageLength = 15, dom = 'frtip'),
                  rownames = FALSE) |>
      formatRound(columns = which(sapply(df, is.numeric)), digits = 3)
  })

  output$hourly_business_plot <- renderPlotly({
    df <- D$fc_business; req(df)
    df <- df |> arrange(EUR_dev_total)
    df$model <- factor(df$model, levels = df$model)
    p <- plot_ly(df, x = ~model, y = ~EUR_dev_total, type = "bar",
                 marker = list(color = ~EUR_dev_total,
                               colorscale = list(c(0, "#00B894"), c(1, "#E17055")),
                               line = list(color = "rgba(255,255,255,0.15)", width = 1)),
                 text = ~scales::dollar(EUR_dev_total, prefix = "€", big.mark = " "),
                 textposition = "outside",
                 hovertemplate = "%{x}<br>EUR_dev = %{y:,.0f}<extra></extra>") |>
      plotly::layout(xaxis = list(title = ""),
                     yaxis = list(title = "Desviación económica total (€)"))
    plotly_dark(p)
  })

  # ---------- Forecast por cluster ----------------------------------------
  output$cluster_fc_picker <- renderUI({
    df <- D$fc_cluster; req(df)
    clusters <- sort(unique(df$cluster))
    pickerInput("sel_cluster_fc", "Cluster:",
                choices = clusters,
                selected = clusters[1],
                options = list(style = "btn-success"))
  })

  output$cluster_fc_plot <- renderPlotly({
    df <- D$fc_cluster; req(df, input$sel_cluster_fc)
    sub <- df |> filter(cluster == as.numeric(input$sel_cluster_fc))
    req(nrow(sub) > 0)
    p <- plot_ly(sub, x = ~date) |>
      add_lines(y = ~actual, name = "Real",
                line = list(color = "#E6EDF3", width = 2)) |>
      add_lines(y = ~pred, name = "Predicción",
                line = list(color = "#00B894", width = 2.5)) |>
      plotly::layout(yaxis = list(title = "kWh / día"),
                     xaxis = list(title = ""),
                     hovermode = "x unified")
    plotly_dark(p)
  })

  output$cluster_fc_dt <- renderDT({
    df <- D$fc_leader_cluster; req(df, input$sel_cluster_fc)
    sub <- df |> filter(cluster == as.numeric(input$sel_cluster_fc))
    if (!nrow(sub)) sub <- df
    DT::datatable(sub, class = "stripe hover compact",
                  options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE) |>
      formatRound(columns = which(sapply(sub, is.numeric)), digits = 3)
  })

  # ---------- Master leaderboard ------------------------------------------
  output$master_leader_dt <- renderDT({
    df <- D$fc_leader_master; req(df)
    DT::datatable(df, class = "stripe hover compact",
                  options = list(pageLength = 20, scrollX = TRUE, dom = 'frtip'),
                  rownames = FALSE,
                  filter = "top") |>
      formatRound(columns = which(sapply(df, is.numeric)), digits = 3)
  })

  output$master_wape_plot <- renderPlotly({
    df <- D$fc_leader_master; req(df)
    df <- df |> mutate(label = paste0(model, ifelse(is.na(cluster), "", paste0(" / c", cluster))))
    p <- plot_ly(df, x = ~target, y = ~WAPE, color = ~model,
                 colors = GOI_PALETTE,
                 type = "scatter", mode = "markers",
                 marker = list(size = 12, line = list(color = "white", width = 1)),
                 text = ~label,
                 hovertemplate = "%{text}<br>%{x}<br>WAPE = %{y:.2f}%<extra></extra>") |>
      plotly::layout(yaxis = list(title = "WAPE (%)"),
                     xaxis = list(title = ""))
    plotly_dark(p)
  })

  output$master_mae_plot <- renderPlotly({
    df <- D$fc_leader_master; req(df)
    df <- df |> mutate(label = paste0(model, ifelse(is.na(cluster), "", paste0(" / c", cluster))))
    p <- plot_ly(df, x = ~target, y = ~MAE, color = ~model,
                 colors = GOI_PALETTE,
                 type = "scatter", mode = "markers",
                 marker = list(size = 12, line = list(color = "white", width = 1)),
                 text = ~label,
                 hovertemplate = "%{text}<br>%{x}<br>MAE = %{y:.0f}<extra></extra>") |>
      plotly::layout(yaxis = list(title = "MAE (kWh)"),
                     xaxis = list(title = ""))
    plotly_dark(p)
  })

  # ---------- Architecture benchmark --------------------------------------
  output$benchmark_time_plot <- renderPlotly({
    df <- D$benchmark_results
    validate(need(!is.null(df) && nrow(df) > 0, "Ejecuta R/07_benchmark/07_benchmark.R para generar benchmark_results.csv."))
    df <- df |> mutate(metodo = forcats::fct_reorder(metodo, mediana_s))
    p <- ggplot(df, aes(x = mediana_s, y = metodo, fill = metodo,
                        text = sprintf("%s<br>%s<br>%.3fs", experimento, metodo, mediana_s))) +
      geom_col(show.legend = FALSE, width = 0.7) +
      facet_wrap(~experimento, scales = "free_y", ncol = 1) +
      scale_fill_manual(values = rep(GOI_PALETTE, length.out = length(unique(df$metodo)))) +
      labs(x = "Mediana (s)", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = "#E6EDF3", face = "bold"),
        axis.text = element_text(color = "#C9D1D9"),
        axis.title = element_text(color = "#E6EDF3")
      )
    plotly_dark(ggplotly(p, tooltip = "text"))
  })

  output$benchmark_disk_plot <- renderPlotly({
    df <- D$benchmark_disk
    validate(need(!is.null(df) && nrow(df) > 0, "Ejecuta R/07_benchmark/07_benchmark.R para generar benchmark_disk_size.csv."))
    df <- df |> filter(!is.na(MB)) |> mutate(formato = factor(formato, levels = rev(formato)))
    p <- plot_ly(df, x = ~MB, y = ~formato, type = "bar", orientation = "h",
                 marker = list(color = ~MB,
                               colorscale = list(c(0, "#00B894"), c(1, "#6C5CE7"))),
                 text = ~sprintf("%.0f MB", MB),
                 textposition = "outside",
                 hovertemplate = "%{y}<br>%{x:.1f} MB<extra></extra>") |>
      plotly::layout(xaxis = list(title = "MB"), yaxis = list(title = ""))
    plotly_dark(p)
  })

  output$benchmark_dt <- renderDT({
    df <- D$benchmark_results
    if (is.null(df) || !nrow(df)) df <- empty_msg("Ejecuta R/07_benchmark/07_benchmark.R para generar resultados.")
    DT::datatable(df, class = "stripe hover compact nowrap",
                  options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
                  rownames = FALSE) |>
      formatRound(columns = which(sapply(df, is.numeric)), digits = 3)
  })

  output$benchmark_env_dt <- renderDT({
    df <- D$benchmark_env
    if (is.null(df) || !nrow(df)) df <- empty_msg("No hay benchmark_environment.csv disponible.")
    DT::datatable(df, class = "stripe hover compact nowrap",
                  options = list(pageLength = 12, scrollX = TRUE, dom = "tip"),
                  rownames = FALSE)
  })

  # ---------- Conclusions --------------------------------------------------
  output$conclusions_ui <- renderUI({
    best_daily <- if (!is.null(D$fc_leader_daily)) {
      D$fc_leader_daily |> filter(!is.na(WAPE)) |> slice_min(WAPE, n = 1, with_ties = FALSE)
    } else {
      NULL
    }
    cluster_n <- if (!is.null(D$cluster_profiles)) nrow(D$cluster_profiles) else NA_integer_
    users <- if (!is.null(D$cluster_profiles)) sum(D$cluster_profiles$n, na.rm = TRUE) else NA_real_
    eda <- D$eda$summary
    median_kwh <- if (!is.null(eda) && nrow(eda)) eda$mediana_kWh[1] else NA_real_

    conclusion_card <- function(icon, title, lead, bullets = NULL, footer = NULL) {
      card(
        card_header(fa_i(icon), " ", title),
        div(
          class = "p-3",
          p(style = "line-height:1.6;color:#C9D1D9;margin-bottom:0.75rem;", lead),
          if (!is.null(bullets)) {
            tags$ul(
              style = "color:#C9D1D9;line-height:1.55;margin-bottom:0.5rem;padding-left:1.1rem;",
              lapply(bullets, function(b) tags$li(b))
            )
          },
          if (!is.null(footer)) {
            p(style = "color:#9BA9B4;font-size:0.85rem;font-style:italic;margin:0;", footer)
          }
        )
      )
    }

    tagList(
      layout_column_wrap(
        width = 1/2,
        conclusion_card(
          "magnifying-glass-chart",
          "EDA — qué dice la cartera",
          sprintf("La cartera presenta una distribución muy asimétrica de consumo: la mediana diaria ronda %s kWh mientras la cola derecha concentra un grupo reducido de hogares de consumo alto. Esto condiciona el resto del pipeline porque cualquier media global queda dominada por esa minoría.",
                  ifelse(is.na(median_kwh), "n/d", sprintf("%.2f", median_kwh))),
          bullets = list(
            "Heterogeneidad entre usuarios mayor que la variabilidad diaria de un mismo hogar: tiene más sentido segmentar antes que ajustar un único modelo agregado.",
            "Estacionalidad clara con valles nocturnos, pico vespertino y mayor consumo en meses fríos: justifica meter calendario, hora y grados-día como features.",
            "Crecimiento sostenido de la cartera 2014→2024: obliga a validación temporal estricta (no aleatoria) para que el forecast no se contamine con altas recientes."
          ),
          footer = "Implicación: filtros de calidad por usuario, features de forma horaria y validación rolling antes de cualquier modelo."
        ),
        conclusion_card(
          "shield-halved",
          "Calidad, privacidad y alcance",
          "Los datos están seudonimizados y se trabajan siempre como agregados o segmentos: la app no muestra hogares individuales y las conclusiones se redactan a nivel de cluster o cartera. El alcance es la cartera GoiEner 2.0 con clima disponible, no la población española.",
          bullets = list(
            "Tasa de nulos y negativos negligible; la gran mayoría de usuarios tiene cobertura ≥90 % y queda traza de los días incompletos.",
            "Spikes y negativos quedan auditados por usuario y filtrados antes de features/clustering para no contaminar la estructura.",
            "Privacidad por diseño: salidas pseudonimizadas, agregadas a cluster y sin atributos identificativos sensibles."
          ),
          footer = "Implicación ética: el TFM informa decisiones de cartera (tarifa, campañas, compras), no segmentaciones individuales."
        ),
        conclusion_card(
          "cloud-sun-rain",
          "Clima — qué aporta AEMET",
          "La capa climática diaria se construye desde AEMET por provincia, se imputa cuando hay huecos y se cruza con calendario (festivos nacionales y autonómicos, puentes, Semana Santa). Permite separar carga térmica de inercia social sin imputar consumo.",
          bullets = list(
            "HDD/CDD resumen presión térmica de manera defendible: no son causales pero capturan invierno y verano con una sola variable cada uno.",
            "La sensibilidad climática (β_HDD, β_CDD) se estima por usuario y luego se promedia por cluster: identifica segmentos donde calefacción o refrigeración eléctrica pesan más.",
            "El calendario diferenciado (nacional vs autonómico) evita confundir señales estatales con festivos de Euskadi, Navarra o Madrid."
          ),
          footer = "Implicación operativa: si el feed AEMET falla, el WAPE del forecast se degrada de forma medible — define un SLO upstream."
        ),
        conclusion_card(
          "layer-group",
          "Clusters — segmentación operativa",
          sprintf("La solución K-Means sobre PCA segmenta %s hogares en %s grupos comparables, más un segmento descriptivo no_habitual para baja ocupación o segundas residencias. Los clusters separan por forma horaria, sensibilidad climática y régimen de consumo, no por kWh absolutos.",
                  ifelse(is.na(users), "n/d", format(users, big.mark = " ")),
                  ifelse(is.na(cluster_n), "n/d", cluster_n)),
          bullets = list(
            "Estabilidad alta (Jaccard medio ≈0,92): los segmentos resisten remuestreo, así que no son artefactos del split concreto.",
            "Las variables más discriminantes son ratio_night_day, peak_share, amplitud estacional y β_CDD/HDD: los clusters se defienden por patrón, no por etiqueta sociodemográfica.",
            "Cada cluster tiene una acción operativa asociada (revisión de potencia, tarifa indexada valle, campaña de invierno, etc.) trazada en cluster_business_mapping."
          ),
          footer = "Implicación: la segmentación es la base de las recomendaciones de tarifa, eficiencia y forecasting por cluster."
        ),
        conclusion_card(
          "chart-line",
          "Forecast — utilidad y límites",
          sprintf("El mejor modelo diario alcanza WAPE %s en test (2023-07 → 2024-01) con XGBoost sobre target log-transformado, calendario extendido, lags semanales/anuales y HDD/CDD. Es útil para planificación agregada de cartera y compras, no como predicción individualizada.",
                  if (!is.null(best_daily) && nrow(best_daily)) sprintf("%.2f%%", best_daily$WAPE[1]) else "n/d"),
          bullets = list(
            "El modelo horario llega cerca del techo informativo (WAPE submilesimal/unidad de %) gracias a diff_lag24/168 y stacking ridge sobre validación.",
            "Los intervalos conformal split quedan más alineados con la cobertura nominal del 90 %; el cuantil directo sirve como contraste, pero sin la misma garantía teórica.",
            "Errores mayores se concentran en lunes, festivos y transiciones de régimen: queda margen vía calendario y clima determinista."
          ),
          footer = "Implicación: usar conformal para regulador/reporting financiero, quantile para pricing interno."
        ),
        conclusion_card(
          "database",
          "Arquitectura — reproducibilidad",
          "El stack Parquet + DuckDB + tablas pre-agregadas hace viable reproducir el TFM en local en horas, sin desplegar Spark ni infraestructura distribuida. El benchmark del Capítulo 6 lo cuantifica con tiempos reales, no afirmaciones genéricas.",
          bullets = list(
            "Compresión ZSTD-9 reduce a 30-50 % del CSV equivalente, partition pruning por año evita I/O innecesario y la lectura columnar acelera consultas que tocan pocas columnas.",
            "El orquestador R/99_orchestrator.R encadena 24 scripts (extracción → calidad → clima → features → clustering → forecasting → benchmark) con log y timings por fase.",
            "El re-render reproducible se completa de extremo a extremo desde el .tar.zst original, incluyendo descarga AEMET cuando el caché está vacío."
          ),
          footer = "Implicación: el TFM se valida 1:1 desde cero; no depende de outputs serializados ad-hoc."
        )
      )
    )
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

app <- shinyApp(ui, server)
app
