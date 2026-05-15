#!/usr/bin/env Rscript
# ==============================================================================
# R/06_forecasting/06f_evaluation.R
#
# Consolidacion: leaderboards unificados + slices de error + impacto.
# Outputs:
#   outputs/tables/forecast_master_leaderboard.csv
#   outputs/tables/forecast_error_slices.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(fs); library(here); library(tidyr)
  library(lubridate)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))
source(here::here("R", "_lib", "forecast_metrics.R"))

log_section("PASO 06f: Consolidacion de leaderboards")
t0 <- proc.time()

read_safe <- function(file) {
  p <- path(TABLE_DIR, file)
  if (file_exists(p)) read.csv(p, stringsAsFactors = FALSE) else NULL
}

daily   <- read_safe("forecast_leaderboard_daily.csv")
hourly  <- read_safe("forecast_leaderboard_hourly.csv")
cluster <- read_safe("forecast_leaderboard_cluster.csv")

master <- bind_rows(
  daily   |> mutate(target = "portfolio_daily")    %||% NULL,
  hourly  |> mutate(target = "portfolio_hourly")   %||% NULL,
  cluster |> mutate(target = "cluster_daily")      %||% NULL
)
`%||%` <- function(a, b) if (is.null(a)) b else a
write_csv_audit(master, "forecast_master_leaderboard.csv")
print(master)

# Error slices on daily predictions: by season, dow, holiday.
preds <- read_safe("forecast_daily_predictions.csv")
if (!is.null(preds)) {
  preds$date <- as.Date(preds$date)
  best_model <- daily$model[1]
  if (!is.null(best_model) && best_model %in% names(preds)) {
    preds$yhat <- preds[[best_model]]
    preds$abs_err <- abs(preds$actual - preds$yhat)
    preds$month <- month(preds$date)
    preds$season <- case_when(
      preds$month %in% c(12, 1, 2) ~ "Invierno",
      preds$month %in% c(3, 4, 5) ~ "Primavera",
      preds$month %in% c(6, 7, 8) ~ "Verano",
      TRUE ~ "Otono"
    )
    preds$dow <- wday(preds$date, week_start = 1, label = TRUE,
                      locale = "en_US.UTF-8")

    slices <- bind_rows(
      preds |> group_by(slice = paste0("season=", season)) |>
        summarise(MAE = mean(abs_err, na.rm = TRUE),
                  WAPE = 100 * sum(abs_err) / sum(actual), .groups = "drop"),
      preds |> group_by(slice = paste0("dow=", dow)) |>
        summarise(MAE = mean(abs_err, na.rm = TRUE),
                  WAPE = 100 * sum(abs_err) / sum(actual), .groups = "drop")
    ) |> mutate(across(where(is.numeric), \(x) round(x, 3))) |>
      mutate(best_model = best_model)
    write_csv_audit(slices, "forecast_error_slices.csv")
  }
}

message(sprintf("06f en %.1f s", (proc.time() - t0)[["elapsed"]]))
