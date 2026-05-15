#!/usr/bin/env Rscript

# ==============================================================================
# GoiEner TFM - Paso 07: Forecasting diario sin leakage
# ==============================================================================
#
# Objetivo principal:
#   Predecir mean_user_kWh para cada dia y reconstruir total_kWh como
#   pred_mean_user_kWh * n_users. Esto separa comportamiento medio de cambios en
#   el numero de usuarios activos.
#
# Objetivo secundario:
#   Mantener un XGBoost directo sobre total_kWh para comparar contra el enfoque
#   normalizado por usuario.
#
# Validacion:
#   - Train: <= TRAIN_END
#   - Val:   VAL_START..VAL_END, usado para early stopping y calibracion conformal
#   - Test:  TEST_START..TEST_END
#
# Outputs principales:
#   outputs/tables/forecast_metrics.csv
#   outputs/tables/forecast_probabilistic_metrics.csv
#   outputs/tables/forecast_reconciliation_metrics.csv
#   outputs/tables/forecast_bottomup_vs_topdown.csv
#   outputs/tables/forecast_bottomup_scope.csv
#   outputs/tables/forecast_error_by_season_dow.csv
#   outputs/tables/forecast_c0_special_segment_review.csv
#   outputs/tables/forecast_cluster_probabilistic_metrics.csv
#   outputs/tables/forecast_reconciled_cluster_metrics.csv
#
# Importante:
#   Todas las features moviles se calculan con valores estrictamente anteriores
#   al dia t. La evaluacion de ML es one-day-ahead con historico observado hasta
#   t - 1.
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(ranger)
  library(xgboost)
  library(glmnet)
  library(forecast)
  library(ggplot2)
  library(glue)
  library(fs)
})

source(here::here("_config.R"))

message("=" |> strrep(60))
message("PASO 07: Forecasting diario sin leakage")
message("=" |> strrep(60))

t0_total <- proc.time()
set.seed(SEED)

ALPHA_INTERVAL <- 0.10

# ==============================================================================
# Helpers
# ==============================================================================

calc_mae <- function(actual, pred) {
  mean(abs(actual - pred), na.rm = TRUE)
}

calc_rmse <- function(actual, pred) {
  sqrt(mean((actual - pred)^2, na.rm = TRUE))
}

calc_mape <- function(actual, pred) {
  valid <- actual != 0 & !is.na(actual) & !is.na(pred)
  if (!any(valid)) return(NA_real_)
  mean(abs((actual[valid] - pred[valid]) / actual[valid])) * 100
}

calc_wape <- function(actual, pred) {
  valid <- !is.na(actual) & !is.na(pred)
  denom <- sum(abs(actual[valid]), na.rm = TRUE)
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  sum(abs(actual[valid] - pred[valid]), na.rm = TRUE) / denom * 100
}

calc_smape <- function(actual, pred) {
  denom <- abs(actual) + abs(pred)
  valid <- denom > 0 & !is.na(actual) & !is.na(pred)
  if (!any(valid)) return(NA_real_)
  mean(2 * abs(pred[valid] - actual[valid]) / denom[valid]) * 100
}

dow_label_es <- function(dow) {
  dplyr::case_when(
    as.integer(dow) == 1L ~ "Lunes",
    as.integer(dow) == 2L ~ "Martes",
    as.integer(dow) == 3L ~ "Miercoles",
    as.integer(dow) == 4L ~ "Jueves",
    as.integer(dow) == 5L ~ "Viernes",
    as.integer(dow) == 6L ~ "Sabado",
    as.integer(dow) == 7L ~ "Domingo",
    TRUE ~ "Desconocido"
  )
}

calc_mase <- function(actual, pred, naive_pred) {
  mae_model <- calc_mae(actual, pred)
  mae_naive <- calc_mae(actual, naive_pred)
  if (!is.finite(mae_naive) || mae_naive == 0) return(NA_real_)
  mae_model / mae_naive
}

cummean_na <- function(x) {
  valid <- !is.na(x)
  n <- cumsum(valid)
  total <- cumsum(ifelse(valid, x, 0))
  ifelse(n > 0, total / n, NA_real_)
}

metric_row <- function(model_key, model_label, target, actual, pred, naive_pred) {
  tibble::tibble(
    model_key = model_key,
    modelo = model_label,
    target = target,
    MAE = round(calc_mae(actual, pred), 1),
    RMSE = round(calc_rmse(actual, pred), 1),
    MAPE = round(calc_mape(actual, pred), 2),
    WAPE = round(calc_wape(actual, pred), 2),
    SMAPE = round(calc_smape(actual, pred), 2),
    MASE = round(calc_mase(actual, pred, naive_pred), 3)
  )
}

add_leak_free_features <- function(df, target_col) {
  out <- df |> arrange(date)
  y <- as.numeric(out[[target_col]])
  y_lag_1 <- dplyr::lag(y, 1)
  roll_complete <- function(x, n, fn) {
    slider::slide_dbl(
      x,
      function(z) {
        if (anyNA(z)) return(NA_real_)
        fn(z)
      },
      .before = n - 1,
      .complete = TRUE
    )
  }

  out$lag_1 <- dplyr::lag(y, 1)
  out$lag_2 <- dplyr::lag(y, 2)
  out$lag_7 <- dplyr::lag(y, 7)
  out$lag_14 <- dplyr::lag(y, 14)
  out$lag_28 <- dplyr::lag(y, 28)

  out$rolling_7 <- roll_complete(y_lag_1, 7, mean)
  out$rolling_14 <- roll_complete(y_lag_1, 14, mean)
  out$rolling_30 <- roll_complete(y_lag_1, 30, mean)
  out$rolling_sd_7 <- roll_complete(y_lag_1, 7, sd)

  out$diff_1 <- out$lag_1 - out$lag_2
  out$diff_7 <- out$lag_1 - dplyr::lag(y, 8)

  out$dow_sin <- sin(2 * pi * out$dow / 7)
  out$dow_cos <- cos(2 * pi * out$dow / 7)
  out$month_sin <- sin(2 * pi * out$month / 12)
  out$month_cos <- cos(2 * pi * out$month / 12)
  out$day_of_year <- as.integer(format(out$date, "%j"))
  out$doy_sin <- sin(2 * pi * out$day_of_year / 365)
  out$doy_cos <- cos(2 * pi * out$day_of_year / 365)

  climate_lag_cols <- intersect(c("tmed", "tmax", "tmin", "hdd", "cdd", "prec"), names(out))
  for (clim_col in climate_lag_cols) {
    clim <- as.numeric(out[[clim_col]])
    clim_lag_1 <- dplyr::lag(clim, 1)
    out[[paste0(clim_col, "_lag_1")]] <- clim_lag_1
    out[[paste0(clim_col, "_rolling_7")]] <- roll_complete(clim_lag_1, 7, mean)
    out[[paste0(clim_col, "_month_normal_past")]] <- ave(
      clim_lag_1,
      out$month,
      FUN = cummean_na
    )
  }

  out |>
    filter(!is.na(rolling_30), !is.na(lag_28))
}

feature_names_for <- function(df, weather_mode = c(
  "expost_weather", "operational_weather", "no_weather"
)) {
  weather_mode <- match.arg(weather_mode)
  base <- c(
    "n_users", "dow", "month", "is_weekend", "is_holiday", "week_of_year",
    "is_bridge_day", "is_easter_window", "days_to_holiday",
    "lag_1", "lag_2", "lag_7", "lag_14", "lag_28",
    "rolling_7", "rolling_14", "rolling_30", "rolling_sd_7",
    "diff_1", "diff_7",
    "dow_sin", "dow_cos", "month_sin", "month_cos", "doy_sin", "doy_cos"
  )
  climate_observed <- c(
    "tmed", "tmax", "tmin", "hdd", "cdd", "prec", "hr_media", "sol"
  )
  climate_known_before_t <- c(
    "daylight_hours",
    "tmed_lag_1", "tmed_rolling_7",
    "tmax_lag_1", "tmax_rolling_7",
    "tmin_lag_1", "tmin_rolling_7",
    "hdd_lag_1", "hdd_rolling_7",
    "cdd_lag_1", "cdd_rolling_7",
    "prec_lag_1", "prec_rolling_7",
    "tmed_month_normal_past", "tmax_month_normal_past",
    "tmin_month_normal_past", "hdd_month_normal_past",
    "cdd_month_normal_past", "prec_month_normal_past"
  )
  weather_features <- switch(
    weather_mode,
    expost_weather = c(climate_observed, climate_known_before_t),
    operational_weather = climate_known_before_t,
    no_weather = character(0)
  )
  intersect(c(base, weather_features), names(df))
}

split_temporal <- function(df) {
  list(
    train = df |> filter(date <= TRAIN_END),
    val = df |> filter(date >= VAL_START & date <= VAL_END),
    test = df |> filter(date >= TEST_START & date <= TEST_END)
  )
}

fit_medians <- function(x) {
  vapply(x, function(col) {
    med <- median(col, na.rm = TRUE)
    if (!is.finite(med)) 0 else med
  }, numeric(1))
}

apply_medians <- function(x, medians) {
  x <- as.data.frame(x)
  for (col in names(x)) {
    x[[col]][is.na(x[[col]])] <- medians[[col]]
  }
  x
}

prep_feature_matrices <- function(train_df, pred_df, features) {
  train_x <- train_df |> select(all_of(features)) |> as.data.frame()
  pred_x <- pred_df |> select(all_of(features)) |> as.data.frame()
  med <- fit_medians(train_x)
  list(
    train_x = apply_medians(train_x, med),
    pred_x = apply_medians(pred_x, med),
    medians = med
  )
}

default_xgb_params <- function() {
  list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.04,
    max_depth = 5,
    subsample = 0.85,
    colsample_bytree = 0.85,
    min_child_weight = 5
  )
}

xgb_params_from_grid <- function(row) {
  params <- default_xgb_params()
  for (name in intersect(names(row), names(params))) {
    value <- row[[name]]
    if (name == "max_depth") value <- as.integer(value)
    params[[name]] <- value
  }
  params
}

fit_xgb_with_validation <- function(train_df, val_df, final_df, test_df,
                                    features, target_col,
                                    model_id = "xgb",
                                    enable_tuning = FALSE) {
  tune_mats <- prep_feature_matrices(train_df, val_df, features)
  dtrain <- xgb.DMatrix(
    data = as.matrix(tune_mats$train_x),
    label = train_df[[target_col]]
  )
  dval <- xgb.DMatrix(
    data = as.matrix(tune_mats$pred_x),
    label = val_df[[target_col]]
  )

  tuning_results <- tibble::tibble()
  params <- default_xgb_params()
  tune_model <- NULL
  val_pred <- NULL

  if (isTRUE(enable_tuning) && isTRUE(FORECAST_XGB_ENABLE_TUNING)) {
    baseline_grid <- tibble::tibble(
      eta = default_xgb_params()$eta,
      max_depth = default_xgb_params()$max_depth,
      min_child_weight = default_xgb_params()$min_child_weight,
      subsample = default_xgb_params()$subsample,
      colsample_bytree = default_xgb_params()$colsample_bytree
    )
    grid <- bind_rows(baseline_grid, FORECAST_XGB_GRID) |>
      distinct() |>
      head(FORECAST_XGB_MAX_GRID_ROWS)
    candidates <- vector("list", nrow(grid))

    for (grid_id in seq_len(nrow(grid))) {
      params_i <- xgb_params_from_grid(grid[grid_id, , drop = FALSE])
      set.seed(SEED + grid_id)
      model_i <- xgb.train(
        params = params_i,
        data = dtrain,
        nrounds = FORECAST_XGB_NROUNDS,
        evals = list(train = dtrain, val = dval),
        early_stopping_rounds = FORECAST_XGB_EARLY_STOPPING,
        verbose = 0
      )
      best_iter_i <- model_i$best_iteration
      if (is.null(best_iter_i) || length(best_iter_i) == 0 || !is.finite(best_iter_i)) {
        best_iter_i <- 300L
      }
      pred_i <- pmax(0, as.numeric(predict(model_i, dval)))
      candidates[[grid_id]] <- list(
        model = model_i,
        params = params_i,
        best_iter = max(50L, as.integer(best_iter_i)),
        val_pred = pred_i,
        metrics = tibble::tibble(
          model_id = model_id,
          target_col = target_col,
          grid_id = grid_id,
          eta = params_i$eta,
          max_depth = params_i$max_depth,
          min_child_weight = params_i$min_child_weight,
          subsample = params_i$subsample,
          colsample_bytree = params_i$colsample_bytree,
          best_iteration = max(50L, as.integer(best_iter_i)),
          val_MAE = round(calc_mae(val_df[[target_col]], pred_i), 5),
          val_RMSE = round(calc_rmse(val_df[[target_col]], pred_i), 5),
          val_WAPE = round(calc_wape(val_df[[target_col]], pred_i), 5)
        )
      )
    }

    tuning_results <- bind_rows(lapply(candidates, `[[`, "metrics")) |>
      arrange(val_RMSE, val_MAE)
    selected_grid_id <- tuning_results$grid_id[1]
    selected <- candidates[[selected_grid_id]]
    params <- selected$params
    tune_model <- selected$model
    best_iter <- selected$best_iter
    val_pred <- selected$val_pred
    tuning_results <- tuning_results |>
      mutate(selected = grid_id == selected_grid_id)
  } else {
    set.seed(SEED)
    tune_model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = FORECAST_XGB_NROUNDS,
      evals = list(train = dtrain, val = dval),
      early_stopping_rounds = FORECAST_XGB_EARLY_STOPPING,
      verbose = 0
    )

    best_iter <- tune_model$best_iteration
    if (is.null(best_iter) || length(best_iter) == 0 || !is.finite(best_iter)) {
      best_iter <- 300L
    }
    best_iter <- max(50L, as.integer(best_iter))
    val_pred <- pmax(0, as.numeric(predict(tune_model, dval)))
  }

  final_mats <- prep_feature_matrices(final_df, test_df, features)
  dfinal <- xgb.DMatrix(
    data = as.matrix(final_mats$train_x),
    label = final_df[[target_col]]
  )
  dtest <- xgb.DMatrix(data = as.matrix(final_mats$pred_x))

  set.seed(SEED)
  final_model <- xgb.train(
    params = params,
    data = dfinal,
    nrounds = best_iter,
    verbose = 0
  )

  list(
    model = final_model,
    tune_model = tune_model,
    val_pred = pmax(0, val_pred),
    test_pred = pmax(0, as.numeric(predict(final_model, dtest))),
    best_iter = best_iter,
    params = params,
    test_x = final_mats$pred_x,
    tuning_results = tuning_results
  )
}

fit_rf_final <- function(final_df, test_df, features, target_col) {
  mats <- prep_feature_matrices(final_df, test_df, features)
  set.seed(SEED)
  model <- ranger(
    x = mats$train_x,
    y = final_df[[target_col]],
    num.trees = 500,
    mtry = max(1, floor(sqrt(length(features)))),
    min.node.size = 5,
    importance = "permutation",
    seed = SEED
  )
  list(
    model = model,
    pred = pmax(0, as.numeric(predict(model, data = mats$pred_x)$predictions))
  )
}

fit_glmnet_with_validation <- function(train_df, val_df, final_df, test_df,
                                       features, target_col) {
  tune_mats <- prep_feature_matrices(train_df, val_df, features)
  fit_path <- glmnet(
    x = as.matrix(tune_mats$train_x),
    y = train_df[[target_col]],
    alpha = 0.5,
    standardize = TRUE
  )
  val_pred_matrix <- predict(fit_path, newx = as.matrix(tune_mats$pred_x))
  rmse_by_lambda <- apply(val_pred_matrix, 2, function(p) {
    calc_rmse(val_df[[target_col]], as.numeric(p))
  })
  best_lambda <- fit_path$lambda[which.min(rmse_by_lambda)]

  final_mats <- prep_feature_matrices(final_df, test_df, features)
  final_fit <- glmnet(
    x = as.matrix(final_mats$train_x),
    y = final_df[[target_col]],
    alpha = 0.5,
    lambda = best_lambda,
    standardize = TRUE
  )

  list(
    model = final_fit,
    lambda = best_lambda,
    pred = pmax(0, as.numeric(predict(
      final_fit, newx = as.matrix(final_mats$pred_x), s = best_lambda
    )))
  )
}

fit_ets <- function(final_df, test_df, target_col) {
  tryCatch({
    y <- ts(final_df[[target_col]], frequency = 7)
    fit <- forecast::ets(y)
    pred <- as.numeric(forecast::forecast(fit, h = nrow(test_df))$mean)
    list(model = fit, pred = pmax(0, pred))
  }, error = function(e) {
    message("  ETS omitido: ", conditionMessage(e))
    list(model = NULL, pred = rep(NA_real_, nrow(test_df)))
  })
}

fit_arima_xreg <- function(final_df, test_df, target_col) {
  xreg_features <- intersect(
    c(
      "n_users", "dow", "month", "is_weekend", "is_holiday", "week_of_year",
      "dow_sin", "dow_cos", "month_sin", "month_cos", "doy_sin", "doy_cos",
      "tmed", "hdd", "cdd", "prec", "daylight_hours"
    ),
    names(final_df)
  )
  tryCatch({
    mats <- prep_feature_matrices(final_df, test_df, xreg_features)
    fit <- forecast::auto.arima(
      ts(final_df[[target_col]], frequency = 7),
      xreg = as.matrix(mats$train_x),
      seasonal = TRUE,
      stepwise = TRUE,
      approximation = TRUE
    )
    pred <- as.numeric(forecast::forecast(
      fit, h = nrow(test_df), xreg = as.matrix(mats$pred_x)
    )$mean)
    list(model = fit, pred = pmax(0, pred))
  }, error = function(e) {
    message("  Auto ARIMA omitido: ", conditionMessage(e))
    list(model = NULL, pred = rep(NA_real_, nrow(test_df)))
  })
}

conformal_q <- function(abs_residuals, alpha = 0.10) {
  r <- abs_residuals[is.finite(abs_residuals)]
  if (length(r) == 0) return(NA_real_)
  n <- length(r)
  q <- ceiling((n + 1) * (1 - alpha)) / n
  q <- min(1, q)
  as.numeric(stats::quantile(r, probs = q, type = 1, na.rm = TRUE))
}

learn_interval_safety_factor <- function(abs_residuals, qhat,
                                         target_coverage = 1 - ALPHA_INTERVAL,
                                         min_factor = max(FORECAST_CLUSTER_INTERVAL_SAFETY_FACTOR, 1.50),
                                         max_factor = 2.50) {
  r <- abs_residuals[is.finite(abs_residuals)]
  if (length(r) == 0 || !is.finite(qhat) || qhat <= 0) return(min_factor)
  factors <- seq(1, max_factor, by = 0.05)
  coverage <- vapply(factors, function(f) mean(r <= qhat * f), numeric(1))
  ok <- which(coverage >= target_coverage)
  learned <- if (length(ok) > 0) factors[min(ok)] else max_factor
  max(min_factor, learned)
}

cluster_interval_min_factor <- function(cluster_id) {
  if (as.integer(cluster_id) == 0L) {
    return(max(FORECAST_RULE_CLUSTER_INTERVAL_SAFETY_FACTOR, 1.50))
  }
  max(FORECAST_CLUSTER_INTERVAL_SAFETY_FACTOR, 1.50)
}

winkler_score <- function(actual, lower, upper, alpha = 0.10) {
  width <- upper - lower
  width +
    ifelse(actual < lower, 2 / alpha * (lower - actual), 0) +
    ifelse(actual > upper, 2 / alpha * (actual - upper), 0)
}

pinball_loss <- function(actual, q_pred, tau) {
  err <- actual - q_pred
  pmax(tau * err, (tau - 1) * err)
}

interval_score_row <- function(scope, actual, lower, upper,
                               alpha = ALPHA_INTERVAL,
                               calibration_source = NA_character_) {
  inside <- actual >= lower & actual <= upper
  tibble::tibble(
    scope = scope,
    n_days = sum(!is.na(actual) & !is.na(lower) & !is.na(upper)),
    intended_coverage = 100 * (1 - alpha),
    empirical_coverage = round(100 * mean(inside, na.rm = TRUE), 2),
    mean_width_kWh = round(mean(upper - lower, na.rm = TRUE), 1),
    mean_winkler = round(mean(winkler_score(actual, lower, upper, alpha),
                              na.rm = TRUE), 1),
    pinball_q05 = round(mean(pinball_loss(actual, lower, 0.05), na.rm = TRUE), 3),
    pinball_q95 = round(mean(pinball_loss(actual, upper, 0.95), na.rm = TRUE), 3),
    calibration_source = calibration_source
  )
}

write_importance <- function(model, file_name) {
  imp <- data.frame(
    feature = names(model$variable.importance),
    importance = as.numeric(model$variable.importance)
  ) |>
    arrange(desc(importance)) |>
    mutate(pct = round(100 * importance / sum(abs(importance), na.rm = TRUE), 1))
  write.csv(imp, path(TABLE_DIR, file_name), row.names = FALSE)
  imp
}

# ==============================================================================
# 1. Serie diaria agregada
# ==============================================================================
message("\n[1/9] Preparando serie diaria agregada...")

con <- connect_duckdb()
has_climate <- file_exists(DAILY_WITH_CLIMATE)
focus_filter_sql <- focus_province_filter_sql("cod_provincia")
focus_filter_sql_d <- focus_province_filter_sql("d.cod_provincia")
message(sprintf(
  "  Alcance modelado: %s (%s)",
  MODEL_SCOPE_NAME, paste(FOCUS_PROVINCES, collapse = ", ")
))

if (has_climate) {
  message("  Usando daily_with_climate.parquet")
  source_abs <- path_abs(DAILY_WITH_CLIMATE) |> path_norm()

  daily_agg <- dbGetQuery(con, glue("
    SELECT
      date,
      SUM(daily_kWh) AS total_kWh,
      COUNT(DISTINCT user_id) AS n_users,
      AVG(daily_kWh) AS mean_user_kWh,
      AVG(tmed) AS tmed,
      AVG(tmax) AS tmax,
      AVG(tmin) AS tmin,
      AVG(hdd) AS hdd,
      AVG(cdd) AS cdd,
      AVG(prec) AS prec,
      AVG(hr_media) AS hr_media,
      AVG(sol) AS sol,
      AVG(daylight_hours) AS daylight_hours,
      MAX(CASE WHEN is_weekend THEN 1 ELSE 0 END) AS is_weekend,
      MAX(CASE WHEN is_holiday THEN 1 ELSE 0 END) AS is_holiday,
      MAX(CASE WHEN is_bridge_day THEN 1 ELSE 0 END) AS is_bridge_day,
      MAX(CASE WHEN is_easter_window THEN 1 ELSE 0 END) AS is_easter_window,
      MAX(days_to_holiday) AS days_to_holiday,
      MAX(dow) AS dow,
      MAX(month) AS month,
      MAX(week_of_year) AS week_of_year,
      CASE
        WHEN MAX(month) IN (12, 1, 2) THEN 'Invierno'
        WHEN MAX(month) IN (3, 4, 5) THEN 'Primavera'
        WHEN MAX(month) IN (6, 7, 8) THEN 'Verano'
        ELSE 'Otono'
      END AS season
    FROM read_parquet('{source_abs}')
    WHERE {focus_filter_sql}
    GROUP BY date
    ORDER BY date
  "))
} else {
  message("  Usando daily_consumption.parquet")
  daily_abs <- path_abs(DAILY_PARQUET) |> path_norm()
  metadata_abs <- path_abs(METADATA_PARQUET) |> path_norm()

  daily_agg <- dbGetQuery(con, glue("
    WITH meta_dedup AS (
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
        AND cod_provincia IS NOT NULL AND cod_provincia <> ''
        AND {focus_filter_sql}
    )
    SELECT
      d.date,
      SUM(d.daily_kWh) AS total_kWh,
      COUNT(DISTINCT d.user_id) AS n_users,
      AVG(d.daily_kWh) AS mean_user_kWh,
      EXTRACT('isodow' FROM d.date)::INTEGER AS dow,
      MONTH(d.date) AS month,
      WEEK(d.date)::INTEGER AS week_of_year,
      CASE WHEN EXTRACT('isodow' FROM d.date) IN (6, 7) THEN 1 ELSE 0 END AS is_weekend,
      0 AS is_holiday,
      0 AS is_bridge_day,
      0 AS is_easter_window,
      30 AS days_to_holiday,
      CASE
        WHEN MONTH(d.date) IN (12, 1, 2) THEN 'Invierno'
        WHEN MONTH(d.date) IN (3, 4, 5) THEN 'Primavera'
        WHEN MONTH(d.date) IN (6, 7, 8) THEN 'Verano'
        ELSE 'Otono'
      END AS season
    FROM read_parquet('{daily_abs}') d
    INNER JOIN meta_dedup m ON d.user_id = m.user_id
    WHERE d.user_id IS NOT NULL AND d.user_id <> ''
      AND d.daily_kWh IS NOT NULL AND d.daily_kWh >= 0
      AND d.hours_recorded = 24
    GROUP BY d.date
    ORDER BY d.date
  "))
}

DBI::dbDisconnect(con, shutdown = TRUE)

daily_agg$date <- as.Date(daily_agg$date)
if (!"is_holiday" %in% names(daily_agg)) daily_agg$is_holiday <- 0L
if (!"is_bridge_day" %in% names(daily_agg)) daily_agg$is_bridge_day <- 0L
if (!"is_easter_window" %in% names(daily_agg)) daily_agg$is_easter_window <- 0L
if (!"days_to_holiday" %in% names(daily_agg)) daily_agg$days_to_holiday <- 30L

message(sprintf(
  "  Serie: %s a %s (%d dias)",
  min(daily_agg$date), max(daily_agg$date), nrow(daily_agg)
))

# ==============================================================================
# 2. Feature engineering sin leakage
# ==============================================================================
message("\n[2/9] Generando features leak-free para mean_user_kWh...")

daily_model <- add_leak_free_features(daily_agg, "mean_user_kWh")
splits <- split_temporal(daily_model)
train <- splits$train
val <- splits$val
test <- splits$test

if (nrow(val) == 0 || nrow(test) == 0) {
  stop("No hay datos suficientes para validacion/test con el split configurado.")
}

train_val <- bind_rows(train, val)
feature_names <- feature_names_for(daily_model, "expost_weather")
feature_names_operational <- feature_names_for(daily_model, "operational_weather")
feature_names_no_weather <- feature_names_for(daily_model, "no_weather")

message(sprintf("  Train: %d dias (%s a %s)", nrow(train), min(train$date), max(train$date)))
message(sprintf("  Val:   %d dias (%s a %s)", nrow(val), min(val$date), max(val$date)))
message(sprintf("  Test:  %d dias (%s a %s)", nrow(test), min(test$date), max(test$date)))
message(sprintf("  Features: %d", length(feature_names)))
message(sprintf("  Features operativas: %d", length(feature_names_operational)))

# ==============================================================================
# 3. Baselines sobre mean_user_kWh
# ==============================================================================
message("\n[3/9] Calculando baselines...")

test$pred_mean_naive7 <- test$lag_7

naive365_lookup <- daily_model |>
  select(date, mean_user_kWh) |>
  mutate(date = date + 365)

test <- test |>
  left_join(
    naive365_lookup |> rename(pred_mean_naive365 = mean_user_kWh),
    by = "date"
  )

historical_means <- train_val |>
  group_by(month, dow) |>
  summarise(pred_mean_hist_mean = mean(mean_user_kWh, na.rm = TRUE),
            .groups = "drop")

test <- test |>
  left_join(historical_means, by = c("month", "dow"))

# ==============================================================================
# 4. Modelos estadisticos y ML sobre mean_user_kWh
# ==============================================================================
message("\n[4/9] Entrenando modelos per-user...")

ets_fit <- fit_ets(train_val, test, "mean_user_kWh")
arima_fit <- fit_arima_xreg(train_val, test, "mean_user_kWh")
glmnet_fit <- fit_glmnet_with_validation(
  train, val, train_val, test, feature_names, "mean_user_kWh"
)
rf_fit <- fit_rf_final(train_val, test, feature_names, "mean_user_kWh")
xgb_fit <- fit_xgb_with_validation(
  train, val, train_val, test, feature_names, "mean_user_kWh",
  model_id = "xgb_mean_user", enable_tuning = TRUE
)
xgb_operational_fit <- fit_xgb_with_validation(
  train, val, train_val, test, feature_names_operational, "mean_user_kWh",
  model_id = "xgb_mean_user_operational", enable_tuning = TRUE
)

if (length(setdiff(feature_names, feature_names_no_weather)) > 0 &&
    length(feature_names_no_weather) >= 5) {
  xgb_no_climate_fit <- fit_xgb_with_validation(
    train, val, train_val, test, feature_names_no_weather, "mean_user_kWh",
    model_id = "xgb_mean_user_no_climate", enable_tuning = TRUE
  )
  test$pred_mean_xgb_no_climate <- xgb_no_climate_fit$test_pred
  test$pred_xgb_no_climate <- test$pred_mean_xgb_no_climate * test$n_users
  saveRDS(xgb_no_climate_fit$model, path(MODEL_DIR, "xgb_daily_no_climate.rds"))
} else {
  test$pred_mean_xgb_no_climate <- NA_real_
  test$pred_xgb_no_climate <- NA_real_
}

test$pred_mean_ets <- ets_fit$pred
test$pred_mean_arima <- arima_fit$pred
test$pred_mean_glmnet <- glmnet_fit$pred
test$pred_mean_rf <- rf_fit$pred
test$pred_mean_xgb <- xgb_fit$test_pred
test$pred_mean_xgb_operational <- xgb_operational_fit$test_pred

test <- test |>
  mutate(
    pred_naive7 = pred_mean_naive7 * n_users,
    pred_naive365 = pred_mean_naive365 * n_users,
    pred_hist_mean = pred_mean_hist_mean * n_users,
    pred_ets = pred_mean_ets * n_users,
    pred_arima = pred_mean_arima * n_users,
    pred_glmnet = pred_mean_glmnet * n_users,
    pred_rf = pred_mean_rf * n_users,
    pred_xgb = pred_mean_xgb * n_users,
    pred_xgb_operational = pred_mean_xgb_operational * n_users
  )

saveRDS(rf_fit$model, path(MODEL_DIR, "rf_daily.rds"))
saveRDS(xgb_fit$model, path(MODEL_DIR, "xgb_daily.rds"))
saveRDS(xgb_operational_fit$model, path(MODEL_DIR, "xgb_daily_operational.rds"))
saveRDS(glmnet_fit$model, path(MODEL_DIR, "glmnet_daily.rds"))
if (!is.null(ets_fit$model)) saveRDS(ets_fit$model, path(MODEL_DIR, "ets_daily.rds"))
if (!is.null(arima_fit$model)) saveRDS(arima_fit$model, path(MODEL_DIR, "arima_daily.rds"))

rf_imp <- write_importance(rf_fit$model, "rf_feature_importance.csv")
xgb_imp <- xgb.importance(model = xgb_fit$model)
write.csv(xgb_imp, path(TABLE_DIR, "xgb_feature_importance.csv"), row.names = FALSE)

# ==============================================================================
# 5. Intervalos conformales para XGBoost per-user
# ==============================================================================
message("\n[5/9] Calibrando intervalo conformal 90%...")

val_abs_resid <- abs(val$mean_user_kWh - xgb_fit$val_pred)
qhat_mean <- conformal_q(val_abs_resid, ALPHA_INTERVAL) *
  FORECAST_AGG_INTERVAL_SAFETY_FACTOR

test <- test |>
  mutate(
    pred_xgb_lower_90 = pmax(0, (pred_mean_xgb - qhat_mean) * n_users),
    pred_xgb_upper_90 = (pred_mean_xgb + qhat_mean) * n_users
  )

inside <- with(test, total_kWh >= pred_xgb_lower_90 & total_kWh <= pred_xgb_upper_90)
interval_metrics <- tibble::tibble(
  model_key = "xgb",
  metodo = "split_conformal_abs_residual_val",
  target = "mean_user_kWh_reconstructed",
  alpha = ALPHA_INTERVAL,
  intended_coverage = 100 * (1 - ALPHA_INTERVAL),
  empirical_coverage = round(100 * mean(inside, na.rm = TRUE), 2),
  mean_width_kWh = round(mean(test$pred_xgb_upper_90 - test$pred_xgb_lower_90,
                             na.rm = TRUE), 1),
  qhat_mean_user_kWh = round(qhat_mean, 5),
  n_calibration = length(val_abs_resid)
)
write.csv(interval_metrics, path(TABLE_DIR, "forecast_interval_metrics.csv"),
          row.names = FALSE)
print(interval_metrics)

score_probabilistic <- function(df, scope) {
  interval_score_row(
    scope = scope,
    actual = df$total_kWh,
    lower = df$pred_xgb_lower_90,
    upper = df$pred_xgb_upper_90,
    calibration_source = "validation_split_2022"
  )
}

prob_metric_parts <- list(
  score_probabilistic(test, "overall_test"),
  bind_rows(lapply(split(test, format(test$date, "%Y-%m")), function(df) {
    score_probabilistic(df, paste0("month_", unique(format(df$date, "%Y-%m"))))
  })),
  bind_rows(lapply(split(test, test$is_weekend), function(df) {
    score_probabilistic(df, paste0("is_weekend_", unique(df$is_weekend)))
  })),
  bind_rows(lapply(split(test, test$is_bridge_day), function(df) {
    score_probabilistic(df, paste0("is_bridge_day_", unique(df$is_bridge_day)))
  })),
  bind_rows(lapply(split(test, test$is_easter_window), function(df) {
    score_probabilistic(df, paste0("is_easter_window_", unique(df$is_easter_window)))
  }))
)

if ("season" %in% names(test)) {
  prob_metric_parts[[length(prob_metric_parts) + 1]] <- bind_rows(
    lapply(split(test, test$season), function(df) {
      score_probabilistic(df, paste0("season_", unique(df$season)))
    })
  )
}

prob_metrics <- bind_rows(prob_metric_parts)
write.csv(prob_metrics, path(TABLE_DIR, "forecast_probabilistic_metrics.csv"),
          row.names = FALSE)

# ==============================================================================
# 6. XGBoost directo sobre total_kWh (objetivo secundario)
# ==============================================================================
message("\n[6/9] Entrenando XGBoost directo sobre total_kWh...")

daily_total_model <- add_leak_free_features(daily_agg, "total_kWh")
total_splits <- split_temporal(daily_total_model)
total_train <- total_splits$train
total_val <- total_splits$val
total_test <- total_splits$test
total_train_val <- bind_rows(total_train, total_val)
total_features <- feature_names_for(daily_total_model)

xgb_total_fit <- fit_xgb_with_validation(
  total_train, total_val, total_train_val, total_test,
  total_features, "total_kWh",
  model_id = "xgb_total_direct", enable_tuning = TRUE
)

xgb_tuning_results <- bind_rows(
  xgb_fit$tuning_results,
  xgb_operational_fit$tuning_results,
  if (exists("xgb_no_climate_fit")) xgb_no_climate_fit$tuning_results else NULL,
  xgb_total_fit$tuning_results
)
if (nrow(xgb_tuning_results) > 0) {
  write.csv(xgb_tuning_results,
            path(TABLE_DIR, "forecast_tuning_results.csv"),
            row.names = FALSE)
}

total_direct_pred <- tibble::tibble(
  date = total_test$date,
  pred_xgb_total_direct = xgb_total_fit$test_pred
)

test <- test |>
  left_join(total_direct_pred, by = "date")

saveRDS(xgb_total_fit$model, path(MODEL_DIR, "xgb_total_direct.rds"))

# ==============================================================================
# 7. Evaluacion, tablas y figuras
# ==============================================================================
message("\n[7/9] Evaluando modelos...")

actual <- test$total_kWh
naive_total <- test$pred_naive7

model_specs <- list(
  list("naive7", "Naive-7", "mean_user_kWh_reconstructed", test$pred_naive7),
  list("naive365", "Naive-365", "mean_user_kWh_reconstructed", test$pred_naive365),
  list("hist_mean", "Media historica", "mean_user_kWh_reconstructed", test$pred_hist_mean),
  list("ets", "ETS", "mean_user_kWh_reconstructed", test$pred_ets),
  list("arima", "Auto ARIMA xreg", "mean_user_kWh_reconstructed", test$pred_arima),
  list("glmnet", "GLMNet", "mean_user_kWh_reconstructed", test$pred_glmnet),
  list("rf", "Random Forest", "mean_user_kWh_reconstructed", test$pred_rf),
  list("xgb", "XGBoost", "mean_user_kWh_reconstructed", test$pred_xgb),
  list("xgb_operational", "XGBoost operativo", "mean_user_kWh_reconstructed_operational",
       test$pred_xgb_operational),
  list("xgb_total_direct", "XGBoost total directo", "total_kWh_direct",
       test$pred_xgb_total_direct)
)

if (any(is.finite(test$pred_xgb_no_climate))) {
  model_specs[[length(model_specs) + 1]] <- list(
    "xgb_no_climate",
    "XGBoost sin clima",
    "mean_user_kWh_reconstructed_ablation",
    test$pred_xgb_no_climate
  )
}

metrics_df <- bind_rows(lapply(model_specs, function(x) {
  metric_row(x[[1]], x[[2]], x[[3]], actual, x[[4]], naive_total)
})) |>
  arrange(MAE)

message("\n  Tabla comparativa de modelos (test):")
print(as.data.frame(metrics_df))
write.csv(metrics_df, path(TABLE_DIR, "forecast_metrics.csv"), row.names = FALSE)

weather_mode_metrics <- bind_rows(
  metric_row(
    "xgb",
    "XGBoost ex-post",
    "mean_user_kWh_reconstructed",
    actual,
    test$pred_xgb,
    naive_total
  ) |>
    mutate(weather_mode = "expost_weather", .before = modelo),
  metric_row(
    "xgb_operational",
    "XGBoost operativo",
    "mean_user_kWh_reconstructed_operational",
    actual,
    test$pred_xgb_operational,
    naive_total
  ) |>
    mutate(weather_mode = "operational_weather", .before = modelo),
  metric_row(
    "naive7",
    "Naive-7",
    "mean_user_kWh_reconstructed",
    actual,
    test$pred_naive7,
    naive_total
  ) |>
    mutate(weather_mode = "baseline_no_weather", .before = modelo)
)
write.csv(weather_mode_metrics,
          path(TABLE_DIR, "forecast_weather_mode_metrics.csv"),
          row.names = FALSE)

predictions_export <- test |>
  select(
    date,
    actual = total_kWh,
    actual_mean_user_kWh = mean_user_kWh,
    n_users,
    starts_with("pred_mean_"),
    pred_naive7, pred_naive365, pred_hist_mean,
    pred_ets, pred_arima, pred_glmnet, pred_rf, pred_xgb,
    pred_xgb_operational,
    pred_xgb_no_climate,
    pred_xgb_lower_90, pred_xgb_upper_90,
    pred_xgb_total_direct
  )
write.csv(predictions_export, path(TABLE_DIR, "forecast_predictions.csv"),
          row.names = FALSE)

make_forecast_error_slices <- function(test_df, model_specs) {
  slice_one <- function(df, model_key, model_label, target, slice_type, slice_value) {
    tibble::tibble(
      model_key = model_key,
      modelo = model_label,
      target = target,
      slice_type = slice_type,
      slice_value = as.character(slice_value),
      n_days = sum(!is.na(df$actual) & !is.na(df$pred)),
      MAE = round(calc_mae(df$actual, df$pred), 1),
      RMSE = round(calc_rmse(df$actual, df$pred), 1),
      MAPE = round(calc_mape(df$actual, df$pred), 2),
      WAPE = round(calc_wape(df$actual, df$pred), 2),
      SMAPE = round(calc_smape(df$actual, df$pred), 2)
    )
  }

  bind_rows(lapply(model_specs, function(spec) {
    df <- tibble::tibble(
      date = test_df$date,
      actual = test_df$total_kWh,
      pred = spec[[4]],
      month = format(test_df$date, "%Y-%m"),
      dow = as.character(test_df$dow),
      dow_label = dow_label_es(test_df$dow),
      is_weekend = as.character(test_df$is_weekend),
      is_bridge_day = if ("is_bridge_day" %in% names(test_df)) as.character(test_df$is_bridge_day) else NA_character_,
      is_easter_window = if ("is_easter_window" %in% names(test_df)) as.character(test_df$is_easter_window) else NA_character_,
      season = if ("season" %in% names(test_df)) as.character(test_df$season) else NA_character_
    )

    parts <- list(slice_one(df, spec[[1]], spec[[2]], spec[[3]], "overall", "test"))
    parts <- c(parts, lapply(split(df, df$month), function(x) {
      slice_one(x, spec[[1]], spec[[2]], spec[[3]], "month", unique(x$month))
    }))
    parts <- c(parts, lapply(split(df, df$is_weekend), function(x) {
      slice_one(x, spec[[1]], spec[[2]], spec[[3]], "is_weekend", unique(x$is_weekend))
    }))
    parts <- c(parts, lapply(split(df, df$dow_label), function(x) {
      slice_one(x, spec[[1]], spec[[2]], spec[[3]], "dow", unique(x$dow_label))
    }))
    if (any(!is.na(df$is_bridge_day))) {
      parts <- c(parts, lapply(split(df, df$is_bridge_day), function(x) {
        slice_one(x, spec[[1]], spec[[2]], spec[[3]], "is_bridge_day", unique(x$is_bridge_day))
      }))
    }
    if (any(!is.na(df$is_easter_window))) {
      parts <- c(parts, lapply(split(df, df$is_easter_window), function(x) {
        slice_one(x, spec[[1]], spec[[2]], spec[[3]], "is_easter_window", unique(x$is_easter_window))
      }))
    }
    if (any(!is.na(df$season))) {
      parts <- c(parts, lapply(split(df, df$season), function(x) {
        slice_one(x, spec[[1]], spec[[2]], spec[[3]], "season", unique(x$season))
      }))
    }
    bind_rows(parts)
  })) |>
    arrange(model_key, slice_type, slice_value)
}

forecast_error_slices <- make_forecast_error_slices(test, model_specs)
write.csv(forecast_error_slices, path(TABLE_DIR, "forecast_error_slices.csv"),
          row.names = FALSE)

make_forecast_error_by_season_dow <- function(test_df, model_specs) {
  bind_rows(lapply(model_specs, function(spec) {
    df <- tibble::tibble(
      model_key = spec[[1]],
      modelo = spec[[2]],
      target = spec[[3]],
      season = if ("season" %in% names(test_df)) as.character(test_df$season) else NA_character_,
      dow = as.integer(test_df$dow),
      dow_label = dow_label_es(test_df$dow),
      actual = test_df$total_kWh,
      pred = spec[[4]]
    )

    df |>
      filter(!is.na(season), !is.na(dow), !is.na(actual), !is.na(pred)) |>
      group_by(model_key, modelo, target, season, dow, dow_label) |>
      summarise(
        n_days = n(),
        MAE = round(calc_mae(actual, pred), 1),
        RMSE = round(calc_rmse(actual, pred), 1),
        MAPE = round(calc_mape(actual, pred), 2),
        WAPE = round(calc_wape(actual, pred), 2),
        SMAPE = round(calc_smape(actual, pred), 2),
        .groups = "drop"
      )
  })) |>
    arrange(model_key, season, dow)
}

forecast_error_by_season_dow <- make_forecast_error_by_season_dow(test, model_specs)
write.csv(forecast_error_by_season_dow,
          path(TABLE_DIR, "forecast_error_by_season_dow.csv"),
          row.names = FALSE)

plot_df <- predictions_export |>
  select(date, actual, pred_naive7, pred_glmnet, pred_rf, pred_xgb,
         pred_xgb_operational, pred_xgb_no_climate, pred_xgb_total_direct) |>
  pivot_longer(-date, names_to = "serie", values_to = "kWh") |>
  mutate(
    serie = recode(
      serie,
      actual = "Real",
      pred_naive7 = "Naive-7",
      pred_glmnet = "GLMNet",
      pred_rf = "Random Forest",
      pred_xgb = "XGBoost per-user",
      pred_xgb_operational = "XGBoost operativo",
      pred_xgb_no_climate = "XGBoost sin clima",
      pred_xgb_total_direct = "XGBoost total directo"
    )
  )

p_forecast <- ggplot(plot_df, aes(date, kWh, color = serie)) +
  geom_line(aes(linewidth = serie == "Real", alpha = serie == "Real")) +
  scale_linewidth_manual(values = c("TRUE" = 0.85, "FALSE" = 0.55), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.75), guide = "none") +
  scale_color_manual(values = c(
    "Real" = "black",
    "Naive-7" = "grey65",
    "GLMNet" = "#7B3294",
    "Random Forest" = PAL_MAIN,
    "XGBoost per-user" = PAL_ACCENT,
    "XGBoost operativo" = "#5EBD72",
    "XGBoost sin clima" = "#A6A6A6",
    "XGBoost total directo" = "#008837"
  )) +
  labs(
    title = "Consumo diario: real vs predicciones",
    subtitle = "Modelos principales entrenados sobre mean_user_kWh y reconstruidos a total_kWh",
    x = NULL, y = "kWh total diario", color = NULL
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "07_forecast_comparison.png"), p_forecast,
       width = 14, height = 6, dpi = 300)

best_model <- metrics_df |> slice_min(MAE, n = 1, with_ties = FALSE)
best_pred_col <- paste0("pred_", best_model$model_key)
if (best_model$model_key == "xgb_total_direct") best_pred_col <- "pred_xgb_total_direct"

p_scatter <- test |>
  ggplot(aes(x = total_kWh, y = .data[[best_pred_col]])) +
  geom_point(alpha = 0.45, color = PAL_MAIN) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = PAL_ACCENT) +
  coord_equal() +
  labs(
    title = sprintf("Real vs predicho (%s)", best_model$modelo),
    x = "Consumo real (kWh)", y = "Consumo predicho (kWh)"
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "07_scatter_real_vs_pred.png"), p_scatter,
       width = 8, height = 8, dpi = 300)

p_intervals <- ggplot(test, aes(date)) +
  geom_ribbon(aes(ymin = pred_xgb_lower_90, ymax = pred_xgb_upper_90),
              fill = PAL_FILL, alpha = 0.7) +
  geom_line(aes(y = total_kWh), color = "black", linewidth = 0.8) +
  geom_line(aes(y = pred_xgb), color = PAL_ACCENT, linewidth = 0.6) +
  labs(
    title = "XGBoost per-user con intervalo conformal 90%",
    subtitle = sprintf(
      "Cobertura empirica %.1f%% (objetivo 90%%)",
      interval_metrics$empirical_coverage
    ),
    x = NULL, y = "kWh total diario"
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "07_xgb_intervals.png"), p_intervals,
       width = 14, height = 6, dpi = 300)

p_rf_imp <- rf_imp |>
  slice_max(importance, n = 20) |>
  mutate(feature = stats::reorder(feature, importance)) |>
  ggplot(aes(feature, importance)) +
  geom_col(fill = PAL_MAIN) +
  coord_flip() +
  labs(
    title = "Importancia de variables - Random Forest",
    subtitle = "Importancia por permutacion",
    x = NULL, y = "Importancia"
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "07_rf_importance.png"), p_rf_imp,
       width = 10, height = 6, dpi = 300)

p_xgb_imp <- xgb_imp |>
  head(20) |>
  mutate(Feature = stats::reorder(Feature, Gain)) |>
  ggplot(aes(Feature, Gain)) +
  geom_col(fill = PAL_ACCENT) +
  coord_flip() +
  labs(
    title = "Importancia de variables - XGBoost",
    subtitle = "Top 20 features por Gain",
    x = NULL, y = "Gain"
  ) +
  theme_goiener()
ggsave(path(FIG_DIR, "07_xgb_importance.png"), p_xgb_imp,
       width = 10, height = 6, dpi = 300)

tryCatch({
  png(path(FIG_DIR, "07_xgb_shap_top.png"),
      width = 2400, height = 1600, res = 240)
  suppressWarnings(
    xgboost::xgb.plot.shap(
      data = as.matrix(xgb_fit$test_x),
      model = xgb_fit$model,
      top_n = 9,
      n_col = 3,
      pch = 16,
      cex = 0.6
    )
  )
  dev.off()
}, error = function(e) {
  try(dev.off(), silent = TRUE)
  message("  SHAP omitido: ", conditionMessage(e))
})

# ==============================================================================
# 8. Rolling-origin CV one-day-ahead
# ==============================================================================
message("\n[8/9] Rolling-origin CV one-day-ahead...")

cv_starts <- seq.Date(VAL_START, TEST_END - CV_HORIZON, by = CV_HORIZON)
cv_starts <- head(cv_starts, CV_FOLDS)

cv_metrics <- list()
cv_prob_metrics <- list()
for (i in seq_along(cv_starts)) {
  fold_start <- cv_starts[i]
  fold_end <- min(fold_start + CV_HORIZON - 1, TEST_END)

  cv_train <- daily_model |> filter(date < fold_start)
  cv_test <- daily_model |> filter(date >= fold_start & date <= fold_end)
  if (nrow(cv_train) < 200 || nrow(cv_test) < 5) next

  mats <- prep_feature_matrices(cv_train, cv_test, feature_names)
  set.seed(SEED)
  cv_fit <- xgboost::xgb.train(
    params = xgb_fit$params,
    data = xgb.DMatrix(as.matrix(mats$train_x),
                       label = cv_train$mean_user_kWh),
    nrounds = xgb_fit$best_iter,
    verbose = 0
  )
  cv_pred_mean <- pmax(0, as.numeric(predict(
    cv_fit, xgb.DMatrix(as.matrix(mats$pred_x))
  )))
  cv_pred_total <- cv_pred_mean * cv_test$n_users
  cv_naive_total <- cv_test$lag_7 * cv_test$n_users

  cv_cal_start <- fold_start - 90
  cv_cal <- cv_train |> filter(date >= cv_cal_start)
  cv_fit_train <- cv_train |> filter(date < cv_cal_start)
  if (nrow(cv_fit_train) >= 200 && nrow(cv_cal) >= 30) {
    cal_mats <- prep_feature_matrices(cv_fit_train, cv_cal, feature_names)
    test_mats <- prep_feature_matrices(cv_fit_train, cv_test, feature_names)
    set.seed(SEED)
    cv_cal_fit <- xgboost::xgb.train(
      params = xgb_fit$params,
      data = xgb.DMatrix(as.matrix(cal_mats$train_x),
                         label = cv_fit_train$mean_user_kWh),
      nrounds = xgb_fit$best_iter,
      verbose = 0
    )
    cv_cal_pred_mean <- pmax(0, as.numeric(predict(
      cv_cal_fit, xgb.DMatrix(as.matrix(cal_mats$pred_x))
    )))
    cv_test_pred_mean_for_interval <- pmax(0, as.numeric(predict(
      cv_cal_fit, xgb.DMatrix(as.matrix(test_mats$pred_x))
    )))
    qhat_cv <- conformal_q(abs(cv_cal$mean_user_kWh - cv_cal_pred_mean),
                           ALPHA_INTERVAL)
    cv_lower <- pmax(0, (cv_test_pred_mean_for_interval - qhat_cv) * cv_test$n_users)
    cv_upper <- (cv_test_pred_mean_for_interval + qhat_cv) * cv_test$n_users

    cv_prob_metrics[[i]] <- interval_score_row(
      scope = sprintf("rolling_fold_%d_%s_%s", i, fold_start, fold_end),
      actual = cv_test$total_kWh,
      lower = cv_lower,
      upper = cv_upper,
      calibration_source = "rolling_origin_last_90_days"
    )
  }

  cv_metrics[[i]] <- metric_row(
    model_key = sprintf("fold_%d", i),
    model_label = "XGBoost",
    target = "mean_user_kWh_reconstructed",
    actual = cv_test$total_kWh,
    pred = cv_pred_total,
    naive_pred = cv_naive_total
  ) |>
    mutate(
      fold = i,
      fold_start = fold_start,
      fold_end = fold_end,
      n_days = nrow(cv_test),
      .before = model_key
    )

  message(sprintf(
    "  Fold %d (%s -> %s): MAE=%.1f  WAPE=%.2f%%",
    i, fold_start, fold_end,
    cv_metrics[[i]]$MAE, cv_metrics[[i]]$WAPE
  ))
}

cv_df <- bind_rows(cv_metrics)
write.csv(cv_df, path(TABLE_DIR, "forecast_cv_metrics.csv"), row.names = FALSE)

cv_prob_df <- bind_rows(cv_prob_metrics)
if (nrow(cv_prob_df) > 0) {
  prob_metrics <- bind_rows(prob_metrics, cv_prob_df)
  write.csv(prob_metrics, path(TABLE_DIR, "forecast_probabilistic_metrics.csv"),
            row.names = FALSE)
}

# ==============================================================================
# 9. Forecasting bottom-up por cluster
# ==============================================================================
message("\n[9/9] Forecasting bottom-up por cluster...")

if (!file_exists(USER_CLUSTERS_PARQUET)) {
  message("  No existe user_clusters.parquet. Se omite bottom-up.")
} else if (!has_climate) {
  message("  Bottom-up requiere daily_with_climate.parquet. Se omite.")
} else {
  con <- connect_duckdb()
  source_abs <- path_abs(DAILY_WITH_CLIMATE) |> path_norm()
  clusters_abs <- path_abs(USER_CLUSTERS_PARQUET) |> path_norm()

  cluster_daily <- dbGetQuery(con, glue("
    WITH cl AS (
      SELECT user_id, cluster, cluster_label
      FROM read_parquet('{clusters_abs}')
      WHERE cluster IS NOT NULL
    )
    SELECT
      cl.cluster,
      MIN(cl.cluster_label) AS cluster_label,
      d.date,
      SUM(d.daily_kWh) AS total_kWh,
      COUNT(DISTINCT d.user_id) AS n_users,
      AVG(d.daily_kWh) AS mean_user_kWh,
      AVG(d.tmed) AS tmed,
      AVG(d.hdd) AS hdd,
      AVG(d.cdd) AS cdd,
      AVG(d.prec) AS prec,
      AVG(d.daylight_hours) AS daylight_hours,
      MAX(CASE WHEN d.is_weekend THEN 1 ELSE 0 END) AS is_weekend,
      MAX(CASE WHEN d.is_holiday THEN 1 ELSE 0 END) AS is_holiday,
      MAX(CASE WHEN d.is_bridge_day THEN 1 ELSE 0 END) AS is_bridge_day,
      MAX(CASE WHEN d.is_easter_window THEN 1 ELSE 0 END) AS is_easter_window,
      MAX(d.days_to_holiday) AS days_to_holiday,
      MAX(d.dow) AS dow,
      MAX(d.month) AS month,
      MAX(d.week_of_year) AS week_of_year,
      MIN(d.season) AS season
    FROM read_parquet('{source_abs}') d
    INNER JOIN cl ON cl.user_id = d.user_id
    WHERE {focus_filter_sql_d}
    GROUP BY cl.cluster, d.date
    ORDER BY cl.cluster, d.date
  "))
  DBI::dbDisconnect(con, shutdown = TRUE)

  if (nrow(cluster_daily) == 0) {
    message("  Sin datos por cluster. Se omite bottom-up.")
  } else {
    cluster_daily$date <- as.Date(cluster_daily$date)

    bottomup_scope_daily <- cluster_daily |>
      group_by(date) |>
      summarise(
        clustered_total_kWh = sum(total_kWh, na.rm = TRUE),
        clustered_n_users = sum(n_users, na.rm = TRUE),
        n_clusters = n_distinct(cluster),
        .groups = "drop"
      ) |>
      inner_join(
        daily_agg |>
          select(date, all_total_kWh = total_kWh, all_n_users = n_users),
        by = "date"
      ) |>
      mutate(
        pct_kWh_clustered = 100 * clustered_total_kWh / all_total_kWh,
        pct_daily_users_clustered = 100 * clustered_n_users / all_n_users
      )

    scope_row <- function(df, period_label) {
      tibble::tibble(
        period = period_label,
        start_date = min(df$date),
        end_date = max(df$date),
        n_days = nrow(df),
        avg_daily_users_all = round(mean(df$all_n_users, na.rm = TRUE), 1),
        avg_daily_users_clustered = round(mean(df$clustered_n_users, na.rm = TRUE), 1),
        pct_avg_daily_users_clustered = round(mean(df$pct_daily_users_clustered, na.rm = TRUE), 2),
        total_kWh_all = round(sum(df$all_total_kWh, na.rm = TRUE), 1),
        total_kWh_clustered = round(sum(df$clustered_total_kWh, na.rm = TRUE), 1),
        pct_kWh_clustered = round(100 * total_kWh_clustered / total_kWh_all, 2),
        note = "El bottom-up se evalua solo sobre usuarios clusterizados; no representa todo el alcance top-5."
      )
    }

    bottomup_scope <- bind_rows(
      scope_row(bottomup_scope_daily, "all_available_dates"),
      scope_row(bottomup_scope_daily |>
                  filter(date >= VAL_START & date <= VAL_END),
                "validation_2022"),
      scope_row(bottomup_scope_daily |>
                  filter(date >= TEST_START & date <= TEST_END),
                "test_2023")
    )

    write.csv(bottomup_scope,
              path(TABLE_DIR, "forecast_bottomup_scope.csv"),
              row.names = FALSE)
    test_scope <- bottomup_scope |> filter(period == "test_2023") |> slice(1)
    if (nrow(test_scope) == 1) {
      message(sprintf(
        "  Bottom-up test: usuarios clusterizados %.1f%% diarios medios; kWh clusterizado %.1f%% del alcance top-5.",
        test_scope$pct_avg_daily_users_clustered,
        test_scope$pct_kWh_clustered
      ))
    }

    cluster_models <- list()
    cluster_preds <- list()
    cluster_interval_calibrations <- list()

    for (cl_id in sort(unique(cluster_daily$cluster))) {
      df_cl <- cluster_daily |>
        filter(cluster == cl_id) |>
        add_leak_free_features("mean_user_kWh")

      cl_splits <- split_temporal(df_cl)
      tr <- cl_splits$train
      va <- cl_splits$val
      te <- cl_splits$test
      if (nrow(tr) < 200 || nrow(va) < 30 || nrow(te) < 5) next

      feat_cl <- feature_names_for(df_cl)
      fit_cl <- fit_xgb_with_validation(
        tr, va, bind_rows(tr, va), te, feat_cl, "mean_user_kWh"
      )
      val_cal <- va |>
        mutate(
          pred_mean_user_kWh = fit_cl$val_pred,
          abs_residual = abs(mean_user_kWh - pred_mean_user_kWh)
        )

      make_calibration_row <- function(cal_df, calibration_group, season_value = NA_character_) {
        qhat_raw <- conformal_q(cal_df$abs_residual, ALPHA_INTERVAL)
        min_safety_factor <- cluster_interval_min_factor(cl_id)
        safety_factor <- learn_interval_safety_factor(
          cal_df$abs_residual,
          qhat_raw,
          min_factor = min_safety_factor,
          max_factor = max(2.50, min_safety_factor)
        )
        qhat <- qhat_raw * safety_factor
        coverage_val <- mean(cal_df$abs_residual <= qhat, na.rm = TRUE)
        tibble::tibble(
          cluster = cl_id,
          cluster_label = unique(df_cl$cluster_label)[1],
          calibration_group = calibration_group,
          season = season_value,
          n_calibration = nrow(cal_df),
          qhat = qhat,
          qhat_raw = qhat_raw,
          safety_factor = safety_factor,
          coverage_val = round(100 * coverage_val, 2)
        )
      }

      cluster_calibration <- make_calibration_row(val_cal, "cluster", NA_character_)
      season_calibration <- bind_rows(lapply(split(val_cal, val_cal$season), function(season_df) {
        season_value <- unique(season_df$season)[1]
        if (nrow(season_df) < 60) return(NULL)
        make_calibration_row(
          season_df,
          paste0("cluster_season_", season_value),
          as.character(season_value)
        )
      }))

      calibration_rows <- bind_rows(cluster_calibration, season_calibration)

      te <- te |>
        left_join(
          season_calibration |>
            select(season, qhat_mean_user_kWh_season = qhat,
                   n_calibration_season = n_calibration,
                   safety_factor_season = safety_factor),
          by = "season"
        ) |>
        mutate(
          qhat_mean_user_kWh_cluster = cluster_calibration$qhat[1],
          n_calibration_cluster = cluster_calibration$n_calibration[1],
          safety_factor_cluster = cluster_calibration$safety_factor[1],
          qhat_mean_user_kWh = if_else(is.finite(qhat_mean_user_kWh_season),
                                       pmax(qhat_mean_user_kWh_cluster,
                                            qhat_mean_user_kWh_season),
                                       qhat_mean_user_kWh_cluster),
          pred_mean_user_kWh = fit_cl$test_pred,
          pred = pred_mean_user_kWh * n_users,
          pred_lower_90 = pmax(0, (pred_mean_user_kWh - qhat_mean_user_kWh) * n_users),
          pred_upper_90 = (pred_mean_user_kWh + qhat_mean_user_kWh) * n_users
        )

      test_cluster_coverage <- mean(
        te$total_kWh >= pmax(0, (te$pred_mean_user_kWh - cluster_calibration$qhat[1]) * te$n_users) &
          te$total_kWh <= (te$pred_mean_user_kWh + cluster_calibration$qhat[1]) * te$n_users,
        na.rm = TRUE
      )
      cluster_calibration$coverage_test <- round(100 * test_cluster_coverage, 2)
      cluster_calibration$n_test <- nrow(te)

      if (nrow(season_calibration) > 0) {
        season_test_coverage <- te |>
          inner_join(season_calibration |> select(season, qhat), by = "season") |>
          group_by(season) |>
          summarise(
            n_test = n(),
            coverage_test = round(100 * mean(
              total_kWh >= pmax(0, (pred_mean_user_kWh - qhat) * n_users) &
                total_kWh <= (pred_mean_user_kWh + qhat) * n_users,
              na.rm = TRUE
            ), 2),
            .groups = "drop"
          )
        season_calibration <- season_calibration |>
          left_join(season_test_coverage, by = "season")
      } else {
        season_calibration$n_test <- integer(0)
        season_calibration$coverage_test <- numeric(0)
      }

      calibration_rows <- bind_rows(cluster_calibration, season_calibration)
      cluster_models[[as.character(cl_id)]] <- fit_cl$model
      if (!exists("cluster_interval_calibrations")) {
        cluster_interval_calibrations <- list()
      }
      cluster_interval_calibrations[[as.character(cl_id)]] <- calibration_rows
      cluster_preds[[as.character(cl_id)]] <- te |>
        select(cluster, cluster_label, date, n_users,
               season,
               actual = total_kWh,
               actual_mean_user_kWh = mean_user_kWh,
               pred_mean_user_kWh, pred,
               pred_lower_90, pred_upper_90, qhat_mean_user_kWh,
               qhat_mean_user_kWh_cluster, qhat_mean_user_kWh_season,
               n_calibration_cluster, n_calibration_season,
               safety_factor_cluster, safety_factor_season)
    }

    if (length(cluster_preds) > 0) {
      cluster_pred_df <- bind_rows(cluster_preds) |>
        mutate(month = format(date, "%Y-%m"))
      write.csv(cluster_pred_df,
                path(TABLE_DIR, "forecast_cluster_predictions.csv"),
                row.names = FALSE)

      cluster_interval_calibration_df <- bind_rows(cluster_interval_calibrations) |>
        mutate(across(where(is.numeric), ~round(., 4)))
      write.csv(cluster_interval_calibration_df,
                path(TABLE_DIR, "forecast_cluster_interval_calibration.csv"),
                row.names = FALSE)

      cluster_metrics <- cluster_pred_df |>
        group_by(cluster, cluster_label) |>
        summarise(
          n_days = n(),
          MAE = round(calc_mae(actual, pred), 1),
          RMSE = round(calc_rmse(actual, pred), 1),
          MAPE = round(calc_mape(actual, pred), 2),
          WAPE = round(calc_wape(actual, pred), 2),
          SMAPE = round(calc_smape(actual, pred), 2),
          .groups = "drop"
        )
      write.csv(cluster_metrics,
                path(TABLE_DIR, "forecast_metrics_by_cluster.csv"),
                row.names = FALSE)
      print(cluster_metrics)

      cluster_prob_metrics <- bind_rows(
        cluster_pred_df |>
          group_by(cluster, cluster_label) |>
          group_modify(~ interval_score_row(
            scope = paste0("cluster_", .y$cluster, "_", .y$cluster_label),
            actual = .x$actual,
            lower = .x$pred_lower_90,
            upper = .x$pred_upper_90,
            calibration_source = "mondrian_cluster_or_cluster_season_2022"
          )) |>
          ungroup(),
        cluster_pred_df |>
          group_by(cluster, cluster_label, season) |>
          group_modify(~ interval_score_row(
            scope = paste0("cluster_", .y$cluster, "_season_", .y$season),
            actual = .x$actual,
            lower = .x$pred_lower_90,
            upper = .x$pred_upper_90,
            calibration_source = "mondrian_cluster_or_cluster_season_2022"
          )) |>
          ungroup(),
        cluster_pred_df |>
          group_by(cluster, cluster_label, month) |>
          group_modify(~ interval_score_row(
            scope = paste0("cluster_", .y$cluster, "_month_", .y$month),
            actual = .x$actual,
            lower = .x$pred_lower_90,
            upper = .x$pred_upper_90,
            calibration_source = "diagnostic_month_not_selection"
          )) |>
          ungroup()
      )

      write.csv(cluster_prob_metrics,
                path(TABLE_DIR, "forecast_cluster_probabilistic_metrics.csv"),
                row.names = FALSE)

      interval_alerts <- cluster_prob_metrics |>
        mutate(
          is_cluster_or_season = is.na(month),
          below_threshold = empirical_coverage < FORECAST_CLUSTER_MIN_COVERAGE,
          alert_level = case_when(
            below_threshold & is.na(season) & is.na(month) ~ "cluster_global",
            below_threshold & !is.na(season) ~ "cluster_season",
            below_threshold ~ "diagnostic_month",
            TRUE ~ "ok"
          )
        ) |>
        filter(below_threshold) |>
        arrange(is_cluster_or_season, empirical_coverage)

      write.csv(interval_alerts,
                path(TABLE_DIR, "forecast_interval_alerts.csv"),
                row.names = FALSE)

      c0_pred <- cluster_pred_df |> filter(cluster == 0)
      if (nrow(c0_pred) > 0) {
        c0_review_one <- function(df, segment, segment_value) {
          tibble::tibble(
            segment = segment,
            segment_value = as.character(segment_value),
            n_days = nrow(df),
            mean_actual_mean_user_kWh = round(mean(df$actual_mean_user_kWh, na.rm = TRUE), 4),
            sd_actual_mean_user_kWh = round(sd(df$actual_mean_user_kWh, na.rm = TRUE), 4),
            cv_actual_mean_user_kWh = round(
              sd(df$actual_mean_user_kWh, na.rm = TRUE) /
                mean(df$actual_mean_user_kWh, na.rm = TRUE),
              4
            ),
            MAE = round(calc_mae(df$actual, df$pred), 1),
            RMSE = round(calc_rmse(df$actual, df$pred), 1),
            WAPE = round(calc_wape(df$actual, df$pred), 2),
            coverage_90 = round(100 * mean(
              df$actual >= df$pred_lower_90 & df$actual <= df$pred_upper_90,
              na.rm = TRUE
            ), 2),
            mean_interval_width_kWh = round(mean(df$pred_upper_90 - df$pred_lower_90,
                                                na.rm = TRUE), 1)
          )
        }

        c0_review <- bind_rows(
          c0_review_one(c0_pred, "overall", "test_2023"),
          bind_rows(lapply(split(c0_pred, c0_pred$season), function(df) {
            c0_review_one(df, "season", unique(df$season))
          }))
        ) |>
          mutate(
            mean_user_kWh_index_vs_overall = round(
              mean_actual_mean_user_kWh /
                mean_actual_mean_user_kWh[segment == "overall"][1],
              3
            ),
            needs_special_treatment = coverage_90 < 90 |
              cv_actual_mean_user_kWh >= 0.15 |
              segment_value == "Verano",
            review_note = case_when(
              segment_value == "Verano" & coverage_90 < FORECAST_CLUSTER_MIN_COVERAGE ~
                "C0 muestra infracobertura clara en verano; mantener calibracion especial y revisar sesgo estacional.",
              segment == "overall" & coverage_90 < 90 ~
                "C0 queda por debajo del objetivo global de cobertura; tratar como segmento especial.",
              cv_actual_mean_user_kWh >= 0.15 ~
                "Variabilidad intra-segmento elevada; conviene seguimiento estacional.",
              TRUE ~ "Sin alerta fuerte, pero C0 se mantiene monitorizado por regla de negocio."
            )
          )

        write.csv(c0_review,
                  path(TABLE_DIR, "forecast_c0_special_segment_review.csv"),
                  row.names = FALSE)
      }

      prob_metrics <- bind_rows(prob_metrics, cluster_prob_metrics)
      write.csv(prob_metrics, path(TABLE_DIR, "forecast_probabilistic_metrics.csv"),
                row.names = FALSE)

      actual_resid <- cluster_pred_df |>
        group_by(date) |>
        summarise(
          actual_resid = sum(actual),
          pred_bottom_up = sum(pred),
          .groups = "drop"
        )

      hist_resid <- cluster_daily |>
        filter(date >= VAL_START, date < TEST_START) |>
        group_by(date) |>
        summarise(actual_resid = sum(total_kWh), .groups = "drop")

      hist_global <- daily_model |>
        filter(date >= VAL_START, date < TEST_START) |>
        select(date, total_kWh)

      scale_df <- hist_resid |>
        inner_join(hist_global, by = "date")
      scale_resid <- sum(scale_df$actual_resid, na.rm = TRUE) /
        sum(scale_df$total_kWh, na.rm = TRUE)

      compare <- test |>
        select(date, pred_xgb) |>
        inner_join(actual_resid, by = "date") |>
        mutate(pred_topdown_resid = pred_xgb * scale_resid)

      write.csv(compare,
                path(TABLE_DIR, "forecast_bottomup_predictions.csv"),
                row.names = FALSE)

      bu_metrics <- bind_rows(
        metric_row(
          "xgb_topdown_rescaled",
          sprintf("XGB top-down reescalado validacion x%.3f", scale_resid),
          "residential_clustered_total",
          compare$actual_resid,
          compare$pred_topdown_resid,
          compare$pred_topdown_resid
        ),
        metric_row(
          "xgb_bottomup_clusters",
          "XGB bottom-up suma clusters",
          "residential_clustered_total",
          compare$actual_resid,
          compare$pred_bottom_up,
          compare$pred_topdown_resid
        )
      ) |>
        select(-MASE)

      write.csv(bu_metrics,
                path(TABLE_DIR, "forecast_bottomup_vs_topdown.csv"),
                row.names = FALSE)
      print(bu_metrics)

      recon_cal_n <- min(90L, max(1L, floor(nrow(compare) / 3)))
      recon_cal <- compare[seq_len(recon_cal_n), ]
      recon_eval <- compare[-seq_len(recon_cal_n), ]
      if (nrow(recon_eval) == 0) recon_eval <- compare

      pred_wide <- cluster_pred_df |>
        mutate(cluster_col = paste0("pred_cluster_", cluster)) |>
        select(date, cluster_col, pred) |>
        pivot_wider(names_from = cluster_col, values_from = pred)

      actual_wide <- cluster_pred_df |>
        mutate(cluster_col = paste0("actual_cluster_", cluster)) |>
        select(date, cluster_col, actual) |>
        pivot_wider(names_from = cluster_col, values_from = actual)

      mint_df <- compare |>
        select(date, actual_resid, pred_topdown_resid, pred_bottom_up) |>
        inner_join(pred_wide, by = "date") |>
        inner_join(actual_wide, by = "date")

      pred_cluster_cols <- grep("^pred_cluster_", names(mint_df), value = TRUE)
      actual_cluster_cols <- sub("^pred_", "actual_", pred_cluster_cols)

      var_bu <- stats::var(recon_cal$actual_resid - recon_cal$pred_bottom_up,
                           na.rm = TRUE)
      var_td <- stats::var(recon_cal$actual_resid - recon_cal$pred_topdown_resid,
                           na.rm = TRUE)
      if (!is.finite(var_bu) || var_bu <= 0) var_bu <- 1
      if (!is.finite(var_td) || var_td <= 0) var_td <- 1
      inv_bu <- 1 / var_bu
      inv_td <- 1 / var_td
      w_bu <- inv_bu / (inv_bu + inv_td)
      w_td <- 1 - w_bu

      recon_eval <- recon_eval |>
        mutate(pred_reconciled = w_bu * pred_bottom_up + w_td * pred_topdown_resid)

      mint_cal <- mint_df |> filter(date %in% recon_cal$date)
      mint_eval <- mint_df |> filter(date %in% recon_eval$date)

      mint_reconciled_top <- rep(NA_real_, nrow(mint_eval))
      reconciled_cluster_metrics <- tibble::tibble()

      if (length(pred_cluster_cols) >= 2 && nrow(mint_cal) >= 10 && nrow(mint_eval) > 0) {
        residual_matrix <- cbind(
          top = mint_cal$actual_resid - mint_cal$pred_topdown_resid,
          as.matrix(mint_cal[, actual_cluster_cols]) -
            as.matrix(mint_cal[, pred_cluster_cols])
        )
        residual_matrix <- residual_matrix[stats::complete.cases(residual_matrix), , drop = FALSE]

        if (nrow(residual_matrix) >= 5) {
          W <- stats::cov(residual_matrix)
          if (any(!is.finite(W))) {
            W <- diag(apply(residual_matrix, 2, stats::var, na.rm = TRUE))
          }
          diag_vals <- diag(W)
          diag_vals[!is.finite(diag_vals) | diag_vals <= 0] <- 1
          W <- 0.5 * W + 0.5 * diag(diag_vals, nrow = length(diag_vals))
          diag(W) <- diag(W) + mean(diag_vals, na.rm = TRUE) * 1e-6

          Winv <- tryCatch(solve(W), error = function(e) diag(1 / diag(W)))
          n_bottom <- length(pred_cluster_cols)
          S <- rbind(rep(1, n_bottom), diag(n_bottom))
          G <- tryCatch(
            solve(t(S) %*% Winv %*% S, t(S) %*% Winv),
            error = function(e) {
              message("  MinT omitido por matriz singular: ", conditionMessage(e))
              NULL
            }
          )

          if (!is.null(G)) {
            yhat_eval <- as.matrix(mint_eval[, c("pred_topdown_resid", pred_cluster_cols)])
            reconciled_nodes <- t(S %*% G %*% t(yhat_eval))
            colnames(reconciled_nodes) <- c("pred_mint_reconciled",
                                            sub("^pred_", "mint_", pred_cluster_cols))
            mint_reconciled_top <- reconciled_nodes[, "pred_mint_reconciled"]

            mint_clusters <- as.data.frame(reconciled_nodes[, -1, drop = FALSE]) |>
              mutate(date = mint_eval$date) |>
              pivot_longer(-date, names_to = "cluster_col", values_to = "pred_mint") |>
              mutate(cluster = sub("^mint_cluster_", "", cluster_col))

            actual_clusters_long <- mint_eval |>
              select(date, all_of(actual_cluster_cols)) |>
              pivot_longer(-date, names_to = "cluster_col", values_to = "actual") |>
              mutate(cluster = sub("^actual_cluster_", "", cluster_col))

            reconciled_cluster_metrics <- mint_clusters |>
              inner_join(actual_clusters_long, by = c("date", "cluster")) |>
              group_by(cluster) |>
              summarise(
                n_days = n(),
                MAE = round(calc_mae(actual, pred_mint), 1),
                RMSE = round(calc_rmse(actual, pred_mint), 1),
                MAPE = round(calc_mape(actual, pred_mint), 2),
                WAPE = round(calc_wape(actual, pred_mint), 2),
                SMAPE = round(calc_smape(actual, pred_mint), 2),
                .groups = "drop"
              ) |>
              left_join(
                cluster_pred_df |>
                  distinct(cluster, cluster_label) |>
                  mutate(cluster = as.character(cluster)),
                by = "cluster"
              ) |>
              relocate(cluster_label, .after = cluster)
          }
        }
      }

      recon_eval <- recon_eval |>
        mutate(pred_mint_reconciled = mint_reconciled_top)

      compare_export <- compare |>
        left_join(
          recon_eval |>
            select(date, pred_reconciled, pred_mint_reconciled),
          by = "date"
        )
      write.csv(compare_export,
                path(TABLE_DIR, "forecast_bottomup_predictions.csv"),
                row.names = FALSE)

      reconciliation_metrics <- bind_rows(
        metric_row(
          "topdown_rescaled",
          "Top-down reescalado",
          "residential_clustered_total",
          recon_eval$actual_resid,
          recon_eval$pred_topdown_resid,
          recon_eval$pred_topdown_resid
        ),
        metric_row(
          "bottomup_clusters",
          "Bottom-up suma clusters",
          "residential_clustered_total",
          recon_eval$actual_resid,
          recon_eval$pred_bottom_up,
          recon_eval$pred_topdown_resid
        ),
        metric_row(
          "variance_weighted_reconciled",
          sprintf("Reconciliado por varianza (BU %.2f / TD %.2f)", w_bu, w_td),
          "residential_clustered_total",
          recon_eval$actual_resid,
          recon_eval$pred_reconciled,
          recon_eval$pred_topdown_resid
        ),
        metric_row(
          "mint_shrink_reconciled",
          "MinT shrink jerarquico total=sum(clusters)",
          "residential_clustered_total",
          recon_eval$actual_resid,
          recon_eval$pred_mint_reconciled,
          recon_eval$pred_topdown_resid
        )
      ) |>
        filter(!(model_key == "mint_shrink_reconciled" & !is.finite(MAE))) |>
        mutate(
          calibration_days = nrow(recon_cal),
          evaluation_days = nrow(recon_eval),
          weight_bottomup = if_else(
            model_key == "variance_weighted_reconciled", round(w_bu, 3), NA_real_
          ),
          weight_topdown = if_else(
            model_key == "variance_weighted_reconciled", round(w_td, 3), NA_real_
          ),
          reconciliation_method = case_when(
            model_key == "mint_shrink_reconciled" ~ "MinT_shrink_covariance",
            model_key == "variance_weighted_reconciled" ~ "inverse_residual_variance",
            TRUE ~ "base_forecast"
          ),
          .before = MAE
        ) |>
        select(-MASE)

      write.csv(reconciliation_metrics,
                path(TABLE_DIR, "forecast_reconciliation_metrics.csv"),
                row.names = FALSE)
      print(reconciliation_metrics)

      if (nrow(reconciled_cluster_metrics) > 0) {
        write.csv(reconciled_cluster_metrics,
                  path(TABLE_DIR, "forecast_reconciled_cluster_metrics.csv"),
                  row.names = FALSE)
      }

      metrics_df <- bind_rows(
        metrics_df,
        bu_metrics |> mutate(MASE = NA_real_),
        reconciliation_metrics |>
          filter(model_key %in% c("variance_weighted_reconciled", "mint_shrink_reconciled")) |>
          select(model_key, modelo, target, MAE, RMSE, MAPE, WAPE, SMAPE) |>
          mutate(MASE = NA_real_)
      ) |>
        arrange(MAE)
      write.csv(metrics_df, path(TABLE_DIR, "forecast_metrics.csv"), row.names = FALSE)

      saveRDS(cluster_models, path(MODEL_DIR, "xgb_by_cluster.rds"))
    }
  }
}

write_model_evidence_summary <- function(metrics_df, interval_metrics) {
  rows <- list()
  add_row <- function(item, result, metric, value, reference_table) {
    rows[[length(rows) + 1]] <<- tibble::tibble(
      evidence_item = item,
      result = result,
      metric = metric,
      value = value,
      reference_table = reference_table
    )
  }

  primary <- metrics_df |>
    filter(target %in% c("mean_user_kWh_reconstructed",
                         "mean_user_kWh_reconstructed_ablation",
                         "total_kWh_direct"))
  if (nrow(primary) > 0) {
    best <- primary |> slice_min(WAPE, n = 1, with_ties = FALSE)
    add_row(
      "best_aggregate_model",
      best$modelo[1],
      "WAPE_test_pct",
      round(best$WAPE[1], 2),
      "forecast_metrics.csv"
    )
  }

  xgb_row <- metrics_df |> filter(model_key == "xgb") |> slice_head(n = 1)
  no_climate_row <- metrics_df |> filter(model_key == "xgb_no_climate") |> slice_head(n = 1)
  if (nrow(xgb_row) == 1 && nrow(no_climate_row) == 1) {
    add_row(
      "climate_ablation",
      "WAPE sin clima menos WAPE con clima; positivo favorece clima",
      "delta_WAPE_pct_points",
      round(no_climate_row$WAPE[1] - xgb_row$WAPE[1], 2),
      "forecast_metrics.csv"
    )
  }

  operational_row <- metrics_df |> filter(model_key == "xgb_operational") |> slice_head(n = 1)
  if (nrow(xgb_row) == 1 && nrow(operational_row) == 1) {
    add_row(
      "operational_weather_cost",
      "Diferencia WAPE entre XGBoost operativo y escenario ex-post",
      "delta_WAPE_pct_points",
      round(operational_row$WAPE[1] - xgb_row$WAPE[1], 2),
      "forecast_weather_mode_metrics.csv"
    )
  }

  recon_rows <- metrics_df |>
    filter(model_key %in% c("xgb_topdown_rescaled", "xgb_bottomup_clusters",
                            "topdown_rescaled", "bottomup_clusters",
                            "variance_weighted_reconciled", "mint_shrink_reconciled"))
  if (nrow(recon_rows) > 0) {
    best_recon <- recon_rows |> slice_min(WAPE, n = 1, with_ties = FALSE)
    add_row(
      "residential_reconciliation",
      best_recon$modelo[1],
      "WAPE_test_pct",
      round(best_recon$WAPE[1], 2),
      "forecast_reconciliation_metrics.csv"
    )
  }

  bottomup_scope_path <- path(TABLE_DIR, "forecast_bottomup_scope.csv")
  if (file_exists(bottomup_scope_path)) {
    bottomup_scope <- read.csv(bottomup_scope_path)
    test_scope <- bottomup_scope |>
      filter(period == "test_2023") |>
      slice_head(n = 1)
    if (nrow(test_scope) == 1) {
      add_row(
        "bottomup_scope",
        "Bottom-up predice solo usuarios clusterizados, no todo el alcance top-5",
        "pct_kWh_clustered_test",
        round(test_scope$pct_kWh_clustered[1], 2),
        "forecast_bottomup_scope.csv"
      )
    }
  }

  c0_review_path <- path(TABLE_DIR, "forecast_c0_special_segment_review.csv")
  if (file_exists(c0_review_path)) {
    c0_review <- read.csv(c0_review_path)
    worst_c0 <- c0_review |>
      filter(segment == "season") |>
      slice_min(coverage_90, n = 1, with_ties = FALSE)
    if (nrow(worst_c0) == 1) {
      add_row(
        "c0_special_segment",
        paste0("Peor cobertura C0 en ", worst_c0$segment_value[1],
               "; requiere seguimiento estacional"),
        "coverage_90_pct",
        round(worst_c0$coverage_90[1], 2),
        "forecast_c0_special_segment_review.csv"
      )
    }
  }

  if (file_exists(path(TABLE_DIR, "cluster_stability.csv"))) {
    stability <- read.csv(path(TABLE_DIR, "cluster_stability.csv"))
    if ("jaccard_mean" %in% names(stability)) {
      add_row(
        "cluster_stability",
        "Jaccard medio bootstrap etapa B",
        "mean_jaccard",
        round(mean(stability$jaccard_mean, na.rm = TRUE), 3),
        "cluster_stability.csv"
      )
    }
  }

  balance_path <- path(TABLE_DIR, "cluster_balance_diagnostics.csv")
  if (file_exists(balance_path)) {
    cluster_balance <- read.csv(balance_path)
    if (all(c("is_stage_b", "pct_stage_b") %in% names(cluster_balance))) {
      stage_b_balance <- cluster_balance |>
        filter(is_stage_b, !is.na(pct_stage_b))
      if (nrow(stage_b_balance) > 0) {
        add_row(
          "cluster_stage_b_balance",
          "Porcentaje maximo de un cluster dentro de etapa B",
          "max_pct_stage_b",
          round(max(stage_b_balance$pct_stage_b, na.rm = TRUE), 2),
          "cluster_balance_diagnostics.csv"
        )
      }
    }
  }

  if (file_exists(path(TABLE_DIR, "cluster_sensitivity.csv"))) {
    sensitivity <- read.csv(path(TABLE_DIR, "cluster_sensitivity.csv"))
    required_sensitivity_cols <- c("selected_current_pipeline", "preprocessing",
                                   "algo", "k", "silhouette_avg")
    if (all(required_sensitivity_cols %in% names(sensitivity))) {
      selected <- sensitivity |> filter(selected_current_pipeline) |> slice_head(n = 1)
    } else {
      selected <- tibble::tibble()
    }
    if (nrow(selected) == 1) {
      add_row(
        "cluster_sensitivity",
        sprintf("%s/%s k=%s", selected$preprocessing[1], selected$algo[1], selected$k[1]),
        "silhouette_avg",
        round(selected$silhouette_avg[1], 4),
        "cluster_sensitivity.csv"
      )
    }
  }

  add_row(
    "aggregate_interval_coverage",
    "Intervalo conformal XGBoost agregado",
    "empirical_coverage_pct",
    round(interval_metrics$empirical_coverage[1], 2),
    "forecast_interval_metrics.csv"
  )

  cluster_prob_path <- path(TABLE_DIR, "forecast_cluster_probabilistic_metrics.csv")
  if (file_exists(cluster_prob_path)) {
    cluster_prob <- read.csv(cluster_prob_path)
    if ("empirical_coverage" %in% names(cluster_prob) && nrow(cluster_prob) > 0) {
      coverage_scope <- cluster_prob
      if (all(c("season", "month") %in% names(cluster_prob))) {
        coverage_scope <- cluster_prob |>
          filter(is.na(season), is.na(month))
      }
      if (nrow(coverage_scope) == 0) coverage_scope <- cluster_prob
      min_cov <- min(coverage_scope$empirical_coverage, na.rm = TRUE)
      add_row(
        "cluster_interval_min_coverage",
        ifelse(min_cov < FORECAST_CLUSTER_MIN_COVERAGE,
               "Revisar clusters con baja cobertura global",
               "Cobertura minima global por cluster dentro del umbral"),
        "min_empirical_coverage_pct",
        round(min_cov, 2),
        "forecast_cluster_probabilistic_metrics.csv"
      )
    }
  }

  evidence <- bind_rows(rows)
  write.csv(evidence, path(TABLE_DIR, "model_evidence_summary.csv"),
            row.names = FALSE)
  evidence
}

evidence_summary <- write_model_evidence_summary(metrics_df, interval_metrics)
message("\n  Evidencia para memoria:")
print(as.data.frame(evidence_summary))

elapsed_total <- (proc.time() - t0_total)["elapsed"]
message(sprintf("\nPaso 07 completado en %.1f s.", elapsed_total))

best_primary <- metrics_df |>
  filter(target %in% c("mean_user_kWh_reconstructed",
                       "mean_user_kWh_reconstructed_ablation",
                       "total_kWh_direct")) |>
  slice_min(MAE, n = 1, with_ties = FALSE)
if (nrow(best_primary) == 1) {
  message(sprintf("Mejor modelo agregado comparable: %s (MAE = %.1f kWh)",
                  best_primary$modelo[1], best_primary$MAE[1]))
}
