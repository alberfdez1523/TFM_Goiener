# R/_lib/forecast_metrics.R - Compact metrics for daily/hourly forecasts.

mae <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
mape <- function(y, yhat) {
  ok <- !is.na(y) & !is.na(yhat) & y != 0
  100 * mean(abs((y[ok] - yhat[ok]) / y[ok]))
}
smape <- function(y, yhat) {
  ok <- !is.na(y) & !is.na(yhat)
  denom <- (abs(y[ok]) + abs(yhat[ok])) / 2
  ok2 <- denom > 0
  100 * mean(abs(y[ok][ok2] - yhat[ok][ok2]) / denom[ok2])
}
wape <- function(y, yhat) {
  ok <- !is.na(y) & !is.na(yhat)
  100 * sum(abs(y[ok] - yhat[ok])) / sum(abs(y[ok]))
}
mase <- function(y, yhat, y_train, m = 1L) {
  ok <- !is.na(y) & !is.na(yhat)
  denom <- mean(abs(diff(y_train, lag = m)), na.rm = TRUE)
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  mean(abs(y[ok] - yhat[ok])) / denom
}

# Pinball loss for quantile q.
pinball_loss <- function(y, q_pred, q) {
  ok <- !is.na(y) & !is.na(q_pred)
  diff <- y[ok] - q_pred[ok]
  mean(pmax(q * diff, (q - 1) * diff))
}

winkler_score <- function(y, lo, hi, alpha = 0.10) {
  ok <- !is.na(y) & !is.na(lo) & !is.na(hi)
  y <- y[ok]; lo <- lo[ok]; hi <- hi[ok]
  width <- hi - lo
  penalty <- ifelse(y < lo, 2/alpha * (lo - y),
                    ifelse(y > hi, 2/alpha * (y - hi), 0))
  mean(width + penalty)
}

empirical_coverage <- function(y, lo, hi) {
  ok <- !is.na(y) & !is.na(lo) & !is.na(hi)
  100 * mean(y[ok] >= lo[ok] & y[ok] <= hi[ok])
}

interval_width <- function(lo, hi) {
  ok <- !is.na(lo) & !is.na(hi)
  mean(hi[ok] - lo[ok])
}

# Business metric: euro deviation cost using OMIE average price.
# error_kwh is per period (hour or day); price is EUR/MWh.
eur_deviation_cost <- function(y, yhat, price_eur_mwh = NULL) {
  if (is.null(price_eur_mwh) && exists("OMIE_AVG_PRICE_EUR_MWH")) {
    price_eur_mwh <- OMIE_AVG_PRICE_EUR_MWH
  }
  if (is.null(price_eur_mwh)) price_eur_mwh <- 90
  ok <- !is.na(y) & !is.na(yhat)
  sum(abs(y[ok] - yhat[ok])) * price_eur_mwh / 1000
}

forecast_summary <- function(y, yhat, y_train = NULL, m = 1L, label = "model") {
  out <- data.frame(
    model = label,
    n = sum(!is.na(y) & !is.na(yhat)),
    MAE = mae(y, yhat),
    RMSE = rmse(y, yhat),
    MAPE = mape(y, yhat),
    sMAPE = smape(y, yhat),
    WAPE = wape(y, yhat),
    EUR_dev = eur_deviation_cost(y, yhat),
    stringsAsFactors = FALSE
  )
  if (!is.null(y_train)) out$MASE <- mase(y, yhat, y_train, m = m)
  out
}

# Split conformal residual quantile (symmetric).
conformal_quantile <- function(residuals_cal, alpha = 0.10) {
  q <- quantile(abs(residuals_cal), probs = 1 - alpha, na.rm = TRUE,
                type = 1)
  unname(q)
}
