#!/usr/bin/env Rscript
# ==============================================================================
# R/05_clustering/05b_matrix.R
#
# Construye la matriz de features (forma horaria + ratios + clima) y proyecciones
# PCA / FPCA. Guarda intermedios para 06c.
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 06b: Matriz de features")
t0 <- proc.time(); set.seed(SEED)

pool <- read_parquet_safe(path(FEATURES_DIR, "cluster_pool.parquet"), "pool")

# Robust scaling helpers
winsorize <- function(x, lo = 0.01, hi = 0.99) {
  q <- quantile(x, c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, q[[1]]), q[[2]])
}
robust_scale <- function(x) {
  med <- median(x, na.rm = TRUE)
  s <- mad(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) s <- 1
  (x - med) / s
}

prep <- function(df, cols) {
  m <- as.data.frame(df[, cols, drop = FALSE])
  for (nm in names(m)) {
    v <- as.numeric(m[[nm]])
    med <- median(v, na.rm = TRUE); if (!is.finite(med)) med <- 0
    v[is.na(v)] <- med
    m[[nm]] <- robust_scale(winsorize(v))
  }
  as.matrix(m)
}

# Feature blocks
hour_cols <- grep("^norm_h\\d{2}$", names(pool), value = TRUE)
shape_cols <- intersect(c(
  "ratio_night_day", "ratio_weekend_weekday",
  "seasonal_amplitude_norm", "monthly_entropy",
  "summer_winter_ratio", "peak_share", "flat_share", "valley_share",
  "peak_to_valley_ratio"
), names(pool))
climate_cols <- intersect(c("beta_hdd_norm", "beta_cdd_norm", "r2_joint"),
                          names(pool))

feature_cols <- c(hour_cols, shape_cols, climate_cols)
X <- prep(pool, feature_cols)
rownames(X) <- pool$user_id

# Ponderacion por bloques: la forma horaria es lo que define al hogar,
# pero queremos que la estacionalidad y la sensibilidad climatica
# tengan peso suficiente para separar segmentos socioeconomicos.
W_HOUR <- 1.0
W_SHAPE <- 1.6
W_CLIMATE <- 2.0
weights <- c(rep(W_HOUR, length(hour_cols)),
             rep(W_SHAPE, length(shape_cols)),
             rep(W_CLIMATE, length(climate_cols)))
X <- sweep(X, 2, weights, `*`)

message(sprintf("  Matriz: %s x %d (hour=%d, shape=%d, climate=%d)",
                fmt_int(nrow(X)), ncol(X),
                length(hour_cols), length(shape_cols), length(climate_cols)))

# PCA
pc <- prcomp(X, scale. = FALSE)
varexp <- (pc$sdev^2) / sum(pc$sdev^2)
keep_pcs <- max(6L, which(cumsum(varexp) >= 0.85)[1])
keep_pcs <- min(keep_pcs, ncol(pc$x), 15L)
X_pca <- pc$x[, 1:keep_pcs, drop = FALSE]
message(sprintf("  PCA: %d componentes (var acumulada=%.1f%%)",
                keep_pcs, 100 * sum(varexp[1:keep_pcs])))

# FPCA over the 24-hour normalised curves (simple basis-free SVD on hour cols)
if (length(hour_cols) == 24L) {
  H <- as.matrix(pool[, hour_cols])
  H[is.na(H)] <- 0
  H_c <- scale(H, center = TRUE, scale = FALSE)
  fsvd <- svd(H_c, nu = 0, nv = 4)
  X_fpca <- H_c %*% fsvd$v
  colnames(X_fpca) <- paste0("FPC", 1:4)
} else {
  X_fpca <- X_pca[, 1:min(4, ncol(X_pca)), drop = FALSE]
  colnames(X_fpca) <- paste0("FPC", seq_len(ncol(X_fpca)))
}

# Persist artefacts as RDS (small) for 06c.
saveRDS(list(
  user_id = pool$user_id,
  feature_cols = feature_cols,
  X = X, X_pca = X_pca, X_fpca = X_fpca,
  pca = pc, varexp = varexp
), path(MODEL_DIR, "cluster_matrix.rds"))

message(sprintf("06b en %.1f s", (proc.time() - t0)[["elapsed"]]))
