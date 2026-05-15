#!/usr/bin/env Rscript
# ==============================================================================
# R/05_clustering/05c_models.R
#
# Entrena seis familias de clustering sobre la matriz de 06b. Cada modelo
# devuelve un vector de etiquetas para todos los usuarios del pool.
# Guarda outputs/models/cluster_models.rds con las etiquetas crudas.
# ==============================================================================

suppressPackageStartupMessages({
  library(cluster); library(fs); library(here)
})

source(here::here("_config.R"))
source(here::here("R", "_lib", "io.R"))

log_section("PASO 06c: Entrenando algoritmos")
t0 <- proc.time(); set.seed(SEED)

art <- readRDS(path(MODEL_DIR, "cluster_matrix.rds"))
X     <- art$X
X_pca <- art$X_pca
X_fpca <- art$X_fpca

# Use PCA-reduced matrix for KMeans / Ward / GMM / HDBSCAN to control noise.
results <- list()

fit_kmeans <- function(M, k) {
  set.seed(SEED + k)
  kmeans(M, centers = k, nstart = CLUSTER_KMEANS_NSTART, iter.max = 100L)$cluster
}

message("[1/6] K-Means sobre PCA...")
for (k in CLUSTER_K_RANGE_SEARCH) {
  results[[sprintf("kmeans_pca_k%d", k)]] <- list(
    algo = "kmeans_pca", k = k,
    labels = fit_kmeans(X_pca, k)
  )
}

message("[2/6] PAM/CLARA sobre PCA (Manhattan)...")
for (k in CLUSTER_K_RANGE_SEARCH) {
  set.seed(SEED + 100 + k)
  fit <- if (nrow(X_pca) > 8000) {
    clara(X_pca, k = k, sampsize = min(4000, nrow(X_pca)),
          samples = 10, pamLike = TRUE, metric = "manhattan")
  } else {
    pam(X_pca, k = k, metric = "manhattan")
  }
  results[[sprintf("pam_pca_k%d", k)]] <- list(
    algo = "pam_pca", k = k, labels = fit$clustering
  )
}

message("[3/6] Ward (jerarquico) sobre muestra...")
sample_n <- min(6000, nrow(X_pca))
set.seed(SEED + 200)
samp_idx <- sample(seq_len(nrow(X_pca)), sample_n)
d_samp <- dist(X_pca[samp_idx, , drop = FALSE])
hc <- hclust(d_samp, method = "ward.D2")
for (k in CLUSTER_K_RANGE_SEARCH) {
  cl_samp <- cutree(hc, k = k)
  # Assign rest by nearest centroid of sample clusters.
  centroids <- t(sapply(seq_len(k), function(c) {
    colMeans(X_pca[samp_idx[cl_samp == c], , drop = FALSE])
  }))
  all_labels <- integer(nrow(X_pca))
  all_labels[samp_idx] <- cl_samp
  rest <- setdiff(seq_len(nrow(X_pca)), samp_idx)
  # nearest centroid
  d_mat <- as.matrix(stats::dist(rbind(centroids, X_pca[rest, , drop = FALSE])))
  d_mat <- d_mat[(k + 1):nrow(d_mat), 1:k, drop = FALSE]
  all_labels[rest] <- apply(d_mat, 1, which.min)
  results[[sprintf("ward_pca_k%d", k)]] <- list(
    algo = "ward_pca", k = k, labels = all_labels
  )
}

message("[4/6] GMM (mclust) sobre PCA reducido...")
if (requireNamespace("mclust", quietly = TRUE)) {
  for (k in CLUSTER_K_RANGE_SEARCH) {
    set.seed(SEED + 300 + k)
    fit <- tryCatch(
      mclust::Mclust(X_pca[samp_idx, , drop = FALSE], G = k,
                     modelNames = "VVI", verbose = FALSE),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    # Classify the rest by predict.
    pred_all <- tryCatch(
      mclust::predict.Mclust(fit, X_pca)$classification,
      error = function(e) NULL
    )
    if (is.null(pred_all)) next
    results[[sprintf("gmm_k%d", k)]] <- list(
      algo = "gmm", k = k, labels = as.integer(pred_all)
    )
  }
} else {
  message("  mclust no disponible; saltando GMM.")
}

message("[5/6] HDBSCAN sobre PCA...")
if (requireNamespace("dbscan", quietly = TRUE)) {
  for (mp in CLUSTER_HDBSCAN_MINPTS) {
    fit <- tryCatch(dbscan::hdbscan(X_pca, minPts = mp),
                    error = function(e) NULL)
    if (is.null(fit)) next
    lbl <- as.integer(fit$cluster)
    # 0 = noise; map to -1 sentinel
    k_eff <- length(unique(lbl[lbl > 0]))
    if (k_eff < 2L) next
    results[[sprintf("hdbscan_mp%d", mp)]] <- list(
      algo = "hdbscan", k = k_eff, labels = lbl, noise_pct = 100 * mean(lbl == 0)
    )
  }
} else {
  message("  dbscan no disponible; saltando HDBSCAN.")
}

message("[6/6] FPCA + K-Means sobre curva horaria...")
for (k in CLUSTER_K_RANGE_SEARCH) {
  set.seed(SEED + 500 + k)
  results[[sprintf("fpca_kmeans_k%d", k)]] <- list(
    algo = "fpca_kmeans", k = k,
    labels = kmeans(X_fpca, centers = k, nstart = CLUSTER_KMEANS_NSTART,
                    iter.max = 100L)$cluster
  )
}

saveRDS(results, path(MODEL_DIR, "cluster_models.rds"))
message(sprintf("  Modelos guardados: %d soluciones", length(results)))
message(sprintf("06c en %.1f s", (proc.time() - t0)[["elapsed"]]))
