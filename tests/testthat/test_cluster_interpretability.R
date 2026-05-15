test_that("clustering mantiene balance y contribuciones interpretables", {
  balance <- read_csv_if_exists(path(TABLE_DIR, "cluster_balance_diagnostics.csv"))
  contribution <- read_csv_if_exists(path(TABLE_DIR, "cluster_feature_contribution.csv"))

  stage_b <- balance[isTRUE(balance$is_stage_b) | balance$is_stage_b == "TRUE", , drop = FALSE]
  stage_b <- stage_b[!is.na(stage_b$pct_stage_b), , drop = FALSE]

  expect_gt(nrow(stage_b), 0)
  expect_lt(max(stage_b$pct_stage_b, na.rm = TRUE), 50)
  expect_true(all(c("cluster", "feature", "standardized_difference", "rank_abs") %in%
                    names(contribution)))
  expect_gt(nrow(contribution), 0)
})

test_that("estabilidad bootstrap se mantiene en umbral defendible", {
  stability <- read_csv_if_exists(path(TABLE_DIR, "cluster_stability.csv"))
  expect_true("jaccard_mean" %in% names(stability))
  expect_gt(mean(stability$jaccard_mean, na.rm = TRUE), 0.85)
})

test_that("lectura CNAE y negocio cubre clusters asignados", {
  coverage <- read_csv_if_exists(path(TABLE_DIR, "cluster_cnae_coverage.csv"))
  distribution <- read_csv_if_exists(path(TABLE_DIR, "cluster_cnae_section_distribution.csv"))
  enrichment <- read_csv_if_exists(path(TABLE_DIR, "cluster_cnae_enrichment.csv"))
  division <- read_csv_if_exists(path(TABLE_DIR, "cluster_cnae_division_distribution.csv"))
  business <- read_csv_if_exists(path(TABLE_DIR, "cluster_business_interpretation.csv"))

  expect_gt(nrow(coverage), 0)
  expect_gt(nrow(distribution), 0)
  expect_gt(nrow(division), 0)
  expect_gt(nrow(business), 0)

  expect_true(all(c("cluster", "cluster_label", "n_users", "n_cnae_known",
                    "n_cnae_unknown", "coverage_pct", "top_cnae_section_label") %in%
                    names(coverage)))
  expect_true(all(c("cluster", "cnae_section", "cnae_section_label",
                    "n_users", "pct_cluster", "pct_global", "support_ok") %in%
                    names(distribution)))
  expect_true(all(c("cluster", "cnae_section_label", "n_users",
                    "enrichment_ratio", "is_interpretable") %in%
                    names(enrichment)))
  expect_true(all(c("cluster", "business_question", "behavioral_signal",
                    "cnae_signal", "goiener_action", "caveat") %in%
                    names(business)))

  expect_equal(coverage$n_users, coverage$n_cnae_known + coverage$n_cnae_unknown)
  expect_equal(sort(unique(business$cluster)), sort(unique(coverage$cluster)))
  expect_equal(nrow(business), length(unique(coverage$cluster)))

  distribution_sum <- aggregate(pct_cluster ~ cluster, distribution, sum)
  expect_true(all(abs(distribution_sum$pct_cluster - 100) < 0.2))

  interpreted <- enrichment[as.logical(enrichment$is_interpretable), , drop = FALSE]
  if (nrow(interpreted) > 0) {
    expect_true(all(interpreted$n_users >= CLUSTER_CNAE_MIN_N))
  }
})

test_that("preguntas empresariales respaldadas por referencias cubren clusters", {
  catalog <- read_csv_if_exists(path(TABLE_DIR, "cluster_business_question_catalog.csv"))
  assessment <- read_csv_if_exists(path(TABLE_DIR, "cluster_business_question_assessment.csv"))
  coverage <- read_csv_if_exists(path(TABLE_DIR, "cluster_cnae_coverage.csv"))
  references <- read.csv(REFERENCE_MATRIX_CSV, stringsAsFactors = FALSE,
                         check.names = FALSE)

  expect_gte(nrow(catalog), 6)
  expect_gt(nrow(assessment), 0)

  expect_true(all(c("question_id", "business_question", "reference_theme",
                    "reference_rows", "reference_basis", "repo_evidence",
                    "conclusion_rule", "decision_scope", "caveat") %in%
                    names(catalog)))
  expect_true(all(c("cluster", "cluster_label", "question_id",
                    "evidence_strength", "cluster_evidence",
                    "recommended_conclusion", "question_caveat") %in%
                    names(assessment)))

  cited_references <- unique(trimws(unlist(strsplit(
    paste(catalog$reference_rows, collapse = ";"),
    ";"
  ), use.names = FALSE)))
  cited_references <- cited_references[nzchar(cited_references)]

  expect_true(all(cited_references %in% references$referencia))
  expect_false(any(is.na(catalog$caveat) | catalog$caveat == ""))
  expect_false(any(is.na(catalog$repo_evidence) | catalog$repo_evidence == ""))
  expect_false(any(is.na(catalog$reference_basis) | catalog$reference_basis == ""))

  expect_equal(sort(unique(assessment$cluster)), sort(unique(coverage$cluster)))
  expect_true(all(assessment$question_id %in% catalog$question_id))

  expected_questions_per_cluster <- nrow(catalog)
  questions_per_cluster <- as.integer(table(assessment$cluster))
  expect_true(all(questions_per_cluster == expected_questions_per_cluster))

  primary_flag <- as.logical(assessment$is_primary_question)
  primary_flag[is.na(primary_flag)] <- FALSE
  expect_gt(sum(primary_flag), 0)
  expect_true(all(assessment$evidence_strength %in% c("Alta", "Media", "Contextual")))
  expect_false(any(is.na(assessment$cluster_evidence) |
                     assessment$cluster_evidence == ""))
})
