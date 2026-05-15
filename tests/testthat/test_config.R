test_that("configuracion temporal y de clustering es coherente", {
  expect_lt(TRAIN_END, VAL_START)
  expect_lt(VAL_END, TEST_START)
  expect_lte(YEAR_MIN, YEAR_MAX)
  expect_true(all(CLUSTER_K_RANGE >= 2))
  expect_gt(CLUSTER_MIN_PCT_PER_CLUSTER, 0)
  expect_lte(CLUSTER_MIN_PCT_PER_CLUSTER, CLUSTER_MAX_PCT_PER_CLUSTER)
  expect_equal(sum(CLUSTER_SCORE_WEIGHTS), 1, tolerance = 1e-8)
})

test_that("rutas principales existen", {
  expect_true(dir_exists(DATA_DIR))
  expect_true(dir_exists(PARQUET_DIR))
  expect_true(dir_exists(TABLE_DIR))
  expect_true(dir_exists(FIG_DIR))
})

