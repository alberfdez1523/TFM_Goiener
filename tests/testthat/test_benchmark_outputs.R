test_that("benchmark mantiene ventaja de DuckDB/Parquet en consulta compleja", {
  bench <- read_csv_if_exists(path(TABLE_DIR, "benchmark_results.csv"))
  complex_rows <- bench[grepl("Consulta compleja", bench$experimento), , drop = FALSE]
  skip_if(nrow(complex_rows) == 0, "No hay filas de query compleja en benchmark")
  raw <- complex_rows[grepl("horario bruto|raw", complex_rows$metodo, ignore.case = TRUE), , drop = FALSE]
  preagg <- complex_rows[grepl("pre-agreg", complex_rows$metodo, ignore.case = TRUE), , drop = FALSE]
  skip_if(nrow(raw) == 0 || nrow(preagg) == 0, "Faltan metodos raw/preagregado")
  expect_lt(min(preagg$mediana_s, na.rm = TRUE), min(raw$mediana_s, na.rm = TRUE))
})
