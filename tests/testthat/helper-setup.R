# ── Test helpers: Eunomia synthetic OMOP CDM ─────────────────────────────────
#
# Uses a GiBleed DuckDB (Eunomia synthetic data) as the test database.
# The test DB is copied to a temp file so tests can write to it freely.

# Path to a known GiBleed DuckDB (bundled with CohortContrast or downloaded)
EUNOMIA_PATHS <- c(
  Sys.getenv("SYRONA_TEST_DB"),
  file.path(Sys.getenv("EUNOMIA_DATA_FOLDER"), "GiBleed_5.3_1.4.duckdb"),
  # Local development fallback
  normalizePath("../../CohortContrast-main/tests/testthat/Eunomia/GiBleed_5.3_1.4.duckdb",
                mustWork = FALSE),
  normalizePath("../../../CohortContrast-main/tests/testthat/Eunomia/GiBleed_5.3_1.4.duckdb",
                mustWork = FALSE)
)

find_eunomia <- function() {
  for (p in EUNOMIA_PATHS) {
    if (nchar(p) > 0 && file.exists(p)) return(p)
  }
  NULL
}

#' Get a writable test CDM connection.
#' Returns a list with con, cdm, tmp_path.
get_test_db <- function() {
  skip_if_not_installed("duckdb")

  src <- find_eunomia()
  if (is.null(src)) {
    skip("No Eunomia GiBleed DuckDB found. Set SYRONA_TEST_DB env var.")
  }

  # Copy to temp so we can write
  tmp_path <- tempfile(fileext = ".duckdb")
  file.copy(src, tmp_path)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmp_path, read_only = FALSE)
  cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")

  list(con = con, cdm = cdm, tmp_path = tmp_path)
}

#' Clean up a test database connection.
cleanup_test_db <- function(db) {
  try(DBI::dbDisconnect(db$con, shutdown = TRUE), silent = TRUE)
  try(unlink(db$tmp_path), silent = TRUE)
}
