test_that("syrona_connect creates a valid CDM reference", {
  skip_if_not_installed("duckdb")

  db <- get_test_db()
  on.exit(cleanup_test_db(db))

  expect_true(inherits(db$con, "duckdb_connection"))
  expect_true(inherits(db$cdm, "cdm_reference"))

  # CDM should have standard OMOP tables
  expect_true("person" %in% names(db$cdm))
  expect_true("observation_period" %in% names(db$cdm))
  expect_true("condition_occurrence" %in% names(db$cdm))

  # person table should have rows
  n_persons <- db$cdm$person |> dplyr::tally() |> dplyr::pull(n)
  expect_gt(n_persons, 0)
})

test_that("syrona_disconnect works cleanly", {
  skip_if_not_installed("duckdb")

  db <- get_test_db()
  syrona_disconnect(db)

  # Connection should be closed
  expect_error(DBI::dbGetQuery(db$con, "SELECT 1"))
  unlink(db$tmp_path)
})
