test_that("insert_cohort creates a valid cohort table via omopgenerics", {
  skip_if_not_installed("duckdb")

  db <- get_test_db()
  on.exit(cleanup_test_db(db))

  # Get real person_ids + their observation periods so dates are valid
  obs <- db$cdm$observation_period |>
    dplyr::select(person_id, observation_period_start_date, observation_period_end_date) |>
    head(10) |>
    dplyr::collect()

  # Build a cohort that sits within each person's observation period
  cohort_df <- data.frame(
    cohort_definition_id = 1L,
    subject_id = obs$person_id,
    cohort_start_date = obs$observation_period_start_date,
    cohort_end_date = obs$observation_period_end_date
  )

  # Insert using omopgenerics pathway
  cdm <- insert_cohort(db$cdm, cohort_df, name = "test_cohort")

  expect_true("test_cohort" %in% names(cdm))

  # Verify row count
  n_rows <- cdm$test_cohort |> dplyr::tally() |> dplyr::pull(n)
  expect_equal(n_rows, 10)
})

test_that("create_cohort_table + delete_cohort round-trip works", {
  skip_if_not_installed("duckdb")

  db <- get_test_db()
  on.exit(cleanup_test_db(db))

  # Create cohort table
  result <- create_cohort_table(db$con, cohort_schema = NULL)
  expect_true(result)

  # Second call should return FALSE (already exists)
  result2 <- create_cohort_table(db$con, cohort_schema = NULL)
  expect_false(result2)

  # Insert a row manually
  DBI::dbExecute(db$con,
    "INSERT INTO cohort VALUES (99, 1, '2000-01-01', '2020-12-31')")

  # Verify via cohort_summary
  summary <- cohort_summary(db$con, cohort_id = 99)
  expect_equal(summary$n_entries, 1)
  expect_equal(summary$n_persons, 1)

  # Delete
  n_del <- delete_cohort(db$con, cohort_id = 99)
  expect_equal(n_del, 1)
})
