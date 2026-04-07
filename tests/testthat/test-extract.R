test_that("extract_all runs on GiBleed and produces expected tables", {
  skip_if_not_installed("duckdb")

  db <- get_test_db()
  on.exit(cleanup_test_db(db))

  # Run extraction for conditions only (GiBleed has conditions + drugs)
  tables <- extract_all(
    dataset_name = "GiBleed_test",
    db = db,
    domains = "conditions",
    save = FALSE
  )

  # Should have shared tables
  expect_true("demographics" %in% names(tables))
  expect_true("death_counts" %in% names(tables))
  expect_true("denominator" %in% names(tables))

  # Should have condition tables
  expect_true("condition_prevalence" %in% names(tables))
  expect_true("condition_info" %in% names(tables))
  expect_true("condition_chapters" %in% names(tables))
  expect_true("condition_attributes" %in% names(tables))

  # Should NOT have procedure/drug tables
  expect_null(tables$procedure_info)
  expect_null(tables$drug_info)

  # Demographics should have rows
  expect_gt(nrow(tables$demographics), 0)
  expect_true(all(c("sex", "birth_year", "patient_count") %in% names(tables$demographics)))

  # Condition prevalence should have correct columns
  prev_cols <- c("concept_id", "year", "sex", "age_group",
                 "patient_count", "record_count", "denominator", "prevalence")
  expect_true(all(prev_cols %in% names(tables$condition_prevalence)))

  # Condition info should have correct columns
  info_cols <- c("concept_id", "concept_name", "concept_code",
                 "vocabulary_id", "n_patients_total")
  expect_true(all(info_cols %in% names(tables$condition_info)))

  # Prevalence values should be between 0 and 1
  expect_true(all(tables$condition_prevalence$prevalence > 0, na.rm = TRUE))
  expect_true(all(tables$condition_prevalence$prevalence <= 1, na.rm = TRUE))
})

test_that("extract_all runs drugs domain on GiBleed", {
  skip_if_not_installed("duckdb")

  db <- get_test_db()
  on.exit(cleanup_test_db(db))

  tables <- extract_all(
    dataset_name = "GiBleed_drugs",
    db = db,
    domains = "drugs",
    save = FALSE
  )

  expect_true("drug_prevalence" %in% names(tables))
  expect_true("drug_info" %in% names(tables))
  expect_true("drug_chapters" %in% names(tables))

  # Drug info should have ingredient-level concepts
  if (nrow(tables$drug_info) > 0) {
    expect_true(all(tables$drug_info$concept_class_id == "Ingredient"))
  }
})

test_that("extract_denominators produces valid output", {
  skip_if_not_installed("duckdb")

  db <- get_test_db()
  on.exit(cleanup_test_db(db))

  denom <- extract_denominators(db$cdm)

  expect_true(all(c("year", "sex", "age_group", "denominator") %in% names(denom)))
  expect_gt(nrow(denom), 0)
  expect_true(all(denom$sex %in% c("F", "M")))
  expect_true(all(denom$denominator > 0))
})
