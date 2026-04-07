test_that("compare_yearly produces valid PR columns", {
  # Create two mock prevalence datasets
  d1 <- list(condition_prevalence = tibble::tibble(
    concept_id = c(1L, 1L, 2L),
    year = c(2010L, 2011L, 2010L),
    sex = c("F", "F", "M"),
    age_group = c("40-49", "40-49", "50-59"),
    patient_count = c(50L, 60L, 30L),
    record_count = c(100L, 120L, 45L),
    denominator = c(1000L, 1000L, 500L),
    prevalence = c(0.05, 0.06, 0.06)
  ))

  d2 <- list(condition_prevalence = tibble::tibble(
    concept_id = c(1L, 1L, 2L),
    year = c(2010L, 2011L, 2010L),
    sex = c("F", "F", "M"),
    age_group = c("40-49", "40-49", "50-59"),
    patient_count = c(80L, 70L, 20L),
    record_count = c(160L, 140L, 30L),
    denominator = c(2000L, 2000L, 800L),
    prevalence = c(0.04, 0.035, 0.025)
  ))

  result <- compare_yearly(d1, d2)

  expect_equal(nrow(result), 3)
  expect_true(all(c("log2_pr", "se", "ci_low", "ci_high", "fold_diff",
                     "p_value", "sig") %in% names(result)))
  expect_true(all(is.finite(result$log2_pr)))
  expect_true(all(result$se > 0))
})

test_that("meta-analysis cascade produces summary rows", {
  skip_if_not_installed("meta")

  # Build a yearly dataset with multiple years per concept x sex x age_group
  yearly <- tibble::tibble(
    concept_id = rep(1L, 6),
    year = rep(c(2010L, 2011L, 2012L), 2),
    sex = rep(c("F", "M"), each = 3),
    age_group = rep("40-49", 6),
    patient_count_d1 = c(50L, 55L, 60L, 40L, 45L, 50L),
    patient_count_d2 = c(80L, 75L, 70L, 60L, 55L, 50L),
    denominator_d1 = rep(1000L, 6),
    denominator_d2 = rep(2000L, 6),
    prevalence_d1 = c(0.05, 0.055, 0.06, 0.04, 0.045, 0.05),
    prevalence_d2 = c(0.04, 0.0375, 0.035, 0.03, 0.0275, 0.025)
  )
  yearly <- add_pr_columns(yearly)

  # Step 2: across years
  meta_ag <- compare_meta_agegroups(yearly)
  expect_equal(nrow(meta_ag), 2)  # F + M, single age_group
  expect_true("meta_model_type" %in% names(meta_ag))
  expect_true("tau2" %in% names(meta_ag))

  # Step 3: across age groups (only 1 age group, so pass-through)
  meta_sex <- compare_meta_by_sex(meta_ag)
  expect_equal(nrow(meta_sex), 2)  # F + M

  # Step 4: across sexes
  meta_sum <- compare_meta_summary(meta_sex)
  expect_equal(nrow(meta_sum), 1)
  expect_equal(meta_sum$sex, "Both")
  expect_true(is.finite(meta_sum$log2_pr))
})

test_that("run_meta handles single study as pass-through", {
  result <- run_meta(te = 0.5, se_te = 0.2, studlab = "2010")

  expect_equal(result$meta_model_type, "Pass-Through")
  expect_equal(result$log2_pr, 0.5)
  expect_equal(result$se, 0.2)
  expect_true(is.na(result$tau2))
})

test_that("run_meta handles empty input", {
  result <- run_meta(te = numeric(0), se_te = numeric(0), studlab = character(0))
  expect_null(result)

  # Non-finite values should be filtered
  result2 <- run_meta(te = c(NaN, Inf), se_te = c(0.1, 0.2), studlab = c("a", "b"))
  expect_null(result2)
})
