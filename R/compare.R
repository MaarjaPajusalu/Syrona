# ── Dataset comparison and meta-analysis ──────────────────────────────────────
#
# Compares two extracted source datasets by computing stratified prevalence
# ratios and aggregating them through a multi-level random-effects
# meta-analysis cascade:
#
#   compare_yearly()         join strata, compute log2 PR + SE + CI per year
#   compare_meta_agegroups() pool across years    (concept x sex x age_group)
#   compare_meta_by_sex()    pool across ages     (concept x sex)
#   compare_meta_summary()   pool across sexes    (concept, sex = "Both")
#
# Each level produces one CSV per domain per comparison, written to
# data/comparisons/<d1>_vs_<d2>/.

# ── Constants ───────────────────────────────────────────────────────────────

#' @keywords internal
COMPARISONS_DIR <- "data/comparisons"

# ── Helper: compute PR columns ──────────────────────────────────────────────

#' Add prevalence ratio columns (log2 + natural + inference) to a data frame.
#'
#' Expects columns: prevalence_d1, prevalence_d2, denominator_d1, denominator_d2.
#'
#' Formulas:
#'   log2_pr = log2(p2 / p1)
#'   SE(ln)  = sqrt((1-p1)/(p1*n1) + (1-p2)/(p2*n2))
#'   SE(log2)= SE(ln) / ln(2)
#'   CI      = log2_pr +/- 1.96 x SE
#'
#' @param df Data frame with prevalence and denominator columns.
#' @return Data frame with PR columns appended.
#' @keywords internal
add_pr_columns <- function(df) {
  df |>
    dplyr::mutate(
      log2_pr = log2(.data$prevalence_d2 / .data$prevalence_d1),

      se = sqrt(
        (1 - .data$prevalence_d1) / (.data$prevalence_d1 * .data$denominator_d1) +
        (1 - .data$prevalence_d2) / (.data$prevalence_d2 * .data$denominator_d2)
      ) / log(2),

      ci_low   = .data$log2_pr - stats::qnorm(0.975) * .data$se,
      ci_high  = .data$log2_pr + stats::qnorm(0.975) * .data$se,
      ci_width = abs(.data$ci_high - .data$ci_low),

      fold_diff      = 2^.data$log2_pr,
      fold_ci_low    = 2^.data$ci_low,
      fold_ci_high   = 2^.data$ci_high,
      fold_ci_width  = abs(.data$fold_ci_high - .data$fold_ci_low),
      fold_symmetric = pmax(.data$fold_diff, 1 / .data$fold_diff),

      z       = .data$log2_pr / .data$se,
      p_value = 2 * (1 - stats::pnorm(abs(.data$z))),
      sig     = .data$ci_low * .data$ci_high > 0
    )
}

# ── Helper: build meta result tibble ────────────────────────────────────────

#' Build a one-row tibble from run_meta output + aggregated source columns.
#' @param meta_result Named list from run_meta.
#' @param id_cols One-row tibble with group ID columns.
#' @param g Group data frame (for aggregated counts).
#' @return One-row tibble.
#' @keywords internal
build_meta_row <- function(meta_result, id_cols, g) {
  dplyr::bind_cols(
    id_cols,
    tibble::tibble(
      patient_count_d1 = sum(g$patient_count_d1),
      patient_count_d2 = sum(g$patient_count_d2),
      denominator_d1   = sum(g$denominator_d1),
      denominator_d2   = sum(g$denominator_d2),
      prevalence_d1    = sum(g$patient_count_d1) / sum(g$denominator_d1),
      prevalence_d2    = sum(g$patient_count_d2) / sum(g$denominator_d2),
      log2_pr         = meta_result$log2_pr,
      se              = meta_result$se,
      ci_low          = meta_result$ci_low,
      ci_high         = meta_result$ci_high,
      ci_width        = abs(meta_result$ci_high - meta_result$ci_low),
      fold_diff       = 2^meta_result$log2_pr,
      fold_ci_low     = 2^meta_result$ci_low,
      fold_ci_high    = 2^meta_result$ci_high,
      fold_ci_width   = abs(2^meta_result$ci_high - 2^meta_result$ci_low),
      fold_symmetric  = max(2^meta_result$log2_pr, 2^(-meta_result$log2_pr)),
      z               = meta_result$z,
      p_value         = meta_result$p_value,
      sig             = meta_result$ci_low * meta_result$ci_high > 0,
      meta_model_type = meta_result$meta_model_type,
      n_strata        = nrow(g),
      tau2            = meta_result$tau2,
      I2              = meta_result$I2,
      Q               = meta_result$Q,
      pval_Q          = meta_result$pval_Q,
      lower_predict   = meta_result$lower_predict,
      upper_predict   = meta_result$upper_predict
    )
  )
}

# ── Helper: run meta-analysis with RE -> FE -> Pass-Through fallback ──────

#' Run meta-analysis on a vector of log2 prevalence ratios.
#'
#' Strategy:
#'   1. Single study: Pass-Through (return as-is)
#'   2. Try random-effects with Paule-Mandel tau
#'   3. If RE fails: try fixed-effect
#'   4. If both fail: return NULL
#'
#' @param te Numeric vector of log2 prevalence ratios.
#' @param se_te Numeric vector of standard errors (log2 scale).
#' @param studlab Character/numeric vector of study labels.
#' @return Named list with log2_pr, se, ci_low, ci_high, z, p_value,
#'   meta_model_type, tau2, I2, Q, pval_Q, lower_predict, upper_predict.
#'   NULL if meta-analysis fails.
#' @keywords internal
run_meta <- function(te, se_te, studlab) {
  rlang::check_installed("meta", reason = "for meta-analysis (metagen)")

  valid <- is.finite(te) & is.finite(se_te) & se_te > 0
  te      <- te[valid]
  se_te   <- se_te[valid]
  studlab <- studlab[valid]

  if (length(te) == 0) return(NULL)

  # Single study: pass-through
  if (length(te) == 1) {
    return(list(
      log2_pr = te,
      se      = se_te,
      ci_low  = te - stats::qnorm(0.975) * se_te,
      ci_high = te + stats::qnorm(0.975) * se_te,
      z       = te / se_te,
      p_value = 2 * (1 - stats::pnorm(abs(te / se_te))),
      meta_model_type = "Pass-Through",
      tau2          = NA_real_,
      I2            = NA_real_,
      Q             = NA_real_,
      pval_Q        = NA_real_,
      lower_predict = NA_real_,
      upper_predict = NA_real_
    ))
  }

  # Try random-effects (Paule-Mandel tau)
  result <- tryCatch({
    m <- meta::metagen(TE = te, seTE = se_te, studlab = studlab, method.tau = "PM")
    list(
      log2_pr = m$TE.random,
      se      = m$seTE.random,
      ci_low  = m$lower.random,
      ci_high = m$upper.random,
      z       = m$zval.random,
      p_value = m$pval.random,
      meta_model_type = "Random-Effects",
      tau2          = as.numeric(m$tau2),
      I2            = as.numeric(m$I2),
      Q             = as.numeric(m$Q),
      pval_Q        = as.numeric(m$pval.Q),
      lower_predict = if (!is.null(m$lower.predict)) as.numeric(m$lower.predict) else NA_real_,
      upper_predict = if (!is.null(m$upper.predict)) as.numeric(m$upper.predict) else NA_real_
    )
  }, error = function(e) {
    # Fixed-effect fallback
    tryCatch({
      m <- meta::metagen(TE = te, seTE = se_te, studlab = studlab, random = FALSE)
      list(
        log2_pr = m$TE.common,
        se      = m$seTE.common,
        ci_low  = m$lower.common,
        ci_high = m$upper.common,
        z       = m$zval.common,
        p_value = m$pval.common,
        meta_model_type = "Fixed-Effect",
        tau2          = NA_real_,
        I2            = as.numeric(m$I2),
        Q             = as.numeric(m$Q),
        pval_Q        = as.numeric(m$pval.Q),
        lower_predict = NA_real_,
        upper_predict = NA_real_
      )
    }, error = function(e2) NULL)
  })

  result
}

# ── Per-year comparison ────────────────────────────────────────────────────

#' Compare two datasets at the per-year stratum level.
#'
#' Matches on concept_id x year x sex x age_group (inner join).
#' Only keeps strata where both datasets have prevalence > 0.
#'
#' @param d1 Named list from \code{load_dataset} (reference).
#' @param d2 Named list from \code{load_dataset} (comparison).
#' @param prev_table Name of the prevalence table to compare.
#' @return tibble with one row per matched stratum, full PR column set.
#' @export
compare_yearly <- function(d1, d2, prev_table = "condition_prevalence") {
  prev1 <- d1[[prev_table]]
  prev2 <- d2[[prev_table]]

  joined <- dplyr::inner_join(
    prev1 |> dplyr::select("concept_id", "year", "sex", "age_group",
                           patient_count_d1 = "patient_count",
                           denominator_d1 = "denominator",
                           prevalence_d1 = "prevalence"),
    prev2 |> dplyr::select("concept_id", "year", "sex", "age_group",
                           patient_count_d2 = "patient_count",
                           denominator_d2 = "denominator",
                           prevalence_d2 = "prevalence"),
    by = c("concept_id", "year", "sex", "age_group")
  )

  joined <- joined |>
    dplyr::filter(.data$prevalence_d1 > 0, .data$prevalence_d2 > 0)

  result <- add_pr_columns(joined)

  result |>
    dplyr::filter(is.finite(.data$log2_pr), is.finite(.data$se), .data$se > 0) |>
    dplyr::arrange(.data$concept_id, .data$year, .data$sex, .data$age_group)
}

# ── Meta-analysis cascade ──────────────────────────────────────────────────

#' Meta-analysis across years: one row per concept_id x sex x age_group.
#'
#' @param yearly_df Output of \code{compare_yearly}.
#' @return tibble with meta-analyzed rows.
#' @export
compare_meta_agegroups <- function(yearly_df) {
  groups <- yearly_df |>
    dplyr::group_by(.data$concept_id, .data$sex, .data$age_group) |>
    dplyr::group_split()

  results <- lapply(groups, function(g) {
    m <- run_meta(g$log2_pr, g$se, as.character(g$year))
    if (is.null(m)) return(NULL)
    id_cols <- tibble::tibble(
      concept_id = g$concept_id[1], sex = g$sex[1], age_group = g$age_group[1]
    )
    build_meta_row(m, id_cols, g)
  })

  dplyr::bind_rows(results) |>
    dplyr::arrange(.data$concept_id, .data$sex, .data$age_group)
}

#' Meta-analysis across age groups: one row per concept_id x sex.
#'
#' @param meta_agegroups_df Output of \code{compare_meta_agegroups}.
#' @return tibble with meta-analyzed rows.
#' @export
compare_meta_by_sex <- function(meta_agegroups_df) {
  groups <- meta_agegroups_df |>
    dplyr::group_by(.data$concept_id, .data$sex) |>
    dplyr::group_split()

  results <- lapply(groups, function(g) {
    m <- run_meta(g$log2_pr, g$se, g$age_group)
    if (is.null(m)) return(NULL)
    id_cols <- tibble::tibble(concept_id = g$concept_id[1], sex = g$sex[1])
    build_meta_row(m, id_cols, g)
  })

  dplyr::bind_rows(results) |>
    dplyr::arrange(.data$concept_id, .data$sex)
}

#' Meta-analysis across sexes: one row per concept_id (sex = "Both").
#'
#' @param meta_by_sex_df Output of \code{compare_meta_by_sex}.
#' @return tibble with one summary row per concept.
#' @export
compare_meta_summary <- function(meta_by_sex_df) {
  groups <- meta_by_sex_df |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::group_split()

  results <- lapply(groups, function(g) {
    m <- run_meta(g$log2_pr, g$se, g$sex)
    if (is.null(m)) return(NULL)
    id_cols <- tibble::tibble(concept_id = g$concept_id[1], sex = "Both")
    build_meta_row(m, id_cols, g)
  })

  dplyr::bind_rows(results) |>
    dplyr::arrange(.data$concept_id)
}

# ── Single-domain pipeline ─────────────────────────────────────────────────

#' Run the full comparison pipeline for one domain.
#' @param d1 Loaded dataset 1.
#' @param d2 Loaded dataset 2.
#' @param prev_table Name of the prevalence table.
#' @param domain_label Label for messages.
#' @return Named list of 4 tibbles, or NULL if no overlapping concepts.
#' @keywords internal
compare_domain <- function(d1, d2, prev_table, domain_label) {
  cli::cli_h3(domain_label)

  cli::cli_alert("Yearly comparison...")
  yearly <- compare_yearly(d1, d2, prev_table)
  if (nrow(yearly) == 0) {
    cli::cli_alert_warning("No overlapping concepts - skipping {domain_label}.")
    return(NULL)
  }
  cli::cli_alert_info("{nrow(yearly)} rows, {dplyr::n_distinct(yearly$concept_id)} concepts")

  cli::cli_alert("Meta-analysis across years...")
  meta_ag <- compare_meta_agegroups(yearly)
  cli::cli_alert_info("{nrow(meta_ag)} rows, {dplyr::n_distinct(meta_ag$concept_id)} concepts")

  cli::cli_alert("Meta-analysis across age groups...")
  meta_sex <- compare_meta_by_sex(meta_ag)
  cli::cli_alert_info("{nrow(meta_sex)} rows, {dplyr::n_distinct(meta_sex$concept_id)} concepts")

  cli::cli_alert("Meta-analysis across sexes...")
  meta_sum <- compare_meta_summary(meta_sex)
  cli::cli_alert_info("{nrow(meta_sum)} summary rows")

  list(
    yearly         = yearly,
    meta_agegroups = meta_ag,
    meta_by_sex    = meta_sex,
    meta_summary   = meta_sum
  )
}

# ── Master comparison function ──────────────────────────────────────────────

#' Run the full comparison pipeline for two datasets across available domains.
#'
#' Loads both datasets, runs the 4-step comparison for each domain present
#' in both, and optionally saves to CSV.
#'
#' @param d1_name Name of dataset 1 (reference).
#' @param d2_name Name of dataset 2 (comparison).
#' @param domains Character vector of domains to compare.
#' @param save If \code{TRUE} (default), writes CSV output.
#' @return Named list of domain results (invisible).
#' @export
compare_all <- function(d1_name, d2_name,
                        domains = c("conditions", "procedures", "drugs"),
                        save = TRUE) {
  domains <- match.arg(domains, c("conditions", "procedures", "drugs"), several.ok = TRUE)
  cli::cli_h2("Comparing {d1_name} vs {d2_name}")

  cli::cli_alert("Loading datasets...")
  d1 <- load_dataset(d1_name)
  d2 <- load_dataset(d2_name)

  domain_map <- list(
    condition = "condition_prevalence",
    procedure = "procedure_prevalence",
    drug      = "drug_prevalence"
  )

  requested <- sub("s$", "", domains)
  all_tables <- list()

  for (domain in intersect(requested, names(domain_map))) {
    prev_table <- domain_map[[domain]]
    if (!is.null(d1[[prev_table]]) && !is.null(d2[[prev_table]]) &&
        nrow(d1[[prev_table]]) > 0 && nrow(d2[[prev_table]]) > 0) {

      result <- compare_domain(d1, d2, prev_table, paste0(domain, "s"))

      if (!is.null(result)) {
        all_tables[[paste0(domain, "_yearly")]]         <- result$yearly
        all_tables[[paste0(domain, "_meta_agegroups")]]  <- result$meta_agegroups
        all_tables[[paste0(domain, "_meta_by_sex")]]     <- result$meta_by_sex
        all_tables[[paste0(domain, "_meta_summary")]]    <- result$meta_summary
      }
    }
  }

  if (length(all_tables) == 0) {
    cli::cli_alert_warning("No domains with overlapping data found.")
    return(invisible(list()))
  }

  if (save) {
    save_comparison(all_tables, d1_name, d2_name)
  }

  for (domain in names(domain_map)) {
    sum_key <- paste0(domain, "_meta_summary")
    if (!is.null(all_tables[[sum_key]])) {
      cli::cli_alert_success("{domain}s: {nrow(all_tables[[sum_key]])} concepts compared.")
    }
  }
  cli::cli_alert_success("Done. {d1_name} vs {d2_name}.")
  invisible(all_tables)
}

# ── Save / load helpers ─────────────────────────────────────────────────────

#' Save comparison tables to CSV.
#' @param tables Named list of tibbles.
#' @param d1_name Dataset 1 name.
#' @param d2_name Dataset 2 name.
#' @keywords internal
save_comparison <- function(tables, d1_name, d2_name) {
  dir_name <- paste0(d1_name, "_vs_", d2_name)
  base <- getOption("syrona.data_dir", ".")
  out_dir <- file.path(base, COMPARISONS_DIR, dir_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  nrow_or <- function(name) if (!is.null(tables[[name]])) nrow(tables[[name]]) else 0L
  ndist_or <- function(name) {
    if (!is.null(tables[[name]])) dplyr::n_distinct(tables[[name]]$concept_id) else 0L
  }

  metadata <- tibble::tibble(
    d1_name     = d1_name,
    d2_name     = d2_name,
    compared_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_condition_concepts = ndist_or("condition_meta_summary"),
    n_condition_yearly_rows = nrow_or("condition_yearly"),
    n_procedure_concepts = ndist_or("procedure_meta_summary"),
    n_procedure_yearly_rows = nrow_or("procedure_yearly"),
    n_drug_concepts = ndist_or("drug_meta_summary"),
    n_drug_yearly_rows = nrow_or("drug_yearly")
  )
  utils::write.csv(metadata, file.path(out_dir, "_metadata.csv"), row.names = FALSE)

  for (name in names(tables)) {
    utils::write.csv(tables[[name]], file.path(out_dir, paste0(name, ".csv")),
                     row.names = FALSE)
  }

  cli::cli_alert_success("Saved comparison to {.path {out_dir}}/")
  invisible(out_dir)
}

#' Load a previously saved comparison from CSV.
#' @param d1_name Dataset 1 name.
#' @param d2_name Dataset 2 name.
#' @return Named list of tibbles.
#' @export
load_comparison <- function(d1_name, d2_name) {
  dir_name <- paste0(d1_name, "_vs_", d2_name)
  base <- getOption("syrona.data_dir", ".")
  in_dir <- file.path(base, COMPARISONS_DIR, dir_name)
  if (!dir.exists(in_dir)) {
    cli::cli_abort("Comparison {.val {dir_name}} not found at {.path {in_dir}}")
  }

  csv_files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)
  csv_files <- csv_files[!grepl("^_", basename(csv_files))]

  tables <- list()
  for (f in csv_files) {
    name <- tools::file_path_sans_ext(basename(f))
    # readr::read_csv is ~5-10x faster than utils::read.csv on these files.
    # Numeric columns come back as double (readr default). Fine for joins.
    tables[[name]] <- readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
  }

  # Backwards compatibility: map old unprefixed names
  old_names <- c("yearly", "meta_agegroups", "meta_by_sex", "meta_summary")
  for (old in old_names) {
    new <- paste0("condition_", old)
    if (old %in% names(tables) && !new %in% names(tables)) {
      tables[[new]] <- tables[[old]]
      tables[[old]] <- NULL
    }
  }

  cli::cli_alert_success("Loaded {length(tables)} comparison tables from {.path {in_dir}}/")
  tables
}

#' List available comparisons.
#' @return Character vector of comparison directory names.
#' @export
list_comparisons <- function() {
  base <- getOption("syrona.data_dir", ".")
  cmp <- file.path(base, COMPARISONS_DIR)
  if (!dir.exists(cmp)) return(character(0))
  dirs <- list.dirs(cmp, full.names = FALSE, recursive = FALSE)
  dirs[dirs != ""]
}
