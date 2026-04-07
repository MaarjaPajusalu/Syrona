# ── OHDSI cohort helpers ──────────────────────────────────────────────────────
#
# Functions for creating and managing OHDSI-standard cohort tables.
# Two creation paths are provided:
#
#   1. `create_caresite_cohort()` issues a server-side INSERT ... SELECT.
#      Used for care-site cohorts where pulling patient-level data into R
#      would be prohibitively expensive on large databases.
#
#   2. `insert_cohort()` uploads a local data frame via
#      `omopgenerics::insertTable()`. Used when cohort membership has
#      already been computed in R (e.g. from a CSV or programmatic rule).
#
# All cohort tables follow the standard OHDSI 4-column schema:
#   cohort_definition_id  INTEGER
#   subject_id            INTEGER
#   cohort_start_date     DATE
#   cohort_end_date       DATE

# ── Internal helpers ────────────────────────────────────────────────────────

.qualify_table <- function(schema = NULL, table) {
  if (is.null(schema) || identical(schema, "") || identical(schema, "main")) {
    table
  } else {
    paste0(schema, ".", table)
  }
}

.table_exists <- function(con, schema = NULL, table) {
  table_ref <- if (!is.null(schema) && !identical(schema, "") && !identical(schema, "main")) {
    DBI::Id(schema = schema, table = table)
  } else {
    table
  }
  DBI::dbExistsTable(con, table_ref)
}

# ── Cohort table creation ───────────────────────────────────────────────────

#' Create an OHDSI-standard cohort table.
#'
#' Creates the 4-column cohort table if it does not already exist.
#' Works with both DuckDB and PostgreSQL.
#'
#' @param con DBI connection.
#' @param cohort_schema Schema for the cohort table. \code{NULL} for default.
#' @return Invisible \code{TRUE} if created, \code{FALSE} if already existed.
#' @keywords internal
#' @importFrom rlang `%||%`
create_cohort_table <- function(con, cohort_schema = NULL) {
  if (.table_exists(con, cohort_schema, "cohort")) {
    cli::cli_alert_info("Cohort table already exists in {.val {cohort_schema %||% 'default'}} schema.")
    return(invisible(FALSE))
  }

  cohort_table <- .qualify_table(cohort_schema, "cohort")
  sql <- sprintf(
    "CREATE TABLE %s (
      cohort_definition_id INTEGER NOT NULL,
      subject_id           INTEGER NOT NULL,
      cohort_start_date    DATE    NOT NULL,
      cohort_end_date      DATE    NOT NULL
    )",
    cohort_table
  )
  DBI::dbExecute(con, sql)
  cli::cli_alert_success("Created cohort table: {.val {cohort_table}}")
  invisible(TRUE)
}

# ── Care-site cohort ────────────────────────────────────────────────────────

#' Create an OHDSI cohort from a care site.
#'
#' For each person who visited the specified care site, their cohort window
#' runs from their first visit start date to their last visit end date at
#' that site. Optionally clips to observation period overlap.
#'
#' This uses server-side \code{INSERT...SELECT} for performance on large
#' databases (avoids downloading + re-uploading patient-level data).
#'
#' @param con DBI connection to the OMOP CDM database.
#' @param care_site_id Integer \code{care_site_id} to define the cohort.
#' @param cohort_id Integer \code{cohort_definition_id} to assign.
#' @param cohort_schema Schema for the cohort table (\code{NULL} = default).
#' @param cdm_schema Schema containing OMOP CDM tables.
#' @param overwrite If \code{TRUE}, deletes existing entries for this
#'   \code{cohort_id} before inserting.
#' @param restrict_to_observation If \code{TRUE} (default), clips cohort
#'   intervals to observation period overlap.
#' @param collapse_strategy How to collapse visits per person:
#'   \code{"person_span"} (default) = first visit start to last visit end;
#'   \code{"visit_occurrence"} = one cohort entry per visit.
#' @return Number of rows inserted (invisible).
#' @export
create_caresite_cohort <- function(con,
                                   care_site_id,
                                   cohort_id,
                                   cohort_schema = NULL,
                                   cdm_schema,
                                   overwrite = FALSE,
                                   restrict_to_observation = TRUE,
                                   collapse_strategy = c("person_span", "visit_occurrence")) {
  collapse_strategy <- match.arg(collapse_strategy)

  # Ensure cohort table exists
  create_cohort_table(con, cohort_schema)

  # Clear existing entries if requested
  if (overwrite) {
    delete_cohort(con, cohort_id, cohort_schema)
  }

  # Build and execute the INSERT...SELECT
  cohort_table <- .qualify_table(cohort_schema, "cohort")
  select_sql <- .build_caresite_sql(
    care_site_id = care_site_id,
    cohort_id = cohort_id,
    cdm_schema = cdm_schema,
    collapse_strategy = collapse_strategy,
    restrict_to_observation = restrict_to_observation
  )

  insert_sql <- sprintf(
    "INSERT INTO %s (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)\n%s",
    cohort_table, select_sql
  )

  n_inserted <- DBI::dbExecute(con, insert_sql)
  cli::cli_alert_success(
    "Cohort {.val {cohort_id}} (care_site {.val {care_site_id}}): {n_inserted} rows inserted."
  )
  invisible(n_inserted)
}

#' Build the SELECT SQL for care-site cohort generation.
#' @keywords internal
.build_caresite_sql <- function(care_site_id, cohort_id, cdm_schema,
                                collapse_strategy, restrict_to_observation) {
  cdm_prefix <- paste0(cdm_schema, ".")

  if (restrict_to_observation) {
    cte <- sprintf(
      "WITH entry_events AS (
         SELECT
           vo.person_id,
           CASE WHEN op.observation_period_start_date > vo.visit_start_date
                THEN op.observation_period_start_date
                ELSE vo.visit_start_date END AS entry_start_date,
           CASE WHEN op.observation_period_end_date < COALESCE(vo.visit_end_date, vo.visit_start_date)
                THEN op.observation_period_end_date
                ELSE COALESCE(vo.visit_end_date, vo.visit_start_date) END AS entry_end_date
         FROM %svisit_occurrence vo
         JOIN %sobservation_period op
           ON op.person_id = vo.person_id
          AND vo.visit_start_date <= op.observation_period_end_date
          AND COALESCE(vo.visit_end_date, vo.visit_start_date) >= op.observation_period_start_date
         WHERE vo.care_site_id = %d
       ),
       valid_events AS (
         SELECT * FROM entry_events WHERE entry_start_date <= entry_end_date
       )",
      cdm_prefix, cdm_prefix, as.integer(care_site_id)
    )
  } else {
    cte <- sprintf(
      "WITH valid_events AS (
         SELECT
           vo.person_id,
           vo.visit_start_date AS entry_start_date,
           COALESCE(vo.visit_end_date, vo.visit_start_date) AS entry_end_date
         FROM %svisit_occurrence vo
         WHERE vo.care_site_id = %d
       )",
      cdm_prefix, as.integer(care_site_id)
    )
  }

  body <- switch(
    collapse_strategy,
    person_span = sprintf(
      "SELECT %d AS cohort_definition_id,
              person_id AS subject_id,
              MIN(entry_start_date) AS cohort_start_date,
              MAX(entry_end_date)   AS cohort_end_date
       FROM valid_events
       GROUP BY person_id",
      as.integer(cohort_id)
    ),
    visit_occurrence = sprintf(
      "SELECT %d AS cohort_definition_id,
              person_id AS subject_id,
              entry_start_date AS cohort_start_date,
              entry_end_date   AS cohort_end_date
       FROM valid_events",
      as.integer(cohort_id)
    )
  )

  paste(cte, body)
}

# ── Cohort from data frame (omopgenerics) ──────────────────────────────────

#' Insert a cohort from a local data frame.
#'
#' Uploads a data frame to the database via \code{omopgenerics::insertTable}
#' and marks it as a cohort table with \code{omopgenerics::newCohortTable}.
#' Use this when cohort membership has already been computed locally
#' (e.g. from a CSV or programmatic cohort definition).
#'
#' Requires that the CDM connection was created with a \code{writeSchema}.
#'
#' @param cdm A CDM reference (from \code{syrona_connect()$cdm}).
#' @param cohort_df Data frame with columns: \code{cohort_definition_id},
#'   \code{subject_id}, \code{cohort_start_date}, \code{cohort_end_date}.
#' @param name Name for the cohort table in the database (default \code{"cohort"}).
#' @return Updated CDM reference with the cohort table attached.
#' @export
insert_cohort <- function(cdm, cohort_df, name = "cohort") {
  required_cols <- c("cohort_definition_id", "subject_id",
                     "cohort_start_date", "cohort_end_date")
  missing <- setdiff(required_cols, names(cohort_df))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required columns: {.val {missing}}")
  }

  # Ensure correct types
  cohort_df <- cohort_df |>
    dplyr::mutate(
      cohort_definition_id = as.integer(.data$cohort_definition_id),
      subject_id = as.integer(.data$subject_id),
      cohort_start_date = as.Date(.data$cohort_start_date),
      cohort_end_date = as.Date(.data$cohort_end_date)
    )

  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = name,
    table = as.data.frame(cohort_df),
    overwrite = TRUE
  )
  cdm[[name]] <- omopgenerics::newCohortTable(cdm[[name]])

  cli::cli_alert_success(
    "Inserted {nrow(cohort_df)} rows into cohort table {.val {name}}."
  )
  cdm
}

# ── Cohort utilities ────────────────────────────────────────────────────────

#' List care sites with patient counts.
#'
#' Useful for identifying \code{care_site_id} values before creating cohorts.
#'
#' @param con DBI connection.
#' @param cdm_schema Schema containing the OMOP CDM tables.
#' @param min_patients Minimum number of patients to include (default 100).
#' @return A tibble with \code{care_site_id}, \code{care_site_name}, \code{n_patients}.
#' @export
list_care_sites <- function(con, cdm_schema, min_patients = 100) {
  sql <- sprintf(
    "SELECT cs.care_site_id, cs.care_site_name,
            COUNT(DISTINCT vo.person_id) AS n_patients
     FROM %s.visit_occurrence vo
     JOIN %s.care_site cs ON vo.care_site_id = cs.care_site_id
     GROUP BY cs.care_site_id, cs.care_site_name
     HAVING COUNT(DISTINCT vo.person_id) >= %d
     ORDER BY n_patients DESC",
    cdm_schema, cdm_schema, as.integer(min_patients)
  )
  DBI::dbGetQuery(con, sql) |> tibble::as_tibble()
}

#' Get summary statistics for a cohort.
#'
#' @param con DBI connection.
#' @param cohort_id Integer \code{cohort_definition_id}.
#' @param cohort_schema Schema containing the cohort table (\code{NULL} = default).
#' @return A tibble with \code{n_entries}, \code{n_persons}, \code{min_start}, \code{max_end}.
#' @export
cohort_summary <- function(con, cohort_id, cohort_schema = NULL) {
  cohort_table <- .qualify_table(cohort_schema, "cohort")
  sql <- sprintf(
    "SELECT cohort_definition_id,
            COUNT(*)                   AS n_entries,
            COUNT(DISTINCT subject_id) AS n_persons,
            MIN(cohort_start_date)     AS min_start,
            MAX(cohort_end_date)       AS max_end
     FROM %s
     WHERE cohort_definition_id = %d
     GROUP BY cohort_definition_id",
    cohort_table, as.integer(cohort_id)
  )
  DBI::dbGetQuery(con, sql) |> tibble::as_tibble()
}

#' Delete a cohort from the cohort table.
#'
#' @param con DBI connection.
#' @param cohort_id Integer \code{cohort_definition_id} to delete.
#' @param cohort_schema Schema containing the cohort table (\code{NULL} = default).
#' @return Number of rows deleted (invisible).
#' @export
delete_cohort <- function(con, cohort_id, cohort_schema = NULL) {
  cohort_table <- .qualify_table(cohort_schema, "cohort")
  sql <- sprintf(
    "DELETE FROM %s WHERE cohort_definition_id = %d",
    cohort_table, as.integer(cohort_id)
  )
  n_deleted <- DBI::dbExecute(con, sql)
  cli::cli_alert_info("Deleted {n_deleted} rows for cohort_id = {.val {cohort_id}}.")
  invisible(n_deleted)
}
