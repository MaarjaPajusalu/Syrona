# ── Syrona Dashboard - Global ──────────────────────────────────────────────────
#
# This file is the single source of truth for the dashboard's startup logic.
# It is the canonical version inside the syrona R package, and is also copied
# verbatim to syrona-web/global.R by scripts/build_web.R for the omop-apps
# Shiny Server deployment. Both deploy modes (package and flat-shiny) use the
# exact same file, so do not split it into per-mode variants.
#
# Mode detection happens at runtime via exists("load_syrona_theme"):
#   1. Package mode (syrona::run_app()): the package is already attached, so
#      load_syrona_theme() is on the search path.
#   2. Flat-shiny mode (omop-apps Shiny Server): no syrona package install;
#      we source the helper R/ files directly from the app folder.
#
# Server install list (flat-shiny mode, run once on the omop-apps host as the
# user that owns the Shiny Server library):
#
#   install.packages(c(
#     "shiny", "dplyr", "DT", "ggplot2", "ggiraph", "ggtext",
#     "scales", "tidyr", "shinycssloaders"
#   ))
#
# Everything else used at runtime (cli, tibble, rlang, ...) is pulled in
# transitively as a hard dependency of dplyr.

library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(DT)
library(ggplot2)
library(ggiraph)
library(ggtext)
library(scales)
library(tidyr)
library(shinycssloaders)

# NULL-coalescing operator. Built into base R since 4.4.0 and exported by
# rlang, but neither is on the search path of this Shiny global env unless we
# attach rlang explicitly. Defining it here is harmless because every
# implementation has the same semantics.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Mode detection: if syrona is attached, use its exported helpers. Otherwise
# we are running flat-shiny on a server without the package installed, and
# need to source the R/ helpers from the app folder directly.
if (exists("load_syrona_theme", mode = "function")) {
  load_syrona_theme()
} else {
  for (helper in c("constants.R", "config.R", "extract.R", "compare.R",
                   "plot_heatmaps.R", "plot_forests.R", "plot_demography.R",
                   "plot_pr_distribution.R")) {
    source(file.path("R", helper))
  }
}

# ── Data directory ───────────────────────────────────────────────────────────
# Resolution order:
#   1. options(syrona.data_dir = ...) — set by run_app() in package mode.
#   2. SYRONA_DATA_DIR environment variable.
#   3. Working directory — under Shiny Server this is the app folder, which
#      already contains data/sources/ alongside this file.
#   4. Two levels up from getwd() — covers running shinyAppDir(inst/shiny/)
#      from a package project root.

DATA_DIR <- getOption("syrona.data_dir", NULL)
if (is.null(DATA_DIR)) {
  candidates <- c(
    getwd(),
    file.path(getwd(), "..", ".."),
    Sys.getenv("SYRONA_DATA_DIR", unset = "")
  )
  DATA_DIR <- "."
  for (cand in candidates) {
    if (nchar(cand) > 0 && dir.exists(file.path(cand, "data", "sources"))) {
      DATA_DIR <- normalizePath(cand)
      break
    }
  }
  # Propagate the discovered path so load_dataset() and load_comparison()
  # (which read getOption("syrona.data_dir", ".")) use the same base as
  # everything above. Without this, flat-shiny mode worked only by
  # coincidence of getwd() happening to equal DATA_DIR.
  options(syrona.data_dir = DATA_DIR)
}
message("[syrona] DATA_DIR = ", DATA_DIR)

# ── ICD-10 Lookup (SNOMED -> ICD-10 code mapping) ────────────────────────────
# Built by scripts/build_icd10_lookup.R from OMOP vocabulary tables.

icd10_path <- file.path(DATA_DIR, "data", "icd10_lookup.csv")
icd10_lookup <- if (file.exists(icd10_path)) {
  read.csv(icd10_path, stringsAsFactors = FALSE) |> as_tibble()
} else {
  tibble(concept_id = integer(), icd10_code = character(),
         icd10_name = character())
}

# ── Vocabulary Lookups (procedure enrichment) ────────────────────────────────
# Adds device, indirect site, and morphology attributes for procedure filtering.

vocab_dir <- file.path(DATA_DIR, "data", "vocabulary")

vocab_device <- if (file.exists(file.path(vocab_dir, "procedure_device_attributes.csv"))) {
  read.csv(file.path(vocab_dir, "procedure_device_attributes.csv"), stringsAsFactors = FALSE) |> as_tibble()
} else NULL

vocab_site_extra <- if (file.exists(file.path(vocab_dir, "procedure_site_extra.csv"))) {
  read.csv(file.path(vocab_dir, "procedure_site_extra.csv"), stringsAsFactors = FALSE) |> as_tibble()
} else NULL

vocab_morphology <- if (file.exists(file.path(vocab_dir, "procedure_morphology.csv"))) {
  read.csv(file.path(vocab_dir, "procedure_morphology.csv"), stringsAsFactors = FALSE) |> as_tibble()
} else NULL

# ── Auto-detect Comparisons ──────────────────────────────────────────────────

comp_dir <- file.path(DATA_DIR, "data", "comparisons")
available_comparisons <- if (dir.exists(comp_dir)) {
  list.dirs(comp_dir, recursive = FALSE, full.names = FALSE)
} else {
  character(0)
}
message("[syrona] comp_dir = ", comp_dir, " | exists: ", dir.exists(comp_dir),
        " | comparisons found: ", length(available_comparisons),
        if (length(available_comparisons) > 0) paste0(" (", paste(available_comparisons, collapse = ", "), ")") else "")
comparison_choices <- setNames(
  available_comparisons,
  gsub("_vs_", " vs ", available_comparisons)
)

# ── Lazy source dataset cache ────────────────────────────────────────────────

source_cache <- new.env(parent = emptyenv())

get_source_dataset <- function(name) {
  if (!exists(name, envir = source_cache)) {
    source_cache[[name]] <- load_dataset(name)
  }
  source_cache[[name]]
}

# ── Domain Configuration ────────────────────────────────────────────────────

DOMAIN_CONFIG <- list(
  condition = list(
    label = "Conditions",
    prev_table   = "condition_prevalence",
    info_table   = "condition_info",
    chapter_table = "condition_chapters",
    attr_table   = "condition_attributes",
    comp_prefix  = "condition_",
    class_types  = c(
      "ICD-10 Chapter"    = "icd10_chapter",
      "Body system"       = "body_system",
      "Disease category"  = "disease_category"
    ),
    attr_types = c("finding_site", "morphology", "clinical_course"),
    attr_labels = list(
      finding_site    = "Finding site",
      morphology      = "Morphology",
      clinical_course = "Clinical course"
    ),
    attr_tips = list(
      finding_site    = "SNOMED 'Has finding site' relationship. Anatomical location where the condition manifests.",
      morphology      = "SNOMED 'Has associated morphology' relationship. Structural change (e.g. inflammation, neoplasm).",
      clinical_course = "SNOMED 'Has clinical course' relationship. Temporal pattern (e.g. acute, chronic)."
    ),
    detail_tab_label = "Concept Detail",
    search_placeholder = "Search by name or code...",
    chapter_facet_label = "Per chapter",
    chapter_tip = "Chapters from the selected classification system. A concept may appear in more than one chapter: via SNOMED CT poly-hierarchy (Body system, Disease category) or via multiple ICD-10 mappings.",
    subchapter_tip = "Sub-chapters within the selected chapter. Narrow down to a specific sub-group."
  ),
  procedure = list(
    label = "Procedures",
    prev_table   = "procedure_prevalence",
    info_table   = "procedure_info",
    chapter_table = "procedure_chapters",
    attr_table   = "procedure_attributes",
    comp_prefix  = "procedure_",
    class_types  = c(
      "By method" = "by_method",
      "By site"   = "by_site"
    ),
    attr_types = c("procedure_site", "method", "device", "morphology"),
    attr_labels = list(
      procedure_site = "Body site",
      method         = "Method",
      device         = "Device",
      morphology     = "Morphology"
    ),
    attr_tips = list(
      procedure_site = "SNOMED procedure site relationships (direct + indirect + generic). Anatomical location of the procedure.",
      method         = "SNOMED 'Has method' relationship. Type of action (e.g. excision, repair, imaging).",
      device         = "SNOMED device relationships (Using device, Has dir/indir/proc device, Using acc device). Device used in the procedure.",
      morphology     = "SNOMED morphology relationships (Has dir/indir/proc morph). Structural change targeted by the procedure."
    ),
    detail_tab_label = "Concept Detail",
    search_placeholder = "Search procedure by name or code...",
    chapter_facet_label = "Per chapter",
    chapter_tip = "Chapters from the selected procedure classification. SNOMED CT poly-hierarchy means a concept may appear in more than one chapter.",
    subchapter_tip = "Sub-chapters within the selected chapter. Narrow down to a specific procedure group."
  ),
  drug = list(
    label = "Drugs",
    prev_table   = "drug_prevalence",
    info_table   = "drug_info",
    chapter_table = "drug_chapters",
    attr_table   = "drug_attributes",
    comp_prefix  = "drug_",
    class_types  = c(
      "ATC 1st level" = "atc_1st"
    ),
    attr_types = character(0),
    attr_labels = list(),
    attr_tips = list(),
    detail_tab_label = "Concept Detail",
    search_placeholder = "Search drug ingredient by name or code...",
    chapter_facet_label = "Per ATC chapter",
    chapter_tip = "ATC 1st level anatomical/therapeutic classification."
  )
)

MAX_ATTR_SLOTS <- 4
DEFAULT_DOMAIN <- "condition"
DEFAULT_CLASS_TYPES <- DOMAIN_CONFIG[["condition"]]$class_types
DEFAULT_ATTR_TYPES  <- DOMAIN_CONFIG[["condition"]]$attr_types

#' Detect which domains are available in a comparison.
detect_domains <- function(comp) {
  domains <- character(0)
  for (d in names(DOMAIN_CONFIG)) {
    key <- paste0(DOMAIN_CONFIG[[d]]$comp_prefix, "yearly")
    if (!is.null(comp[[key]]) && nrow(comp[[key]]) > 0) {
      domains <- c(domains, d)
    }
  }
  domains
}

#' Count unique concepts per domain in a comparison.
get_domain_counts <- function(comp) {
  counts <- list()
  for (d in names(DOMAIN_CONFIG)) {
    key <- paste0(DOMAIN_CONFIG[[d]]$comp_prefix, "yearly")
    tbl <- comp[[key]]
    counts[[d]] <- if (!is.null(tbl) && nrow(tbl) > 0) length(unique(tbl$concept_id)) else 0L
  }
  counts
}

#' Build derived data for a comparison + domain.
build_comparison_data <- function(comp_name, domain = "condition") {
  parts <- strsplit(comp_name, "_vs_")[[1]]
  d1_name <- parts[1]; d2_name <- parts[2]
  d1 <- get_source_dataset(d1_name)
  d2 <- get_source_dataset(d2_name)
  comp <- load_comparison(d1_name, d2_name)

  cfg <- DOMAIN_CONFIG[[domain]]
  prefix <- cfg$comp_prefix

  yearly_df      <- comp[[paste0(prefix, "yearly")]]
  meta_ag_df     <- comp[[paste0(prefix, "meta_agegroups")]]
  meta_by_sex_df <- comp[[paste0(prefix, "meta_by_sex")]]
  meta_summary_df <- comp[[paste0(prefix, "meta_summary")]]

  if (is.null(yearly_df) || nrow(yearly_df) == 0) {
    return(list(
      available = FALSE, domain = domain, d1_name = d1_name, d2_name = d2_name,
      d1 = d1, d2 = d2
    ))
  }

  compared_concept_ids <- unique(yearly_df$concept_id)

  info1 <- d1[[cfg$info_table]]; info2 <- d2[[cfg$info_table]]
  chap1 <- d1[[cfg$chapter_table]]; chap2 <- d2[[cfg$chapter_table]]
  attr1 <- d1[[cfg$attr_table]]; attr2 <- d2[[cfg$attr_table]]

  concept_info <- bind_rows(
    info1 |> select(concept_id, concept_name, concept_code),
    info2 |> select(concept_id, concept_name, concept_code)
  ) |>
    filter(concept_id %in% compared_concept_ids) |>
    distinct(concept_id, .keep_all = TRUE) |>
    arrange(concept_name)

  chapters_df <- bind_rows(chap1, chap2) |>
    filter(concept_id %in% compared_concept_ids) |>
    distinct()

  attrs_df <- if (!is.null(attr1) && !is.null(attr2)) {
    bind_rows(attr1, attr2) |>
      filter(concept_id %in% compared_concept_ids) |>
      distinct()
  } else {
    tibble(concept_id = integer(), relationship = character(),
           target_concept_id = integer(), target_concept_name = character())
  }

  # Merge vocabulary lookups for procedures
  if (domain == "procedure") {
    vocab_cols <- c("concept_id", "relationship", "target_concept_id", "target_concept_name")
    if (!is.null(vocab_device)) {
      attrs_df <- bind_rows(attrs_df,
        vocab_device |>
          filter(concept_id %in% compared_concept_ids) |>
          select(all_of(vocab_cols))
      ) |> distinct()
    }
    if (!is.null(vocab_site_extra)) {
      attrs_df <- bind_rows(attrs_df,
        vocab_site_extra |>
          filter(concept_id %in% compared_concept_ids) |>
          select(all_of(vocab_cols))
      ) |> distinct()
    }
    if (!is.null(vocab_morphology)) {
      attrs_df <- bind_rows(attrs_df,
        vocab_morphology |>
          filter(concept_id %in% compared_concept_ids) |>
          select(all_of(vocab_cols))
      ) |> distinct()
    }
  }

  total_persons_d1 <- sum(d1$demographics$patient_count)
  total_persons_d2 <- sum(d2$demographics$patient_count)

  counts_df <- info1 |>
    select(concept_id, concept_name, concept_code, patients_d1 = n_patients_total) |>
    full_join(
      info2 |> select(concept_id, patients_d2 = n_patients_total),
      by = "concept_id"
    ) |>
    filter(concept_id %in% compared_concept_ids) |>
    mutate(
      pct_d1 = patients_d1 / total_persons_d1 * 100,
      pct_d2 = patients_d2 / total_persons_d2 * 100
    ) |>
    arrange(concept_name)

  pop_weight_df <- counts_df |>
    transmute(concept_id,
              pop_weight = pmax(
                replace_na(pct_d1, 0),
                replace_na(pct_d2, 0)))

  concept_info <- concept_info |>
    left_join(pop_weight_df, by = "concept_id") |>
    mutate(pop_weight = replace_na(pop_weight, 0))

  list(
    available = TRUE, domain = domain,
    d1 = d1, d2 = d2, d1_name = d1_name, d2_name = d2_name,
    yearly_df = yearly_df, meta_ag_df = meta_ag_df,
    meta_by_sex_df = meta_by_sex_df, meta_summary_df = meta_summary_df,
    compared_concept_ids = compared_concept_ids,
    concept_info = concept_info, chapters_df = chapters_df,
    attrs_df = attrs_df, counts_df = counts_df,
    available_domains = detect_domains(comp),
    domain_counts = get_domain_counts(comp)
  )
}

# ── Constants ────────────────────────────────────────────────────────────────

age_group_levels <- c("0-9", "10-19", "20-29", "30-39", "40-49",
                      "50-59", "60-69", "70-79", "80+")

HEATMAP_MAX_CONCEPTS <- 180

# ── Filter Helper Functions ──────────────────────────────────────────────────

get_attribute_choices <- function(df, rel_type, concept_ids = NULL) {
  d <- df |> filter(relationship == rel_type)
  if (!is.null(concept_ids)) d <- d |> filter(concept_id %in% concept_ids)
  choices <- d |>
    group_by(target_concept_id, target_concept_name) |>
    summarise(n = n_distinct(concept_id), .groups = "drop") |>
    arrange(desc(n), target_concept_name)
  if (nrow(choices) == 0) return(character(0))
  setNames(
    as.character(choices$target_concept_id),
    paste0(choices$target_concept_name, "  (", choices$n, ")")
  )
}

get_chapter_choices <- function(df, chapter_type_val, concept_ids = NULL) {
  d <- df |> filter(chapter_type == chapter_type_val, chapter_level == 1L)
  if (!is.null(concept_ids)) d <- d |> filter(concept_id %in% concept_ids)
  choices <- d |>
    group_by(chapter_id, chapter_name) |>
    summarise(n = n_distinct(concept_id), .groups = "drop") |>
    filter(n >= 1L) |>
    arrange(desc(n), chapter_name)
  if (nrow(choices) == 0) return(character(0))
  setNames(
    as.character(choices$chapter_id),
    paste0(choices$chapter_name, "  (", choices$n, ")")
  )
}

get_subchapter_choices <- function(df, chapter_type_val, sel_L1_ids = NULL,
                                   concept_ids = NULL) {
  d <- df |> filter(chapter_type == chapter_type_val, chapter_level == 2L)
  if (!is.null(sel_L1_ids) && length(sel_L1_ids) > 0) {
    d <- d |> filter(parent_chapter_id %in% as.integer(sel_L1_ids))
  }
  if (!is.null(concept_ids)) d <- d |> filter(concept_id %in% concept_ids)
  choices <- d |>
    group_by(chapter_id, chapter_name) |>
    summarise(n = n_distinct(concept_id), .groups = "drop") |>
    filter(n >= 1L) |>
    arrange(desc(n), chapter_name)
  if (nrow(choices) == 0) return(character(0))
  setNames(
    as.character(choices$chapter_id),
    paste0(choices$chapter_name, "  (", choices$n, ")")
  )
}

has_subchapters <- function(df, chapter_type_val) {
  any(df$chapter_type == chapter_type_val & df$chapter_level == 2L)
}

get_chapter_concept_ids <- function(df, chapter_type_val, selected) {
  if (length(selected) == 0) return(NULL)
  df |>
    filter(chapter_type == chapter_type_val,
           chapter_id %in% as.integer(selected)) |>
    pull(concept_id) |>
    unique()
}

get_chapter_scope <- function(chapters_df, class_type, sel_L1, sel_L2 = NULL) {
  l1_ids <- if (length(sel_L1) > 0) {
    get_chapter_concept_ids(chapters_df, class_type, sel_L1)
  } else {
    NULL
  }
  l2_ids <- if (length(sel_L2) > 0) {
    get_chapter_concept_ids(chapters_df, class_type, sel_L2)
  } else {
    NULL
  }
  if (is.null(l1_ids) && is.null(l2_ids)) return(NULL)
  if (!is.null(l2_ids)) return(l2_ids)
  l1_ids
}

# ── UI Helper Functions ──────────────────────────────────────────────────────

info_icon <- function(tooltip) {
  tags$span(class = "filter-info", `data-tooltip` = tooltip, "i")
}

filter_label <- function(text, tooltip, counter_id) {
  tags$span(
    text, " ",
    info_icon(tooltip),
    " ",
    uiOutput(counter_id, inline = TRUE)
  )
}
