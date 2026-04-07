# ── Syrona: Extraction Constants ──────────────────────────────────────────────
#
# Domain-specific concept IDs, relationship types, and thresholds
# used by the extraction pipeline.

# ── Shared constants ────────────────────────────────────────────────────────

#' Gender concept_id to label mapping.
#' @keywords internal
GENDER_LABELS <- c("8532" = "F", "8507" = "M")

#' Age decade clamping: all ages >= this value merge into a single "80+" group.
#' @keywords internal
AGE_CLAMP_MAX <- 80L

#' k-anonymity threshold. Any stratum cell with fewer patients is suppressed.
#' @keywords internal
K_ANONYMITY <- 5L

# ── Condition domain ────────────────────────────────────────────────────────

#' Anchor concept_ids for condition chapter assignment.
#' @keywords internal
CHAPTER_ROOTS <- list(
  body_system      = 4180628L,
  disease_category = 4274025L
)

#' SNOMED relationship types to extract as condition attributes.
#' @keywords internal
CONDITION_RELATIONSHIPS <- c(
  finding_site    = "Has finding site",
  morphology      = "Has asso morph",
  clinical_course = "Has clinical course",
  causative_agent = "Has causative agent",
  occurrence      = "Has occurrence",
  pathology       = "Has pathology"
)

# ── Procedure domain ───────────────────────────────────────────────────────

#' Anchor concept_ids for procedure chapter assignment.
#' @keywords internal
PROCEDURE_CHAPTER_ROOTS <- list(
  by_method = 4029205L,
  by_site   = 4180627L
)

#' SNOMED relationship types to extract as procedure attributes.
#' @keywords internal
PROCEDURE_RELATIONSHIPS <- c(
  procedure_site = "Has dir proc site",
  method         = "Has method"
)

# ── Drug domain ─────────────────────────────────────────────────────────────

#' ATC vocabulary constants for chapter lookup.
#' @keywords internal
ATC_CHAPTER_VOCAB <- "ATC"

#' @keywords internal
ATC_CHAPTER_CLASS <- "ATC 1st"

#' Drug attribute relationships (empty - RxNorm relationships are structural).
#' @keywords internal
DRUG_RELATIONSHIPS <- character(0)

# ── Output directories ──────────────────────────────────────────────────────

#' Base directory for extracted source data.
#' @keywords internal
SOURCES_DIR <- "data/sources"
