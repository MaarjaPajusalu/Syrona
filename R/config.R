# ── Syrona Design Tokens & Helpers ─────────────────────────────────────────────
#
# Color constants, thresholds, and formatting functions used across the
# Syrona dashboard. Derived from docs/technical/DESIGN_TOKENS.md.

# ── Color Tokens ──────────────────────────────────────────────────────────────

# Dataset palette (role-based: blue = reference, orange = comparison)
COLOR_DS_REF        <- "#005FC8"
COLOR_DS_COMP       <- "#FF6600"
COLOR_DS_REF_ALPHA  <- "rgba(0, 95, 200, 0.08)"
COLOR_DS_COMP_ALPHA <- "rgba(255, 102, 0, 0.08)"

# Sex encoding
COLOR_SEX_F         <- "#C44D88"
COLOR_SEX_M         <- "#5B50C9"
COLOR_SEX_BOTH      <- "#6B7B8D"
COLOR_SEX_F_ALPHA   <- "rgba(196, 77, 136, 0.10)"
COLOR_SEX_M_ALPHA   <- "rgba(91, 80, 201, 0.10)"

# Significance
COLOR_SIG           <- "#1A1A1A"
COLOR_NOSIG         <- "#8C8C8C"

# UI Chrome
COLOR_INK           <- "#1A1A1A"
COLOR_INK_DATA      <- "#333333"
COLOR_INK_MUTED     <- "#666666"
COLOR_INK_SUBTLE    <- "#767676"
COLOR_BG            <- "#F7F8FA"
COLOR_SURFACE       <- "#FFFFFF"
COLOR_BORDER        <- "#E2E2DE"
COLOR_SPINNER       <- "#3D8FBE"

# ── Constants ─────────────────────────────────────────────────────────────────

FOLD_THRESHOLD      <- 1.3
CONCEPT_NAME_MAX    <- 50
NOSIG_ALPHA         <- 0.45

# Population weight gradient (light gray -> dark charcoal)
COLOR_WEIGHT_LO     <- "#D8D8D8"
COLOR_WEIGHT_HI     <- "#2D2D2D"

#' Load all Syrona design tokens into the calling environment
#'
#' Makes color constants, thresholds, and other design tokens available
#' as variables. Used by the Shiny dashboard's global.R.
#'
#' @param envir Environment to load into (default: caller's environment).
#' @export
load_syrona_theme <- function(envir = parent.frame()) {
  tokens <- list(
    COLOR_DS_REF = COLOR_DS_REF, COLOR_DS_COMP = COLOR_DS_COMP,
    COLOR_DS_REF_ALPHA = COLOR_DS_REF_ALPHA, COLOR_DS_COMP_ALPHA = COLOR_DS_COMP_ALPHA,
    COLOR_SEX_F = COLOR_SEX_F, COLOR_SEX_M = COLOR_SEX_M, COLOR_SEX_BOTH = COLOR_SEX_BOTH,
    COLOR_SEX_F_ALPHA = COLOR_SEX_F_ALPHA, COLOR_SEX_M_ALPHA = COLOR_SEX_M_ALPHA,
    COLOR_SIG = COLOR_SIG, COLOR_NOSIG = COLOR_NOSIG,
    COLOR_INK = COLOR_INK, COLOR_INK_DATA = COLOR_INK_DATA,
    COLOR_INK_MUTED = COLOR_INK_MUTED, COLOR_INK_SUBTLE = COLOR_INK_SUBTLE,
    COLOR_BG = COLOR_BG, COLOR_SURFACE = COLOR_SURFACE,
    COLOR_BORDER = COLOR_BORDER, COLOR_SPINNER = COLOR_SPINNER,
    COLOR_WEIGHT_LO = COLOR_WEIGHT_LO, COLOR_WEIGHT_HI = COLOR_WEIGHT_HI,
    FOLD_THRESHOLD = FOLD_THRESHOLD, CONCEPT_NAME_MAX = CONCEPT_NAME_MAX,
    NOSIG_ALPHA = NOSIG_ALPHA
  )
  list2env(tokens, envir = envir)
  invisible(NULL)
}

# ── Formatting Helpers ────────────────────────────────────────────────────────

#' Format a log2 prevalence ratio as a fold difference string
#'
#' Vectorized. For values < 1, shows reciprocal in parentheses.
#'
#' @param log2_val Numeric vector of log2 PR values.
#' @return Character vector of formatted fold strings.
#' @export
format_fold <- function(log2_val) {
  fold <- 2^log2_val
  ifelse(
    is.na(fold), "\u2014",
    ifelse(fold >= 1,
           sprintf("%.2f", fold),
           sprintf("%.2f (%.1f\u00d7)", fold, 1 / fold))
  )
}

#' Format p-values in Nature style
#'
#' Vectorized. Shows < 0.001 for very small values.
#'
#' @param p Numeric vector of p-values.
#' @return Character vector of formatted p-value strings.
#' @export
format_pval <- function(p) {
  ifelse(
    is.na(p), "\u2014",
    ifelse(p < 0.001, "< 0.001",
           ifelse(p < 0.1, sprintf("%.3f", p),
                  sprintf("%.2f", p)))
  )
}

#' Truncate a concept name to a maximum number of characters
#'
#' Adds "..." if truncated.
#'
#' @param name Character vector of names.
#' @param max Maximum character length (default CONCEPT_NAME_MAX).
#' @return Character vector of (possibly truncated) names.
#' @export
truncate_name <- function(name, max = CONCEPT_NAME_MAX) {
  ifelse(
    nchar(name) > max,
    paste0(substr(name, 1, max - 3), "..."),
    name
  )
}

#' Map population weight (%) to a hex color on a log-scale gradient
#'
#' Light gray for rare concepts, dark charcoal for common ones.
#'
#' @param pct Numeric vector of population weight percentages (0-100).
#' @return Character vector of hex color strings.
#' @export
weight_color <- function(pct) {
  log_val <- log10(pmax(pct, 0.01))
  t <- pmin(pmax((log_val + 1) / 2.5, 0), 1)
  lo <- grDevices::col2rgb(COLOR_WEIGHT_LO) / 255
  hi <- grDevices::col2rgb(COLOR_WEIGHT_HI) / 255
  r <- lo[1, ] + t * (hi[1, ] - lo[1, ])
  g <- lo[2, ] + t * (hi[2, ] - lo[2, ])
  b <- lo[3, ] + t * (hi[3, ] - lo[3, ])
  grDevices::rgb(r, g, b)
}
