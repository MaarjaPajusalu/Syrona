#' syrona: Stratified prevalence comparison across OMOP CDM datasets
#'
#' Extracts stratified prevalence tables from OMOP CDM databases
#' (conditions, procedures, drugs), computes log2 prevalence ratios between
#' paired datasets, and synthesizes them via random-effects meta-analysis
#' at multiple aggregation levels (year, age group, sex).
#'
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom rlang `%||%`
"_PACKAGE"
