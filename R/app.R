# ── Syrona Dashboard Launcher ─────────────────────────────────────────────────

#' Run the Syrona dashboard
#'
#' Launches the Shiny dashboard for exploring prevalence comparisons.
#' The app looks for \code{data/sources/} and \code{data/comparisons/}
#' relative to your current working directory. Run this from the directory
#' that contains your \code{data/} folder.
#'
#' @param data_dir Path to the directory containing \code{data/sources/} and
#'   \code{data/comparisons/}. Defaults to the current working directory.
#' @param port Port to run the app on (default: auto-select).
#' @param launch.browser Whether to open a browser window (default: TRUE).
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @export
run_app <- function(data_dir = getwd(), port = NULL, launch.browser = TRUE, ...) {
  app_dir <- system.file("shiny", package = "syrona")
  if (app_dir == "") {
    stop("Dashboard not found. Is the syrona package installed correctly?",
         call. = FALSE)
  }

  # Store the user's data directory so global.R can find the data
  options(syrona.data_dir = normalizePath(data_dir, mustWork = TRUE))

  args <- list(appDir = app_dir, launch.browser = launch.browser, ...)
  if (!is.null(port)) args$port <- port

  do.call(shiny::runApp, args)
}
