# ── Build syrona-web from the canonical syrona/ package ──────────────────────
#
# Generates the flat Shiny app at ../syrona-web/ from the canonical sources
# in this package, so that omop-apps Shiny Server can serve it without an R
# package install. There is no transformation step: every copied file is
# byte-identical to the package version.
#
# Run this from the syrona/ package root after every change you want to ship
# to the omop-apps GitLab deployment:
#
#   Rscript scripts/build_web.R
#   cd ../syrona-web && git add -A && git status   # review the diff
#   git commit -m "Sync from syrona <commit-sha>"
#   git push origin main:master
#
# What gets copied:
#   - inst/shiny/{global,server,ui}.R  ->  syrona-web/{global,server,ui}.R
#   - R/{8 dashboard helpers}.R         ->  syrona-web/R/
#   - data/                             ->  syrona-web/data/   (rsync)
#
# What does NOT get copied (package-only):
#   - R/app.R         (run_app entry point)
#   - R/connect.R     (DB connection helpers, only used in extraction)
#   - R/cohort.R      (cohort helpers, only used in extraction)
#   - DESCRIPTION, NAMESPACE, man/, vignettes/, tests/, _pkgdown.yml
#
# What stays in syrona-web/ untouched:
#   - .git/, .gitignore, rsconnect/

# ── Paths ────────────────────────────────────────────────────────────────────
PKG <- normalizePath(".", mustWork = TRUE)
WEB <- normalizePath(file.path("..", "syrona-web"), mustWork = TRUE)

stopifnot(
  file.exists(file.path(PKG, "DESCRIPTION")),
  file.exists(file.path(WEB, ".gitignore"))
)

# Helper R/ files that the dashboard sources at startup. The package-only
# files (app.R, connect.R, cohort.R) are intentionally excluded.
WEB_R_FILES <- c(
  "constants.R",
  "config.R",
  "extract.R",
  "compare.R",
  "plot_heatmaps.R",
  "plot_forests.R",
  "plot_demography.R",
  "plot_pr_distribution.R"
)

# ── 1. R/ helpers ────────────────────────────────────────────────────────────
dir.create(file.path(WEB, "R"), showWarnings = FALSE, recursive = TRUE)
for (f in WEB_R_FILES) {
  src <- file.path(PKG, "R", f)
  dst <- file.path(WEB, "R", f)
  if (!file.exists(src)) stop("Missing helper in package: R/", f)
  file.copy(src, dst, overwrite = TRUE)
  cat("  R/", f, "\n", sep = "")
}

# ── 2. Shiny app entry files (global, server, ui) ────────────────────────────
for (f in c("global.R", "server.R", "ui.R")) {
  src <- file.path(PKG, "inst", "shiny", f)
  dst <- file.path(WEB, f)
  if (!file.exists(src)) stop("Missing shiny file in package: inst/shiny/", f)
  file.copy(src, dst, overwrite = TRUE)
  cat("  ", f, "\n", sep = "")
}

# ── 3. Data folder via rsync ─────────────────────────────────────────────────
# rsync only copies changed files (checksums + mtime), so this is fast on
# repeat builds. --delete keeps syrona-web/data/ from accumulating stale files
# that have been removed in the package, but never touches anything outside
# data/ in syrona-web (so .git/, rsconnect/, etc. are safe).
data_src <- paste0(file.path(PKG, "data"), "/")
data_dst <- paste0(file.path(WEB, "data"), "/")
cat("  rsync data/ ...\n")
status <- system2(
  "rsync",
  args = c("-a", "--delete", shQuote(data_src), shQuote(data_dst)),
  stdout = "", stderr = ""
)
if (status != 0) stop("rsync failed with status ", status)

# ── Done ─────────────────────────────────────────────────────────────────────
cat("\n[OK] syrona-web rebuilt from syrona/\n")
cat("\nNext steps:\n")
cat("  cd ", WEB, "\n", sep = "")
cat("  git status                   # review the diff\n")
cat("  git add -A && git commit\n")
cat("  git push origin main:master\n")
