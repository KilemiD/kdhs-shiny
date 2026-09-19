## Setup for a fresh machine
## ---------------------------------------------------------------------------
## Everything else this project needs travels on the external SSD -- the code,
## the raw Stata files, the six processed .rds, the .Rproj and .git/config
## (which already carries the commit identity and the GitHub remote).
##
## The R package library does NOT travel: it lives under the user profile of
## whichever machine installed it. This script rebuilds it.
##
## Run once, from the dashboard/ folder, with the .Rproj open in RStudio:
##   source("setup_new_machine.R")
## Expect 10-20 minutes. sf and spdep are the slow ones.

required <- c(
  # app framework
  "shiny", "bslib", "htmltools", "shinycssloaders", "DT",
  # data handling
  "haven", "dplyr", "tidyr", "purrr", "tibble", "stringr", "glue",
  # survey estimation -- the core of every figure in this dashboard
  "survey", "srvyr",
  # modelling
  "broom",
  # plotting
  "ggplot2", "plotly", "scales", "RColorBrewer",
  # spatial
  "sf", "leaflet", "spdep"
)
# survival ships with R as a recommended package, so it is not installed here.

missing <- setdiff(required, rownames(installed.packages()))

if (length(missing) == 0) {
  cat("All", length(required), "packages already present.\n")
} else {
  cat("Installing", length(missing), "of", length(required), "packages:\n  ",
      paste(missing, collapse = ", "), "\n\n")
  install.packages(missing)
}

## Verify every package actually loads, not merely that it installed.
## A package can install and still fail to load if a system dependency is
## absent -- sf is the usual culprit.
cat("\nLoad check:\n")
failed <- character(0)
for (p in required) {
  ok <- suppressWarnings(suppressMessages(
    requireNamespace(p, quietly = TRUE)
  ))
  if (!ok) failed <- c(failed, p)
  cat(sprintf("  %-18s %s\n", p, if (ok) "ok" else "FAILED TO LOAD"))
}

cat("\n")
if (length(failed)) {
  cat("These failed and the app will not start without them:\n  ",
      paste(failed, collapse = ", "), "\n")
} else {
  cat("All packages load. Next:\n")
  cat("  1. Confirm the data is visible -- source('local_test_script.R')\n")
  cat("  2. Start the app -- shiny::runApp()\n")
  cat("  3. Optional, confirms the figures still match the published report:\n")
  cat("     source('R/validate_indicators.R')\n")
}

cat("\nR version on this machine:", R.version.string, "\n")
cat("Library path:", .libPaths()[1], "\n")
