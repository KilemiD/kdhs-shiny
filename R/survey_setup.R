# =============================================================================
# R/survey_setup.R
# Sets up complex survey design objects for each recode file.
# This is the most critical file — all estimates MUST use these objects.
#
# DHS survey design components:
#   - Weight:  v005 / 1,000,000  (normalize to person-weight)
#   - Strata:  v023               (sampling strata)
#   - PSU:     v021               (primary sampling unit / cluster)
# =============================================================================

library(survey)
library(srvyr)

# ---- WOMEN (IR) -------------------------------------------------------------
#' Create survey design for Women's Recode (IR)
#' @param ir A data frame loaded from load_recode("ir")
#' @return srvyr survey design object
create_ir_design <- function(ir) {
  ir |>
    mutate(wt = as.numeric(v005) / 1e6) |>
    as_survey_design(
      ids     = v021,     # PSU
      strata  = v023,     # Strata
      weights = wt,       # Normalized weight
      nest    = TRUE      # PSUs are nested within strata
    )
}

# ---- MEN (MR) ---------------------------------------------------------------
#' Create survey design for Men's Recode (MR)
create_mr_design <- function(mr) {
  mr |>
    mutate(wt = as.numeric(mv005) / 1e6) |>
    as_survey_design(
      ids     = mv021,
      strata  = mv023,
      weights = wt,
      nest    = TRUE
    )
}

# ---- CHILDREN (KR) ----------------------------------------------------------
#' Create survey design for Children's Recode (KR)
create_kr_design <- function(kr) {
  kr |>
    mutate(wt = as.numeric(v005) / 1e6) |>
    as_survey_design(
      ids     = v021,
      strata  = v023,
      weights = wt,
      nest    = TRUE
    )
}

# ---- BIRTH RECODE (BR) ------------------------------------------------------
create_br_design <- function(br) {
  br |>
    mutate(wt = as.numeric(v005) / 1e6) |>
    as_survey_design(
      ids     = v021,
      strata  = v023,
      weights = wt,
      nest    = TRUE
    )
}

# ---- HOUSEHOLD (HR) ---------------------------------------------------------
create_hr_design <- function(hr) {
  hr |>
    mutate(wt = as.numeric(hv005) / 1e6) |>
    as_survey_design(
      ids     = hv021,
      strata  = hv023,
      weights = wt,
      nest    = TRUE
    )
}

# ---- HALF-SAMPLE MODULE FILTER ----------------------------------------------
#' KDHS 2022 introduced several modules collected on only HALF the sample.
#' Always filter to the relevant sub-sample before analysis.
#' Modules: disability, chronic disease, COVID-19, food expenditure.
#'
#' @param df A data frame (HR recode)
#' @param module One of: "disability", "chronic", "covid", "food"
filter_half_sample <- function(df, module) {
  # DHS typically uses a half-sample flag. Check your codebook for exact variable.
  # Common approach: households in half-sample have a specific hv027 value (0 or 1)
  # Adjust the variable name and value below to match your codebook.
  half_sample_modules <- list(
    disability = quote(hv027 == 1),
    chronic    = quote(hv027 == 1),
    covid      = quote(hv027 == 1),
    food       = quote(hv027 == 1)
  )
  filter_expr <- half_sample_modules[[module]]
  if (is.null(filter_expr)) stop("Unknown module: ", module)
  filter(df, !!filter_expr)
}

# ---- VALIDATION HELPER ------------------------------------------------------
#' Quick check: compare a weighted estimate to the published report value.
#' Use this after setup to confirm your survey design is correct.
#'
#' @param design srvyr design object
#' @param var Bare variable name (e.g., v312)
#' @param value_label Value to estimate proportion for (e.g., "Modern method")
#' @param expected_pct Published % from KDHS 2022 report (for comparison)
validate_estimate <- function(design, var, value_label, expected_pct) {
  result <- design |>
    filter(!!sym(var) == value_label) |>
    summarise(pct = survey_mean(na.rm = TRUE)) |>
    pull(pct) * 100

  cat(glue::glue(
    "Estimated: {round(result, 1)}% | ",
    "Published: {expected_pct}% | ",
    "Diff: {round(result - expected_pct, 1)} pp\n"
  ))
}

