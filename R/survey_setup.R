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

# ---- HOUSEHOLD MEMBERS (PR) -------------------------------------------------
#' Create survey design for the Household Members Recode (one row per person).
#' Source for the Washington Group disability questions and insurance type.
create_pr_design <- function(pr) {
  pr |>
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
  # hv027 flags the half-sample selected for the men's survey, which is also the
  # sub-sample asked the extra KDHS 2022 modules (19,747 of 37,911 households).
  #
  # preprocess.R converts labelled values to FACTORS, so hv027 has the levels
  # "not selected" / "men's survey" / "husband's survey" — comparing it to the
  # numeric 1 matched nothing and silently returned an empty design.
  # as.character() makes this work whether hv027 arrives as a factor or as codes.
  in_subsample <- quote(as.character(hv027) %in% c("men's survey", "1"))
  half_sample_modules <- list(
    disability = in_subsample,
    chronic    = in_subsample,
    covid      = in_subsample,
    food       = in_subsample,
    insurance  = in_subsample
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

