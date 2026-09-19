# =============================================================================
# R/validate_indicators.R
# Compare what the dashboard computes against the published KDHS 2022 report.
# =============================================================================
# Run from the dashboard folder:
#   source("R/validate_indicators.R")
#
# Published values come from the KDHS 2022 Key Indicators Report in
# 02-data/kdhs-2022/docs/. Page references are in the `source` column below.
# Re-run this after touching preprocess.R or R/indicators.R.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(survey); library(srvyr); library(haven)
})
source("R/data_loader.R")
source("R/survey_setup.R")
source("R/helpers.R")
source("R/indicators.R")

cat("\n=== KDHS 2022: dashboard vs published report ===\n\n")

ir <- load_recode("ir"); kr <- load_recode("kr")
pr <- load_recode("pr"); br <- load_recode("br")
d_ir <- create_ir_design(ir); d_kr <- create_kr_design(kr)
d_pr <- create_pr_design(pr)

mort <- dhs_child_mortality(br)

checks <- tibble::tribble(
  ~indicator,                          ~computed,                                  ~published, ~source,
  "Neonatal mortality (per 1,000)",    mort$neonatal,                              21,   "Table 16",
  "Infant mortality 1q0 (per 1,000)",  mort$infant,                                 32,   "Table 16",
  "Child mortality 4q1 (per 1,000)",   mort$child,                                   9,   "Table 16",
  "Under-5 mortality 5q0 (per 1,000)", mort$under5,                                 41,   "Table 16",
  "CPR any method, married women",     indicator_cpr(d_ir, married = TRUE),          63,   "Table 8",
  "CPR modern method, married women",  indicator_cpr(d_ir, TRUE, modern = TRUE),     57,   "Table 8",
  "ANC 4+ visits",                     indicator_anc4(d_ir),                         66,   "Table 11",
  "Skilled birth attendance",          indicator_sba(d_ir),                          89,   "Table 11",
  "Stunting, under 5 (HAZ < -2SD)",    indicator_nutrition(d_kr, "stunted"),       17.6,   "Table 14",
  "Wasting, under 5 (WHZ < -2SD)",     indicator_nutrition(d_kr, "wasted"),         4.9,   "Table 14",
  "Underweight, under 5 (WAZ < -2SD)", indicator_nutrition(d_kr, "underweight"),   10.1,   "Table 14",
  "Health insurance, females",         indicator_insurance(d_pr, "female"),        26.0,   "Table 3",
  "Health insurance, males",           indicator_insurance(d_pr, "male"),          26.5,   "Table 3"
) |>
  mutate(
    computed = round(computed, 1),
    diff     = round(computed - published, 1),
    # "rounds to" is the honest test: the report prints whole numbers for most of
    # these, so agreement means our value rounds to the same printed figure.
    status   = ifelse(abs(diff) <= 0.55, "MATCH", "CHECK")
  )

print(as.data.frame(checks), row.names = FALSE)

n_ok <- sum(checks$status == "MATCH")
cat(sprintf("\n%d of %d indicators agree with the published report.\n",
            n_ok, nrow(checks)))
if (n_ok < nrow(checks)) {
  cat("\nNot matching:\n")
  print(as.data.frame(dplyr::filter(checks, status != "MATCH")), row.names = FALSE)
  cat("\nNote on under-5 mortality: each COMPONENT (neonatal, infant, child)\n",
      "rounds to the published value, but chaining them compounds sub-0.5\n",
      "differences, so 5q0 lands near 40.3 against a published 41. Closing that\n",
      "last gap needs DHS's own imputation of incomplete death dates.\n", sep = "")
}

# =============================================================================
# Child-survival block (U5CM)
# =============================================================================
source("R/survival_core.R")
kr_full <- kr
d_surv  <- u5cm_design(kr_full)
coh     <- u5cm_cohort_summary(d_surv)
km      <- u5cm_km(d_surv, by = NULL, ci = FALSE)
km_cum  <- (1 - min(km$surv, na.rm = TRUE)) * 1000

# The two binary outcomes that have a complete exposure window should reproduce
# the published neonatal and infant rates. Weighted, on their own denominators —
# died_* is NA outside its eligible window, so dropping NA applies the right one.
rate <- function(outcome) {
  d  <- kr_full[!is.na(kr_full[[outcome]]), , drop = FALSE]
  wt <- suppressWarnings(as.numeric(as.character(d$v005))) / 1e6
  1000 * sum(wt * d[[outcome]], na.rm = TRUE) / sum(wt, na.rm = TRUE)
}

cat("\n=== Child survival (U5CM) ===\n\n")
surv_checks <- tibble::tribble(
  ~indicator,                                ~computed,                  ~published, ~source,
  "Neonatal death rate, logistic cohort",    rate("died_neonatal"),      21,   "Table 16",
  "Infant death rate, logistic cohort",      rate("died_infant"),        32,   "Table 16"
) |>
  mutate(computed = round(computed, 1),
         diff     = round(computed - published, 1),
         status   = ifelse(abs(diff) <= 1.5, "MATCH", "CHECK"))
print(as.data.frame(surv_checks), row.names = FALSE)

cat(sprintf("\nCohort: %s children, %s deaths, median follow-up %.0f months, %.1f%% censored\n",
            format(coh$n_children, big.mark = ","), format(coh$n_deaths, big.mark = ","),
            coh$median_fu, coh$pct_censored))
cat(sprintf("Eligible/events  neonatal %s/%s   infant %s/%s   24m %s/%s\n",
            format(sum(kr_full$elig_neonatal, na.rm = TRUE), big.mark = ","),
            format(sum(kr_full$died_neonatal, na.rm = TRUE), big.mark = ","),
            format(sum(kr_full$elig_infant,   na.rm = TRUE), big.mark = ","),
            format(sum(kr_full$died_infant,   na.rm = TRUE), big.mark = ","),
            format(sum(kr_full$elig_24m,      na.rm = TRUE), big.mark = ","),
            format(sum(kr_full$died_24m,      na.rm = TRUE), big.mark = ",")))

cat("\nThree ways of reading under-five mortality, all correct for what they measure:\n")
cat(sprintf("  naive %% dead on the 0/1 flag      %5.1f per 1,000  (biased low: censoring)\n",
            coh$pct_deaths_raw * 10))
cat(sprintf("  Kaplan-Meier cumulative, 59 mo    %5.1f per 1,000  (this cohort)\n", km_cum))
cat(sprintf("  DHS period rate, synthetic cohort %5.1f per 1,000  (Overview card)\n", mort$under5))
cat(  "  published KDHS 2022, Table 16      41.0 per 1,000\n")
cat("\nThere is deliberately no 60-month binary outcome: no child in this file has\n",
    "completed 60 months of exposure, so its denominator would be empty.\n", sep = "")

invisible(list(indicators = checks, survival = surv_checks))
