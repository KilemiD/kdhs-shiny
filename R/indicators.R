# =============================================================================
# R/indicators.R
# Headline KDHS 2022 indicators, computed to match the published report.
# =============================================================================
# Every function here was checked against the KDHS 2022 Key Indicators Report
# (02-data/kdhs-2022/docs/Kenya-Demographic-and-Health-Survey-2022-Key-Indicators-Report.pdf).
# Run R/validate_indicators.R to re-run that comparison.
#
# The denominators are the fiddly part and they are NOT interchangeable — using
# the wrong one is how this dashboard previously reported 46.6% contraceptive
# prevalence against a published 63%. Each function documents its own.
# =============================================================================

num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# ---- FILTERS ACROSS RECODES -------------------------------------------------
# The recodes name the same background characteristics differently: the women's,
# children's and birth files use v024/v025/v190, the household-member file uses
# hv024/hv025/hv270. This maps one set of dashboard filter values onto whichever
# design it is handed, and silently skips a filter the design cannot support
# (e.g. woman's age group has no meaning in the household-member file).
INDICATOR_VARS <- list(
  ir = c(county = "v024",  residence = "v025",  wealth = "v190",  age = "v013"),
  kr = c(county = "v024",  residence = "v025",  wealth = "v190"),
  br = c(county = "v024",  residence = "v025",  wealth = "v190"),
  pr = c(county = "hv024", residence = "hv025", wealth = "hv270")
)

#' Apply the shared dashboard filters to any recode's design.
#' @param design  srvyr design
#' @param recode  one of "ir", "kr", "br", "pr"
#' @param filters named list with any of county/residence/wealth/age; "All" is a no-op
apply_filters <- function(design, recode, filters = list()) {
  map <- INDICATOR_VARS[[recode]]
  if (is.null(map)) stop("Unknown recode: ", recode)
  for (key in names(filters)) {
    # `map` is a named character vector, so map[[key]] THROWS "subscript out of
    # bounds" for a name it does not have — it does not return NULL the way a
    # list would. Check membership first; a missing key just means this recode
    # has no such column (e.g. woman's age group in the household-member file).
    if (!key %in% names(map)) next
    col <- map[[key]]
    if (is.na(col) || !col %in% names(design$variables)) next
    design <- svy_filter_eq(design, col, filters[[key]])
  }
  design
}

# ---- CHILDHOOD MORTALITY ----------------------------------------------------
# DHS age segments for the synthetic-cohort (direct) estimate, in months.
MORT_SEGMENTS <- list(c(0, 0), c(1, 2), c(3, 5), c(6, 11),
                      c(12, 23), c(24, 35), c(36, 47), c(48, 59))

#' Childhood mortality rates by the DHS direct (synthetic cohort) method.
#'
#' This is NOT a simple proportion of births that died. A child born last year
#' cannot yet have died before age 5, so a raw proportion is biased downward by
#' right-censoring. DHS instead computes a mortality probability for each age
#' segment from the children actually exposed to that segment during the
#' reference window, then chains them:  q = 1 - prod(1 - q_i).
#'
#' Reference window is the 60 months before interview ("0-4 years preceding the
#' survey"), which is what the report's headline rates use. Exposure is whole for
#' a segment lying entirely inside the window and half for one that straddles a
#' boundary, per the Guide to Statistics approximation.
#'
#' @param br_df  the birth-recode data frame (one row per live birth)
#' @param months_back  window length in months (60 = 0-4 years preceding)
#' @return list with neonatal, infant, child and under5 rates per 1,000
dhs_child_mortality <- function(br_df, months_back = 60) {
  need <- c("v008", "b3", "b5", "b7", "v005")
  if (!all(need %in% names(br_df)) || nrow(br_df) == 0) {
    return(list(neonatal = NA_real_, infant = NA_real_,
                child = NA_real_, under5 = NA_real_, n_births = 0L))
  }
  v008 <- num(br_df$v008); b3 <- num(br_df$b3)
  wt   <- num(br_df$v005) / 1e6
  died <- tolower(as.character(br_df$b5)) == "no"
  b7   <- num(br_df$b7)                     # age at death, months
  hi <- v008; lo <- v008 - months_back

  q <- vapply(MORT_SEGMENTS, function(seg) {
    a <- seg[1]; b <- seg[2]
    seg_lo <- b3 + a; seg_hi <- b3 + b
    inside  <- seg_lo >= lo & seg_hi <  hi
    partial <- !inside & seg_hi >= lo & seg_lo < hi
    expo <- ifelse(inside, 1, ifelse(partial, 0.5, 0))
    # a child who died before reaching this segment was never exposed to it
    expo[died & !is.na(b7) & b7 < a] <- 0
    deaths <- died & !is.na(b7) & b7 >= a & b7 <= b
    denom  <- sum(wt * expo, na.rm = TRUE)
    if (!is.finite(denom) || denom <= 0) return(NA_real_)
    sum(wt * ifelse(deaths, expo, 0), na.rm = TRUE) / denom
  }, numeric(1))

  chain <- function(idx) {
    qi <- q[idx]
    if (all(is.na(qi))) return(NA_real_)
    (1 - prod(1 - qi[!is.na(qi)])) * 1000
  }
  list(
    neonatal = chain(1),      # first month
    infant   = chain(1:4),    # 1q0, under 12 months
    child    = chain(5:8),    # 4q1, ages 1-4
    under5   = chain(1:8),    # 5q0
    n_births = nrow(br_df)
  )
}

# ---- CONTRACEPTIVE PREVALENCE ----------------------------------------------
#' Contraceptive prevalence rate.
#'
#' DENOMINATOR MATTERS. The report's headline CPR (63% any / 57% modern) is among
#' CURRENTLY MARRIED women 15-49. Over all women it is roughly 46%, which is a
#' valid but different figure and does not appear in the report.
#'
#' @param design  IR design
#' @param married restrict to currently married/in union (TRUE = report definition)
#' @param modern  TRUE for modern methods only, FALSE for any method
indicator_cpr <- function(design, married = TRUE, modern = FALSE) {
  if (married) {
    design <- design |>
      filter(as.character(v502) == "currently in union/living with a man")
  }
  if (!svy_has_rows(design)) return(NA_real_)
  design |>
    mutate(.x = if (modern) as.numeric(v313 == "modern method")
                else        as.numeric(v313 != "no method")) |>
    summarise(v = survey_mean(.x, na.rm = TRUE)) |>
    pull(v) * 100
}

# ---- ANTENATAL CARE ---------------------------------------------------------
#' Percentage with 4+ ANC visits for the most recent birth.
#'
#' Denominator: women with a live birth or stillbirth in the 2 years before the
#' survey (v222 <= 23 months since last birth). "Don't know" number of visits
#' stays IN the denominator and counts as not-4+; dropping it instead returns
#' 67.3% against the published 66%.
indicator_anc4 <- function(design) {
  design <- design |> mutate(.mo = num(v222)) |> filter(!is.na(.mo), .mo <= 23)
  if (!svy_has_rows(design)) return(NA_real_)
  design |>
    filter(!is.na(m14_1)) |>
    mutate(.x = as.numeric(!is.na(num(m14_1)) & num(m14_1) >= 4)) |>
    summarise(v = survey_mean(.x, na.rm = TRUE)) |>
    pull(v) * 100
}

#' Percentage of births in the last 2 years delivered by a skilled provider.
#' m3a = doctor, m3b = nurse/midwife/clinical officer. m3c is "NA - ..." and
#' entirely empty — including it forces the estimate to a flat 100%.
indicator_sba <- function(design) {
  design <- design |> mutate(.mo = num(v222)) |> filter(!is.na(.mo), .mo <= 23)
  if (!svy_has_rows(design)) return(NA_real_)
  design |>
    filter(!is.na(m3a_1)) |>
    mutate(.x = as.numeric(as.character(m3a_1) == "yes" |
                           as.character(m3b_1) == "yes")) |>
    summarise(v = survey_mean(.x, na.rm = TRUE)) |>
    pull(v) * 100
}

# ---- CHILD NUTRITION --------------------------------------------------------
#' Percentage of children under 5 below -2 SD on a WHO growth index.
#' `flag` is one of the columns derived in preprocess.R: stunted (height-for-age),
#' wasted (weight-for-height), underweight (weight-for-age). Out-of-range DHS
#' values are already removed there.
indicator_nutrition <- function(design, flag = "stunted") {
  if (!flag %in% names(design$variables)) return(NA_real_)
  design <- design |> filter(!is.na(.data[[flag]]))
  if (!svy_has_rows(design)) return(NA_real_)
  design |>
    summarise(v = survey_mean(.data[[flag]], na.rm = TRUE)) |>
    pull(v) * 100
}

# ---- HEALTH INSURANCE -------------------------------------------------------
#' Percentage of the household population covered by any health insurance.
#'
#' This is a PER-PERSON measure on the household-member (PR) recode — the report
#' gives 26.0% of females and 26.5% of males. It is not available in the women's
#' file at all: IR v481 is labelled "NA - Covered by health insurance" and is
#' 100% missing, which is why this card used to read 0%.
#'
#' @param design PR design
#' @param sex    NULL for both sexes, or "female" / "male"
indicator_insurance <- function(design, sex = NULL) {
  if (!"insured" %in% names(design$variables)) return(NA_real_)
  design <- design |> filter(!is.na(insured))
  if (!is.null(sex)) design <- design |> filter(as.character(sex) == !!sex)
  if (!svy_has_rows(design)) return(NA_real_)
  design |>
    summarise(v = survey_mean(insured, na.rm = TRUE)) |>
    pull(v) * 100
}
