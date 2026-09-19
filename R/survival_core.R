# =============================================================================
# R/survival_core.R
# Survival-analysis building blocks for under-five child mortality (U5CM).
# =============================================================================
# The survival object is defined in preprocess.R and matches the thesis pipeline
# (01-thesis/analysis/R/01_data_management/03_clean_data-v2.R) exactly:
#
#     dead      = child is not alive
#     surv_time = age at death if dead, else current age, clamped to [0, 59] months
#
# Everything here is SURVEY-WEIGHTED via the `survey` package: svykm(),
# svylogrank(), svycoxph(). DHS data is a stratified multi-stage sample, so the
# unweighted survival:: equivalents give the wrong standard errors.
#
# One documented exception: the county frailty model. svycoxph() refuses penalised
# terms outright ("svycoxph does not support penalised terms") and coxph() warns
# that robust variance is undefined for a penalised model. Design-based standard
# errors and a frailty term cannot be combined in this stack, so the frailty model
# is fitted with coxph(weights = wt) and its SE limitation is stated on the panel.
# =============================================================================

# ---- COVARIATE MENU ---------------------------------------------------------
# Grouped as in the Mosley-Chen framework of child-survival determinants, which
# is the structure the thesis uses. Labels are what the UI shows; values are the
# columns derived in preprocess.R.
U5CM_COVARIATES <- list(
  "Child" = c(
    "Sex"                        = "sex",
    "Twin / multiple birth"      = "twin",
    "Birth order"                = "birth_order_grp",
    "Preceding birth interval"   = "short_birth_interval",
    "Size at birth"              = "size_at_birth"
  ),
  "Maternal" = c(
    "Mother's age group"         = "maternal_age_grp",
    "Mother's education"         = "maternal_education",
    "Union status"               = "union_status"
  ),
  "Healthcare use" = c(
    "ANC 4+ visits"              = "anc_4plus",
    "Facility birth"             = "facility_birth",
    "Caesarean delivery"         = "caesarean",
    "Ever breastfed"             = "ever_breastfed"
  ),
  "Household & environment" = c(
    "Wealth quintile"            = "v190",
    "Residence"                  = "v025",
    "Improved water"             = "improved_water",
    "Improved sanitation"        = "improved_sanitation",
    "Clean cooking fuel"         = "clean_fuel",
    "Electricity"                = "electricity",
    "Distance a big problem"     = "distance_barrier"
  ),
  "Geography" = c(
    "County"                     = "county"
  )
)

#' Flat named vector of every covariate, for selectInput choices.
#'
#' unlist() on a nested list prefixes each name with its group, giving labels like
#' "Household & environment.Wealth quintile" — which then showed up as a chart
#' title. Strip the prefix back off so the label is just the covariate name.
u5cm_covariate_choices <- function() {
  flat <- unlist(U5CM_COVARIATES, use.names = TRUE)
  names(flat) <- sub("^.*?\\.", "", names(flat))
  flat
}

# ---- ANALYSIS DESIGN --------------------------------------------------------
#' Build the survey design for the child-survival analysis set.
#'
#' Drops rows with no usable survival time, and — importantly — keeps only the
#' columns the survival code actually uses. The children's file carries 126
#' columns; handing all of them to a survey design makes the object large enough
#' that svykm() consumed 7 GB and never returned on this data. Slimming it first
#' keeps every model below a second.
u5cm_design <- function(kr_df) {
  keep <- !is.na(kr_df$surv_time) & !is.na(kr_df$dead)
  df <- kr_df[keep, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  needed <- unique(c("surv_time", "dead", "v005", "v021", "v023",
                     # the fixed-horizon binary outcomes and their denominators,
                     # so the logistic and the validation script can use the
                     # same slimmed design
                     "died_neonatal", "died_infant", "died_24m",
                     "elig_neonatal", "elig_infant", "elig_24m",
                     unname(u5cm_covariate_choices())))
  needed <- intersect(needed, names(df))
  df <- df[, needed, drop = FALSE]
  df$wt <- suppressWarnings(as.numeric(as.character(df$v005))) / 1e6
  df <- df[!is.na(df$wt) & df$wt > 0, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  survey::svydesign(ids = ~v021, strata = ~v023, weights = ~wt,
                    data = df, nest = TRUE)
}

#' Headline description of the analysis cohort, for the definitions panel.
u5cm_cohort_summary <- function(design) {
  d <- design$variables
  wt <- d$wt
  list(
    n_children     = nrow(d),
    n_deaths       = sum(d$dead == 1, na.rm = TRUE),
    pct_deaths_raw = 100 * sum(wt * d$dead, na.rm = TRUE) / sum(wt, na.rm = TRUE),
    median_fu      = stats::median(d$surv_time, na.rm = TRUE),
    max_fu         = max(d$surv_time, na.rm = TRUE),
    pct_censored   = 100 * mean(d$dead == 0, na.rm = TRUE),
    n_under12      = sum(d$surv_time < 12 & d$dead == 0, na.rm = TRUE)
  )
}

# ---- KAPLAN-MEIER -----------------------------------------------------------
#' Weighted Kaplan-Meier, returned as a tidy data frame.
#'
#' Uses survival::survfit() with the sampling weights and cluster-robust variance
#' (clustered on the PSU) rather than survey::svykm(). The point estimates are
#' identical — both are the weighted KM estimator, checked at 0.9627 survival by
#' 59 months — but svykm() builds an influence matrix over every event time and
#' on this data it consumed 7 GB without finishing, while survfit() returns in
#' 0.04 s. The `robust`/`cluster` arguments keep the intervals honest about the
#' clustered design; strata are not carried into the variance, so treat the bands
#' as slightly conservative and use the log-rank / Cox output for inference.
#'
#' @param design  survey design from u5cm_design()
#' @param by      NULL for an overall curve, or a column name to stratify on
#' @param ci      TRUE to return confidence bands
#' Build the grouping vector for a "compare groups by" selection.
#'
#' Two things have to be right here, and both were wrong before.
#'
#' Labels: nine of the covariates are stored as integer 0/1, so stratifying on
#' them directly produced a legend reading "0" and "1" — which is not an answer
#' to "compare by ANC 4+ visits". They become No/Yes.
#'
#' Order: the group must be a FACTOR carrying its level order, because ordering
#' downstream with order() on a character vector sorts alphabetically. Wealth
#' came out "middle, poorer, poorest, richer, richest", which looks like a bug
#' in the estimates rather than in the sort.
u5cm_group_vector <- function(d, by) {
  x <- d[[by]]

  if (is.factor(x)) {
    present <- unique(as.character(x))
    return(factor(as.character(x),
                  levels = levels(x)[levels(x) %in% present]))
  }

  vals <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.na(vals) | vals %in% c(0, 1)) && any(!is.na(vals))) {
    return(factor(ifelse(vals == 1, "Yes", "No"), levels = c("No", "Yes")))
  }

  chr <- as.character(x)
  factor(chr, levels = sort(unique(chr[!is.na(chr)])))
}

u5cm_km <- function(design, by = NULL, ci = TRUE) {
  d <- design$variables

  if (is.null(by) || !by %in% names(d)) {
    f <- survival::Surv(surv_time, dead) ~ 1
    g_levels <- "All children"
  } else {
    d <- d[!is.na(d[[by]]), , drop = FALSE]
    if (nrow(d) == 0) return(data.frame())
    # Stratify on a derived column rather than on `by` itself, so the labels and
    # the level order are ours and survive the trip through survfit().
    d$.grp    <- u5cm_group_vector(d, by)
    g_levels  <- levels(d$.grp)
    f <- stats::as.formula("survival::Surv(surv_time, dead) ~ .grp")
  }
  if (nrow(d) == 0) return(data.frame())

  sf <- try(survival::survfit(f, data = d, weights = d$wt,
                              robust = TRUE, cluster = d$v021,
                              conf.type = if (ci) "log-log" else "none"),
            silent = TRUE)
  if (inherits(sf, "try-error")) return(data.frame())

  # survfit stacks all strata into one vector and records the run lengths in
  # $strata, so unpack it into an explicit group column.
  groups <- if (is.null(sf$strata)) rep("All children", length(sf$time))
            else rep(sub("^[^=]*=", "", names(sf$strata)), times = sf$strata)

  res <- data.frame(
    time  = sf$time,
    surv  = sf$surv,
    group = groups,
    lower = if (!is.null(sf$lower)) sf$lower else NA_real_,
    upper = if (!is.null(sf$upper)) sf$upper else NA_real_,
    stringsAsFactors = FALSE
  )
  # Anchor every curve at (0, 1) so the step plot starts from full survival.
  anchor <- do.call(rbind, lapply(unique(res$group), function(g)
    data.frame(time = 0, surv = 1, group = g, lower = 1, upper = 1)))
  res <- rbind(anchor, res)
  res$cum_mort <- (1 - res$surv) * 1000   # deaths per 1,000, the DHS scale

  res$group <- factor(res$group,
                      levels = g_levels[g_levels %in% unique(res$group)])
  res[order(res$group, res$time), ]
}

#' Number still at risk at chosen times — the risk table under a KM plot.
u5cm_risk_table <- function(design, times = c(0, 12, 24, 36, 48, 59), by = NULL) {
  d <- design$variables

  # Must use the same helper as the curves: deriving groups independently here
  # put "0"/"1" and an alphabetical order in the table directly beneath a
  # correctly labelled and ordered plot.
  if (is.null(by) || !by %in% names(d)) {
    grp <- factor(rep("All children", nrow(d)))
  } else {
    grp <- u5cm_group_vector(d, by)
  }
  lev <- levels(droplevels(grp))

  do.call(rbind, lapply(lev, function(g) {
    sel <- !is.na(grp) & as.character(grp) == g
    data.frame(group = g, time = times,
               at_risk = vapply(times, function(t) sum(d$surv_time[sel] >= t), integer(1)),
               events  = vapply(times, function(t)
                 sum(d$dead[sel] == 1 & d$surv_time[sel] <= t, na.rm = TRUE), integer(1)))
  }))
}

#' Survey-weighted log-rank test. Returns a list(stat, p) or NULL when not
#' applicable (a single group, or too few events).
u5cm_logrank <- function(design, by) {
  if (is.null(by) || !by %in% names(design$variables)) return(NULL)

  # Stratify on the same derived factor the curves use. Passing `by` straight
  # into the formula let svylogrank treat a 0/1 integer covariate as CONTINUOUS:
  # it then returned a different structure, the p-value failed to parse, and the
  # panel fell back to "pick a grouping variable" while one was selected.
  keep <- !is.na(design$variables[[by]])
  if (!any(keep)) return(NULL)
  design <- design[keep, ]
  design$variables$.grp <- u5cm_group_vector(design$variables, by)
  if (nlevels(droplevels(design$variables$.grp)) < 2) return(NULL)
  by <- ".grp"
  # svylogrank interpolates internally and emits "collapsing to unique 'x'
  # values" from approx() whenever there are tied event times — which there
  # always are, since age at death is recorded in whole months. Harmless, and
  # suppressed here so it does not fill the console on every group change.
  out <- try(suppressWarnings(survey::svylogrank(
    stats::as.formula(paste("survival::Surv(surv_time, dead) ~", by)),
    design = design)), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL)
  # svylogrank returns list(coef-ish, c(statistic, p))
  stat <- tryCatch(unname(out[[2]][1]), error = function(e) NA_real_)
  p    <- tryCatch(unname(out[[2]][2]), error = function(e) NA_real_)
  list(stat = stat, p = p)
}

# ---- HAZARD -----------------------------------------------------------------
#' Discrete hazard by DHS age segment.
#'
#' Reuses MORT_SEGMENTS from R/indicators.R — the same segments behind the
#' published under-5 rate — so the curve and the headline number are built on one
#' definition. q = deaths in the segment / children entering it.
u5cm_segment_hazard <- function(design, by = NULL) {
  d <- design$variables
  if (!is.null(by) && !by %in% names(d)) by <- NULL

  one <- function(d) {
    wt <- d$wt
    do.call(rbind, lapply(MORT_SEGMENTS, function(seg) {
      a <- seg[1]; b <- seg[2]
      entered <- d$surv_time >= a
      died_in <- d$dead == 1 & d$surv_time >= a & d$surv_time <= b
      denom <- sum(wt[entered], na.rm = TRUE)
      data.frame(
        segment  = sprintf("%d-%d", a, b),
        lo       = a,
        n_entered = sum(entered, na.rm = TRUE),
        n_deaths  = sum(died_in, na.rm = TRUE),
        q        = if (denom > 0) sum(wt[died_in], na.rm = TRUE) / denom else NA_real_
      )
    }))
  }

  if (is.null(by)) {
    res <- one(d)
    res$group <- "All children"
    return(res)
  }

  # Drop children with no value for the grouping variable rather than letting
  # them form a silent "NA" stratum that reads as a real category.
  ok <- !is.na(d[[by]])
  if (!any(ok)) return(NULL)
  d <- d[ok, , drop = FALSE]

  # Same helper as u5cm_km, so the labels and the group order on the hazard and
  # life-table panels match the Kaplan-Meier legend exactly.
  gf       <- u5cm_group_vector(d, by)
  g        <- as.character(gf)
  g_levels <- levels(gf)

  res <- do.call(rbind, lapply(g_levels, function(lv) {
    part <- one(d[g == lv, , drop = FALSE])
    part$group <- lv
    part
  }))
  res$group <- factor(res$group, levels = g_levels)
  res
}

#' DHS-style abridged life table over the age segments, with cumulative survival.
#'
#' @param by optional column name; when given, one life table per group. The
#'   cumulation must run WITHIN a group — chaining across groups would multiply
#'   unrelated survival probabilities together and silently produce nonsense.
u5cm_life_table <- function(design, by = NULL) {
  h <- u5cm_segment_hazard(design, by = by)
  if (is.null(h) || !nrow(h)) return(NULL)

  h$q_per_1000 <- h$q * 1000
  h <- h[order(h$group, h$lo), , drop = FALSE]
  h$cum_surv <- unlist(lapply(split(h$q, h$group), function(q)
    cumprod(1 - ifelse(is.na(q), 0, q))), use.names = FALSE)
  h$cum_mort_1000 <- (1 - h$cum_surv) * 1000

  seg_levels <- unique(h$segment[order(h$lo)])
  h$segment  <- factor(h$segment, levels = seg_levels)
  h
}

# ---- COX MODELS -------------------------------------------------------------
#' Survey-weighted Cox proportional hazards model.
#' @param covariates character vector of column names
u5cm_cox <- function(design, covariates) {
  covariates <- intersect(covariates, names(design$variables))
  if (length(covariates) == 0) return(NULL)
  f <- stats::as.formula(paste("survival::Surv(surv_time, dead) ~",
                               paste(covariates, collapse = " + ")))
  fit <- try(survey::svycoxph(f, design = design), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  fit
}

#' Schoenfeld test of the proportional-hazards assumption.
#' Returns a data frame of per-term and GLOBAL p-values, or NULL if it cannot run.
u5cm_ph_test <- function(fit) {
  if (is.null(fit)) return(NULL)
  z <- try(survival::cox.zph(fit), silent = TRUE)
  if (inherits(z, "try-error")) return(NULL)
  tab <- as.data.frame(z$table)
  tab$term <- rownames(tab)
  rownames(tab) <- NULL
  tab[, c("term", "chisq", "df", "p")]
}

#' Cox model with a county-level frailty (shared random effect).
#'
#' IMPORTANT: this is fitted with coxph(weights = wt), NOT svycoxph, because
#' svycoxph rejects penalised terms. The point estimates use the sampling weights
#' but the standard errors are model-based, not design-based. It is an
#' exchangeable county random effect, not an ICAR spatial prior — true ICAR needs
#' INLA or Stan and belongs in the thesis pipeline.
u5cm_frailty_cox <- function(design, covariates, county_col = "county") {
  d <- design$variables
  if (!county_col %in% names(d)) return(NULL)
  covariates <- setdiff(intersect(covariates, names(d)), county_col)
  rhs <- c(covariates, sprintf("survival::frailty(%s)", county_col))
  f <- stats::as.formula(paste("survival::Surv(surv_time, dead) ~",
                               paste(rhs, collapse = " + ")))
  fit <- try(suppressWarnings(
    survival::coxph(f, data = d, weights = d$wt, x = FALSE, model = FALSE)),
    silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  # coxph does not name the frailty coefficients, and fit$xlevels holds the
  # levels of the ORDINARY factors — reading the last of those gave the wealth
  # quintile's 5 levels against 47 frailties. Carry the county levels ourselves.
  attr(fit, "county_levels") <- levels(droplevels(as.factor(d[[county_col]])))
  fit
}

#' Per-county frailty estimates on the hazard-ratio scale, for mapping.
u5cm_frailty_values <- function(fit) {
  if (is.null(fit) || is.null(fit$frail)) return(NULL)
  fr <- as.numeric(fit$frail)
  counties <- names(fit$frail)
  if (is.null(counties)) counties <- attr(fit, "county_levels")
  if (is.null(counties) || length(counties) != length(fr)) return(NULL)
  data.frame(county = counties, frailty = fr, hr = exp(fr),
             stringsAsFactors = FALSE)
}

#' The estimated frailty variance (theta), or NA.
u5cm_frailty_theta <- function(fit) {
  if (is.null(fit) || is.null(fit$history)) return(NA_real_)
  tryCatch(fit$history[[1]]$theta, error = function(e) NA_real_)
}

# ---- SPATIAL DIAGNOSTICS (spdep) -------------------------------------------
# The frailty above is EXCHANGEABLE: it gives each county its own random effect
# but has no idea which counties border which. The thesis premise is that
# neighbouring counties share risk, and that premise is testable — which is what
# these functions are for. They do NOT fit a spatially structured (ICAR) model;
# that needs INLA or Stan. What they give you is the adjacency matrix such a model
# would consume, plus an answer to "is there spatial structure to exploit?".

#' Join county frailties onto the county polygons and build the neighbour list.
#'
#' @param frailty_df from u5cm_frailty_values()
#' @param counties_sf the county shapefile (raw_data$counties)
#' @return list(sf, nb, listw) or NULL
u5cm_spatial_join <- function(frailty_df, counties_sf) {
  if (is.null(frailty_df) || is.null(counties_sf)) return(NULL)
  if (!"NAME_1" %in% names(counties_sf)) return(NULL)

  shp <- counties_sf
  shp$.key <- trimws(tolower(shp$NAME_1))
  fd <- frailty_df
  fd$.key <- trimws(tolower(fd$county))
  joined <- dplyr::left_join(shp, fd, by = ".key")

  nb <- try(spdep::poly2nb(joined, queen = TRUE), silent = TRUE)
  if (inherits(nb, "try-error")) return(NULL)
  # zero.policy keeps any island county in the object instead of erroring
  lw <- try(spdep::nb2listw(nb, style = "W", zero.policy = TRUE), silent = TRUE)
  if (inherits(lw, "try-error")) return(NULL)

  list(sf = joined, nb = nb, listw = lw)
}

#' Global Moran's I on the county frailties.
#'
#' The single number that says whether spatial modelling is justified. I near its
#' expectation (-1/(n-1)) with a large p-value means the county effects are
#' scattered, not clustered — in which case an ICAR prior, which assumes
#' neighbours are alike, buys little over the exchangeable frailty already fitted.
u5cm_moran <- function(spatial) {
  if (is.null(spatial)) return(NULL)
  x <- spatial$sf$frailty
  if (sum(!is.na(x)) < 5) return(NULL)
  mt <- try(spdep::moran.test(x, spatial$listw, zero.policy = TRUE,
                              na.action = stats::na.exclude), silent = TRUE)
  if (inherits(mt, "try-error")) return(NULL)
  list(
    I        = unname(mt$estimate[[1]]),
    expected = unname(mt$estimate[[2]]),
    variance = unname(mt$estimate[[3]]),
    p        = mt$p.value,
    n_nb     = mean(spdep::card(spatial$nb)),
    islands  = sum(spdep::card(spatial$nb) == 0)
  )
}

#' Local Moran's I (LISA): per-county cluster classification.
#'
#' Splits counties into the four LISA quadrants plus "not significant", which
#' distinguishes "high risk AND surrounded by high risk" from "high risk but
#' isolated" — a more useful map than raw frailty shading.
u5cm_lisa <- function(spatial, alpha = 0.05) {
  if (is.null(spatial)) return(NULL)
  x <- spatial$sf$frailty
  if (sum(!is.na(x)) < 5) return(NULL)
  lm_ <- try(spdep::localmoran(x, spatial$listw, zero.policy = TRUE,
                               na.action = stats::na.exclude), silent = TRUE)
  if (inherits(lm_, "try-error")) return(NULL)

  pcol <- grep("^Pr", colnames(lm_), value = TRUE)[1]
  p    <- lm_[, pcol]
  # lag = the neighbourhood average, so quadrant needs both own and lagged value
  lagx <- spdep::lag.listw(spatial$listw, x, zero.policy = TRUE, NAOK = TRUE)
  hi   <- x    > mean(x, na.rm = TRUE)
  hi_l <- lagx > mean(lagx, na.rm = TRUE)

  cluster <- ifelse(is.na(p) | p >= alpha, "Not significant",
             ifelse( hi &  hi_l, "High-high (risk cluster)",
             ifelse(!hi & !hi_l, "Low-low (low-risk cluster)",
             ifelse( hi & !hi_l, "High-low (isolated high)",
                                 "Low-high (isolated low)"))))
  data.frame(
    county  = spatial$sf$NAME_1,
    frailty = x,
    lag     = lagx,
    Ii      = lm_[, "Ii"],
    p       = p,
    cluster = factor(cluster, levels = c(
      "High-high (risk cluster)", "Low-low (low-risk cluster)",
      "High-low (isolated high)", "Low-high (isolated low)", "Not significant")),
    stringsAsFactors = FALSE
  )
}

#' Colours for the LISA classes — red for risk clusters, blue for low-risk.
LISA_COLOURS <- c(
  "High-high (risk cluster)"   = "#b2182b",
  "Low-low (low-risk cluster)" = "#2166ac",
  "High-low (isolated high)"   = "#ef8a62",
  "Low-high (isolated low)"    = "#67a9cf",
  "Not significant"            = "#e0e0e0"
)
