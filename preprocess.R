# =============================================================================
# preprocess.R — Run ONCE before launching the app.
# Variable names verified against actual KEIR8CFL.DTA column list.
#
# KEY INSIGHT: This IR file uses wide format for child-level variables.
# Each birth gets its own column suffix: _1 to _6 (up to 6 most recent births)
# e.g. m14_1 = ANC visits for birth 1, h1_1 = vax card for child 1
# Birth history goes up to _20: b1_01..b1_20
# =============================================================================

library(haven)
library(dplyr)
library(glue)

DATA_DIR <- "../../02-data/kdhs-2022/stata"
OUT_DIR  <- "../../02-data/kdhs-2022/processed"
dir.create(OUT_DIR, showWarnings = FALSE)

t_start <- proc.time()
cat("=== KDHS 2022 Preprocessing ===\n\n")

save_rds <- function(df, name) {
  path <- file.path(OUT_DIR, paste0(name, ".rds"))
  saveRDS(df, path, version = 2)
  cat(glue("  ✅ {name}.rds — {nrow(df)} rows x {ncol(df)} cols ",
           "({round(file.size(path)/1e6,1)} MB)\n\n"))
}

read_dta_safe <- function(key, ...) {
  filenames <- c(...)
  path <- NULL
  for (fn in filenames) {
    p <- file.path(DATA_DIR, fn)
    if (file.exists(p)) { path <- p; break }
  }
  if (is.null(path)) {
    cat(glue("  ⚠️  SKIPPING {key} — tried: {paste(filenames, collapse=', ')}\n\n"))
    return(NULL)
  }
  cat(glue("  Loading {key} from {path}...\n"))
  haven::read_dta(path, encoding = "latin1") |> haven::as_factor()
}

# Select only columns that exist — skips missing ones with a warning
select_existing <- function(df, vars) {
  existing <- vars[vars %in% names(df)]
  missing  <- setdiff(vars, existing)
  if (length(missing) > 0)
    cat(glue("    ⚠️  Not found (skipped): {paste(missing, collapse=', ')}\n"))
  df |> select(all_of(existing))
}

# Character -> factor, with the DHS non-response labels turned into real NA so they
# never become a model category of their own.
NON_RESPONSE <- c("don't know", "dont know", "missing", "not a dejure resident",
                  "inconsistent", "other", "")
fct_or_na <- function(x) {
  # Pass the FACTOR in, not ch(x). The recodes are read with haven::as_factor(),
  # so every column arrives already carrying the DHS level order — and ch() is
  # as.character(), which throws that order away before this function can see it.
  # fct_or_na(ch(v106)) therefore looks correct and silently alphabetises.
  # Keep the ORIGINAL level order. factor() on a character vector assigns levels
  # alphabetically, which silently destroys the DHS coding order: education came
  # out "higher, no education, primary, secondary" and union status "currently,
  # formerly, never". Both are ordered categories, and in a Cox model the first
  # level is the reference — so the alphabetical accident was also choosing
  # "higher education" as the baseline every hazard ratio was measured against.
  lv <- if (is.factor(x)) levels(x) else NULL
  x  <- as.character(x)
  x[tolower(trimws(x)) %in% NON_RESPONSE] <- NA_character_
  if (is.null(lv)) return(factor(x))
  factor(x, levels = lv[!tolower(trimws(lv)) %in% NON_RESPONSE])
}

# Generate wide child-level suffixes _1 to _n (IR format)
ch_vars <- function(prefix, n = 6)  paste0(prefix, "_", 1:n)
# Generate wide birth-history suffixes _01 to _n (birth history format)
bh_vars <- function(prefix, n = 20) sprintf("%s_%02d", prefix, 1:n)

# =============================================================================
# IR — Women's Individual Recode
# =============================================================================
cat("--- IR: Women's Individual Recode ---\n")
ir_raw <- read_dta_safe("IR", "KEIR8CFL.DTA", "KEIR8AFL.DTA")

if (!is.null(ir_raw)) {
  ir_keep <- c(
    # Survey design
    "caseid", "v001", "v002", "v003",
    "v005", "v021", "v022", "v023",
    
    # Background
    "v006", "v007", "v008", "v009", "v010", "v011",
    "v012", "v013", "v024", "v025",
    "v101", "v102", "v106", "v107", "v133", "v149",
    "v130", "v131", "v136", "v137",
    "v150", "v151", "v152", "v155",
    "v190", "v191",
    
    # Marital / union
    "v501", "v502", "v503", "v504", "v505",
    "v511", "v512", "v531", "v535",
    
    # Fertility
    "v201", "v202", "v203", "v204", "v205",
    "v206", "v207", "v208", "v209",
    "v212", "v213", "v218", "v219", "v220", "v222",
    
    # Birth history — wide: b0_01..b0_20, b1_01..b21_20
    bh_vars("b0"),  bh_vars("b1"),  bh_vars("b2"),  bh_vars("b3"),
    bh_vars("b4"),  bh_vars("b5"),  bh_vars("b6"),  bh_vars("b7"),
    bh_vars("b8"),  bh_vars("b11"),
    
    # Family planning
    "v301", "v302", "v312", "v313",
    "v317", "v318", "v319", "v320",
    "v362", "v364", "v367",
    "v375a", "v379", "v394", "v395",
    
    # ANC & maternity — wide: m14_1..m14_6 (one per birth, up to 6)
    ch_vars("m2a"), ch_vars("m2b"), ch_vars("m2c"),   # ANC provider
    ch_vars("m14"),   # ANC visits count
    ch_vars("m15"),   # place of delivery
    ch_vars("m17"),   # C-section
    ch_vars("m3a"), ch_vars("m3b"), ch_vars("m3c"),   # delivery assistance
    ch_vars("m4"),    # breastfeeding duration
    ch_vars("m5"),    # still breastfeeding
    ch_vars("m10"),   # birth size
    ch_vars("m11"),   # birth weight category
    ch_vars("m18"),   # weighed at birth
    ch_vars("m19"),   # birth weight in kg
    ch_vars("m19a"),  # source of birth weight
    
    # Vaccination — wide: h1_1..h1_6
    ch_vars("h1"),    # has vax card
    ch_vars("h2"),    # BCG
    ch_vars("h3"),    # DPT1
    ch_vars("h4"),    # Polio1
    ch_vars("h5"),    # DPT2
    ch_vars("h6"),    # Polio2
    ch_vars("h7"),    # DPT3
    ch_vars("h8"),    # Polio3
    ch_vars("h9"),    # Measles1
    ch_vars("h33"),   # fully vaccinated
    
    # Anthropometry — wide: hw1_1..hw1_6
    ch_vars("hw1"),   # age in months
    ch_vars("hw2"),   # weight
    ch_vars("hw3"),   # height
    ch_vars("hw70"),  # HAZ score
    ch_vars("hw71"),  # WAZ score
    ch_vars("hw72"),  # WHZ score
    
    # HIV knowledge & testing (single variables — not wide)
    "v754bp", "v754cp", "v754dp", "v754jp", "v754wp",
    "v774b",            # comprehensive HIV knowledge
    "v781",             # ever tested
    "v783",             # tested last 12 months
    "v824",             # drugs to avoid HIV transmission to baby
    "v837",             # heard of ARVs
    "v859",             # knowledge and attitude to PrEP
    
    # GBV  (d108 = any sexual violence; needed for the violence-by-type panel)
    "v044",
    "d104", "d105a", "d105b", "d105c", "d105d",
    "d106", "d107", "d108",
    "v744a", "v744b", "v744c", "v744d", "v744e",

    # FGM/C
    "g100", "g101", "g102", "g103", "g105", "g106",

    # Women's autonomy
    "v743a", "v743b", "v743c", "v743d", "v743e",
    "v745a", "v745b", "v746",
    "v739",             # who decides how respondent's earnings are spent
    "v170",             # has a bank / financial institution account

    # ANC provider: m2n = "Prenatal: no one" (needed to separate "no ANC")
    ch_vars("m2n"),

    # Chronic conditions (KDHS 2022 NCD module, women)
    "chd02",            # hypertension
    "chd07",            # diabetes
    "chd11",            # heart disease
    "chd13",            # lung disease
    "chd17",            # depression
    "chd18",            # anxiety
    "chd20",            # arthritis

    # Health insurance — kept only to document that KDHS 2022 did NOT collect it
    # in the women's questionnaire (all "NA - ..." and 100% missing). The usable
    # source is the household roster; see the HR and PR blocks below.
    "v481", "v481a", "v481b", "v481c", "v481d",
    "v481e", "v481f", "v481g", "v481h"
  )
  
  ir <- select_existing(ir_raw, ir_keep) |>
    mutate(
      wt            = as.numeric(v005) / 1e6,
      # FP — use scalar v312/v313 (current method, not birth-specific)
      modern_fp     = as.integer(v313 == "modern method"),
      any_fp        = as.integer(!is.na(v312) & v312 != "not currently using"),
      # ANC 4+ — use most recent birth (column _1)
      anc4_plus     = as.integer(
        suppressWarnings(as.numeric(as.character(m14_1))) >= 4),
      # Delivery assistance — m3a/m3b are yes/no flags ("Assistance: doctor",
      # "Assistance: nurse/midwife/clinical officer"), NOT a provider-name field,
      # so the old %in% c("doctor","nurse/midwife") test never matched.
      # m3c is excluded on purpose: it is "NA - ..." and 100% missing, and mixing
      # it in turns every FALSE into NA (FALSE | NA is NA).
      skilled_birth = as.integer(as.character(m3a_1) == "yes" |
                                 as.character(m3b_1) == "yes"),
      # v481 is "NA - Covered by health insurance" and entirely empty in KDHS 2022,
      # so this stays all-NA by construction. Use hh_insured (HR) / sh27 (PR).
      insured       = as.integer(v481 == "yes"),
      hiv_tested    = as.integer(v781 == "yes"),
      # v774b is likewise "NA - ..." and empty, so hiv_know_comp is unusable.
      hiv_know_comp = as.integer(suppressWarnings(as.numeric(v774b)) == 1),
      # Chronic conditions: any of the seven NCDs ever diagnosed
      chronic_any   = {
        .chd <- cbind(as.character(chd02), as.character(chd07),
                      as.character(chd11), as.character(chd13),
                      as.character(chd17), as.character(chd18),
                      as.character(chd20)) == "yes"
        ifelse(rowSums(!is.na(.chd)) > 0,
               as.integer(rowSums(.chd, na.rm = TRUE) > 0), NA_integer_)
      },
      bank_account  = as.integer(as.character(v170) == "yes"),
      owns_land     = as.integer(!is.na(v745b) & as.character(v745b) != "does not own"),
      owns_house    = as.integer(!is.na(v745a) & as.character(v745a) != "does not own"),
      fgm_yes       = as.integer(g102 %in% c("yes, circumcised",
                                             "yes, others circumcised",
                                             "yes")),
      married_u18   = as.integer(
        !is.na(v511) & suppressWarnings(as.numeric(v511)) < 18),
      # Stunting from most recent child (column _1).
      # as_factor() turns the z-scores into factors whose labels are the numbers,
      # so they must go through as.character() before any numeric comparison.
      stunted_1     = as.integer(suppressWarnings(as.numeric(as.character(hw70_1))) < -200),
      wasted_1      = as.integer(suppressWarnings(as.numeric(as.character(hw72_1))) < -200),
      full_vax_1    = as.integer(!is.na(h33_1) &
                                   as.character(h33_1) %in%
                                   c("all vaccinations","all basic vaccinations")),
      # Labels
      wealth_q  = as.character(v190),
      residence = as.character(v025),
      region    = as.character(v024),
      education = as.character(v106),
      age_grp   = as.character(v013)
    )
  
  save_rds(ir, "ir")
  rm(ir_raw, ir); gc()
}

# =============================================================================
# HR — Household Recode
# =============================================================================
cat("--- HR: Household Recode ---\n")
hr_raw <- read_dta_safe("HR", "KEHR8CFL.DTA", "KEHR8AFL.DTA")

if (!is.null(hr_raw)) {
  hr_keep <- c(
    "hv001", "hv002",
    "hv005", "hv021", "hv022", "hv023",
    "hv006", "hv007",
    "hv009", "hv010", "hv011", "hv014",
    "hv024", "hv025", "hv026",
    "hv219", "hv220",
    "hv270", "hv271",
    "hv201", "hv204", "hv205", "hv206",
    "hv207", "hv208", "hv209",
    "hv213", "hv214", "hv215", "hv216", "hv226", "hv227",
    "sh44", "sh44a", "sh44b", "sh44c",
    "sh44d", "sh44e", "sh44f", "sh44g",
    "hv027"
  )
  # Grab any sh109/sh35/sh131/sh132/sh161 cols (chronic disease, disability, COVID, ECDI)
  # sh27_* = "Covered by health insurance", asked once per household member slot.
  # KDHS 2022 did NOT collect insurance in the women's/men's questionnaires — the
  # IR v481* and MR mv481* blocks are labelled "NA -" and are entirely empty. The
  # household roster is the only source, so these columns must be kept.
  # sh135f/h/j/l are the COVID-19 household counts (members tested, positive,
  # died, vaccinated) — the only COVID data in this survey.
  sh_extra <- names(hr_raw)[grepl("^sh109|^sh35|^sh131|^sh132|^sh161|^sh27_|^sh135|^ecd",
                                  names(hr_raw))]
  hr_keep  <- unique(c(hr_keep, sh_extra))

  hr <- select_existing(hr_raw, hr_keep) |>
    mutate(
      wt        = as.numeric(hv005) / 1e6,
      region    = as.character(hv024),
      residence = as.character(hv025),
      wealth_q  = as.character(hv270)
    )

  # Derived: does this household have at least one member covered by insurance?
  # The insurance module was only asked of the half-sample selected for the men's
  # survey (hv027), so households never asked must stay NA, not be counted as "no".
  sh27_cols <- grep("^sh27_", names(hr), value = TRUE)
  if (length(sh27_cols) > 0) {
    ins_mat <- vapply(hr[sh27_cols], function(x) as.character(x) == "yes",
                      logical(nrow(hr)))
    asked   <- rowSums(!is.na(ins_mat)) > 0
    hr$hh_insured <- ifelse(asked, rowSums(ins_mat, na.rm = TRUE) > 0, NA)
    cat(glue("    hh_insured derived from {length(sh27_cols)} sh27_* columns; ",
             "{sum(asked)} of {nrow(hr)} households were asked\n"))
  }

  # COVID-19: sh135f/h/j/l are counts of household members tested / positive /
  # died / vaccinated. Households never asked stay NA rather than becoming zero.
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  if ("sh135l" %in% names(hr)) hr$covid_vax_hh    <- as.integer(num(hr$sh135l) > 0)
  if ("sh135f" %in% names(hr)) hr$covid_tested_hh <- as.integer(num(hr$sh135f) > 0)
  if ("sh135h" %in% names(hr)) hr$covid_pos_hh    <- as.integer(num(hr$sh135h) > 0)
  
  save_rds(hr, "hr")
  rm(hr_raw, hr); gc()
}

# =============================================================================
# KR — Children's Recode (separate file — variables are NOT wide here)
# =============================================================================
cat("--- KR: Children's Recode ---\n")
kr_raw <- read_dta_safe("KR", "KEKR8CFL.DTA", "KEKR8AFL.DTA")

if (!is.null(kr_raw)) {
  kr_keep <- c(
    "v001", "v002", "v003", "v005", "v021", "v022", "v023",
    "v012", "v013", "v024", "v025", "v106", "v190",
    "b4", "b5", "b6", "b7", "b8", "b11",
    # b8 is age in YEARS (0-4). b19 is age in MONTHS, which is what the 12-23
    # month vaccination and nutrition denominators actually need.
    "b19",
    # ---- U5CM analysis covariates, grouped as in the Mosley-Chen framework ----
    # All of these are present in KEKR8CFL, so no household merge is needed.
    "b0",               # twin / multiple birth
    "bord",             # birth order
    "m18", "m19",       # size at birth; birth weight in grams
    "m14", "m15", "m17",# ANC visits; place of delivery; caesarean
    "v404",             # currently breastfeeding
    "v502",             # union status
    "v208", "v218",     # births in last 5 years; living children
    "v113", "v116",     # drinking water; toilet facility
    "v119",             # electricity
    "v136", "v137",     # household size; children under 5 in household
    "v161",             # cooking fuel
    "v467d",            # distance to facility a problem
    "h1", "h2", "h3", "h4", "h5",
    "h6", "h7", "h8", "h9", "h33",
    "hw1", "hw2", "hw3", "hw70", "hw71", "hw72", "hw73",
    "h11a", "h21", "h31",
    "m4", "m5",
    # ECDI 2030 items, asked for children 24-59 months (ecd21..ecd40)
    paste0("ecd", 21:40)
  )

  kr <- select_existing(kr_raw, kr_keep) |>
    mutate(
      wt          = as.numeric(v005) / 1e6,
      # The anthropometry z-scores arrive as factors labelled with their numbers,
      # so convert via as.character() before comparing. Values >= 9996 are DHS
      # flags for "not measured"/"out of range" and must not count as stunted.
      haz         = suppressWarnings(as.numeric(as.character(hw70))),
      waz         = suppressWarnings(as.numeric(as.character(hw71))),
      whz         = suppressWarnings(as.numeric(as.character(hw72))),
      haz         = ifelse(!is.na(haz) & abs(haz) > 9000, NA_real_, haz),
      waz         = ifelse(!is.na(waz) & abs(waz) > 9000, NA_real_, waz),
      whz         = ifelse(!is.na(whz) & abs(whz) > 9000, NA_real_, whz),
      stunted     = as.integer(haz < -200),
      wasted      = as.integer(whz < -200),
      underweight = as.integer(waz < -200),
      region      = as.character(v024),
      residence   = as.character(v025),
      wealth_q    = as.character(v190)
    )

  # ---- Full ("basic") immunisation ----------------------------------------
  # h33 is NOT a fully-vaccinated flag — the dictionary labels it "Received
  # Vitamin A1 (most recent)", and none of its levels are "all vaccinations",
  # so the old derivation returned 0 for every child in the file.
  #
  # Basic immunisation = BCG + 3 doses DPT + 3 doses polio + measles, per the
  # standard DHS definition. Each h* is "no" / "vaccination date on card" /
  # "reported by mother" / "vaccination marked on card": anything but "no"
  # counts as received (card or maternal recall).
  vax_antigens <- c("h2",             # BCG
                    "h3", "h5", "h7", # DPT 1-3
                    "h4", "h6", "h8", # Polio 1-3
                    "h9")             # Measles 1
  vax_antigens <- intersect(vax_antigens, names(kr))
  if (length(vax_antigens) == 8) {
    got <- vapply(kr[vax_antigens], function(x) {
      ch <- as.character(x)
      ifelse(is.na(ch), NA, ch != "no")
    }, logical(nrow(kr)))
    asked_v <- rowSums(!is.na(got)) > 0
    kr$full_vax <- ifelse(asked_v,
                          as.integer(rowSums(got, na.rm = TRUE) == length(vax_antigens)),
                          NA_integer_)
    cat(glue("    full_vax from {length(vax_antigens)} antigens; ",
             "{sum(asked_v)} children with any vaccination record\n"))
  } else {
    kr$full_vax <- NA_integer_
    cat(glue("    ⚠️  full_vax unavailable — only {length(vax_antigens)}/8 antigens present\n"))
  }

  # ---- ECDI domain scores --------------------------------------------------
  # NOTE: this is NOT the official UNICEF ECDI2030 index, which uses age-specific
  # scoring rules. Each score here is simply the share of that domain's items the
  # child can do, among items with a non-missing answer. The two negatively worded
  # items (ecd39 sadness, ecd40 aggression) are reversed so "higher = better"
  # holds across every domain. Panels label it as such.
  ecdi_domains <- list(
    `Physical Development` = paste0("ecd", 21:24),
    `Literacy & Numeracy`  = paste0("ecd", c(25:35)),
    `Socio-Emotional`      = paste0("ecd", 36:38),
    `Learning Approaches`  = paste0("ecd", 39:40)     # reverse-scored
  )
  ecdi_score <- function(df, items, reverse = FALSE) {
    items <- intersect(items, names(df))
    if (length(items) == 0) return(rep(NA_real_, nrow(df)))
    m <- vapply(df[items], function(x) as.character(x) == "yes", logical(nrow(df)))
    if (reverse) m <- !m
    n <- rowSums(!is.na(m))
    ifelse(n > 0, rowSums(m, na.rm = TRUE) / n * 100, NA_real_)
  }
  for (dom in names(ecdi_domains)) {
    col <- paste0("ecdi_", tolower(gsub("[^A-Za-z]+", "_", dom)))
    kr[[col]] <- ecdi_score(kr, ecdi_domains[[dom]],
                            reverse = dom == "Learning Approaches")
  }
  ecdi_cols <- grep("^ecdi_", names(kr), value = TRUE)
  kr$ecdi_overall <- if (length(ecdi_cols)) rowMeans(kr[ecdi_cols], na.rm = TRUE) else NA_real_
  kr$ecdi_overall[is.nan(kr$ecdi_overall)] <- NA_real_
  cat(glue("    ECDI scored for {sum(!is.na(kr$ecdi_overall))} children ",
           "across {length(ecdi_cols)} domains\n"))

  # ===========================================================================
  # U5CM SURVIVAL OBJECT + MOSLEY-CHEN COVARIATES
  # ===========================================================================
  # The survival definition is copied deliberately from the thesis pipeline
  # (01-thesis/analysis/R/01_data_management/03_clean_data-v2.R) so the dashboard
  # and the thesis cannot drift apart:
  #     dead      = child is not alive
  #     surv_time = age at death if dead, else current age, clamped to [0, 59]
  n_ <- function(x) suppressWarnings(as.numeric(as.character(x)))
  ch <- function(x) as.character(x)

  kr <- kr |>
    mutate(
      dead      = as.integer(ch(b5) == "no"),
      .age_m    = n_(b19),
      .aad      = n_(b7),                       # age at death, months
      surv_time = ifelse(dead == 1, .aad, pmin(.age_m, 59)),
      surv_time = pmin(pmax(surv_time, 0), 59),

      # ---- Binary outcomes with an honest exposure denominator -------------
      # A child must have LIVED THROUGH the horizon to be eligible. Without this
      # the "alive" group is padded with children who simply have not had time
      # to die yet, and the estimate is biased downward (33.9 vs a published
      # 41 per 1,000 for under-5). elig_* carries that denominator.
      elig_neonatal  = as.integer(!is.na(.age_m) & .age_m >= 1),
      elig_infant    = as.integer(!is.na(.age_m) & .age_m >= 12),
      elig_24m       = as.integer(!is.na(.age_m) & .age_m >= 24),
      died_neonatal  = ifelse(elig_neonatal == 1,
                              as.integer(dead == 1 & !is.na(.aad) & .aad < 1),  NA_integer_),
      died_infant    = ifelse(elig_infant == 1,
                              as.integer(dead == 1 & !is.na(.aad) & .aad < 12), NA_integer_),
      died_24m       = ifelse(elig_24m == 1,
                              as.integer(dead == 1 & !is.na(.aad) & .aad < 24), NA_integer_),
      # NOTE: there is deliberately no died_60m. No child in this file has
      # completed 60 months of exposure (max age is 59), so a 60-month binary
      # outcome has an empty denominator. That is why U5CM needs survival models.

      # ---- Child factors ---------------------------------------------------
      sex                  = fct_or_na(b4),
      twin                 = factor(ifelse(ch(b0) == "single birth", "Single", "Multiple"),
                                    levels = c("Single", "Multiple")),
      birth_order_grp      = cut(n_(bord), c(0, 1, 3, 6, Inf),
                                 labels = c("1", "2-3", "4-6", "7+")),
      short_birth_interval = factor(
        ifelse(is.na(n_(b11)), "First birth",
               ifelse(n_(b11) < 24, "Under 24 months", "24+ months")),
        levels = c("24+ months", "Under 24 months", "First birth")),
      size_at_birth        = factor(dplyr::case_when(
        ch(m18) %in% c("very small", "smaller than average") ~ "Small",
        ch(m18) %in% c("average")                            ~ "Average",
        ch(m18) %in% c("large", "larger than average", "very large") ~ "Large",
        TRUE                                                 ~ NA_character_),
        levels = c("Average", "Small", "Large")),
      birth_weight_g       = ifelse(n_(m19) %in% c(9996, 9998, 9999), NA_real_, n_(m19)),
      low_birth_weight     = ifelse(is.na(birth_weight_g), NA_integer_,
                                    as.integer(birth_weight_g < 2500)),

      # ---- Maternal factors ------------------------------------------------
      maternal_age_grp     = fct_or_na(v013),
      maternal_education   = fct_or_na(v106),
      union_status         = fct_or_na(v502),
      living_children      = n_(v218),

      # ---- Healthcare use --------------------------------------------------
      anc_4plus            = ifelse(is.na(m14), NA_integer_,
                                    as.integer(!is.na(n_(m14)) & n_(m14) >= 4)),
      facility_birth       = ifelse(is.na(m15), NA_integer_,
                                    as.integer(!grepl("home", ch(m15)))),
      caesarean            = ifelse(is.na(m17), NA_integer_, as.integer(ch(m17) == "yes")),
      ever_breastfed       = ifelse(is.na(m4), NA_integer_,
                                    as.integer(ch(m4) != "never breastfed")),

      # ---- Household environment (JMP "improved" definitions) --------------
      improved_water = dplyr::case_when(
        ch(v113) == "not a dejure resident" ~ NA_integer_,
        grepl("^piped|public tap|tube well|borehole|protected well|protected spring|rainwater|bottled",
              ch(v113)) & !grepl("^unprotected", ch(v113)) ~ 1L,
        is.na(v113) ~ NA_integer_, TRUE ~ 0L),
      improved_sanitation = dplyr::case_when(
        ch(v116) == "not a dejure resident" ~ NA_integer_,
        grepl("^flush|ventilated improved|pit latrine with slab|composting", ch(v116)) ~ 1L,
        is.na(v116) ~ NA_integer_, TRUE ~ 0L),
      clean_fuel = dplyr::case_when(
        ch(v161) == "not a dejure resident" ~ NA_integer_,
        ch(v161) %in% c("electricity", "lpg", "natural gas", "biogas",
                        "solar power", "alcohol/ethanol") ~ 1L,
        is.na(v161) ~ NA_integer_, TRUE ~ 0L),
      electricity = dplyr::case_when(
        ch(v119) == "not a dejure resident" ~ NA_integer_,
        ch(v119) == "yes" ~ 1L, ch(v119) == "no" ~ 0L, TRUE ~ NA_integer_),
      hh_size     = n_(v136),
      children_u5 = n_(v137),
      distance_barrier = dplyr::case_when(
        ch(v467d) == "big problem" ~ 1L,
        ch(v467d) %in% c("no problem", "not a big problem") ~ 0L,
        TRUE ~ NA_integer_),
      # Counties are deliberately alphabetical, not in DHS code order: the codes
      # run by official county number (Mombasa 1, Kwale 2, ...), which is not a
      # useful order to scan in a 47-item dropdown or legend.
      county = factor(as.character(fct_or_na(v024)))
    ) |>
    select(-.age_m, -.aad)

  cat(glue("    survival object: {sum(!is.na(kr$surv_time))} children, ",
           "{sum(kr$dead, na.rm = TRUE)} deaths, ",
           "median follow-up {stats::median(kr$surv_time, na.rm = TRUE)} months\n"))
  cat(glue("    binary outcomes (eligible/events): ",
           "neonatal {sum(kr$elig_neonatal, na.rm=TRUE)}/{sum(kr$died_neonatal, na.rm=TRUE)}, ",
           "infant {sum(kr$elig_infant, na.rm=TRUE)}/{sum(kr$died_infant, na.rm=TRUE)}, ",
           "24m {sum(kr$elig_24m, na.rm=TRUE)}/{sum(kr$died_24m, na.rm=TRUE)}\n"))

  save_rds(kr, "kr")
  rm(kr_raw, kr); gc()
}

# =============================================================================
# MR — Men's Recode
# =============================================================================
cat("--- MR: Men's Recode ---\n")
mr_raw <- read_dta_safe("MR", "KEMR8CFL.DTA", "KEMR8AFL.DTA")

if (!is.null(mr_raw)) {
  mr_keep <- c(
    "mv001", "mv002", "mv003",
    "mv005", "mv021", "mv022", "mv023",
    "mv012", "mv013", "mv024", "mv025",
    "mv106", "mv107", "mv149",
    "mv130", "mv190", "mv191",
    "mv501", "mv502", "mv531",
    "mv754bp", "mv754cp", "mv774b",
    "mv781", "mv783",
    "mv824", "mv859",   # PMTCT drugs; PrEP knowledge and attitude
    "mv301", "mv302", "mv312", "mv313",
    "mv744a", "mv744b", "mv744c", "mv744d", "mv744e",
    # Chronic conditions (NCD module, men) — mirrors chd* in the women's file
    "mchd02", "mchd07", "mchd11", "mchd13", "mchd17", "mchd18", "mchd20"
  )

  mr <- select_existing(mr_raw, mr_keep) |>
    mutate(
      wt            = as.numeric(mv005) / 1e6,
      # mv774b is "NA - ..." and 100% missing, so this is unusable; kept only so
      # the column shape matches the women's file.
      hiv_know_comp = as.integer(suppressWarnings(as.numeric(mv774b)) == 1),
      hiv_tested    = as.integer(mv781 == "yes"),
      chronic_any   = {
        .chd <- cbind(as.character(mchd02), as.character(mchd07),
                      as.character(mchd11), as.character(mchd13),
                      as.character(mchd17), as.character(mchd18),
                      as.character(mchd20)) == "yes"
        ifelse(rowSums(!is.na(.chd)) > 0,
               as.integer(rowSums(.chd, na.rm = TRUE) > 0), NA_integer_)
      },
      region        = as.character(mv024),
      residence     = as.character(mv025),
      wealth_q      = as.character(mv190)
    )
  
  save_rds(mr, "mr")
  rm(mr_raw, mr); gc()
}

# =============================================================================
# PR — Household Members Recode (one row per person)
# =============================================================================
# The only per-person source for the Washington Group disability questions and
# for health-insurance TYPE. The household file carries the same items only in
# wide form (hdis2_01..hdis2_24), which is far more awkward to analyse.
cat("--- PR: Household Members Recode ---\n")
pr_raw <- read_dta_safe("PR", "KEPR8CFL.DTA", "KEPR8AFL.DTA")

if (!is.null(pr_raw)) {
  pr_keep <- c(
    "hv001", "hv002", "hvidx",
    "hv005", "hv021", "hv022", "hv023",
    "hv024", "hv025", "hv027", "hv270",
    "hv104",            # sex
    "hv105",            # age in years
    # Washington Group short set
    "hdis2",            # seeing
    "hdis4",            # hearing
    "hdis5",            # communicating
    "hdis6",            # remembering / concentrating
    "hdis7",            # walking / climbing
    "hdis8",            # washing / dressing
    "hdis9",            # highest degree of difficulty, any domain
    # Health insurance
    "sh27",             # covered by health insurance
    "sh28a",            # type: NHIF
    "sh28b",            # type: private / commercial
    "sh28c",            # type: community-based
    "sh28x"             # type: other
  )

  pr <- select_existing(pr_raw, pr_keep) |>
    mutate(
      wt        = as.numeric(hv005) / 1e6,
      age       = suppressWarnings(as.numeric(as.character(hv105))),
      sex       = as.character(hv104),
      region    = as.character(hv024),
      residence = as.character(hv025),
      wealth_q  = as.character(hv270),
      insured   = as.integer(as.character(sh27) == "yes")
    )

  # Any disability = "a lot of difficulty" or "cannot do at all" in at least one
  # domain. This is the standard Washington Group cut-off; "some difficulty" is
  # deliberately excluded. People never asked the module stay NA.
  dis_cols <- intersect(c("hdis2","hdis4","hdis5","hdis6","hdis7","hdis8"), names(pr))
  if (length(dis_cols) > 0) {
    # grepl() returns FALSE for NA rather than NA, so the missingness has to be
    # restored explicitly — otherwise everyone looks like they answered and the
    # denominator silently becomes the whole file.
    dis_mat <- vapply(pr[dis_cols], function(x) {
      ch <- as.character(x)
      ifelse(is.na(ch), NA, grepl("^a lot of difficulty|^cannot", ch))
    }, logical(nrow(pr)))
    asked_d <- rowSums(!is.na(dis_mat)) > 0
    pr$disabled <- ifelse(asked_d, as.integer(rowSums(dis_mat, na.rm = TRUE) > 0),
                          NA_integer_)
    cat(glue("    disability scored for {sum(asked_d)} of {nrow(pr)} people ",
             "across {length(dis_cols)} domains\n"))
  }

  save_rds(pr, "pr")
  rm(pr_raw, pr); gc()
}

# =============================================================================
# BR — Birth Recode (mortality)
# =============================================================================
cat("--- BR: Birth Recode ---\n")
br_raw <- read_dta_safe("BR", "KEBR8CFL.DTA", "KEBR8AFL.DTA")

if (!is.null(br_raw)) {
  br_keep <- c(
    "v001", "v002", "v003",
    "v005", "v021", "v022", "v023",
    "v008", "v011",
    "v024", "v025", "v190",
    "b1", "b2", "b3", "b4", "b5",
    "b6", "b7", "b8", "b11", "b15"
  )
  
  br <- select_existing(br_raw, br_keep) |>
    mutate(
      wt        = as.numeric(v005) / 1e6,
      region    = as.character(v024),
      residence = as.character(v025),
      wealth_q  = as.character(v190),
      u5_death  = as.integer(b5 == "no" & !is.na(b7) &
                               suppressWarnings(as.numeric(b7)) < 60),
      nn_death  = as.integer(b5 == "no" & !is.na(b7) &
                               suppressWarnings(as.numeric(b7)) < 1),
      inf_death = as.integer(b5 == "no" & !is.na(b7) &
                               suppressWarnings(as.numeric(b7)) < 12)
    )
  
  save_rds(br, "br")
  rm(br_raw, br); gc()
}

# =============================================================================
# DONE
# =============================================================================
elapsed <- round((proc.time() - t_start)["elapsed"] / 60, 1)
cat(glue("\n=== Done in {elapsed} minutes ===\n\nFiles saved to {OUT_DIR}:\n"))
for (f in list.files(OUT_DIR, pattern = "\\.rds$")) {
  size <- round(file.size(file.path(OUT_DIR, f)) / 1e6, 1)
  cat(glue("  {f}  ({size} MB)\n"))
}
cat("\nRun shiny::runApp() — app will now load in seconds.\n")
