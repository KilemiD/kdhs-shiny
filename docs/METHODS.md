# Methods — KDHS 2022 dashboard

What every figure in the dashboard measures, on which population, by which formula,
and how it compares with the published report.

> **This file is hand-written and can drift from the code.**
> `R/validate_indicators.R` is the authoritative check — it recomputes every number
> below from the data and prints the comparison. Run it after any change to
> `preprocess.R`, `R/indicators.R` or `R/survival_core.R`, and update the tables here:
>
> ```r
> source("R/validate_indicators.R")
> ```
>
> Figures last verified **2026-09-16** against the KDHS 2022 Key Indicators Report
> in `02-data/kdhs-2022/docs/`.

---

## 1. Data and survey design

Six recodes, preprocessed once by `preprocess.R` into `02-data/kdhs-2022/processed/`:

| File | Unit | Rows | Used for |
|---|---|---|---|
| `ir.rds` | woman 15-49 | 32,156 | fertility, contraception, ANC, HIV, GBV, FGM/C |
| `kr.rds` | child born in last 5 yrs | 19,530 | nutrition, vaccination, **child survival** |
| `br.rds` | live birth (full history) | 77,381 | childhood mortality rates |
| `hr.rds` | household | 37,911 | household insurance, COVID-19 |
| `pr.rds` | household member | 156,571 | per-person insurance, disability |
| `mr.rds` | man 15-54 | 14,453 | male HIV testing, chronic disease |

Every estimate is weighted for the stratified multi-stage design:

```
weight = v005 / 1,000,000   (hv005 for household files, mv005 for men)
strata = v023               PSU = v021
```

built by `create_*_design()` in `R/survey_setup.R`. Unweighted figures are not
comparable with the report.

**Values arrive as labelled factors.** `preprocess.R` applies
`haven::as_factor()`, so DHS codes become label strings. Two consequences that
have already caused wrong numbers here:

- `v313 %in% c(1, 2, 3)` matches nothing — compare to labels (`v313 != "no method"`).
- `as.numeric(m14_1)` returns the **level index**, not the value. Go through
  `as.character()` first for factors whose labels are numbers.

---

## 2. Headline indicators

Computed by `R/indicators.R`. **Denominators are not interchangeable** — the wrong
one produces a plausible but unpublishable number.

| Indicator | Population | Definition | Dashboard | Report |
|---|---|---|---|---|
| Contraceptive prevalence, any | currently married women 15-49 (`v502`) | `v313 != "no method"` | 62.5% | 63% (T8) |
| Contraceptive prevalence, modern | same | `v313 == "modern method"` | 56.9% | 57% (T8) |
| ANC 4+ visits | women with a birth in last 2 yrs (`v222 <= 23`) | `m14_1 >= 4`, "don't know" kept as not-4+ | 66.0% | 66% (T11) |
| Skilled birth attendance | births in last 2 yrs | `m3a_1 == "yes"` or `m3b_1 == "yes"` | 89.4% | 89% (T11) |
| Stunting | children under 5 measured | height-for-age < −2 SD | 17.4% | 17.6% (T14) |
| Wasting | same | weight-for-height < −2 SD | 4.9% | 4.9% (T14) |
| Underweight | same | weight-for-age < −2 SD | 10.0% | 10.1% (T14) |
| Health insurance, female | de jure household population | `sh27 == "yes"` (PR) | 26.0% | 26.0% (T3) |
| Health insurance, male | same | same | 26.6% | 26.5% (T3) |
| Neonatal mortality | births, 5 yrs before survey | DHS synthetic cohort | 21.2 | 21 (T16) |
| Infant mortality (1q0) | same | same | 31.9 | 32 (T16) |
| Child mortality (4q1) | same | same | 8.6 | 9 (T16) |
| Under-5 mortality (5q0) | same | same | 40.3 | 41 (T16) |

**12 of 13 round to the published figure.** The exception is composite under-5
mortality: each component rounds correctly, but chaining three sub-0.5 differences
puts 5q0 at 40.3 against a published 41. Closing that needs DHS's own imputation of
incomplete death dates, which is not in the public recode.

### Traps that produced wrong numbers here

- **CPR denominator.** The report's 63% is married women only. Over all women it is
  ~46%, which the dashboard showed for a while.
- **"don't know" in the ANC denominator.** Keeping it as not-4+ gives 66.0%;
  dropping it gives 67.3%.
- **Insurance is per-person, not per-household.** The household version gives 40.9%.
- **A raw proportion is not a mortality rate.** See §3.
- **An empty variable inside an `|` chain forces the estimate to 100%.** `FALSE | NA`
  is `NA`, those rows are dropped by `na.rm = TRUE`, and only the TRUEs survive.
  That is what `m3c_1` did to skilled birth attendance.
- **`factor()` on a character vector assigns levels alphabetically**, discarding the
  DHS coding order. `fct_or_na()` did this to five derived variables. Two were
  materially wrong: `maternal_education` became "higher, no education, primary,
  secondary" and `union_status` became "currently, formerly, never". Because the
  first level of a factor is a model's reference category, the alphabetical
  accident had also made *higher education* the baseline that every education
  hazard ratio was measured against — a choice nobody made. Levels are now
  preserved, so education, union status and sex take their DHS reference
  ("no education", "never in union", "male"); `maternal_age_grp` was unaffected,
  as alphabetical and DHS order coincide there. `county` is alphabetical by
  explicit choice, because DHS codes counties by official number (Mombasa 1,
  Kwale 2), which is not an order anyone can scan in a 47-item list.
  **The subtlety:** the recodes are read with `haven::as_factor()`, so the order
  is present and correct until `ch()` — plain `as.character()` — is applied.
  `fct_or_na(ch(v106))` loses it before any guard inside the helper can act, so
  the factor must be passed in directly.

### Variables KDHS 2022 did not collect

Labelled `"NA - ..."` in the DHS dictionary and 100% missing:

| Variable | Intended use | What replaced it |
|---|---|---|
| `v481`, `v481a` | health insurance (women) | `sh27` / `insured` on PR |
| `v774b`, `mv774b` | comprehensive HIV knowledge | `v781`/`mv781` ever tested, panel retitled |
| `m3c_1` | third delivery-assistance category | dropped from SBA |

### Variables whose names mislead

Verify against `02-data/kdhs-2022/stata/*.DO` before trusting any name:

| Variable | Assumed | Actually |
|---|---|---|
| `h1` | BCG | "Has health card" — BCG is `h2` |
| `h33` | fully vaccinated | "Received Vitamin A1" |
| `b8` | age in months | age in **years** (0-4) — use `b19` |
| `sh131a` | health insurance | "Own non-agricultural land" |

### Half-sample modules

Insurance, disability, chronic disease and COVID-19 went only to households
selected for the men's survey (`hv027`; 19,747 of 37,911). Use
`filter_half_sample()` in `R/survey_setup.R`, and keep households never asked as
`NA` rather than "no".

---

## 3. Child survival (U5CM)

`R/survival_core.R` and `modules/mod_survival.R`. The children's file is the source
(one row per child born in the 5 years before the survey).

### The survival object

Copied verbatim from the thesis pipeline
(`01-thesis/analysis/R/01_data_management/03_clean_data-v2.R`) so the two cannot
diverge:

```r
dead      = child is not alive
surv_time = age at death         if dead
            min(current age, 59)  if alive     # clamped to [0, 59] months
Surv(surv_time, dead)
```

Cohort: **19,530 children, 694 deaths, median follow-up 28 months, 96.4% censored.**

### Why a 0/1 logistic understates under-five mortality

`b5` is a genuine alive/dead flag, but it records death **by the age each child is
now**, not before five. Exposure is very uneven — median 28 months, and 4,029
children alive with under 12 months observed — so the "alive" group is padded with
children who have not had time to die. Four readings of the same data:

| | Per 1,000 |
|---|---|
| Naive weighted % dead on `b5` | 33.9 |
| Kaplan-Meier cumulative by 59 months | 37.3 |
| DHS period rate, synthetic cohort | 40.3 |
| Published (T16) | 41.0 |

The naive figure is ~17% low, and the size of that bias depends on the age mix of
whatever subgroup is filtered to.

### Binary outcomes, and the horizon that does not exist

Offered only where every child in the denominator lived through the horizon;
`elig_*` in `preprocess.R` enforces it:

| Outcome | Eligible | Events | Rate | Report |
|---|---|---|---|---|
| `died_neonatal` (<1 mo) | 19,197 | 426 | 20.2 | 21 |
| `died_infant` (<12 mo) | 15,390 | 506 | 32.3 | 32 |
| `died_24m` (<24 mo) | 11,581 | 423 | — | — |
| 60 months | **0** | — | **not estimable** | 41 |

No child in this file has completed 60 months, so a 60-month binary outcome has an
empty denominator. That is the concrete reason U5CM needs survival models.

### DHS synthetic-cohort mortality

`dhs_child_mortality()` in `R/indicators.R`. A mortality probability per age segment
(0, 1-2, 3-5, 6-11, 12-23, 24-35, 36-47, 48-59 months) from the children actually
exposed to that segment in the 60 months before interview, chained as
`q = 1 - prod(1 - q_i)`. Exposure is whole for a segment inside the window and half
for one straddling a boundary. A raw proportion of births that died is biased
downward by right-censoring.

### Kaplan-Meier — and why not `svykm()`

`u5cm_km()` uses `survival::survfit(weights = wt, robust = TRUE, cluster = v021)`.
`survey::svykm()` gives the same point estimates (0.9627 survival at 59 months,
checked both ways) but builds an influence matrix over every event time: on this
design it consumed **7 GB and never returned**, against **0.04 s** for `survfit`.
`u5cm_design()` also slims the 126-column children's file to the ~25 columns the
models need, which is what makes `svylogrank` and `svycoxph` fast too.

Caveat: the `robust`/`cluster` variance accounts for clustering but not strata, so
treat the bands as slightly conservative and use the log-rank and Cox output for
inference.

### Covariates (Mosley-Chen blocks)

`U5CM_COVARIATES` in `R/survival_core.R`, derived in `preprocess.R`:

- **Child** — sex, twin, birth order, preceding birth interval (<24 months), size at birth
- **Maternal** — age group, education, union status
- **Healthcare** — ANC 4+, facility birth, caesarean, ever breastfed
- **Household** — wealth, residence, improved water, improved sanitation, clean fuel, electricity, distance a big problem
- **Geography** — county

Water, sanitation and fuel use the JMP "improved" / WHO "clean" definitions; see the
`case_when` blocks in `preprocess.R`.

### Cox model and the PH assumption

`svycoxph()`, survey-weighted. `cox.zph()` reports a Schoenfeld test; a small
global p-value means at least one effect changes with the child's age, so a single
hazard ratio is averaging over a moving target — the concrete limitation that
motivates random survival forests and neural survival models.

---

## 4. Spatial analysis

### County frailty — and what it cannot give you

`u5cm_frailty_cox()` fits `coxph(... + frailty(county), weights = wt)`.
Theta ≈ 0.0143, hazard-ratio spread 0.88-1.14 across 47 counties.

**Design-based standard errors and a frailty term cannot be combined here.**
`svycoxph()` refuses penalised terms outright; `coxph()` warns that robust variance
is undefined for one. So the point estimates use the sampling weights, but the
standard errors are model-based. The panel states this rather than burying it.

It is also an **exchangeable** random effect — each county independent, with no
knowledge of which counties border which.

### Is the variation actually spatial?

`spdep` builds the adjacency (`poly2nb`, queen contiguity; 5.06 neighbours per
county on average, no islands) and tests it.

**Result as of 2026-09-15: Moran's I = 0.0004, expected −0.0217, p = 0.404.**

That is essentially no spatial autocorrelation in the residual county risk, and two
local clusters reach p < 0.05. Three readings to separate before concluding
anything:

1. The covariates may already have absorbed the spatial signal — wealth and
   education are themselves geographically clustered in Kenya.
2. Forty-seven counties may be too coarse; structure may only appear at DHS cluster
   level, which needs the GPS dataset (not currently held — see
   `../analysis/README.md`).
3. There may genuinely be none.

The first two are testable; the third would be a finding. Either way, an ICAR prior
— which borrows strength across borders — has no clear advantage over the
exchangeable frailty **at county level on these covariates**.

**Update 2026-09-16 — the national figure masks an urban signal.** Once the tab's
filters actually reached the frailty model (they did not until this date — see
[DECISIONS.md](DECISIONS.md)), the same test run within subgroups gives:

| Subset | n | deaths | theta | Moran's I | p |
|---|---|---|---|---|---|
| All children | 19,530 | 694 | 0.017 | −0.005 | 0.43 |
| **Urban** | 6,686 | 250 | 0.032 | **0.241** | **0.0008** |
| Rural | 12,844 | 444 | 0.024 | −0.013 | 0.46 |
| Poorest | 6,432 | 220 | 0.000 | 0.053 | 0.21 |
| Poorer | 3,330 | 123 | 0.233 | −0.051 | 0.63 |
| Richest | 2,725 | 95 | 0.135 | 0.086 | 0.07 |

Covariates: sex + preceding birth interval + mother's education. Among **urban**
children, residual county risk *is* spatially clustered; p = 0.0008 survives
Bonferroni correction for the six subsets (0.05/6 = 0.0083), and it is not a
sparse-county artifact — every one of the 47 counties carries at least 55 urban
children.

**Read this as a lead, not a result.** Median urban deaths per county is 5 and one
county has none, so each county's frailty is imprecise even where the pattern
across counties is not. And Moran's I here is computed on *residuals*, so it is
conditional on the covariate set — a different adjustment gives a different number,
which must be stated wherever the figure is quoted. What it does establish is that
"is the county variation spatial?" needs asking **within residence strata**, not
only nationally, before concluding an ICAR prior adds nothing.

**Expect the dashboard to show a slightly different number, and that is the point.**
The table above uses three covariates; the Cox tab ships with four selected by
default (it adds wealth). Urban then reads **I = 0.205, p = 0.003** in the running
app rather than 0.241 / 0.0008. Same direction, same conclusion, different
adjustment — which is the covariate-dependence caveat made concrete. Always record
the covariate set beside any Moran's I quoted from this dashboard.

### What is still not implemented

Fitting an ICAR spatially structured survival model. `spdep` supplies the W matrix
(`nb2mat(style = "B")`) that such a model consumes, but the fitting needs INLA (own
repository, not CRAN) or Stan. That work sits in
`../analysis/R/03_models/01_spatial_cox.R`, which already calls `poly2nb`/`nb2mat`
and is now unblocked on that dependency.

---

## 5. Provenance — what is computed, and what once was not

Kept because it is a methods-integrity question, not a changelog: if someone asks
"is this figure real?", this section is the answer.

**Every panel now computes from the data.** All **76 UI outputs across the seven
tabs have a server implementation** — verified by comparing declared outputs to
implemented ones in each module. Nothing renders a constant, and no panel spins
forever on an unimplemented output.

### Five panels previously displayed fabricated numbers

These carried hand-typed constants that looked like results. All now compute from
the recodes. **Do not cite any figure taken from these panels before 2026-09-15.**

| Panel | Module | Was | Now |
|---|---|---|---|
| ECDI by domain | `mod_new_modules` | four invented percentages | `ecd21`-`ecd40` items, scored in `preprocess.R` |
| Insurance by type | `mod_new_modules` | six invented percentages | `sh28a`-`sh28x` on the PR recode |
| Disability by domain | `mod_new_modules` | a fabricated severity matrix | Washington Group `hdis2`-`hdis8` on PR |
| Violence by type | `mod_gender` | six invented percentages | `d104`, `d105a`-`d105d`, `d106`-`d108` |
| Breastfeeding indicators | `mod_maternal` | five invented percentages | `m4` by age band from `b19` |

Two KPI cards were also hard-coded strings (under-5 mortality "41/1,000" and
stunting "18%") and are now computed and filter-responsive — see §2 and §3.

### The one series that is still not computed, deliberately

`plot_trends` on the Overview draws the 2003 → 2022 trend from **published DHS
report figures**, typed in at `mod_overview.R:385`. That is correct: those are
historical survey rounds and cannot be recomputed from the 2022 recodes. It is
labelled in the code; treat it as a citation, not an estimate.

### Rendering dependencies worth knowing

- **Basemap.** `CartoDB.Positron` began requiring an API key and rendered "API KEY
  REQUIRED" watermarks across the county maps. All maps now use `addTiles()`
  (OpenStreetMap), which needs no key.
- **`purrr` must be loaded.** `map_dfr()` is used in `mod_maternal`; it was in the
  documented dependency list but never actually `library()`d, so the vaccination
  panel failed with `could not find function "map_dfr"`.
- **`R/_disable_autoload.R` must stay.** Shiny ≥ 1.5 auto-sources `R/` before
  `app.R`, and `R/ui.R` builds `ui` at source time by calling `mod_*_ui()` from
  `modules/` — which Shiny does not autoload. Without that file the app dies at
  startup with `could not find function "mod_overview_ui"`.

---

## 6. Reproducing this

```r
# once, or after changing which variables are kept (~8 minutes)
source("preprocess.R")

# verify every figure above against the report
source("R/validate_indicators.R")

# run the dashboard
shiny::runApp(".")
```

Tested on R 4.6.0 with shiny 1.14.0, survey 4.5, survival, sf 1.1.2, spdep 1.4.2.
`R/_disable_autoload.R` must stay — see `../README.md`.
