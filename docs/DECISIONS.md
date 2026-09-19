# Decision log — KDHS 2022 dashboard

Dated record of choices that affect results, so the reasoning survives after the
conversation that produced it. **Newest first.**

Each entry answers three things: what was decided, what else was on the table, and
why this one won. If a decision is later reversed, add a new entry rather than
editing the old one — the history is the point.

**Adding an entry:** insert it directly below this header, above the current
newest. Appending to the bottom is the easy mistake — the file is reverse
chronological.

**Scope:** started 2026-09-15. Decisions made before this date are documented in
[METHODS.md](METHODS.md) and in comments at the relevant code, not here. The
methods themselves live in METHODS.md; this file is only the *why*.

---

## 2026-09-16 (later) — Cox and county frailty now follow the filters

**Reported:** selecting a filter had no effect on the county frailty map.

**Cause:** `cox_fit` and `frailty_fit` were `eventReactive(input$run_cox, ...)`.
`eventReactive` **isolates** its value expression, so reading `des()` inside it
created no dependency — only clicking "Fit Cox model" could retrigger a fit.

This was worse than an unresponsive control. The panel kept displaying a model
fitted to a **different cohort than the filter bar described**, with nothing
indicating it was stale. A reader setting Wealth = Poorer and screenshotting the
map would have captured national estimates labelled as poorer-quintile ones.

**Fixed** by making both plain reactives that depend on `des()` and on the
button, with `isolate(selected_covariates())` so ticking a covariate box still
does not fit until asked. The fit costs ~0.2 s, so there was never anything to
protect against. Everything downstream — map, ranked plot, summary, Moran's I,
LISA — hangs off `frailty_fit`, so one fix restores all of them.

**What the County filter *should* do, since it is the one filter that disables
this tab rather than reshaping it:** a county frailty estimates one random
effect per county, so its whole purpose is variation *between* counties.
Filtering to one county leaves the random effect nothing to vary over, and the
guard correctly refuses: "County frailty needs several counties — this selection
has 1." Verified that it now fires immediately rather than after a button press.

---

**A finding this bug was hiding, and the reason it matters.** With filters now
live, Moran's I on the county frailties, covariates sex + preceding birth
interval + mother's education:

| Subset | n | deaths | theta | Moran's I | p |
|---|---|---|---|---|---|
| All children | 19,530 | 694 | 0.017 | −0.005 | 0.43 |
| **Urban** | 6,686 | 250 | 0.032 | **0.241** | **0.0008** |
| Rural | 12,844 | 444 | 0.024 | −0.013 | 0.46 |
| Poorest | 6,432 | 220 | 0.000 | 0.053 | 0.21 |
| Poorer | 3,330 | 123 | 0.233 | −0.051 | 0.63 |
| Richest | 2,725 | 95 | 0.135 | 0.086 | 0.07 |

The national null recorded on 2026-09-15 is reproduced, but it was **masking an
urban signal**: among urban children, residual county risk *is* spatially
clustered. p = 0.0008 survives Bonferroni correction for the six subsets tested
(0.05/6 = 0.0083), and it is not a sparse-county artifact — all 47 counties
carry at least 55 urban children.

**Treat this as a lead, not a result.** Median urban deaths per county is 5 and
one county has none, so each county's frailty is imprecise even though the
pattern across them is not. Moran's I is also computed on *residuals*, so it
depends on the covariate set — a different adjustment gives a different answer,
and that must be stated wherever the number is reported. What the result does
justify is that "is the county variation spatial?" should be asked **within
residence strata**, not only nationally, before concluding an ICAR prior adds
nothing.

---

## 2026-09-16 — "Compare groups by" made to actually group, and a factor-order bug behind it

**Reported:** the "Compare groups by" control in Child Survival was not working.

**It was three separate faults wearing one symptom**, plus a fourth found while
fixing them.

1. **Two panels could not group at all.** `u5cm_segment_hazard()` and
   `u5cm_life_table()` took only a design — there was no `by` argument to pass,
   so the hazard bars and the life table ignored the control by construction.
   Both now accept `by`, and the life table cumulates *within* group; chaining
   across groups would have multiplied unrelated survival probabilities together
   and produced a plausible-looking wrong number.

2. **Nine covariates grouped as "0" and "1".** They are stored as integer
   flags, so a legend answered "compare by ANC 4+ visits" with `0` and `1`.

3. **Group order was alphabetical.** Kaplan-Meier ordered with `order()` on a
   character vector, so wealth read "middle, poorer, poorest, richer, richest".
   That looks like broken estimates rather than a broken sort.

   2 and 3 are now handled in one place, `u5cm_group_vector()`, shared by the
   Kaplan-Meier, hazard and life-table paths so their legends cannot drift apart.

4. **The control legitimately cannot apply to every tab** — Cox and county
   frailty take covariates from their own checkbox blocks. Rather than leave the
   reader to conclude it is broken there too, the control now states its scope.

**The fourth fault, which is the more serious one:** `fct_or_na()` ended with
`factor(x)` on a character vector, and `factor()` assigns levels
**alphabetically**. That silently discarded the DHS coding order for five derived
variables. Two were materially wrong — `maternal_education` became "higher, no
education, primary, secondary", `union_status` became "currently, formerly,
never".

This was never only cosmetic. **The first level of a factor is a model's
reference category**, so the alphabetical accident had quietly made *higher
education* the baseline that every education hazard ratio was measured against.
Nobody chose that. `fct_or_na()` now preserves the source order, which requires
re-running `preprocess.R`.

**And the fix needed a second pass, which is worth recording.** Making
`fct_or_na()` order-preserving changed nothing on the first rebuild, because the
call sites read `fct_or_na(ch(v106))` and `ch()` is `as.character()` — the order
was already gone before the helper could protect it. The recodes are read with
`haven::as_factor()`, so the factor has to be passed in directly. The helper now
carries that warning at its definition, since a function that looks
order-preserving and is silently defeated upstream is worse than one that never
claimed to be.

**Consequence to expect, not a regression:** education and union-status hazard
ratios change, because their baseline changes to "no education" and "never in
union". Same model, correct reference. `sex` also shifts: DHS codes b4 as
male = 1, female = 2, so the reference moves from female to male — the hazard
ratio inverts and means the same thing. `maternal_age_grp` is genuinely
unaffected: alphabetical and DHS order coincide for "15-19".."45-49". `county` is
deliberately kept alphabetical, since DHS codes counties by official number
(Mombasa 1, Kwale 2) and that is not an order anyone can scan in a 47-item list.

**Lesson, and it generalises past this repo:** the first three faults were
visible — a control that did nothing, "0/1" in a legend, a scrambled axis. The
fourth was invisible and changed what the models meant. Checking that a factor's
*levels* are in the intended order belongs with checking that its *values* are
right.

---

## 2026-09-15 (later) — Child Survival tab: filters, overall curve, county palette

**Reported:** filters not working from Cox PH through frailty to the life table; no
overall curve in Kaplan-Meier; grouping by county said "only 13 supplied".

**Four distinct causes, all now fixed:**

1. **The tab had no filters at all.** It shipped with only a "Compare groups by"
   control, which *groups* rather than *restricts*. Added Residence / County /
   Wealth / Mother's education, feeding one filtered design that every panel reads,
   plus a line stating how many children and deaths the current selection holds.

2. **`svy_filter_eq()` only worked on srvyr designs.** It used `dplyr::filter`,
   which has no method for the plain `survey.design2` that `u5cm_design()` returns
   — so the first filter attempt failed with *"no applicable method for 'filter'
   applied to an object of class survey.design2"*. It now detects the class and
   row-subsets with `[` for plain survey designs.

3. **The county filter silently did nothing.** Routing it through
   `apply_filters(..., "kr", ...)` mapped `county` to `v024`, but `u5cm_design()`
   slims to the covariate list, which carries the derived `county` and not `v024`.
   The guard for "column not in this design" then skipped it without complaint —
   all 47 counties stayed in. The tab now filters each column explicitly.

4. **The "overall curve" option never existed.** Its value was `""`, and selectize
   silently drops a choice whose value is the empty string, so it was absent from
   the dropdown. Replaced with an explicit `"__overall__"` token, now the default.

5. **13-colour palette against 47 counties.** `scale_colour_manual` failed with
   *"Insufficient values in manual scale. 47 needed but only 13 provided."*, which
   ggplotly surfaced as a silently blank panel rather than an error. Added
   `kdhs_palette(n)` in `R/helpers.R`, which keeps the house colours up to 13 and
   falls back to an evenly spaced hue wheel above that.

**Also added, because filtering makes them reachable:** a minimum of 20 deaths
before a Cox model will fit (deaths are rare — 694 in the full cohort, 16 in
Mandera alone), and a minimum of 5 counties before the county frailty will run,
since a single-county selection leaves the random effect nothing to vary over.

**And one consistency fix:** the DHS period rate shown beside the Kaplan-Meier
figure on tabs 1 and 6 now uses the *filtered* birth history. Previously a filter
moved one number and left the other national, which reads as a contradiction.
Mother's education is not carried in the birth recode, so that one filter cannot
apply there — the UI says so when it is set.

**Lesson worth keeping:** two of these failed *silently* — a skipped filter and a
dropped dropdown option. Neither raised an error. When a control appears to do
nothing, check that the column exists in the design being filtered and that the
choice value survived the widget.

---

## 2026-09-15 (later) — Add a provenance section to METHODS.md

**Decided:** record in `METHODS.md` §5 that five panels once displayed hand-typed
constants, and that all 76 UI outputs now compute from the data.

**Why:** a documentation audit found the earlier dashboard work was mostly covered
but three things were not written down anywhere — the panel-completion work, the
five fabricated-number panels, and the basemap/`purrr` rendering dependencies. The
second of those is a research-integrity matter, not a changelog entry: a figure
screenshotted from the ECDI, insurance-type, disability, violence-type or
breastfeeding panels before 2026-09-15 was not an estimate. That needed to be
findable by someone other than me.

**Also recorded there:** `plot_trends` remains hand-typed on purpose — those are
published figures from earlier DHS rounds and cannot be recomputed from the 2022
recodes. It is a citation, not an estimate.

---

## 2026-09-15 — Un-ignore `docs/` in .gitignore

**Decided:** comment out the blanket `docs/` rule inherited from the stock R
`.gitignore` template.

**Why:** the template ignores `docs/` because pkgdown generates a website there.
This project has no pkgdown site, so the rule silently made the entire
documentation folder untracked — `METHODS.md` and `DECISIONS.md` would never have
been committed, and the omission is invisible unless you run `git check-ignore`.
Caught immediately after creating the folder.

**Note for later:** if pkgdown is ever added, ignore its generated output by a
specific path (e.g. `docs/reference/`) rather than reinstating the blanket rule.

---

## 2026-09-15 — Documentation split into three files

**Decided:** `README.md` becomes an operating manual only (420 → 225 lines).
Methods move to `docs/METHODS.md`, rationale to this file.

**Alternatives:** leave everything in one README; or make `METHODS.Rmd`
executable so it re-runs the validation on each render.

**Why:** the single README had three incompatible audiences — someone installing
the app, someone checking a statistical definition, and someone asking why a bug
was fixed a particular way. Plain Markdown was chosen over an executable `.Rmd`
for simplicity.

**Known cost, accepted:** the numbers in `METHODS.md` are hand-written and can
drift from the code without anyone noticing. Mitigation — `METHODS.md` names
`R/validate_indicators.R` as the authoritative check, carries a "last verified"
date, and every figure in it is reproducible by running that script. **Re-run it
after any change to `preprocess.R`, `R/indicators.R` or `R/survival_core.R` and
update the tables.**

---

## 2026-09-15 — Install spdep and test whether the frailty is spatial

**Decided:** install `spdep` (pulls `spData`, `deldir`) and use it for county
adjacency (`poly2nb` / `nb2mat`), a global Moran's I test on the county
frailties, and a LISA cluster map.

**Alternatives:** skip it and leave the frailty as an untested exchangeable
effect; or wait for INLA and do the whole spatial model at once.

**Why:** the frailty in the dashboard treats every county as independent —
Mandera and Wajir share a border but the model does not know it. `theta = 0.0143`
establishes that county-level variation exists; it cannot say whether that
variation is *geographic*. Moran's I answers exactly that, which is the empirical
premise the thesis rests on. spdep also unblocks `poly2nb`/`nb2mat` at
`../../analysis/R/03_models/01_spatial_cox.R:47`, which already calls them.

**Result, and it matters:** **Moran's I = 0.0004, expected −0.0217, p = 0.404** —
essentially no spatial autocorrelation in the residual county risk (2 of 47
counties reach local significance, both low-low). So an ICAR prior has no clear
advantage over the exchangeable frailty *at county level, on these covariates*.
Three readings to separate before concluding anything: the covariates may already
have absorbed the spatial signal (wealth and education are themselves clustered in
Kenya); 47 counties may be too coarse, with structure only visible at DHS cluster
level; or there may genuinely be none. The first two are testable — the third
would be a finding. Recorded here so the negative result is not quietly lost.

**Explicitly not solved by this:** fitting an ICAR survival model. That needs INLA
(own repository, not CRAN) or Stan. spdep supplies the diagnosis and the W matrix.

---

## 2026-09-15 — Decision log starts here

**Decided:** begin the log from today rather than reconstructing earlier decisions.

**Why:** the earlier reasoning is already written down — in `METHODS.md` and in
comments at the code it explains. Backfilling would duplicate it with a second,
possibly diverging account. New decisions land here from now on.

**Where the earlier reasoning lives, if you need it:**

| Topic | Look in |
|---|---|
| Why each indicator uses the denominator it does | [METHODS.md](METHODS.md) §2 |
| Variables KDHS 2022 did not collect, and misleading variable names | [METHODS.md](METHODS.md) §2 |
| The survival object, and why there is no 60-month logistic | [METHODS.md](METHODS.md) §3 |
| Why `svykm()` is not used | [METHODS.md](METHODS.md) §3, and `R/survival_core.R` → `u5cm_km()` |
| Why the frailty model cannot have design-based SEs | [METHODS.md](METHODS.md) §4, and `R/survival_core.R` → `u5cm_frailty_cox()` |
| Why `reorder()` and `if()` must stay out of `aes()` | `../README.md` → Plot labels |
| Why filters compare case-insensitively and guard empty selections | `../README.md` → Filters |
| Why `R/_disable_autoload.R` must stay | `../README.md` → Common Issues |

