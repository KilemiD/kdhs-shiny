# KDHS 2022 R Shiny Dashboard
Setup and operating guide.

## Documentation map

Three files, each with one job — start with whichever matches your question:

| Question | File |
|---|---|
| How do I install and run this? | **this file** |
| What does this number mean, and does it match the report? | [docs/METHODS.md](docs/METHODS.md) |
| Why was it built this way? | [docs/DECISIONS.md](docs/DECISIONS.md) |

`R/validate_indicators.R` is the authoritative check on every published
comparison — run it after changing `preprocess.R` or `R/indicators.R`.

## Project Structure

```
01-thesis/dashboard/
├── app.R                    # Entry point — run this to launch the app
├── preprocess.R             # One-off: reads raw .DTA, writes the .rds the app loads
├── local_test_script.R      # Ad-hoc checks against the survey design
├── R/
│   ├── data_loader.R        # Reads the preprocessed .rds files
│   ├── survey_setup.R       # Creates weighted survey design objects
│   ├── helpers.R            # Shared plotting, format and filter-guard functions
│   ├── indicators.R         # Report-aligned indicator definitions
│   ├── survival_core.R      # U5CM survival blocks + spatial diagnostics
│   ├── validate_indicators.R# Recomputes every figure vs the published report
│   ├── _disable_autoload.R  # Must stay — see "Common Issues"
│   ├── ui.R                 # Main app UI (navbar + tabs)
│   └── server.R             # Main server (loads data, calls modules)
├── modules/
│   ├── mod_overview.R       # Tab 1: National overview + county map
│   ├── mod_survival.R       # Tab 2: Child Survival (U5CM) — the thesis core
│   ├── mod_maternal.R       # Tab 3: Maternal & child health
│   ├── mod_gender.R         # Tab 4: Gender, GBV, FGM/C
│   ├── mod_hiv.R            # Tab 5: HIV & sexual health
│   ├── mod_new_modules.R    # Tab 6: ECDI, insurance, disability, COVID-19
│   └── mod_explorer.R       # Tab 7: Research explorer (crosstabs, regression)
├── docs/
│   ├── METHODS.md           # What every figure measures, vs the report
│   └── DECISIONS.md         # Dated log of why it is built this way
└── www/
    └── kenya_flag.png       # Optional: flag image for navbar
```

## Where the data lives

Data is **not** in this repo. It sits in the shared data root one level up, so the
thesis analysis pipeline can use the same files:

```
phd/02-data/
├── kdhs-2022/
│   ├── stata/       KEIR8CFL.DTA, KEHR8CFL.DTA, … (raw recodes — preprocess.R only)
│   ├── processed/   hr.rds, ir.rds, mr.rds, kr.rds, br.rds  ← what the app loads
│   ├── archives/    DT/FL/SV/SD.zip as downloaded from DHS
│   └── docs/        DDI documentation, questionnaires, KDHS 2022 reports
└── spatial/         Kenya_Counties_(080719).shp, Subcounty Kenya.zip
```

Three constants control this — change them together if the tree moves:

| File | Constant | Value |
|---|---|---|
| `R/data_loader.R` | `DATA_DIR` | `../../02-data/kdhs-2022/processed` |
| `R/data_loader.R` | `SPATIAL_DIR` | `../../02-data/spatial` |
| `preprocess.R` | `DATA_DIR` / `OUT_DIR` | `../../02-data/kdhs-2022/{stata,processed}` |

The app itself only reads the five `.rds` files plus the shapefile (~10 MB). The
600 MB of raw `.DTA` is touched only when you re-run `preprocess.R`.

## Step 1: Get the Data

Already downloaded — see `02-data/kdhs-2022/`. To obtain it fresh:

1. Register at https://dhsprogram.com/data/dataset_admin/login_main.cfm
2. Request access to **Kenya 2022 (KE8)** — select all recode types
3. Download **Stata (.dta)** format for all recodes
4. Place files in `02-data/kdhs-2022/stata/`

**The DHS data licence forbids redistributing these files.** `.gitignore` blocks
`*.DTA`, `*.rds` and `data/` so they cannot be committed by accident.

Kenya county shapefile:
- Download from: https://gadm.org/download_country.html → Kenya → Level 1
- Current copy: `02-data/spatial/Kenya_Counties_(080719).shp`

---

## Step 2: Install R Packages

```r
install.packages(c(
  "shiny", "bslib",       # UI framework
  "haven",                 # Read .dta Stata files
  "survey", "srvyr",      # Weighted survey analysis
  "dplyr", "tidyr",       # Data wrangling
  "ggplot2", "plotly",    # Charts
  "leaflet", "sf",        # Maps
  "DT",                   # Interactive tables
  "scales",               # Formatting
  "shinycssloaders",      # Loading spinners
  "glue",                 # String interpolation
  "purrr",                # map_dfr() — required by mod_maternal
  "broom",                # tidy model output for the regression tables
  "survival",             # Surv(), survfit(), coxph(), cox.zph()
  "spdep"                 # county adjacency, Moran's I, LISA clusters
))

# Optional: rdhs package to programmatically download DHS data
# install.packages("rdhs")
```

---

## Step 3: Run the App

```r
# From R console:
setwd("path/to/phd/01-thesis/dashboard")
shiny::runApp()

# Or open app.R in RStudio and click "Run App"
```

---

## Key Design Decisions

| Decision | Choice | Reason |
|---|---|---|
| UI framework | `bslib` (Bootstrap 5) | Modern, responsive, easy theming |
| Survey weighting | `srvyr` + `survey` | Correct complex survey estimates |
| Charts | `ggplot2` + `plotly` | Publication-quality + interactive |
| Maps | `leaflet` + `sf` | County-level choropleths |
| Module pattern | Shiny modules | Scalable, organized, testable |
| Data loading | Once per session | Efficient — large files |
| Half-sample | Explicit filter flag | Prevents common analysis errors |

---

## Deployment (shinyapps.io)

```r
library(rsconnect)
rsconnect::deployApp(
  appDir   = ".",
  appName  = "kdhs2022-dashboard",
  account  = "your-account-name"
)
```

**Before deploying:** Do not include raw data files. Pre-compute summary statistics
and save as `.rds` files, then load those instead of raw recodes.

```r
# Example: Pre-compute and save IR summaries
ir_summaries <- precompute_ir_summaries(ir_svy)
saveRDS(ir_summaries, file.path(DATA_DIR, "ir_summaries.rds"))
```

---

## Plot labels: never put an expression inside aes()


ggplotly() uses the **aes expression itself** as the hover label and the axis
title. So `aes(x = reorder(county, pct))` shows readers a tooltip reading
`reorder(county, pct): Mandera` and an axis titled `reorder(county, pct)`, and
`aes(colour = if (!is.null(group)) ...)` put the whole `if` statement in the
legend. Two rules follow:

- **Never call `reorder()` inside `aes()`.** Use `order_levels(data, "county", "pct")`
  from `R/helpers.R` first, then map the column by name.
- **Set an explicit `text` aesthetic and pass `tooltip = "text"`** to `ggplotly()`.
  Otherwise hovers show raw DHS codes at full precision (`v190: Poorest /
  pct: 34.03149` instead of `Poorest: 34.0%`).

`coord_polar` + `theme_void` cannot be converted at all — ggplotly fails with
`rng[[xy]]$get_labels: attempt to apply non-function`. Build pie charts with
`plot_ly(type = "pie")` directly; see `auto_health` in `modules/mod_gender.R`.

## Filters: match the data's case, and guard empty selections

The filter dropdowns are written in Title Case ("Poorest", "No education") while
the DHS value labels are lower case ("poorest", "no education"). A plain
`as.character(v190) == input$wealth` therefore matched **nothing**, leaving an
empty design — and srvyr's `summarise()` on an empty grouped design dies with
an opaque `subscript out of bounds` from `cur_svy_env$split[[cur_group_id()]]`.

Use the two helpers instead of comparing directly:

- `svy_filter_eq(design, "v190", input$wealth)` — case-insensitive, and a no-op
  when the choice is "All".
- `svy_has_rows(design)` — call it in a `validate(need(...))` before summarising
  anything derived from a user filter, so an impossible combination shows
  "No respondents match this filter combination." instead of crashing.

**Populate county dropdowns from the data.** The Region controls were hard-coded
to the eight pre-2010 provinces while `v024` holds the 47 counties, so they could
never match — and in the Overview the filter was not even read. Both are now
filled by `updateSelectInput()` from `levels(v024)` on startup.

**Cross-tab variable lists must follow the dataset.** `IR_VARS` only describes the
women's file; picking "Age group (5yr)" = `v013` with the Men or Household dataset
selected crashed with `Element v013 doesn't exist`. `crosstab_choices()` in
`modules/mod_explorer.R` now rebuilds the choices from the selected design's own
categorical columns.

## Common Issues

**"File not found" on startup**
→ Check `DATA_DIR` in `R/data_loader.R` still points at `../../02-data/kdhs-2022/processed`

**Estimates don't match report**
→ Check v005 is divided by 1,000,000. Check v023 strata variable. Use `survey::svytable()` to debug.

**Half-sample variables show NA for all**
→ Filter to the half-sample before computing. Check the `filter_half_sample()` function in `survey_setup.R`.

**Map doesn't appear**
→ Confirm `SPATIAL_DIR` resolves to `../../02-data/spatial`. Check the CRS matches (should be WGS84 / EPSG:4326).
