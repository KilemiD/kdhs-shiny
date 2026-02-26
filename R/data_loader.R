# =============================================================================
# R/data_loader.R
# Loads all KDHS 2022 recode files from the /data folder.
# Keeps originals immutable — all transformations happen downstream.
# =============================================================================

# ---- PATHS ------------------------------------------------------------------
# Place your raw .dta files in the /data folder.
# Rename them to the convention below (or adjust these paths).
DATA_DIR <- "data"

PATHS <- list(
  hr = file.path(DATA_DIR, "KEHR8CFL.DTA"),   # Household Recode
  pr = file.path(DATA_DIR, "KEPR8CFL.DTA"),   # Household Members (PR)
  ir = file.path(DATA_DIR, "KEIR8CFL.DTA"),   # Women's Individual Recode
  mr = file.path(DATA_DIR, "KEMR8CFL.DTA"),   # Men's Recode
  kr = file.path(DATA_DIR, "KEKR8CFL.DTA"),   # Children's Recode
  br = file.path(DATA_DIR, "KEBR8CFL.DTA"),   # Birth Recode
  cr = file.path(DATA_DIR, "KECR8CFL.DTA"),   # Couples Recode
  ge = file.path(DATA_DIR, "Kenya_Counties_(080719).shp")    # GPS / Spatial (shapefile)
)

# ---- LOADER FUNCTION --------------------------------------------------------
#' Load a KDHS recode file safely, with informative error messages.
#' @param key Character. One of: "hr", "pr", "ir", "mr", "kr", "br", "cr"
#' @return A tibble with variable labels preserved.
load_recode <- function(key) {
  path <- PATHS[[key]]
  if (!file.exists(path)) {
    stop(glue::glue(
      "File not found: {path}\n",
      "Please download the 2022 KDHS {toupper(key)} recode from https://dhsprogram.com ",
      "and place it in the /data folder."
    ))
  }
  message(glue::glue("Loading {toupper(key)} recode from {path}..."))
  haven::read_dta(path, encoding = "latin1") |>
    haven::as_factor()  # Convert value labels to factors
}

# ---- LOAD GPS SHAPEFILE -----------------------------------------------------
load_gps <- function() {
  path <- PATHS[["ge"]]
  if (!file.exists(path)) {
    warning("GPS file not found. Maps will be disabled.")
    return(NULL)
  }
  sf::st_read(path, quiet = TRUE)
}

# ---- LOAD COUNTY SHAPEFILE --------------------------------------------------
# Download Kenya county boundaries from GADM: https://gadm.org/download_country.html
# Save as data/kenya_counties.shp
load_county_shapefile <- function() {
  path <- file.path(DATA_DIR, "Kenya_Counties_(080719).shp")
  if (!file.exists(path)) {
    warning("County shapefile not found. Choropleth maps will be disabled.")
    return(NULL)
  }
  sf::st_read(path, quiet = TRUE)
}

# ---- CONVENIENCE: LOAD ALL AT ONCE ------------------------------------------
#' Load all recode files into a named list.
#' Only call this if you have all files. Use load_recode() individually otherwise.
load_all_recodes <- function() {
  list(
    hr = load_recode("hr"),
    pr = load_recode("pr"),
    ir = load_recode("ir"),
    mr = load_recode("mr"),
    kr = load_recode("kr"),
    br = load_recode("br"),
    cr = load_recode("cr"),
    ge = load_gps(),
    counties = load_county_shapefile()
  )
}
