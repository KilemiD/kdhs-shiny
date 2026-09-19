# =============================================================================
# R/data_loader.R
# Loads all KDHS 2022 recode files from the /data folder.
# Keeps originals immutable — all transformations happen downstream.
# =============================================================================

# ---- PATHS ------------------------------------------------------------------
# Raw .dta files live in 02-data/kdhs-2022/stata/ and are read only by preprocess.R.
# This loader reads the .rds files preprocess.R writes.
# Shared data root (see phd/README.md). Paths are relative to this app folder.
DATA_DIR    <- "../../02-data/kdhs-2022/processed"
SPATIAL_DIR <- "../../02-data/spatial"

PATHS <- list(
  hr = file.path(DATA_DIR, "hr.rds"),   # Household Recode
  pr = file.path(DATA_DIR, "pr.rds"),   # Household Members (one row per person)
  ir = file.path(DATA_DIR, "ir.rds"),   # Women's Individual Recode
  mr = file.path(DATA_DIR, "mr.rds"),   # Men's Recode
  kr = file.path(DATA_DIR, "kr.rds"),   # Children's Recode
  br = file.path(DATA_DIR, "br.rds"),   # Birth Recode
  #cr = file.path(DATA_DIR, "KECR8CFL.DTA"),   # Couples Recode
  ge = file.path(SPATIAL_DIR, "Kenya_Counties_(080719).shp")    # GPS / Spatial (shapefile)
)

# ---- LOADER FUNCTION --------------------------------------------------------
#' Load a KDHS recode file safely, with informative error messages.
#' @param key Character. One of: "hr", "pr", "ir", "mr", "kr", "br", "cr"
#' @return A tibble with variable labels preserved.
load_recode <- function(key) {
  # Use the RDS path defined in your PATHS list
  path <- PATHS[[key]]
  
  if (!file.exists(path)) {
    stop(paste("File not found:", path))
  }
  
  # Use native readRDS for .rds files. 
  # Avoid using haven::read_dta on .rds files.
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    return(readRDS(path))
  } else {
    return(haven::read_dta(path) %>% haven::as_factor())
  }
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
# Stored at 02-data/spatial/Kenya_Counties_(080719).shp
load_county_shapefile <- function() {
  path <- file.path(SPATIAL_DIR, "Kenya_Counties_(080719).shp")
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
    #cr = load_recode("cr"),
    ge = load_gps(),
    counties = load_county_shapefile()
  )
}
