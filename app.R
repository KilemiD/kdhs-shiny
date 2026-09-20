# =============================================================================
# KDHS 2022 R Shiny Dashboard — app.R
# Entry point: loads UI and server, sources all modules before running
# =============================================================================
# Clear memory and close all connections before starting
# Clear the R environment and trigger Garbage Collection
rm(list = ls(all.names = TRUE))
gc()

# Clear the Temp directory (targets R-specific temporary files)
temp_files <- list.files(tempdir(), full.names = TRUE)
unlink(temp_files, recursive = TRUE)

cat("🧹 Environment and Temp files cleaned.\n")

library(shiny)
library(bslib)        # Modern Bootstrap-based UI
library(haven)        # Read Stata .dta files
library(survey)       # Complex survey analysis (weighted)
library(srvyr)        # Tidyverse-style survey wrappers
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)       # Interactive charts
library(leaflet)      # Maps
library(sf)           # Spatial data (county shapefiles)
library(scales)
library(DT)           # Interactive tables
library(shinycssloaders) # Loading spinners
library(spdep)        # county adjacency, Moran's I, LISA clusters
library(purrr)        # map_dfr() in mod_maternal. Listed in the README's# dependency list but never actually loaded, so the # vaccination panel failed with: #   could not find function "map_dfr"
library(broom)        # tidy() for the regression table in mod_explorer

# Source all module files
source("R/data_loader.R")
source("R/survey_setup.R")
source("R/helpers.R")
# Report-aligned indicator definitions. Checked against the published KDHS 2022
# Key Indicators Report — run source("R/validate_indicators.R") to re-verify.
source("R/indicators.R")
source("R/survival_core.R")   # U5CM survival building blocks
source("modules/mod_overview.R")
source("modules/mod_maternal.R")
source("modules/mod_gender.R")
source("modules/mod_hiv.R")
source("modules/mod_new_modules.R")
source("modules/mod_explorer.R")
source("modules/mod_survival.R")

# Load app UI and server
source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)