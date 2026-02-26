# =============================================================================
# KDHS 2022 R Shiny Dashboard — app.R
# Entry point: loads UI and server, sources all modules
# =============================================================================

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

# Source all module files
source("R/data_loader.R")
source("R/survey_setup.R")
source("R/helpers.R")
source("modules/mod_overview.R")
source("modules/mod_maternal.R")
source("modules/mod_gender.R")
source("modules/mod_hiv.R")
source("modules/mod_new_modules.R")
source("modules/mod_explorer.R")

# Load app UI and server
source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)
