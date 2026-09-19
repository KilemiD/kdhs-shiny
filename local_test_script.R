## We need to test that the data works locally, and properly
## so that we can effectively debug the dashboard

# 1. LOAD LIBRARIES & SOURCE YOUR FILES
library(haven)
library(dplyr)
library(srvyr)
library(ggplot2)
library(sf)
library(leaflet)
library(plotly)

source("R/data_loader.R") # Uses your existing PATHS and load functions

# 2. LOAD DATA (ONLY ONCE)
# If this takes too long, load only the 'ir' and 'counties'
cat("Checking data files...\n")
ir_raw <- load_recode("ir")
counties_shp <- load_county_shapefile()

## SET UP THE SURVEY DESIGN
create_ir_design <- function(ir) {
  ir |>
    mutate(wt = as.numeric(v005) / 1e6) |>
    as_survey_design(
      ids     = v021,     # PSU
      strata  = v023,     # Strata
      weights = wt,       # Normalized weight
      nest    = TRUE      # PSUs are nested within strata
    )
}

ir_design <- create_ir_design(ir_raw)

# calculate statistics using the design object
county_wealth <- ir_design |>
  group_by(v024) |>
  summarise(
    rich_percent = survey_mean(v190 == "richest", na.rm = T) * 100 
  )

# Checking if m14 column exists
# Testing ANC visits using the design object
anc_test <-  ir_design |>
  summarise(
    avg_anc = survey_mean(as.numeric(m14_1), na.rm = T)
  )

# Use 'ir_raw' to check names, as 'ir_design' is a complex object
available_vars <- names(ir_raw) 
anc_matches <- available_vars[grep("m14", available_vars)]

if (length(anc_matches) > 0) {
  # For 2022, this is almost certainly "m14_1"
  anc_var <- anc_matches[1] 
  cat("Success! Using variable:", anc_var, "\n")
} else {
  stop("m14 not found in the dataset.")
}

# try to print kpi results utilizing weights
kpi_results <- ir_design %>% 
  summarise(
    tfr = survey_mean(v201, na.rm = T),
    anc4 = survey_mean(as.numeric(get(anc_var)) >=4, na.rm = T)*100
  )

# checking whether the map works locally
# REVISED STEP 3: THE MAP JOIN (Ensuring Region-to-County Alignment)
# =============================================================================
cat("\n--- Testing Map Join with Survey Weights ---\n")

county_data <- ir_design %>% 
  group_by(v024) %>% 
  summarise(
    pct = survey_mean(as.numeric(v313 %in% c("modern method", "traditional method", "folkloric method")), na.rm = T)*100
  ) %>% 
  mutate(county_name = as.character(v024))

# naming consistency for county names is crucia, including caps
county_data <- county_data %>% 
  mutate(county_name = trimws(tolower(county_name)))

counties_shp_clean <- counties_shp %>% 
  mutate(NAME_1_clean = trimws(tolower(NAME_1)))

# perform the join
map_joined <-  counties_shp_clean %>% 
  left_join(county_data , by = c("NAME_1_clean" = "county_name"))

# quick diagnostic that the map works
na_count <- sum(is.na(map_joined$pct))

# check whether the map works
pal <- colorNumeric("YlOrRd", domain = map_joined$pct, na.color = "#808080")

test_map <- leaflet(map_joined) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    fillColor = ~pal(pct),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    label = ~paste0(NAME_1, ": ", round(pct, 1), "%"),
    highlightOptions = highlightOptions(weight = 3, color = "black",
                                        bringToFront = T)
  )


# If these return 'FALSE', we need to join your IR data with the KR data
exists("hw70_1", ir_raw) # Stunting
exists("b5_01", ir_raw)   # Child Survival
exists("m3a_1", ir_raw)   # Delivery Assistance
exists("v024", ir_raw)


# checking other variables for the map
# 1. CLEANING & INDICATOR DEFINITION
# We use as.character() to safely handle factor labels like "no" or "died"
cat("Processing Under-5 Mortality data...\n")

ir_u5m <- ir_raw %>%
  mutate(
    # Century Month Code of interview (v008) and birth (b3_01)
    # We check the first child (_01). For a full rate, you'd check all children,
    # but for mapping, the most recent birth is the standard proxy.
    months_ago = v008 - b3_01,
    
    # Target: Children born in the last 5 years
    is_recent_birth = ifelse(months_ago >= 0 & months_ago < 60, 1, 0),
    
    # Mortality Indicator: 1 if child died, 0 if alive, NA if no birth in last 5 yrs
    u5m_dead = case_when(
      is_recent_birth == 0 ~ NA_real_,
      tolower(as.character(b5_01)) %in% c("0", "no", "died") ~ 1,
      tolower(as.character(b5_01)) %in% c("1", "yes", "alive") ~ 0,
      TRUE ~ NA_real_
    )
  )

# 2. CREATE DESIGN
u5m_design <- create_ir_design(ir_u5m)

# 3. CALCULATE COUNTY STATS
u5m_county <- u5m_design %>%
  group_by(v024) %>%
  summarise(
    # Multiply by 1000 if you want "Deaths per 1,000 live births" (DHS standard)
    # or by 100 for a simple percentage. Let's use 1000 for the map.
    rate = survey_mean(u5m_dead, na.rm = TRUE) * 1000
  ) %>%
  mutate(county_name = as.character(v024))

# 4. JOIN TO SHAPEFILE
u5m_map_sf <- counties_shp %>%
  mutate(match_name = tolower(trimws(NAME_1))) %>%
  left_join(
    u5m_county %>% mutate(match_name = tolower(trimws(county_name))),
    by = "match_name"
  )

# 5. DRAW THE MAP
# U5MR is usually low (e.g., 20 - 60), so we use a specific palette
pal <- colorNumeric(palette = "Reds", domain = u5m_map_sf$rate, na.color = "#808080")

leaflet(u5m_map_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(rate),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    label = ~paste0(NAME_1, ": ", round(rate, 1), " per 1k births")
  ) %>%
  addLegend("bottomright", pal = pal, values = ~rate, 
            title = "U5 Mortality Rate", labFormat = labelFormat(suffix = ""))
