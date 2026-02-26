# =============================================================================
# modules/mod_overview.R
# Dashboard 1: The Big Picture — key national indicators with county maps
# =============================================================================

# ---- UI ---------------------------------------------------------------------
mod_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Filters row
    wellPanel(
      fluidRow(
        column(3, selectInput(ns("residence"), "Residence",
                              choices = c("All","Urban","Rural"), selected = "All")),
        column(3, selectInput(ns("region"), "Region",
                              choices = c("All","Nairobi","Central","Coast",
                                          "Eastern","Nyanza","Rift Valley",
                                          "Western","North Eastern"), selected = "All")),
        column(3, selectInput(ns("wealth"), "Wealth Quintile",
                              choices = c("All","Poorest","Poorer","Middle",
                                          "Richer","Richest"), selected = "All")),
        column(3, selectInput(ns("age_group"), "Age Group (Women)",
                              choices = c("All","15-19","20-24","25-29",
                                          "30-34","35-39","40-44","45-49"),
                              selected = "All"))
      )
    ),

    # KPI Cards row
    fluidRow(
      column(2, uiOutput(ns("kpi_tfr"))),
      column(2, uiOutput(ns("kpi_cpr"))),
      column(2, uiOutput(ns("kpi_anc"))),
      column(2, uiOutput(ns("kpi_u5mr"))),
      column(2, uiOutput(ns("kpi_insurance"))),
      column(2, uiOutput(ns("kpi_stunting")))
    ),

    br(),

    # Charts row
    fluidRow(
      column(6,
        wellPanel(
          h5("Contraceptive Prevalence by Wealth Quintile"),
          plotlyOutput(ns("plot_cpr_wealth"), height = "320px") |> withSpinner()
        )
      ),
      column(6,
        wellPanel(
          h5("ANC 4+ Visits: Urban vs Rural by Region"),
          plotlyOutput(ns("plot_anc_region"), height = "320px") |> withSpinner()
        )
      )
    ),

    # Map row
    fluidRow(
      column(7,
        wellPanel(
          fluidRow(
            column(6, h5("County Map")),
            column(6, selectInput(ns("map_indicator"), NULL,
                                  choices = c(
                                    "Contraceptive Prevalence"  = "cpr",
                                    "ANC 4+ Visits"             = "anc4",
                                    "Skilled Birth Attendance"  = "sba",
                                    "Under-5 Mortality Rate"    = "u5mr",
                                    "Health Insurance Coverage" = "insurance",
                                    "Stunting (children <5)"    = "stunting"
                                  )))
          ),
          leafletOutput(ns("county_map"), height = "380px") |> withSpinner()
        )
      ),
      column(5,
        wellPanel(
          h5("Trend: Key Indicators 2003–2022"),
          plotlyOutput(ns("plot_trends"), height = "380px") |> withSpinner()
        )
      )
    )
  )
}

# ---- SERVER -----------------------------------------------------------------
mod_overview_server <- function(id, ir_design, hr_design, raw_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Filtered design (responds to filter inputs) ------------------------
    filtered_ir <- reactive({
      design <- ir_design()
      req(design)

      if (input$residence != "All") {
        residence_val <- ifelse(input$residence == "Urban", "urban", "rural")
        design <- design |> filter(v025 == residence_val)
      }
      if (input$wealth != "All") {
        design <- design |> filter(as.character(v190) == input$wealth)
      }
      if (input$age_group != "All") {
        # v013 is the 5-year age group variable in IR
        design <- design |> filter(as.character(v013) == input$age_group)
      }
      design
    })

    # ---- KPI Cards ----------------------------------------------------------
    output$kpi_tfr <- renderUI({
      req(filtered_ir())
      val <- filtered_ir() |>
        summarise(tfr = survey_mean(as.numeric(v201), na.rm = TRUE)) |>
        pull(tfr)
      stat_box(round(val, 1), "Total Fertility Rate", "children per woman")
    })

    output$kpi_cpr <- renderUI({
      req(filtered_ir())
      val <- filtered_ir() |>
        mutate(.fp = as.numeric(v313 %in% c(1, 2, 3))) |>
        summarise(cpr = survey_mean(.fp, na.rm = TRUE)) |>
        pull(cpr) * 100
      stat_box(fmt_pct(val), "Contraceptive Prevalence", "any method", color = "green")
    })

    output$kpi_anc <- renderUI({
      req(filtered_ir())
      val <- filtered_ir() |>
        filter(!is.na(m14_1)) |>
        mutate(.anc4 = as.numeric(as.numeric(m14_1) >= 4)) |>
        summarise(anc4 = survey_mean(.anc4, na.rm = TRUE)) |>
        pull(anc4) * 100
      stat_box(fmt_pct(val), "ANC 4+ Visits", "last birth")
    })

    output$kpi_u5mr <- renderUI({
      stat_box("41/1,000", "Under-5 Mortality", "from BR recode", color = "red")
    })

    output$kpi_insurance <- renderUI({
      req(filtered_ir())
      val <- filtered_ir() |>
        mutate(.ins = as.numeric(v481 == "yes")) |>
        summarise(ins = survey_mean(.ins, na.rm = TRUE)) |>
        pull(ins) * 100
      stat_box(fmt_pct(val), "Health Insurance", "any type", color = "red")
    })

    output$kpi_stunting <- renderUI({
      stat_box("18%", "Stunting (<5 yrs)", "from KR recode", color = "red")
    })

    # ---- CPR by Wealth Quintile ---------------------------------------------
    output$plot_cpr_wealth <- renderPlotly({
      req(filtered_ir())
      data <- filtered_ir() |>
        group_by(v190) |>
        mutate(.fp = as.numeric(v313 %in% c(1, 2, 3))) |>
        summarise(
          pct     = survey_mean(.fp, na.rm = TRUE, vartype = "ci") * 100
        ) |>
        mutate(v190 = fmt_label(v190))

      kdhs_bar(data, x = "v190", y = "pct",
               title = "Modern Contraceptive Use by Wealth",
               subtitle = "Weighted estimates with 95% CI")
    })

    # ---- ANC 4+ by Region & Residence --------------------------------------
    output$plot_anc_region <- renderPlotly({
      req(filtered_ir())
      data <- ir_design() |>
        filter(!is.na(m14_1)) |>
        mutate(
          .anc4     = as.numeric(as.numeric(m14_1) >= 4),
          residence = fmt_label(v025)
        ) |>
        group_by(v024, residence) |>
        summarise(pct = survey_mean(.anc4, na.rm = TRUE, vartype = NULL) * 100) |>
        mutate(v024 = fmt_label(v024))

      kdhs_grouped_bar(data, x = "v024", y = "pct", group = "residence",
                       title = "ANC 4+ by Region and Residence")
    })
    
    ## reactive element for counties
    county_stats <- reactive({
      req(filtered_ir(), input$map_indicator)
      
      # Calculate indicator based on dropdown selection
      stats <- filtered_ir() |>
        mutate(
          # Define the binary indicator based on user choice
          .ind = case_when(
            input$map_indicator == "cpr"       ~ as.numeric(v313 %in% c("modern method", "traditional method", "folkloric method")),
            input$map_indicator == "anc4"      ~ as.numeric(as.numeric(m14_1) >= 4), # Note: Using m14_1 for 2022
            input$map_indicator == "insurance" ~ as.numeric(v481 == "yes"),
            TRUE ~ 0
          )
        ) |>
        group_by(v024) |> # v024 = County in KDHS 2022
        summarise(
          val = survey_mean(.ind, na.rm = TRUE) * 100
        ) |>
        mutate(county_name = as.character(v024))
      
      stats
    })
    
    # ---- 2. Rewritten Map Output --------------------------------------------
    output$county_map <- renderLeaflet({
      req(raw_data$counties, county_stats())
      
      # Prepare the Shapefile Data
      # IMPORTANT: Ensure 'NAME_1' in your SHP matches 'county_name' in DHS
      # You may need: mutate(NAME_1 = toupper(NAME_1)) if casing differs
      map_sf <- raw_data$counties %>% 
        mutate(NAME_1_clean = trimws(tolower(NAME_1)))|>
        left_join(county_stats(), by = c("NAME_1_clean" = "county_name"))
      
      # Create a dynamic color palette based on the current data range
      pal <- colorNumeric(
        palette = "YlOrRd", 
        domain = map_sf$val, 
        na.color = "#808080"
      )
      
      leaflet(map_sf) |>
        addProviderTiles("CartoDB.Positron") |>
        setView(lng = 37.9, lat = 0.02, zoom = 6) |> # Center on Kenya
        addPolygons(
          fillColor   = ~pal(val), 
          fillOpacity = 0.8,
          color       = "white",
          weight      = 1,
          # Popup/Label showing the county name and the value
          label       = ~paste0(NAME_1, ": ", round(val, 1), "%"),
          highlightOptions = highlightOptions(
            color = "black", 
            weight = 2, 
            bringToFront = TRUE
          )
        ) |>
        addLegend(
          "bottomright", 
          pal = pal, 
          values = ~val,
          title = paste0(input$map_indicator, " (%)"),
          labFormat = labelFormat(suffix = "%"),
          opacity = 1
        )
    })

    # # ---- County Map ---------------------------------------------------------
    # output$county_map <- renderLeaflet({
    #   req(raw_data$counties)
    #   # Placeholder: build county-level indicator summaries and join to shapefile
    #   # In production: compute county proportions from ir_design() grouped by v024/county
    #   leaflet(raw_data$counties) |>
    #     addProviderTiles("CartoDB.Positron") |>
    #     addPolygons(
    #       fillColor  = "steelblue",
    #       fillOpacity = 0.6,
    #       color      = "white",
    #       weight     = 1,
    #       label      = ~NAME_1,
    #       highlightOptions = highlightOptions(
    #         color = "white", weight = 2.5, bringToFront = TRUE
    #       )
    #     ) |>
    #     addLegend("bottomright", title = input$map_indicator,
    #               colors = c("#fee5d9","#fc9272","#de2d26"),
    #               labels = c("Low", "Medium", "High"))
    # })

    # ---- Trend Data (hardcoded from DHS reports 2003–2022) ------------------
    trend_data <- tibble(
      year      = c(2003, 2008, 2014, 2022),
      cpr       = c(39.3, 45.5, 58.0, 65.0),
      anc4      = c(52.0, 47.0, 58.0, 73.0),
      sba       = c(41.6, 43.8, 61.2, 87.0),
      insurance = c(NA,   NA,   17.0, 26.0)
    ) |>
      pivot_longer(-year, names_to = "indicator", values_to = "pct")

    output$plot_trends <- renderPlotly({
      kdhs_trend(trend_data, x = "year", y = "pct", group = "indicator",
                 title = "Progress on Key Indicators: 2003–2022")
    })
  })
}
