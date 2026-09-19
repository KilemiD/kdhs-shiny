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
                                    # "Health Insurance Coverage" removed: it is a
                                    # household-level (HR) measure and this map is
                                    # built from the women's (IR) design. Needs a
                                    # separate HR-based branch to come back.
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
mod_overview_server <- function(id, ir_design, hr_design,
                                kr_design, br_design, pr_design, raw_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # One place to read the filter controls. apply_filters() (R/indicators.R)
    # maps these onto whichever recode it is given — the women's, children's and
    # birth files use v024/v025/v190, the household-member file hv024/hv025/hv270.
    # `age` is a woman's age group, so it only applies to the women's file and is
    # skipped elsewhere.
    active_filters <- reactive(list(
      residence = input$residence,
      wealth    = input$wealth,
      county    = input$region,
      age       = input$age_group
    ))

    # TRUE when a filter is set that the child/household cards cannot honour.
    age_only_filter <- reactive(!identical(input$age_group, "All"))

    filtered_kr <- reactive({ req(kr_design()); apply_filters(kr_design(), "kr", active_filters()) })
    filtered_br <- reactive({ req(br_design()); apply_filters(br_design(), "br", active_filters()) })
    filtered_pr <- reactive({ req(pr_design()); apply_filters(pr_design(), "pr", active_filters()) })

    # ---- Filtered design (responds to filter inputs) ------------------------
    # The dropdowns are Title Case ("Poorest") but the DHS value labels are lower
    # case ("poorest"), so `as.character(v190) == input$wealth` matched nothing.
    # That left an EMPTY design, and srvyr's summarise() on an empty grouped
    # design fails with "subscript out of bounds" — which is what broke the
    # wealth chart and the county map the moment a filter was touched.
    # svy_filter_eq() compares case-insensitively; see R/helpers.R.
    filtered_ir <- reactive({
      design <- ir_design()
      req(design)
      design <- svy_filter_eq(design, "v025", input$residence)
      design <- svy_filter_eq(design, "v190", input$wealth)
      design <- svy_filter_eq(design, "v013", input$age_group)   # 5-year age group
      design <- svy_filter_eq(design, "v024", input$region)      # county
      design
    })

    # Region was a dead control: the dropdown was hard-coded to the eight old
    # provinces while v024 holds the 47 counties, and filtered_ir() never even
    # read it. Populate it from the data so the labels always match.
    observeEvent(ir_design(), once = TRUE, {
      counties <- sort(unique(as.character(ir_design()$variables$v024)))
      updateSelectInput(session, "region",
                        label   = "County",
                        choices = c("All", stats::setNames(counties, fmt_label(counties))),
                        selected = "All")
    })

    # ---- KPI Cards ----------------------------------------------------------
    # NOTE: v201 is "total children ever born", so this is the mean CEB of women
    # 15-49, NOT the total fertility rate. TFR needs age-specific fertility rates
    # from the birth history; it is not a mean of a single column. Labelled for
    # what it actually measures.
    output$kpi_tfr <- renderUI({
      req(filtered_ir())
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      val <- filtered_ir() |>
        summarise(ceb = survey_mean(as.numeric(v201), na.rm = TRUE)) |>
        pull(ceb)
      stat_box(round(val, 1), "Children Ever Born", "mean, women 15-49")
    })

    # Report definition: CPR is among CURRENTLY MARRIED women 15-49 (63% any
    # method, 57% modern in Table 8). Over all women it is ~46% — a valid figure
    # but not the one in the report, which is what this card used to show.
    output$kpi_cpr <- renderUI({
      req(filtered_ir())
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      val <- indicator_cpr(filtered_ir(), married = TRUE)
      stat_box(fmt_pct(val), "Contraceptive Prevalence",
               "any method, married women 15-49", color = "green")
    })

    # Report definition (Table 11): among women with a live birth or stillbirth
    # in the 2 years before the survey, with "don't know" kept in the denominator
    # as not-4+. Dropping don't-know instead gives 67.3% against a published 66%.
    output$kpi_anc <- renderUI({
      req(filtered_ir())
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      val <- indicator_anc4(filtered_ir())
      validate(need(!is.na(val), "No births in the last 2 years for this selection."))
      stat_box(fmt_pct(val), "ANC 4+ Visits", "birth in last 2 years")
    })

    # Under-5 mortality by the DHS direct (synthetic cohort) method on the birth
    # history — see dhs_child_mortality() in R/indicators.R. A raw proportion of
    # births that died is biased downward by right-censoring; this is not that.
    output$kpi_u5mr <- renderUI({
      req(filtered_br())
      validate(need(svy_has_rows(filtered_br()),
                    "No births match this filter combination."))
      m <- dhs_child_mortality(filtered_br()$variables)
      validate(need(!is.na(m$under5), "Too few births to estimate mortality."))
      stat_box(paste0(round(m$under5, 1), "/1,000"), "Under-5 Mortality",
               if (age_only_filter()) "per 1,000 births; ignores age filter"
               else "deaths before age 5, per 1,000",
               color = "red")
    })

    # Per-person coverage from the household-member (PR) recode: the report gives
    # 26.0% of females and 26.5% of males (Table 3). It is NOT in the women's file
    # — IR v481 is labelled "NA - Covered by health insurance" and is 100%
    # missing, which is why this card used to read 0%.
    output$kpi_insurance <- renderUI({
      req(filtered_pr())
      validate(need(svy_has_rows(filtered_pr()),
                    "No household members match this filter combination."))
      val <- indicator_insurance(filtered_pr())
      validate(need(!is.na(val), "Insurance module not available for this selection."))
      stat_box(fmt_pct(val), "Health Insurance",
               if (age_only_filter()) "of people; ignores age filter"
               else "of household population", color = "red")
    })

    # Height-for-age below -2 SD among children under 5 (Table 14: 17.6%).
    output$kpi_stunting <- renderUI({
      req(filtered_kr())
      validate(need(svy_has_rows(filtered_kr()),
                    "No children match this filter combination."))
      val <- indicator_nutrition(filtered_kr(), "stunted")
      validate(need(!is.na(val), "No measured children for this selection."))
      stat_box(fmt_pct(val), "Stunting (<5 yrs)",
               if (age_only_filter()) "HAZ < -2SD; ignores age filter"
               else "height-for-age < -2 SD", color = "red")
    })

    # ---- CPR by Wealth Quintile ---------------------------------------------
    output$plot_cpr_wealth <- renderPlotly({
      req(filtered_ir())
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      # Title says "Modern", so measure modern methods. (The old c(1,2,3) test
      # compared a labelled factor to numeric codes and returned all zeroes.)
      data <- filtered_ir() |>
        group_by(v190) |>
        mutate(.fp = as.numeric(v313 == "modern method")) |>
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
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      data <- ir_design() |>
        filter(!is.na(m14_1)) |>
        mutate(
          # as.character() first — as.numeric() on this factor returns level indices
          .anc4     = as.numeric(suppressWarnings(as.numeric(as.character(m14_1))) >= 4),
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
      # Guard before summarising: srvyr fails with "subscript out of bounds" on an
      # empty design rather than returning zero rows.
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      
      # Calculate indicator based on dropdown selection
      stats <- filtered_ir() |>
        mutate(
          # Force types for specific indicators
          anc_visits = suppressWarnings(as.numeric(as.character(m14_1))),
          # Some hw70 levels are non-numeric DHS flags, so coercion warns; that
          # is expected and the flagged values become NA, which is what we want.
          hw70_num   = suppressWarnings(as.numeric(as.character(hw70_1))),
          # m3a = "Assistance: doctor", m3b = "Assistance: nurse/midwife/clinical
          # officer" — both plain yes/no flags. The old code ran grepl() for
          # profession names against a factor whose only levels are yes/no, so
          # skilled birth attendance always evaluated to 0.
          sba_flag   = as.numeric(as.character(m3a_1) == "yes" |
                                  as.character(m3b_1) == "yes"),
          b5_char    = tolower(as.character(b5_01)),
          v008_num   = as.numeric(as.character(v008)),
          b3_01_num  = as.numeric(as.character(b3_01))
        ) |>
        # The indicator choice is a single value, not a per-row condition, so it
        # selects ONE expression rather than branching row by row. case_when()
        # with a length-1 LHS against length-n RHS is deprecated in dplyr 1.2 and
        # is flagged there as a source of "subtle silent bugs".
        mutate(
          .ind = switch(input$map_indicator,
            "cpr"      = as.numeric(v313 != "no method"),
            "anc4"     = as.numeric(anc_visits >= 4),
            "sba"      = sba_flag,
            "stunting" = ifelse(!is.na(hw70_num) & hw70_num < 9000,
                                as.numeric(hw70_num < -200), NA_real_),
            # Under-5 mortality, simplified for the county map: among births in
            # the 60 months before the interview, did the child die?
            "u5mr"     = ifelse((v008_num - b3_01_num) < 60,
                                as.numeric(b5_char %in% c("0", "no", "died")),
                                NA_real_),
            NA_real_
          )
        ) |>
        group_by(v024) |> # v024 = County in KDHS 2022
        summarise(val = survey_mean(.ind, na.rm = TRUE) *
                        if (input$map_indicator == "u5mr") 1000 else 100) |>
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
        # CartoDB.Positron now requires an API key and renders "API KEY REQUIRED"
        # watermarks over the basemap. OpenStreetMap needs no key.
        addTiles(attribution = "&copy; OpenStreetMap contributors") |>
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
      pivot_longer(-year, names_to = "indicator", values_to = "pct") |>
      # The legend and tooltips showed the raw keys (cpr, anc4, sba, insurance)
      mutate(indicator = recode(indicator,
        cpr       = "Contraceptive prevalence",
        anc4      = "ANC 4+ visits",
        sba       = "Skilled birth attendance",
        insurance = "Health insurance"
      ))

    output$plot_trends <- renderPlotly({
      kdhs_trend(trend_data, x = "year", y = "pct", group = "indicator",
                 title = "Progress on Key Indicators: 2003–2022",
                 grouplab = "Indicator")
    })
  })
}
