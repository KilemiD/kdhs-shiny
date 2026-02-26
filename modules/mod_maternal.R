# =============================================================================
# modules/mod_maternal.R
# Dashboard 2: Maternal & Child Health
# Datasets: IR (mothers), KR (children), BR (births)
# =============================================================================

mod_maternal_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(

      tabPanel("Antenatal & Delivery",
        br(),
        wellPanel(
          fluidRow(
            column(3, selectInput(ns("anc_region"),    "Region",    choices = c("All","Nairobi","Central","Coast","Eastern","Nyanza","Rift Valley","Western","North Eastern"))),
            column(3, selectInput(ns("anc_wealth"),    "Wealth",    choices = c("All","Poorest","Poorer","Middle","Richer","Richest"))),
            column(3, selectInput(ns("anc_education"), "Education", choices = c("All","No education","Primary","Secondary","Higher"))),
            column(3, selectInput(ns("anc_residence"), "Residence", choices = c("All","Urban","Rural")))
          )
        ),
        fluidRow(
          column(6, wellPanel(h5("ANC Visits Distribution"),          plotlyOutput(ns("plot_anc_dist"),      height="300px") |> withSpinner())),
          column(6, wellPanel(h5("Skilled Birth Attendance by Education"), plotlyOutput(ns("plot_sba_edu"), height="300px") |> withSpinner()))
        ),
        fluidRow(
          column(4, wellPanel(h5("ANC Provider Type"),   plotlyOutput(ns("plot_anc_provider"),    height="260px") |> withSpinner())),
          column(4, wellPanel(h5("Place of Delivery"),   plotlyOutput(ns("plot_delivery_place"),  height="260px") |> withSpinner())),
          column(4, wellPanel(h5("C-Section Rate"),      plotlyOutput(ns("plot_csection"),        height="260px") |> withSpinner()))
        )
      ),

      tabPanel("Child Vaccination",
        br(),
        fluidRow(
          column(6, wellPanel(h5("Vaccination Coverage by Vaccine"),   plotlyOutput(ns("plot_vaccines"),  height="360px") |> withSpinner())),
          column(6, wellPanel(h5("Full Immunization by Region"),        plotlyOutput(ns("plot_full_vax"), height="360px") |> withSpinner()))
        )
      ),

      tabPanel("Child Nutrition",
        br(),
        fluidRow(
          column(4, wellPanel(h5("Stunting by Wealth"),  plotlyOutput(ns("plot_stunting"),    height="280px") |> withSpinner())),
          column(4, wellPanel(h5("Wasting by Region"),   plotlyOutput(ns("plot_wasting"),     height="280px") |> withSpinner())),
          column(4, wellPanel(h5("Underweight by Age"),  plotlyOutput(ns("plot_underweight"), height="280px") |> withSpinner()))
        ),
        fluidRow(
          column(12, wellPanel(h5("Breastfeeding Indicators"), plotlyOutput(ns("plot_breastfeeding"), height="280px") |> withSpinner()))
        )
      ),

      tabPanel("Child Mortality",
        br(),
        fluidRow(
          column(12, wellPanel(h5("Under-5 Mortality Rate by Region (per 1,000 live births)"), plotlyOutput(ns("plot_u5mr"), height="340px") |> withSpinner()))
        ),
        fluidRow(
          column(6, wellPanel(h5("Neonatal Mortality Rate"),            plotlyOutput(ns("plot_nmr"),       height="280px") |> withSpinner())),
          column(6, wellPanel(h5("Trend: Infant Mortality 2003–2022"), plotlyOutput(ns("plot_imr_trend"), height="280px") |> withSpinner()))
        )
      )
    )
  )
}

mod_maternal_server <- function(id, ir_design, kr_design, raw_data) {
  moduleServer(id, function(input, output, session) {

    # ---- Filter IR by inputs ------------------------------------------------
    filtered_ir <- reactive({
      design <- ir_design()
      req(design)
      if (input$anc_residence != "All")
        design <- design |> filter(as.character(v025) == tolower(input$anc_residence))
      if (input$anc_wealth != "All")
        design <- design |> filter(as.character(v190) == input$anc_wealth)
      if (input$anc_education != "All")
        design <- design |> filter(as.character(v106) == input$anc_education)
      design
    })

    # ---- ANC Distribution ---------------------------------------------------
    output$plot_anc_dist <- renderPlotly({
      req(filtered_ir())
      data <- filtered_ir() |>
        filter(!is.na(m14)) |>
        mutate(anc_cat = case_when(
          as.numeric(m14) == 0 ~ "No visits",
          as.numeric(m14) %in% 1:3 ~ "1–3 visits",
          as.numeric(m14) >= 4 ~ "4+ visits (recommended)",
          TRUE ~ NA_character_
        )) |>
        group_by(anc_cat) |>
        summarise(pct = survey_prop(na.rm = TRUE, vartype = NULL) * 100)

      p <- ggplot(data, aes(x = anc_cat, y = pct, fill = anc_cat)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        scale_fill_manual(values = c("#fee2e2", "#fca5a5", "#1a6b4a")) +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of births (last 2 years)") +
        theme_kdhs()
      ggplotly(p)
    })

    # ---- SBA by Education ---------------------------------------------------
    output$plot_sba_edu <- renderPlotly({
      req(filtered_ir())
      data <- filtered_ir() |>
        filter(!is.na(m15)) |>
        mutate(.sba = as.numeric(m3a == 1 | m3b == 1 | m3c == 1)) |>
        group_by(v106) |>
        summarise(pct = survey_mean(.sba, na.rm = TRUE, vartype = "ci") * 100) |>
        mutate(v106 = fmt_label(v106))

      kdhs_bar(data, x = "v106", y = "pct",
               title = "Skilled Birth Attendance by Education Level",
               subtitle = "% of births attended by skilled provider")
    })

    # ---- Vaccination Coverage -----------------------------------------------
    output$plot_vaccines <- renderPlotly({
      req(kr_design())

      # DHS vaccine variables: h1=BCG, h2-h4=DPT/Penta, h5=Polio, h7-h9=Measles, etc.
      vax_vars <- c(BCG = "h1", DPT1 = "h3", DPT2 = "h5", DPT3 = "h7",
                    Polio1 = "h0", Measles = "h9")

      vax_data <- map_dfr(names(vax_vars), function(vax_name) {
        var <- vax_vars[[vax_name]]
        kr_design() |>
          filter(b5 == 1, b8 < 24) |>  # Alive children 12-23 months (adjust b8 as needed)
          mutate(.got = as.numeric(!!sym(var) %in% c(1, 2, 3))) |>
          summarise(pct = survey_mean(.got, na.rm = TRUE) * 100) |>
          mutate(vaccine = vax_name)
      })

      p <- ggplot(vax_data, aes(x = reorder(vaccine, pct), y = pct)) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.7) +
        geom_text(aes(label = fmt_pct(pct)), hjust = -0.2, size = 3.5) +
        coord_flip() +
        scale_y_continuous(limits = c(0, 110), labels = label_percent(scale = 1)) +
        labs(x = "", y = "Coverage (%)") +
        theme_kdhs()
      ggplotly(p)
    })

    # ---- Child Mortality Trend (hardcoded from DHS reports) -----------------
    output$plot_imr_trend <- renderPlotly({
      trend <- tibble(
        year = c(2003, 2008, 2014, 2022),
        imr  = c(77, 52, 39, 30),
        u5mr = c(115, 74, 52, 41)
      ) |>
        pivot_longer(-year, names_to = "indicator", values_to = "rate")

      kdhs_trend(trend, x = "year", y = "rate", group = "indicator",
                 title = "Child Mortality Trends",
                 ylab  = "Deaths per 1,000 live births")
    })

    # ---- Breastfeeding Indicators -------------------------------------------
    output$plot_breastfeeding <- renderPlotly({
      req(filtered_ir())
      # v404 = currently breastfeeding, m4 = duration of breastfeeding
      # Add relevant breastfeeding indicators from KR or IR as needed
      data <- tibble(
        indicator = c(
          "Early initiation (<1 hr)",
          "Exclusive BF (0–5 months)",
          "Continued BF at 1 year",
          "Continued BF at 2 years",
          "Introduction solid foods (6–8 mo)"
        ),
        pct = c(60.3, 61.4, 83.7, 47.2, 72.1)  # Replace with computed values
      )

      p <- ggplot(data, aes(x = reorder(indicator, pct), y = pct)) +
        geom_col(fill = KDHS_COLORS$accent2, width = 0.6) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
        labs(x = "", y = "Percentage (%)") +
        theme_kdhs()
      ggplotly(p)
    })
  })
}
