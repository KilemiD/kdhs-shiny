# =============================================================================
# modules/mod_hiv.R
# Dashboard 4: HIV & Sexual Health
# =============================================================================

mod_hiv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("HIV Knowledge", br(),
               fluidRow(
                 column(6, wellPanel(h5("Comprehensive HIV Knowledge: Women vs Men"),
                                     plotlyOutput(ns("hiv_knowledge_gender"), height="320px") |> withSpinner())),
                 column(6, wellPanel(h5("Knowledge by Education Level"),
                                     plotlyOutput(ns("hiv_knowledge_edu"), height="320px") |> withSpinner()))
               )
      ),
      tabPanel("HIV Testing", br(),
               fluidRow(
                 column(6, wellPanel(h5("Ever Tested for HIV by Region"),
                                     plotlyOutput(ns("hiv_tested_region"), height="320px") |> withSpinner())),
                 column(6, wellPanel(h5("Tested in Last 12 Months"),
                                     plotlyOutput(ns("hiv_tested_recent"), height="320px") |> withSpinner()))
               )
      ),
      tabPanel("MTCT & PrEP", br(),
               fluidRow(
                 column(6, wellPanel(h5("PMTCT Knowledge Trend 2003-2022"),
                                     plotlyOutput(ns("mtct_trend"), height="320px") |> withSpinner())),
                 column(6, wellPanel(h5("PrEP Awareness by Region"),
                                     plotlyOutput(ns("prep_awareness"), height="320px") |> withSpinner()))
               )
      )
    )
  )
}

mod_hiv_server <- function(id, ir_design, mr_design, raw_data) {
  moduleServer(id, function(input, output, session) {
    
    output$hiv_knowledge_gender <- renderPlotly({
      req(ir_design())
      women <- ir_design() |>
        mutate(.know = as.numeric(v774b == 1)) |>
        summarise(pct = survey_mean(.know, na.rm = TRUE) * 100) |>
        mutate(gender = "Women")
      
      men <- tibble(pct = 42.1, gender = "Men")  # placeholder
      data <- bind_rows(women, men)
      
      p <- ggplot(data, aes(x = gender, y = pct, fill = gender)) +
        geom_col(width = 0.5, show.legend = FALSE) +
        scale_fill_manual(values = c(KDHS_COLORS$female, KDHS_COLORS$male)) +
        scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
        geom_text(aes(label = fmt_pct(pct)), vjust = -0.5, fontface = "bold") +
        labs(x = "", y = "% with comprehensive HIV knowledge") +
        theme_kdhs()
      ggplotly(p)
    })
    
    output$mtct_trend <- renderPlotly({
      trend <- tibble(
        year      = c(2003, 2008, 2014, 2022),
        women_pct = c(43.4, 59.0, 72.3, 84.1),
        men_pct   = c(35.2, 51.0, 66.7, 79.5)
      ) |> pivot_longer(-year, names_to = "group", values_to = "pct")
      
      kdhs_trend(trend, x = "year", y = "pct", group = "group",
                 title  = "PMTCT Knowledge: Women aware that risk can be reduced",
                 ylab   = "Percentage (%)")
    })
  })
}
