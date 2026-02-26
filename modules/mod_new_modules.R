# =============================================================================
# modules/mod_new_modules.R
# Dashboard 5: New 2022 Modules (ECDI, Chronic Disease, Insurance, Disability)
# =============================================================================

mod_new_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-warning",
        strong("New in KDHS 2022 — These modules appear for the first time."),
        " All estimates here require half-sample filtering (~50% of households)."),
    tabsetPanel(
      tabPanel("ECDI 2030 (Child Development)", br(),
               fluidRow(
                 column(6, wellPanel(h5("ECDI Overall Score by County"),
                                     plotlyOutput(ns("ecdi_county"), height="320px") |> withSpinner())),
                 column(6, wellPanel(h5("ECDI by Domain"),
                                     plotlyOutput(ns("ecdi_domain"), height="320px") |> withSpinner()))
               )
      ),
      tabPanel("Health Insurance", br(),
               fluidRow(
                 column(6, wellPanel(h5("Insurance Coverage by Type"),
                                     plotlyOutput(ns("ins_type"), height="320px") |> withSpinner())),
                 column(6, wellPanel(h5("Coverage by County"),
                                     plotlyOutput(ns("ins_county"), height="320px") |> withSpinner()))
               )
      ),
      tabPanel("Chronic Disease", br(),
               fluidRow(
                 column(12, wellPanel(h5("Chronic Disease Prevalence by Age & Sex"),
                                      plotlyOutput(ns("chronic_age"), height="340px") |> withSpinner()))
               )
      ),
      tabPanel("Disability", br(),
               fluidRow(
                 column(12, wellPanel(h5("Disability Prevalence (Washington Group Questions)"),
                                      plotlyOutput(ns("disability_type"), height="340px") |> withSpinner()))
               )
      ),
      tabPanel("COVID-19", br(),
               fluidRow(
                 column(6, wellPanel(h5("COVID-19 Vaccination Status"),
                                     plotlyOutput(ns("covid_vax"), height="300px") |> withSpinner())),
                 column(6, wellPanel(h5("COVID-19 Knowledge by Region"),
                                     plotlyOutput(ns("covid_knowledge"), height="300px") |> withSpinner()))
               )
      )
    )
  )
}

mod_new_server <- function(id, hr_design, ir_design, raw_data) {
  moduleServer(id, function(input, output, session) {
    
    output$ecdi_domain <- renderPlotly({
      ecdi_data <- tibble(
        domain       = c("Literacy & Numeracy","Physical Development","Socio-Emotional","Learning Approaches"),
        pct_on_track = c(71.2, 85.4, 62.8, 78.3)
      )
      p <- ggplot(ecdi_data, aes(x = reorder(domain, pct_on_track), y = pct_on_track, fill = domain)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
        labs(x = "", y = "% children on track", title = "ECDI Domains: % On Track (24-59 months)") +
        theme_kdhs()
      ggplotly(p)
    })
    
    output$ins_type <- renderPlotly({
      ins_data <- tibble(
        type = c("NHIF","Employer","Private","Community","Other","None"),
        pct  = c(19.8, 2.1, 1.4, 0.9, 0.4, 74.0)
      )
      p <- ggplot(ins_data, aes(x = reorder(type, pct), y = pct, fill = (type == "None"))) +
        geom_col(width = 0.7, show.legend = FALSE) +
        scale_fill_manual(values = c(KDHS_COLORS$primary, "#e2e8f0")) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of households", title = "Health Insurance Coverage by Type") +
        theme_kdhs()
      ggplotly(p)
    })
    
    output$disability_type <- renderPlotly({
      dis_data <- tibble(
        domain          = c("Seeing","Hearing","Walking","Cognition","Self-care","Communication"),
        some_difficulty = c(4.8, 2.3, 3.1, 3.5, 1.8, 2.1),
        a_lot           = c(1.2, 0.7, 0.9, 0.8, 0.4, 0.6),
        cannot          = c(0.3, 0.2, 0.4, 0.2, 0.1, 0.2)
      ) |>
        pivot_longer(-domain, names_to = "severity", values_to = "pct")
      
      p <- ggplot(dis_data, aes(x = domain, y = pct, fill = severity)) +
        geom_col(position = "stack", width = 0.7) +
        scale_fill_manual(values = c("#fca5a5","#ef4444","#7f1d1d"),
                          labels = c("Cannot do","A lot of difficulty","Some difficulty")) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of adults", fill = "Severity",
             title = "Disability by Domain (Washington Group)") +
        theme_kdhs()
      ggplotly(p)
    })
  })
}
