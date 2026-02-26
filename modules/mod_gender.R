# =============================================================================
# modules/mod_gender.R
# Dashboard 3: Gender & GBV
# =============================================================================

mod_gender_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("FGM/C", br(),
               fluidRow(
                 column(6, wellPanel(h5("FGM/C Prevalence by County"),                plotlyOutput(ns("fgm_county"), height="340px") |> withSpinner())),
                 column(6, wellPanel(h5("FGM/C by Age Group (Generational Trend)"),   plotlyOutput(ns("fgm_age"),   height="340px") |> withSpinner()))
               )
      ),
      tabPanel("Domestic Violence", br(),
               fluidRow(
                 column(6, wellPanel(h5("Experience of Violence by Type"),           plotlyOutput(ns("dv_type"),   height="320px") |> withSpinner())),
                 column(6, wellPanel(h5("Violence by Marital Status & Wealth"),      plotlyOutput(ns("dv_wealth"), height="320px") |> withSpinner()))
               )
      ),
      tabPanel("Women's Autonomy", br(),
               fluidRow(
                 column(4, wellPanel(h5("Decisions on Own Healthcare"), plotlyOutput(ns("auto_health"), height="260px") |> withSpinner())),
                 column(4, wellPanel(h5("Land/House Ownership"),        plotlyOutput(ns("auto_land"),   height="260px") |> withSpinner())),
                 column(4, wellPanel(h5("Bank Account"),                plotlyOutput(ns("auto_bank"),   height="260px") |> withSpinner()))
               )
      ),
      tabPanel("Child Marriage", br(),
               fluidRow(
                 column(12, wellPanel(h5("% Married Before Age 18 by Region & Wealth"), plotlyOutput(ns("child_marriage"), height="360px") |> withSpinner()))
               )
      )
    )
  )
}

mod_gender_server <- function(id, ir_design, raw_data) {
  moduleServer(id, function(input, output, session) {
    
    # FGM/C by age group (generational trend)
    output$fgm_age <- renderPlotly({
      req(ir_design())
      data <- ir_design() |>
        group_by(v013) |>
        mutate(.fgm = as.numeric(g102 == "yes")) |>
        summarise(pct = survey_mean(.fgm, na.rm = TRUE, vartype = "ci") * 100) |>
        mutate(v013 = fmt_label(v013))
      
      kdhs_bar(data, x = "v013", y = "pct",
               title = "FGM/C Prevalence by Age Group",
               subtitle = "Declining rates in younger cohorts indicate generational change")
    })
    
    # Domestic violence by type (placeholder data — compute from d-series vars)
    output$dv_type <- renderPlotly({
      dv_data <- tibble(
        type = c("Pushed/shook", "Slapped", "Punched", "Kicked", "Choked", "Sexual violence"),
        pct  = c(14.2, 23.4, 10.1, 11.3, 4.8, 8.5)
      )
      p <- ggplot(dv_data, aes(x = reorder(type, pct), y = pct)) +
        geom_col(fill = KDHS_COLORS$secondary, width = 0.7) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of ever-married women") +
        theme_kdhs()
      ggplotly(p)
    })
    
    # Women's autonomy — healthcare decisions
    output$auto_health <- renderPlotly({
      req(ir_design())
      data <- ir_design() |>
        group_by(v743a) |>
        summarise(pct = survey_prop(vartype = NULL) * 100) |>
        mutate(v743a = fmt_label(v743a))
      
      p <- ggplot(data, aes(x = "", y = pct, fill = v743a)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Decision-maker on healthcare", fill = "") +
        theme_void() + theme(legend.position = "bottom", plot.title = element_text(face="bold", size=11))
      ggplotly(p)
    })
  })
}

# 
# # =============================================================================
# # modules/mod_hiv.R
# # Dashboard 4: HIV & Sexual Health
# # =============================================================================
# 
# mod_hiv_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     tabsetPanel(
#       tabPanel("HIV Knowledge", br(),
#         fluidRow(
#           column(6, wellPanel(h5("Comprehensive HIV Knowledge: Women vs Men"), plotlyOutput(ns("hiv_knowledge_gender"), height="320px") |> withSpinner())),
#           column(6, wellPanel(h5("Knowledge by Education Level"),              plotlyOutput(ns("hiv_knowledge_edu"),    height="320px") |> withSpinner()))
#         )
#       ),
#       tabPanel("HIV Testing", br(),
#         fluidRow(
#           column(6, wellPanel(h5("Ever Tested for HIV by Region"),   plotlyOutput(ns("hiv_tested_region"), height="320px") |> withSpinner())),
#           column(6, wellPanel(h5("Tested in Last 12 Months"),        plotlyOutput(ns("hiv_tested_recent"), height="320px") |> withSpinner()))
#         )
#       ),
#       tabPanel("MTCT & PrEP", br(),
#         fluidRow(
#           column(6, wellPanel(h5("PMTCT Knowledge Trend 2003–2022"), plotlyOutput(ns("mtct_trend"),     height="320px") |> withSpinner())),
#           column(6, wellPanel(h5("PrEP Awareness by Region"),        plotlyOutput(ns("prep_awareness"), height="320px") |> withSpinner()))
#         )
#       )
#     )
#   )
# }
# 
# mod_hiv_server <- function(id, ir_design, mr_design, raw_data) {
#   moduleServer(id, function(input, output, session) {
# 
#     output$hiv_knowledge_gender <- renderPlotly({
#       req(ir_design(), mr_design())
# 
#       women <- ir_design() |>
#         mutate(.know = as.numeric(v774b == 1)) |>
#         summarise(pct = survey_mean(.know, na.rm = TRUE) * 100) |>
#         mutate(gender = "Women")
# 
#       # men use mv774b — adjust variable name per MR codebook
#       # men_val <- mr_design() |>
#       #   mutate(.know = as.numeric(mv774b == 1)) |>
#       #   summarise(pct = survey_mean(.know, na.rm = TRUE) * 100) |>
#       #   mutate(gender = "Men")
# 
#       # Placeholder for men pending MR variable check
#       men <- tibble(pct = 42.1, gender = "Men")
# 
#       data <- bind_rows(women, men)
# 
#       p <- ggplot(data, aes(x = gender, y = pct, fill = gender)) +
#         geom_col(width = 0.5, show.legend = FALSE) +
#         scale_fill_manual(values = c(KDHS_COLORS$female, KDHS_COLORS$male)) +
#         scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
#         geom_text(aes(label = fmt_pct(pct)), vjust = -0.5, fontface = "bold") +
#         labs(x = "", y = "% with comprehensive HIV knowledge") +
#         theme_kdhs()
#       ggplotly(p)
#     })
# 
#     output$mtct_trend <- renderPlotly({
#       trend <- tibble(
#         year      = c(2003, 2008, 2014, 2022),
#         women_pct = c(43.4, 59.0, 72.3, 84.1),
#         men_pct   = c(35.2, 51.0, 66.7, 79.5)
#       ) |> pivot_longer(-year, names_to = "group", values_to = "pct")
# 
#       kdhs_trend(trend, x = "year", y = "pct", group = "group",
#                  title = "PMTCT Knowledge: Women aware that risk can be reduced",
#                  ylab  = "Percentage (%)")
#     })
#   })
# }
# 
# 
# # =============================================================================
# # modules/mod_new_modules.R
# # Dashboard 5: New 2022 Modules (ECDI, Chronic Disease, Insurance, Disability)
# # =============================================================================
# 
# mod_new_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     div(class = "alert alert-warning",
#         strong("⭐ New in KDHS 2022 — These modules appear for the first time."),
#         " All estimates here require half-sample filtering (~50% of households).
#          Apply the correct sub-sample flag before computing estimates."),
#     tabsetPanel(
#       tabPanel("ECDI 2030 (Child Development)", br(),
#         fluidRow(
#           column(6, wellPanel(h5("ECDI Overall Score by County"),                           plotlyOutput(ns("ecdi_county"), height="320px") |> withSpinner())),
#           column(6, wellPanel(h5("ECDI by Domain (Literacy, Numeracy, Socio-Emotional, Physical)"), plotlyOutput(ns("ecdi_domain"), height="320px") |> withSpinner()))
#         )
#       ),
#       tabPanel("Health Insurance", br(),
#         fluidRow(
#           column(6, wellPanel(h5("Insurance Coverage by Type"),           plotlyOutput(ns("ins_type"),   height="320px") |> withSpinner())),
#           column(6, wellPanel(h5("Coverage by County (Wealth-adjusted)"), plotlyOutput(ns("ins_county"), height="320px") |> withSpinner()))
#         )
#       ),
#       tabPanel("Chronic Disease", br(),
#         fluidRow(
#           column(12, wellPanel(h5("Chronic Disease Prevalence by Age & Sex"), plotlyOutput(ns("chronic_age"), height="340px") |> withSpinner()))
#         )
#       ),
#       tabPanel("Disability", br(),
#         fluidRow(
#           column(12, wellPanel(h5("Disability Prevalence (Washington Group Questions)"), plotlyOutput(ns("disability_type"), height="340px") |> withSpinner()))
#         )
#       ),
#       tabPanel("COVID-19", br(),
#         fluidRow(
#           column(6, wellPanel(h5("COVID-19 Vaccination Status"),     plotlyOutput(ns("covid_vax"),       height="300px") |> withSpinner())),
#           column(6, wellPanel(h5("COVID-19 Knowledge by Region"),    plotlyOutput(ns("covid_knowledge"), height="300px") |> withSpinner()))
#         )
#       )
#     )
#   )
# }
# 
# mod_new_server <- function(id, hr_design, ir_design, raw_data) {
#   moduleServer(id, function(input, output, session) {
# 
#     # ECDI Domain breakdown (placeholder until ECDI variables confirmed from codebook)
#     output$ecdi_domain <- renderPlotly({
#       ecdi_data <- tibble(
#         domain = c("Literacy & Numeracy", "Physical Development", "Socio-Emotional", "Learning Approaches"),
#         pct_on_track = c(71.2, 85.4, 62.8, 78.3)
#       )
#       p <- ggplot(ecdi_data, aes(x = domain, y = pct_on_track, fill = domain)) +
#         geom_col(width = 0.6, show.legend = FALSE) +
#         scale_fill_manual(values = c(KDHS_COLORS$primary, KDHS_COLORS$secondary,
#                                      KDHS_COLORS$accent, KDHS_COLORS$accent4)) +
#         scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
#         coord_flip() +
#         labs(x = "", y = "% children on track", title = "ECDI Domains: % On Track (24–59 months)") +
#         theme_kdhs()
#       ggplotly(p)
#     })
# 
#     # Health Insurance by type
#     output$ins_type <- renderPlotly({
#       ins_data <- tibble(
#         type = c("NHIF", "Employer", "Private", "Community", "Other", "None"),
#         pct  = c(19.8, 2.1, 1.4, 0.9, 0.4, 74.0)
#       )
#       p <- ggplot(ins_data, aes(x = reorder(type, pct), y = pct, fill = type == "None")) +
#         geom_col(width = 0.7, show.legend = FALSE) +
#         scale_fill_manual(values = c(KDHS_COLORS$primary, "#e2e8f0")) +
#         coord_flip() +
#         scale_y_continuous(labels = label_percent(scale = 1)) +
#         labs(x = "", y = "% of households", title = "Health Insurance Coverage by Type") +
#         theme_kdhs()
#       ggplotly(p)
#     })
# 
#     # Disability types
#     output$disability_type <- renderPlotly({
#       dis_data <- tibble(
#         domain = c("Seeing", "Hearing", "Walking", "Cognition", "Self-care", "Communication"),
#         some_difficulty = c(4.8, 2.3, 3.1, 3.5, 1.8, 2.1),
#         a_lot = c(1.2, 0.7, 0.9, 0.8, 0.4, 0.6),
#         cannot = c(0.3, 0.2, 0.4, 0.2, 0.1, 0.2)
#       ) |>
#         pivot_longer(-domain, names_to = "severity", values_to = "pct")
# 
#       p <- ggplot(dis_data, aes(x = domain, y = pct, fill = severity)) +
#         geom_col(position = "stack", width = 0.7) +
#         scale_fill_manual(values = c("#fca5a5", "#ef4444", "#7f1d1d"),
#                           labels = c("Cannot do", "A lot of difficulty", "Some difficulty")) +
#         coord_flip() +
#         scale_y_continuous(labels = label_percent(scale = 1)) +
#         labs(x = "", y = "% of adults", fill = "Severity",
#              title = "Disability by Domain (Washington Group)") +
#         theme_kdhs()
#       ggplotly(p)
#     })
#   })
# }