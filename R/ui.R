# =============================================================================
# R/ui.R
# Main UI layout using bslib (Bootstrap 5).
# Each tab delegates to a module UI function.
# =============================================================================
library(shiny)
library(bslib)
library(htmltools) # Sometimes needed for layout utilities
ui <- navbarPage(
  title       = "KDHS 2022 Dashboard",
  id          = "main_nav",
  collapsible = TRUE,

  # bs_theme() works on bslib 0.3+; remove this line if you get a theme error
  theme = bslib::bs_theme(
    version    = 4,
    bootswatch = "flatly",
    primary    = "#1a6b4a",
    secondary  = "#b5121b"
  ),

  # ---- TABS -----------------------------------------------------------------
  tabPanel("Overview",             icon = icon("chart-pie"),   mod_overview_ui("overview")),
  tabPanel("Maternal & Child",     icon = icon("baby"),        mod_maternal_ui("maternal")),
  tabPanel("Gender & GBV",         icon = icon("venus-mars"),  mod_gender_ui("gender")),
  tabPanel("HIV & Sexual Health",  icon = icon("ribbon"),      mod_hiv_ui("hiv")),
  tabPanel("New 2022 Modules",     icon = icon("star"),        mod_new_ui("new_modules")),
  tabPanel("Research Explorer",    icon = icon("flask"),       mod_explorer_ui("explorer")),

  tabPanel(
    "About", icon = icon("info-circle"),
    fluidRow(
      column(8,
        wellPanel(
          h4("About this Dashboard"),
          p("Interactive, weighted analysis of the ",
            strong("2022 Kenya Demographic and Health Survey (KDHS)"),
            " — the 7th in a series conducted since 1989."),
          p("All estimates use the DHS complex survey design:
             strata (v023), PSU (v021), weight (v005 / 1,000,000)."),
          p("Data: Kenya National Bureau of Statistics (KNBS), 2023. ",
            a("dhsprogram.com", href = "https://dhsprogram.com", target = "_blank")),
          hr(),
          h6("Built with"),
          tags$ul(
            tags$li("R + Shiny (survey, srvyr)"),
            tags$li("ggplot2 + plotly"),
            tags$li("leaflet + sf"),
            tags$li("DT for tables")
          )
        )
      ),
      column(4,
        wellPanel(
          h4("Key Sample Sizes"),
          uiOutput("global_kpis")
        )
      )
    )
  ),

  footer = div(
    style = paste(
      "text-align:center; padding:6px 0; font-size:0.75rem;",
      "color:#718096; border-top:1px solid #dee2e6; margin-top:1rem;"
    ),
    "KDHS 2022 Dashboard | Data: KNBS / DHS Program | All estimates survey-weighted"
  )
)
