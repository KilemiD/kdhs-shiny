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
                 column(6, wellPanel(h5("Ever Tested for HIV: Women vs Men"),
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
    
    # Comprehensive HIV knowledge is NOT computable from KDHS 2022: its components
    # v774b / mv774b are labelled "NA - HIV transmitted during delivery" in the DHS
    # dictionary and are 100% missing, so the derived hiv_know_comp column is empty
    # too. The old version compared v774b to the numeric 1 (it is a factor) and
    # hard-coded men at 42.1 as a placeholder.
    #
    # v781 / mv781 "Ever been tested for HIV" IS collected for both sexes, so this
    # panel now reports that — a real, directly comparable indicator. Retitled to
    # match. If you want comprehensive knowledge back, it needs a different source.
    output$hiv_knowledge_gender <- renderPlotly({
      req(ir_design(), mr_design())
      women <- ir_design() |>
        filter(!is.na(hiv_tested)) |>
        summarise(pct = survey_mean(as.numeric(hiv_tested), na.rm = TRUE) * 100) |>
        mutate(gender = "Women")

      men <- mr_design() |>
        filter(!is.na(hiv_tested)) |>
        summarise(pct = survey_mean(as.numeric(hiv_tested), na.rm = TRUE) * 100) |>
        mutate(gender = "Men")

      data <- bind_rows(women, men) |>
        mutate(gender = factor(gender, levels = c("Women", "Men")))

      p <- ggplot(data, aes(x = gender, y = pct, fill = gender, text = paste0(gender, " — ", gender, "<br>", round(pct, 1)))) +
        geom_col(width = 0.5, show.legend = FALSE) +
        scale_fill_manual(values = c(Women = KDHS_COLORS$female,
                                     Men   = KDHS_COLORS$male)) +
        scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
        geom_text(aes(label = fmt_pct(pct)), vjust = -0.5, fontface = "bold") +
        labs(x = "", y = "% ever tested for HIV") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })
    
    # ---- HIV testing by education ------------------------------------------
    output$hiv_knowledge_edu <- renderPlotly({
      req(ir_design())
      data <- ir_design() |>
        filter(!is.na(hiv_tested)) |>
        group_by(v106) |>
        summarise(pct = survey_mean(hiv_tested, na.rm = TRUE, vartype = "ci") * 100) |>
        mutate(education = fmt_label(v106))
      validate(need(nrow(data) > 0, "No HIV testing data available."))

      kdhs_bar(data, x = "education", y = "pct",
               title = "Ever Tested for HIV by Education",
               subtitle = "Women 15-49, weighted")
    })

    # ---- Ever tested, by county --------------------------------------------
    output$hiv_tested_region <- renderPlotly({
      req(ir_design())
      data <- ir_design() |>
        filter(!is.na(hiv_tested)) |>
        group_by(v024) |>
        summarise(pct = survey_mean(hiv_tested, na.rm = TRUE, vartype = NULL) * 100) |>
        mutate(county = fmt_label(v024)) |>
        filter(!is.na(pct)) |>
        arrange(desc(pct)) |>
        slice_head(n = 20)
      validate(need(nrow(data) > 0, "No HIV testing data available."))

      data <- order_levels(data, "county", "pct")


      p <- ggplot(data, aes(x = county, y = pct, text = paste0(county, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.78) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of women ever tested for HIV",
             title = "20 highest-coverage counties") +
        theme_kdhs() +
        theme(axis.text.y = element_text(size = 8))
      ggplotly(p, tooltip = "text")
    })

    # ---- Tested in the last 12 months --------------------------------------
    # v783 "Tested for HIV in last 12 months" (women), mv783 for men.
    output$hiv_tested_recent <- renderPlotly({
      req(ir_design(), mr_design())
      side <- function(design, var, label) {
        if (!var %in% names(design$variables)) return(NULL)
        design |>
          filter(!is.na(.data[[var]])) |>
          mutate(.x = as.numeric(as.character(.data[[var]]) == "yes")) |>
          summarise(pct = survey_mean(.x, na.rm = TRUE, vartype = "ci") * 100) |>
          mutate(group = label)
      }
      data <- bind_rows(side(ir_design(), "v783", "Women"),
                        side(mr_design(), "mv783", "Men"))
      validate(need(nrow(data) > 0,
                    "Recent-testing variable (v783 / mv783) not available."))
      data <- mutate(data, group = factor(group, levels = c("Women", "Men")))

      p <- ggplot(data, aes(x = group, y = pct, fill = group, text = paste0(group, " — ", group, "<br>", round(pct, 1)))) +
        geom_col(width = 0.5, show.legend = FALSE) +
        geom_errorbar(aes(ymin = pct_low, ymax = pct_upp), width = 0.12) +
        geom_text(aes(label = fmt_pct(pct)), vjust = -1.1, fontface = "bold", size = 3.4) +
        scale_fill_manual(values = c(Women = KDHS_COLORS$female,
                                     Men   = KDHS_COLORS$male)) +
        scale_y_continuous(labels = label_percent(scale = 1),
                           expand = expansion(mult = c(0, 0.25))) +
        labs(x = "", y = "% tested in the last 12 months") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
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

    # ---- PrEP knowledge and attitude ---------------------------------------
    # v859 / mv859 "Knowledge and attitude to PrEP to prevent getting HIV".
    # This is a multi-category attitude item, not a yes/no, so the panel shows
    # the full distribution for women and men rather than a single percentage.
    output$prep_awareness <- renderPlotly({
      req(ir_design(), mr_design())
      side <- function(design, var, label) {
        if (!var %in% names(design$variables)) return(NULL)
        design |>
          filter(!is.na(.data[[var]])) |>
          group_by(.resp = .data[[var]]) |>
          summarise(pct = survey_prop(na.rm = TRUE, vartype = NULL) * 100,
                    .groups = "drop") |>
          mutate(group = label, response = fmt_label(.resp))
      }
      data <- bind_rows(side(ir_design(), "v859", "Women"),
                        side(mr_design(), "mv859", "Men")) |>
        filter(!is.na(pct))
      validate(need(nrow(data) > 0,
                    "PrEP item (v859 / mv859) not available in this extract."))
      data <- mutate(data, group = factor(group, levels = c("Women", "Men")))

      data <- order_levels(data, "response", "pct")


      p <- ggplot(data, aes(x = response, y = pct, fill = group, text = paste0(response, " — ", group, "<br>", round(pct, 1)))) +
        geom_col(position = "dodge", width = 0.75) +
        coord_flip() +
        scale_fill_manual(values = c(Women = KDHS_COLORS$female,
                                     Men   = KDHS_COLORS$male)) +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of respondents", fill = "",
             title = "Knowledge and attitude to PrEP") +
        theme_kdhs() +
        theme(axis.text.y = element_text(size = 8))
      ggplotly(p, tooltip = "text")
    })
  })
}
