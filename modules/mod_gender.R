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
    
    # ---- Domestic violence by type -----------------------------------------
    # Computed from the DHS domestic-violence module (d105* / d106-d108) instead
    # of the placeholder numbers that were here before. v044 flags the woman
    # selected for the module; the module goes to one eligible woman per
    # household, so the denominator must be restricted to those selected.
    output$dv_type <- renderPlotly({
      req(ir_design())
      d <- ir_design()

      one <- function(var, label) {
        if (!var %in% names(d$variables)) return(NULL)
        d |>
          filter(!is.na(.data[[var]])) |>
          mutate(.x = as.numeric(as.character(.data[[var]]) == "yes")) |>
          summarise(pct = survey_mean(.x, na.rm = TRUE) * 100) |>
          mutate(type = label)
      }
      dv_data <- bind_rows(
        one("d105a", "Pushed / shook"),
        one("d105b", "Slapped"),
        one("d105c", "Punched"),
        one("d105d", "Kicked / dragged"),
        one("d104",  "Emotional violence"),
        one("d106",  "Any less severe physical"),
        one("d107",  "Any severe physical"),
        one("d108",  "Any sexual violence")
      )
      validate(need(nrow(dv_data) > 0, "Domestic violence module not available."))

      dv_data <- order_levels(dv_data, "type", "pct")


      p <- ggplot(dv_data, aes(x = type, y = pct, text = paste0(type, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$secondary, width = 0.7) +
        geom_text(aes(label = fmt_pct(pct)), hjust = -0.15, size = 3) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1),
                           expand = expansion(mult = c(0, 0.18))) +
        labs(x = "", y = "% of women asked the DV module") +
        theme_kdhs()
      ggplotly(p)
    })

    # ---- Violence by wealth -------------------------------------------------
    output$dv_wealth <- renderPlotly({
      req(ir_design())
      validate(need("d106" %in% names(ir_design()$variables),
                    "Domestic violence module not available."))
      data <- ir_design() |>
        filter(!is.na(d106) | !is.na(d107)) |>
        mutate(.any = as.numeric(as.character(d106) == "yes" |
                                 as.character(d107) == "yes")) |>
        group_by(v190) |>
        summarise(pct = survey_mean(.any, na.rm = TRUE, vartype = "ci") * 100) |>
        mutate(wealth = fmt_label(v190))
      validate(need(nrow(data) > 0, "No data for this selection."))

      kdhs_bar(data, x = "wealth", y = "pct",
               title = "Any Physical Violence by Wealth Quintile",
               subtitle = "Less severe or severe, by husband/partner")
    })

    # ---- FGM/C by county ----------------------------------------------------
    output$fgm_county <- renderPlotly({
      req(ir_design())
      data <- ir_design() |>
        filter(!is.na(g102)) |>
        mutate(.fgm = as.numeric(as.character(g102) == "yes")) |>
        group_by(v024) |>
        summarise(pct = survey_mean(.fgm, na.rm = TRUE, vartype = NULL) * 100) |>
        mutate(county = fmt_label(v024)) |>
        filter(!is.na(pct)) |>
        arrange(desc(pct)) |>
        slice_head(n = 20)
      validate(need(nrow(data) > 0, "No FGM/C data available."))

      data <- order_levels(data, "county", "pct")


      p <- ggplot(data, aes(x = county, y = pct, text = paste0(county, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$secondary, width = 0.78) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of women 15-49 circumcised",
             title = "20 highest-prevalence counties") +
        theme_kdhs() +
        theme(axis.text.y = element_text(size = 8))
      ggplotly(p)
    })
    
    # ---- Women's autonomy: healthcare decisions -----------------------------
    # Built as a native plotly pie rather than ggplot + coord_polar. ggplotly()
    # cannot convert a polar/theme_void plot: it fails with
    #   "Error in rng[[xy]]$get_labels: attempt to apply non-function"
    # after warning "no non-missing arguments to min; returning Inf", because the
    # polar coordinate system has no x/y scale for it to read labels off.
    output$auto_health <- renderPlotly({
      req(ir_design())
      data <- ir_design() |>
        filter(!is.na(v743a)) |>
        group_by(v743a) |>
        summarise(pct = survey_prop(vartype = NULL) * 100) |>
        mutate(who = fmt_label(v743a)) |>
        filter(!is.na(pct))
      validate(need(nrow(data) > 0, "No decision-making data available."))

      plot_ly(
        data,
        labels = ~who, values = ~pct, type = "pie", sort = FALSE,
        textinfo = "percent",
        hovertemplate = "%{label}<br>%{value:.1f}%<extra></extra>",
        marker = list(colors = RColorBrewer::brewer.pal(max(3, nrow(data)), "Set2"),
                      line = list(color = "white", width = 1))
      ) |>
        layout(
          title  = list(text = "Decision-maker on healthcare", font = list(size = 12)),
          legend = list(orientation = "h", y = -0.1),
          margin = list(t = 40)
        )
    })

    # ---- Land / house ownership --------------------------------------------
    # v745a "Owns a house alone or jointly", v745b "Owns land alone or jointly".
    # Both are categorical: "does not own" / "alone only" / "jointly ..." / "both".
    output$auto_land <- renderPlotly({
      req(ir_design())
      d <- ir_design()
      one <- function(var, label) {
        if (!var %in% names(d$variables)) return(NULL)
        d |>
          filter(!is.na(.data[[var]])) |>
          mutate(.own = as.numeric(as.character(.data[[var]]) != "does not own")) |>
          summarise(pct = survey_mean(.own, na.rm = TRUE, vartype = "ci") * 100) |>
          mutate(asset = label)
      }
      data <- bind_rows(one("v745a", "House"), one("v745b", "Land"))
      validate(need(nrow(data) > 0, "Ownership variables not available."))

      p <- ggplot(data, aes(x = asset, y = pct, text = paste0(asset, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.5) +
        geom_errorbar(aes(ymin = pct_low, ymax = pct_upp), width = 0.12) +
        geom_text(aes(label = fmt_pct(pct)), vjust = -0.9, size = 3.2) +
        scale_y_continuous(labels = label_percent(scale = 1),
                           expand = expansion(mult = c(0, 0.25))) +
        labs(x = "", y = "% owning, alone or jointly") +
        theme_kdhs()
      ggplotly(p)
    })

    # ---- Bank account -------------------------------------------------------
    output$auto_bank <- renderPlotly({
      req(ir_design())
      validate(need("v170" %in% names(ir_design()$variables),
                    "Bank-account variable (v170) not available."))
      data <- ir_design() |>
        filter(!is.na(v170)) |>
        mutate(.bank = as.numeric(as.character(v170) == "yes")) |>
        group_by(v106) |>
        summarise(pct = survey_mean(.bank, na.rm = TRUE, vartype = "ci") * 100) |>
        mutate(education = fmt_label(v106))
      validate(need(nrow(data) > 0, "No data for this selection."))

      kdhs_bar(data, x = "education", y = "pct",
               title = "Bank Account by Education",
               subtitle = "% of women with an account at a financial institution")
    })

    # ---- Child marriage -----------------------------------------------------
    # v511 = age at first cohabitation. Restricted to women 20-49 so that every
    # woman in the denominator has already passed 18 — including 15-19 year-olds
    # would understate it, since some will still marry before turning 18.
    output$child_marriage <- renderPlotly({
      req(ir_design())
      data <- ir_design() |>
        mutate(.age_union = suppressWarnings(as.numeric(as.character(v511))),
               .age_now   = suppressWarnings(as.numeric(as.character(v012)))) |>
        filter(!is.na(.age_now), .age_now >= 20) |>
        mutate(.u18 = as.numeric(!is.na(.age_union) & .age_union < 18)) |>
        group_by(v024, v190) |>
        summarise(pct = survey_mean(.u18, na.rm = TRUE, vartype = NULL) * 100,
                  .groups = "drop") |>
        mutate(county = fmt_label(v024), wealth = fmt_label(v190)) |>
        filter(!is.na(pct))
      validate(need(nrow(data) > 0, "No child-marriage data available."))

      top <- data |>
        group_by(county) |>
        summarise(m = mean(pct, na.rm = TRUE), .groups = "drop") |>
        arrange(desc(m)) |>
        slice_head(n = 15) |>
        pull(county)

      plot_data <- data |>
        filter(county %in% top) |>
        order_levels("county", "pct")

      p <- ggplot(plot_data, aes(x = county, y = pct, fill = wealth, text = paste0(county, " — ", wealth, "<br>", round(pct, 1)))) +
        geom_col(position = "dodge", width = 0.8) +
        coord_flip() +
        scale_fill_brewer(palette = "YlOrRd") +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% married or cohabiting before age 18", fill = "Wealth",
             title = "Women aged 20-49, 15 highest-prevalence counties") +
        theme_kdhs() +
        theme(axis.text.y = element_text(size = 8))
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