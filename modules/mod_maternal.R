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

mod_maternal_server <- function(id, ir_design, kr_design, br_design, raw_data) {
  moduleServer(id, function(input, output, session) {

    # ---- Filter IR by inputs ------------------------------------------------
    # Case-insensitive matching (see svy_filter_eq in R/helpers.R): the dropdowns
    # say "Poorest" / "No education" while the DHS labels are lower case, so the
    # old == comparisons emptied the design and crashed every panel below.
    filtered_ir <- reactive({
      design <- ir_design()
      req(design)
      design <- svy_filter_eq(design, "v025", input$anc_residence)
      design <- svy_filter_eq(design, "v190", input$anc_wealth)
      design <- svy_filter_eq(design, "v106", input$anc_education)
      design <- svy_filter_eq(design, "v024", input$anc_region)
      design
    })

    # anc_region was hard-coded to the eight old provinces and never applied.
    # v024 is the 47 counties, so fill the control from the data.
    observeEvent(ir_design(), once = TRUE, {
      counties <- sort(unique(as.character(ir_design()$variables$v024)))
      updateSelectInput(session, "anc_region",
                        label   = "County",
                        choices = c("All", stats::setNames(counties, fmt_label(counties))),
                        selected = "All")
    })

    # ---- ANC Distribution ---------------------------------------------------
    output$plot_anc_dist <- renderPlotly({
      req(filtered_ir())
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      # In the IR recode the maternity-care variables are wide, one column per
      # birth: m14_1, m15_1, m3a_1 ... There is no bare `m14`/`m15`/`m3a`, which
      # is why this panel errored. _1 = most recent birth.
      # m14_1 is a factor ("no antenatal visits", "1", "2", ...), so read the
      # label as a number rather than taking as.numeric() of the factor.
      data <- filtered_ir() |>
        mutate(.visits = suppressWarnings(as.numeric(as.character(m14_1))),
               .visits = ifelse(as.character(m14_1) == "no antenatal visits", 0, .visits)) |>
        filter(!is.na(.visits)) |>
        mutate(anc_cat = case_when(
          .visits == 0            ~ "No visits",
          .visits %in% 1:3        ~ "1–3 visits",
          .visits >= 4            ~ "4+ visits (recommended)",
          TRUE                    ~ NA_character_
        )) |>
        filter(!is.na(anc_cat)) |>
        group_by(anc_cat) |>
        summarise(pct = survey_prop(na.rm = TRUE, vartype = NULL) * 100)

      p <- ggplot(data, aes(x = anc_cat, y = pct, fill = anc_cat, text = paste0(anc_cat, " — ", anc_cat, "<br>", round(pct, 1)))) +
        geom_col(width = 0.6, show.legend = FALSE) +
        scale_fill_manual(values = c("No visits" = "#fee2e2",
                                     "1–3 visits" = "#fca5a5",
                                     "4+ visits (recommended)" = "#1a6b4a")) +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of births (last 2 years)") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- SBA by Education ---------------------------------------------------
    output$plot_sba_edu <- renderPlotly({
      req(filtered_ir())
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      # Same wide-format issue: m15/m3a/m3b do not exist, only m15_1 / m3a_1 ...
      # m3a_1 = "Assistance: doctor", m3b_1 = "Assistance: nurse/midwife/clinical
      # officer" — plain yes/no factors, so "== 1" never matched.
      #
      # m3c_1 is deliberately NOT included: it is labelled "NA - Assistance: CS
      # health professional" and is 100% missing. Adding it to the OR chain turns
      # every FALSE into NA (FALSE | NA is NA), those rows get dropped by
      # na.rm = TRUE, and the estimate reads a flat 100% at every education level.
      data <- filtered_ir() |>
        filter(!is.na(m15_1)) |>
        mutate(.sba = as.numeric(as.character(m3a_1) == "yes" |
                                 as.character(m3b_1) == "yes")) |>
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

      # DHS vaccination variables, per the KEKR8CFL dictionary:
      #   h1 = "Has health card"  <- NOT a vaccine; the old code used it as BCG
      #   h2 = BCG, h3 = DPT 1, h4 = POLIO 1, h5 = DPT 2,
      #   h6 = POLIO 2, h7 = DPT 3, h8 = POLIO 3, h9 = MEASLES 1
      # h0 (POLIO 0) is not present in this extract.
      vax_vars <- c(BCG = "h2", DPT1 = "h3", DPT2 = "h5", DPT3 = "h7",
                    Polio1 = "h4", Measles = "h9")

      vax_data <- map_dfr(names(vax_vars), function(vax_name) {
        var <- vax_vars[[vax_name]]
        # These arrive as labelled factors, so numeric-code tests do not work:
        #   b5 == 1              -> b5 is "no"/"yes"
        #   h* %in% c(1, 2, 3)   -> h* is "no" / "vaccination date on card" /
        #                           "reported by mother" / "vaccination marked on card"
        # Vaccinated = any response other than "no" (card or recall).
        kr_design() |>
          filter(as.character(b5) == "yes",
                 !is.na(b19), b19 >= 12, b19 < 24) |>  # living children aged 12-23 months
          mutate(.got = as.numeric(!is.na(!!sym(var)) & as.character(!!sym(var)) != "no")) |>
          summarise(pct = survey_mean(.got, na.rm = TRUE) * 100) |>
          mutate(vaccine = vax_name)
      })

      vax_data <- order_levels(vax_data, "vaccine", "pct")


      p <- ggplot(vax_data, aes(x = vaccine, y = pct, text = paste0(vaccine, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.7) +
        geom_text(aes(label = fmt_pct(pct)), hjust = -0.2, size = 3.5) +
        coord_flip() +
        scale_y_continuous(limits = c(0, 110), labels = label_percent(scale = 1)) +
        labs(x = "", y = "Coverage (%)") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
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
    # Computed from m4 ("Duration of breastfeeding": never breastfed / ever but
    # not currently / still breastfeeding) in the KR recode, replacing the
    # hard-coded numbers that were here.
    #
    # Early initiation (<1 hr), exclusive breastfeeding and complementary feeding
    # are NOT shown: they need m34 (time to first breastfeed) and the 24-hour
    # dietary-recall block (v409-v414), which preprocess.R does not currently
    # keep. Add those to kr_keep if you want the full WHO IYCF set.
    output$plot_breastfeeding <- renderPlotly({
      req(kr_design())
      band <- function(lo, hi, label) {
        kr_design() |>
          filter(as.character(b5) == "yes", !is.na(b19), b19 >= lo, b19 <= hi,
                 !is.na(m4)) |>
          mutate(.bf = as.numeric(as.character(m4) == "still breastfeeding")) |>
          summarise(pct = survey_mean(.bf, na.rm = TRUE) * 100) |>
          mutate(indicator = label)
      }
      ever <- kr_design() |>
        filter(!is.na(m4)) |>
        mutate(.ev = as.numeric(as.character(m4) != "never breastfed")) |>
        summarise(pct = survey_mean(.ev, na.rm = TRUE) * 100) |>
        mutate(indicator = "Ever breastfed (all children)")

      data <- bind_rows(
        ever,
        band(0, 5,   "Still BF at 0-5 months"),
        band(6, 11,  "Still BF at 6-11 months"),
        band(12, 17, "Still BF at 12-17 months"),
        band(18, 23, "Still BF at 18-23 months")
      ) |> filter(!is.na(pct))
      validate(need(nrow(data) > 0, "No breastfeeding data available."))

      data <- order_levels(data, "indicator", "pct")


      p <- ggplot(data, aes(x = indicator, y = pct, text = paste0(indicator, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$accent, width = 0.6) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
        labs(x = "", y = "Percentage (%)") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- ANC Provider Type --------------------------------------------------
    # m2a_1 / m2b_1 / m2n_1 are yes/no flags for the most recent birth:
    # "Prenatal: doctor", "Prenatal: nurse/midwife/clinical officer",
    # "Prenatal: no one". A woman can report more than one provider, so these
    # are overlapping shares of women with a recent birth, not a partition.
    output$plot_anc_provider <- renderPlotly({
      req(filtered_ir())
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      d <- filtered_ir() |> filter(!is.na(m2a_1))

      one <- function(design, var, label) {
        if (!var %in% names(design$variables)) return(NULL)
        design |>
          mutate(.x = as.numeric(as.character(.data[[var]]) == "yes")) |>
          summarise(pct = survey_mean(.x, na.rm = TRUE) * 100) |>
          mutate(provider = label)
      }
      data <- bind_rows(
        one(d, "m2a_1", "Doctor"),
        one(d, "m2b_1", "Nurse / midwife / CO"),
        one(d, "m2n_1", "No one")
      )
      validate(need(nrow(data) > 0, "No ANC provider data available."))

      data <- order_levels(data, "provider", "pct")


      p <- ggplot(data, aes(x = provider, y = pct, text = paste0(provider, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.6) +
        geom_text(aes(label = fmt_pct(pct)), hjust = -0.15, size = 3.2) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 110)) +
        labs(x = "", y = "% of women with a birth in last 2 years") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- Place of Delivery --------------------------------------------------
    output$plot_delivery_place <- renderPlotly({
      req(filtered_ir())
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      data <- filtered_ir() |>
        filter(!is.na(m15_1)) |>
        group_by(m15_1) |>
        summarise(pct = survey_prop(na.rm = TRUE, vartype = NULL) * 100) |>
        mutate(place = fmt_label(m15_1)) |>
        arrange(desc(pct)) |>
        slice_head(n = 8)
      validate(need(nrow(data) > 0, "No place-of-delivery data available."))

      data <- order_levels(data, "place", "pct")


      p <- ggplot(data, aes(x = place, y = pct, text = paste0(place, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.7) +
        geom_text(aes(label = fmt_pct(pct)), hjust = -0.15, size = 3) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1),
                           expand = expansion(mult = c(0, 0.18))) +
        labs(x = "", y = "% of births (last 2 years)") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- C-Section Rate -----------------------------------------------------
    output$plot_csection <- renderPlotly({
      req(filtered_ir())
      validate(need(svy_has_rows(filtered_ir()),
                    "No respondents match this filter combination."))
      data <- filtered_ir() |>
        filter(!is.na(m17_1)) |>
        mutate(.cs = as.numeric(as.character(m17_1) == "yes")) |>
        group_by(v025) |>
        summarise(pct = survey_mean(.cs, na.rm = TRUE, vartype = "ci") * 100) |>
        mutate(residence = fmt_label(v025))
      validate(need(nrow(data) > 0, "No caesarean-section data available."))

      p <- ggplot(data, aes(x = residence, y = pct, text = paste0(residence, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$secondary, width = 0.5) +
        geom_errorbar(aes(ymin = pct_low, ymax = pct_upp), width = 0.12) +
        geom_text(aes(label = fmt_pct(pct)), vjust = -0.9, size = 3.2) +
        scale_y_continuous(labels = label_percent(scale = 1),
                           expand = expansion(mult = c(0, 0.25))) +
        labs(x = "", y = "% of births delivered by C-section") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- Full Immunization by Region ---------------------------------------
    # full_vax is derived in preprocess.R from h33 ("all basic vaccinations").
    # Restricted to living children aged 12-23 months, the standard denominator.
    output$plot_full_vax <- renderPlotly({
      req(kr_design())
      data <- kr_design() |>
        filter(as.character(b5) == "yes", !is.na(b19), b19 >= 12, b19 < 24,
               !is.na(full_vax)) |>
        group_by(v024) |>
        summarise(pct = survey_mean(full_vax, na.rm = TRUE, vartype = NULL) * 100) |>
        mutate(county = fmt_label(v024)) |>
        filter(!is.na(pct)) |>
        arrange(desc(pct)) |>
        slice_head(n = 20)
      validate(need(nrow(data) > 0, "No immunization data for this selection."))

      data <- order_levels(data, "county", "pct")


      p <- ggplot(data, aes(x = county, y = pct, text = paste0(county, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.75) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% fully vaccinated (12-23 months)",
             title = "Top 20 counties") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- Child Nutrition ----------------------------------------------------
    # stunted / wasted / underweight are derived in preprocess.R from the WHO
    # z-scores (hw70/hw72/hw71 < -2 SD), with DHS out-of-range flags removed.
    output$plot_stunting <- renderPlotly({
      req(kr_design())
      data <- kr_design() |>
        filter(!is.na(stunted)) |>
        group_by(v190) |>
        summarise(pct = survey_mean(stunted, na.rm = TRUE, vartype = "ci") * 100) |>
        mutate(wealth = fmt_label(v190))
      validate(need(nrow(data) > 0, "No stunting data available."))

      kdhs_bar(data, x = "wealth", y = "pct",
               title = "Stunting by Wealth Quintile",
               subtitle = "Height-for-age < -2 SD, children under 5")
    })

    output$plot_wasting <- renderPlotly({
      req(kr_design())
      data <- kr_design() |>
        filter(!is.na(wasted)) |>
        group_by(v024) |>
        summarise(pct = survey_mean(wasted, na.rm = TRUE, vartype = NULL) * 100) |>
        mutate(county = fmt_label(v024)) |>
        filter(!is.na(pct)) |>
        arrange(desc(pct)) |>
        slice_head(n = 15)
      validate(need(nrow(data) > 0, "No wasting data available."))

      data <- order_levels(data, "county", "pct")


      p <- ggplot(data, aes(x = county, y = pct, text = paste0(county, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$secondary, width = 0.75) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% wasted (weight-for-height < -2 SD)",
             title = "15 highest-burden counties") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    output$plot_underweight <- renderPlotly({
      req(kr_design())
      data <- kr_design() |>
        filter(!is.na(underweight), !is.na(b19)) |>
        mutate(age_grp = cut(as.numeric(b19),
                             breaks = c(-1, 5, 11, 23, 35, 47, 59),
                             labels = c("0-5 m", "6-11 m", "12-23 m",
                                        "24-35 m", "36-47 m", "48-59 m"))) |>
        filter(!is.na(age_grp)) |>
        group_by(age_grp) |>
        summarise(pct = survey_mean(underweight, na.rm = TRUE, vartype = "ci") * 100)
      validate(need(nrow(data) > 0, "No underweight data available."))

      p <- ggplot(data, aes(x = age_grp, y = pct, group = 1)) +
        geom_col(fill = KDHS_COLORS$accent, width = 0.7) +
        geom_errorbar(aes(ymin = pct_low, ymax = pct_upp), width = 0.15) +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% underweight (weight-for-age < -2 SD)") +
        theme_kdhs() +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))
      ggplotly(p, tooltip = "text")
    })

    # ---- Child Mortality ----------------------------------------------------
    # IMPORTANT: these are NOT the DHS synthetic-cohort life-table rates published
    # in the report. They are direct proportions from the birth history, which is
    # the honest thing a dashboard can compute quickly. To avoid right-censoring
    # (a child born last year cannot yet have died before age 5), the under-5
    # measure uses only births 5-14 years before the survey, so every child in the
    # denominator had the full five years of exposure. Expect a difference from
    # the published 41/1,000, which refers to the 5 years before the survey.
    output$plot_u5mr <- renderPlotly({
      req(br_design())
      data <- br_design() |>
        mutate(.months_ago = as.numeric(v008) - as.numeric(b3)) |>
        filter(!is.na(.months_ago), .months_ago >= 60, .months_ago < 180,
               !is.na(u5_death)) |>
        group_by(v024) |>
        summarise(rate = survey_mean(u5_death, na.rm = TRUE, vartype = NULL) * 1000) |>
        mutate(county = fmt_label(v024)) |>
        filter(!is.na(rate)) |>
        arrange(desc(rate))
      validate(need(nrow(data) > 0, "No child-mortality data available."))

      data <- order_levels(data, "county", "rate")


      p <- ggplot(data, aes(x = county, y = rate, text = paste0(county, ": ", round(rate, 1)))) +
        geom_col(fill = KDHS_COLORS$secondary, width = 0.8) +
        coord_flip() +
        labs(x = "", y = "Deaths before age 5 per 1,000 live births",
             title = "Births 5-14 years before the survey (full exposure)") +
        theme_kdhs() +
        theme(axis.text.y = element_text(size = 7))
      ggplotly(p, tooltip = "text")
    })

    output$plot_nmr <- renderPlotly({
      req(br_design())
      # Neonatal deaths occur in the first month, so births in the 5 years before
      # the survey are effectively fully exposed and no restriction is needed.
      data <- br_design() |>
        mutate(.months_ago = as.numeric(v008) - as.numeric(b3)) |>
        filter(!is.na(.months_ago), .months_ago < 60, !is.na(nn_death)) |>
        group_by(v025) |>
        summarise(rate = survey_mean(nn_death, na.rm = TRUE, vartype = "ci") * 1000) |>
        mutate(residence = fmt_label(v025))
      validate(need(nrow(data) > 0, "No neonatal-mortality data available."))

      p <- ggplot(data, aes(x = residence, y = rate, text = paste0(residence, ": ", round(rate, 1)))) +
        geom_col(fill = KDHS_COLORS$secondary, width = 0.5) +
        geom_errorbar(aes(ymin = rate_low, ymax = rate_upp), width = 0.12) +
        geom_text(aes(label = round(rate, 1)), vjust = -0.9, size = 3.2) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
        labs(x = "", y = "Neonatal deaths per 1,000 live births",
             title = "Births in the 5 years before the survey") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })
  })
}
