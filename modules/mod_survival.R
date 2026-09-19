# =============================================================================
# modules/mod_survival.R
# Child Survival (U5CM) — the thesis's core analysis, built as teaching blocks.
# =============================================================================
# Ordered so each panel depends only on what came before it:
#   1 Cohort      — what the analysis set is, and why censoring matters
#   2 Kaplan-Meier— non-parametric survival, stratified + log-rank
#   3 Hazard      — where in the first five years the risk actually sits
#   4 Cox PH      — semi-parametric regression + assumption check
#   5 County frailty — unobserved county-level risk, mapped
#   6 Life table  — the arithmetic behind the published under-5 rate
#
# All estimates are survey-weighted. See R/survival_core.R for the definitions.
# =============================================================================

mod_survival_ui <- function(id) {
  ns <- NS(id)
  cov_choices <- u5cm_covariate_choices()

  tagList(
    wellPanel(
      style = "background:#f0f7f4; border-left:4px solid #1a6b4a;",
      fluidRow(
        column(9,
          h4(style = "margin-top:0;", "Under-Five Child Mortality — survival analysis"),
          p(style = "margin-bottom:0; font-size:0.88rem;",
            "One row per child born in the 5 years before the survey. ",
            strong("Time"), " is age in months (censored at 59), ",
            strong("event"), " is death. Every estimate is weighted for the DHS ",
            "stratified multi-stage design.")
        ),
        column(3,
          # The sentinel must not be "" — selectize silently DROPS a choice whose
          # value is the empty string, so the "overall curve" option never
          # appeared in the dropdown at all and there was no way to get an
          # ungrouped curve. Use a real token instead.
          selectInput(ns("strata_var"), "Compare groups by",
                      choices  = c("Overall — no grouping" = "__overall__",
                                   cov_choices),
                      selected = "__overall__"),
          # This control cannot apply to every tab, and saying so beats leaving
          # the reader to conclude it is broken: Cox and the county frailty take
          # their covariates from their own checkbox blocks.
          p(style = "font-size:0.75rem; color:#718096; margin:-0.5rem 0 0;",
            "Splits the Kaplan-Meier, Hazard and Life table tabs. Cox and County ",
            "frailty use their own covariate selection.")
        )
      )
    ),

    # ---- FILTERS -------------------------------------------------------------
    # These restrict the analysis cohort and feed EVERY panel below — Kaplan-Meier,
    # hazard, Cox, frailty and the life table all read the same filtered design.
    wellPanel(
      fluidRow(
        column(3, selectInput(ns("f_residence"), "Residence",
                              choices = c("All", "Urban", "Rural"), selected = "All")),
        column(3, selectInput(ns("f_county"), "County",
                              choices = "All", selected = "All")),
        column(3, selectInput(ns("f_wealth"), "Wealth quintile",
                              choices = c("All", "Poorest", "Poorer", "Middle",
                                          "Richer", "Richest"), selected = "All")),
        column(3, selectInput(ns("f_education"), "Mother's education",
                              choices = "All", selected = "All"))
      ),
      div(style = "font-size:0.78rem; color:#4a5568;",
          textOutput(ns("filter_note"), inline = TRUE))
    ),

    tabsetPanel(
      id = ns("surv_tabs"),

      # ---- 1. COHORT ---------------------------------------------------------
      tabPanel("1. Cohort & definitions", br(),
        fluidRow(
          column(5, wellPanel(
            h5("The analysis cohort"),
            tableOutput(ns("cohort_tbl")),
            hr(),
            h6("Survival object"),
            tags$pre(style = "font-size:0.75rem; background:#f8f9fa; padding:8px;",
"dead      = child is not alive
surv_time = age at death       if dead
            min(current age, 59) if alive
Surv(surv_time, dead)"),
            p(style = "font-size:0.78rem; color:#4a5568; margin-bottom:0;",
              "Identical to the thesis pipeline's definition in ",
              code("03_clean_data-v2.R"), ", so the two cannot drift apart.")
          )),
          column(7, wellPanel(
            h5("Why a plain 0/1 logistic understates under-five mortality"),
            p(style = "font-size:0.88rem;",
              "The file records whether each child has died ", em("by the age they are now"),
              " — not whether they died before five. Exposure is very uneven, so the ",
              "\"alive\" group is padded with children who simply have not had time to die."),
            tableOutput(ns("censor_tbl")),
            p(style = "font-size:0.82rem; color:#4a5568;",
              "The naive figure sits well below the published rate, and the gap depends on ",
              "the age mix of whatever subgroup you filter to. Kaplan-Meier uses each ",
              "child's actual exposure and discards no one."),
            plotlyOutput(ns("followup_plot"), height = "190px") |> withSpinner()
          ))
        )
      ),

      # ---- 2. KAPLAN-MEIER ---------------------------------------------------
      tabPanel("2. Kaplan-Meier", br(),
        fluidRow(
          column(8, wellPanel(
            h5(textOutput(ns("km_title"), inline = TRUE)),
            plotlyOutput(ns("km_plot"), height = "330px") |> withSpinner(),
            uiOutput(ns("logrank_note"))
          )),
          column(4, wellPanel(
            h5("Cumulative mortality"),
            p(style = "font-size:0.8rem; color:#4a5568;",
              "The same curve read the DHS way: deaths per 1,000 live births."),
            plotlyOutput(ns("cummort_plot"), height = "230px") |> withSpinner()
          ))
        ),
        fluidRow(column(12, wellPanel(
          h5("Numbers at risk"),
          p(style = "font-size:0.8rem; color:#4a5568;",
            "How many children are still under observation at each age, and how many ",
            "deaths have accumulated. Thinning numbers are why the right-hand end of a ",
            "survival curve is the least certain part of it."),
          tableOutput(ns("risk_tbl"))
        )))
      ),

      # ---- 3. HAZARD ---------------------------------------------------------
      tabPanel("3. Hazard", br(),
        fluidRow(
          column(6, wellPanel(
            h5("Death probability by age segment"),
            p(style = "font-size:0.8rem; color:#4a5568;",
              "The DHS age segments — the same ones behind the published under-five ",
              "rate. Risk is heavily front-loaded into the first month."),
            plotlyOutput(ns("hazard_plot"), height = "300px") |> withSpinner()
          )),
          column(6, wellPanel(
            h5("Cumulative hazard (Nelson-Aalen)"),
            p(style = "font-size:0.8rem; color:#4a5568;",
              "Accumulated risk of death. A straight line means a constant hazard; ",
              "the steep early rise here is neonatal mortality."),
            plotlyOutput(ns("cumhaz_plot"), height = "300px") |> withSpinner()
          ))
        )
      ),

      # ---- 4. COX ------------------------------------------------------------
      tabPanel("4. Cox proportional hazards", br(),
        fluidRow(
          column(3, wellPanel(
            h5("Covariates"),
            p(style = "font-size:0.78rem; color:#4a5568;",
              "Grouped as in the Mosley-Chen framework of child-survival determinants."),
            lapply(names(U5CM_COVARIATES), function(grp) {
              tagList(
                h6(style = "margin-bottom:4px; color:#1a6b4a;", grp),
                checkboxGroupInput(
                  ns(paste0("cox_", gsub("[^A-Za-z]", "", grp))), NULL,
                  choices  = U5CM_COVARIATES[[grp]],
                  selected = if (grp == "Child") c("sex", "short_birth_interval")
                             else if (grp == "Maternal") "maternal_education"
                             else if (grp == "Household & environment") "v190"
                             else character(0)
                )
              )
            }),
            actionButton(ns("run_cox"), "Fit Cox model",
                         class = "btn-success btn-sm", icon = icon("play"))
          )),
          column(9,
            wellPanel(
              h5("Adjusted hazard ratios"),
              p(style = "font-size:0.8rem; color:#718096;",
                "Survey-weighted Cox model via svycoxph(). An HR above 1 means a ",
                "higher death rate than the reference category."),
              plotlyOutput(ns("cox_forest"), height = "330px") |> withSpinner(),
              hr(),
              tableOutput(ns("cox_tbl"))
            ),
            wellPanel(
              h5("Proportional-hazards assumption"),
              p(style = "font-size:0.8rem; color:#4a5568;",
                "Schoenfeld residual test. A small p-value means that term's effect ",
                "changes with age, so a single hazard ratio is not describing it well — ",
                "which is exactly the limitation that motivates the machine-learning models."),
              tableOutput(ns("ph_tbl")),
              uiOutput(ns("ph_note"))
            )
          )
        )
      ),

      # ---- 5. COUNTY FRAILTY -------------------------------------------------
      tabPanel("5. County frailty", br(),
        fluidRow(column(12,
          div(class = "alert alert-warning", style = "font-size:0.85rem;",
              strong("Read the caveat. "),
              "svycoxph() rejects penalised terms, and coxph() cannot produce a robust ",
              "variance for one. This model therefore uses the sampling weights for the ",
              "point estimates but its standard errors are model-based, not design-based. ",
              "It is also an ", strong("exchangeable"), " county random effect, not an ICAR ",
              "spatial prior — true ICAR needs INLA or Stan and stays in the thesis pipeline.")
        )),
        fluidRow(column(12, wellPanel(
          style = "background:#f0f7f4; border-left:4px solid #1a6b4a;",
          h5(style = "margin-top:0;", "Is the county variation actually spatial?"),
          p(style = "font-size:0.85rem;",
            "The frailty above is ", strong("exchangeable"), " — every county gets its own ",
            "random effect, but the model has no idea which counties share a border. ",
            "The thesis premise is that neighbours share risk, and that premise is ",
            "testable. Moran's I on the 47 frailty estimates is the test."),
          uiOutput(ns("moran_result"))
        ))),
        fluidRow(
          column(7, wellPanel(
            div(style = "display:flex; justify-content:space-between; align-items:center;",
                h5(style = "margin:0;", "County map"),
                radioButtons(ns("map_mode"), NULL, inline = TRUE,
                             choices = c("Frailty HR" = "hr", "LISA clusters" = "lisa"),
                             selected = "hr")),
            p(style = "font-size:0.8rem; color:#4a5568;",
              "Frailty HR shades residual county risk. LISA classifies each county by ",
              "whether it and its neighbours are jointly high or low — separating a ",
              "genuine cluster from an isolated outlier."),
            leafletOutput(ns("frailty_map"), height = "360px") |> withSpinner()
          )),
          column(5, wellPanel(
            h5("Model summary"),
            uiOutput(ns("frailty_summary")),
            hr(),
            h6("Counties ranked by frailty"),
            plotlyOutput(ns("frailty_plot"), height = "260px") |> withSpinner(),
            hr(),
            h6("LISA classification"),
            tableOutput(ns("lisa_tbl"))
          ))
        )
      ),

      # ---- 6. LIFE TABLE -----------------------------------------------------
      tabPanel("6. Life table", br(),
        fluidRow(column(12, wellPanel(
          h5("Abridged life table, ages 0-59 months"),
          p(style = "font-size:0.85rem; color:#4a5568;",
            "The arithmetic behind the under-five rate: a death probability q for each ",
            "age segment, chained into cumulative survival as ", code("1 - prod(1 - q)"),
            ". This is the same machinery as the Overview's under-five card."),
          tableOutput(ns("life_tbl")),
          uiOutput(ns("life_note"))
        )))
      )
    )
  )
}

# =============================================================================
mod_survival_server <- function(id, kr_design, br_design, raw_data) {
  moduleServer(id, function(input, output, session) {

    # ---- shared reactives --------------------------------------------------
    # The FULL cohort, before any filter — needed for the "of N children" context
    # and so the filter dropdowns can be populated from real values.
    des_all <- reactive({
      req(kr_design())
      d <- u5cm_design(kr_design()$variables)
      validate(need(!is.null(d), "No children with usable survival time."))
      d
    })

    # Populate the two data-driven dropdowns from the cohort itself, so the labels
    # always match the values (the Overview's Region control was once hard-coded
    # to the eight old provinces and could never match v024's 47 counties).
    observeEvent(des_all(), once = TRUE, {
      v <- des_all()$variables
      cty <- sort(unique(as.character(v$county)))
      updateSelectInput(session, "f_county",
                        choices = c("All", stats::setNames(cty, fmt_label(cty))),
                        selected = "All")
      edu <- sort(unique(as.character(v$maternal_education)))
      updateSelectInput(session, "f_education",
                        choices = c("All", stats::setNames(edu, fmt_label(edu))),
                        selected = "All")
    })

    # THE filtered design. Everything below reads this, so a filter change flows
    # through Kaplan-Meier, hazard, Cox, frailty and the life table alike.
    des <- reactive({
      d <- des_all()
      # Filter on the columns THIS design actually carries. apply_filters() maps
      # `county` to v024, but u5cm_design() slims to the covariate list, which
      # holds the derived `county` and not v024 — so routing the county filter
      # through apply_filters() silently did nothing and left all 47 counties in.
      # Filter each column explicitly instead; svy_filter_eq() is a no-op on "All"
      # and on a column the design does not have.
      d <- svy_filter_eq(d, "v025",               input$f_residence)
      d <- svy_filter_eq(d, "v190",               input$f_wealth)
      d <- svy_filter_eq(d, "county",             input$f_county)
      d <- svy_filter_eq(d, "maternal_education", input$f_education)
      validate(need(svy_has_rows(d),
                    "No children match this filter combination. Widen it."))
      d
    })

    # The birth history, filtered the same way, so the DHS period rate shown
    # alongside the Kaplan-Meier figure refers to the SAME subgroup. Without this
    # a filter moved one number and left the other national, which reads as a
    # contradiction. Mother's education is not carried in the birth recode, so
    # that one filter cannot apply here — flagged in the UI when it is set.
    des_br <- reactive({
      req(br_design())
      apply_filters(br_design(), "br", list(
        residence = input$f_residence,
        county    = input$f_county,
        wealth    = input$f_wealth
      ))
    })

    n_filtered <- reactive(nrow(des()$variables))
    any_filter <- reactive(!all(c(input$f_residence, input$f_county,
                                  input$f_wealth, input$f_education) == "All"))

    output$filter_note <- renderText({
      if (!any_filter()) {
        sprintf("No filter applied — all %s children in the cohort.",
                fmt_n(nrow(des_all()$variables)))
      } else {
        d <- des()$variables
        base <- sprintf(
          "Filtered to %s of %s children, %s deaths. Every panel below uses this subset.",
          fmt_n(nrow(d)), fmt_n(nrow(des_all()$variables)),
          fmt_n(sum(d$dead == 1, na.rm = TRUE)))
        # Mother's education is not carried in the birth recode, so the DHS period
        # rate on tabs 1 and 6 cannot honour that one filter. Say so rather than
        # letting the two figures disagree silently.
        if (!identical(input$f_education, "All"))
          paste(base, "Note: the DHS period rate shown on tabs 1 and 6 comes from",
                "the birth history, which does not carry mother's education — that",
                "one filter does not apply to it.")
        else base
      }
    })

    strata_var <- reactive({
      v <- input$strata_var
      if (is.null(v) || v == "" || identical(v, "__overall__")) NULL else v
    })

    cohort <- reactive(u5cm_cohort_summary(des()))

    # ---- 1. COHORT ---------------------------------------------------------
    output$cohort_tbl <- renderTable({
      s <- cohort()
      data.frame(
        Measure = c("Children in cohort", "Deaths (events)", "Censored (still alive)",
                    "Median follow-up", "Maximum follow-up",
                    "Alive with under 12 months observed"),
        Value = c(fmt_n(s$n_children), fmt_n(s$n_deaths),
                  sprintf("%s (%.1f%%)", fmt_n(s$n_children - s$n_deaths), s$pct_censored),
                  sprintf("%.0f months", s$median_fu),
                  sprintf("%.0f months", s$max_fu),
                  fmt_n(s$n_under12)),
        check.names = FALSE
      )
    }, striped = TRUE, spacing = "xs", width = "100%")

    output$censor_tbl <- renderTable({
      s  <- cohort()
      km <- u5cm_km(des(), by = NULL, ci = FALSE)
      km_final <- (1 - min(km$surv, na.rm = TRUE)) * 1000
      period <- dhs_child_mortality(des_br()$variables)
      data.frame(
        Estimate = c("Naive % dead on the 0/1 flag (all children)",
                     "Kaplan-Meier cumulative mortality by 59 months",
                     "DHS period rate, synthetic cohort (Overview card)",
                     "Published KDHS 2022, Table 16"),
        `Per 1,000` = c(sprintf("%.1f", s$pct_deaths_raw * 10),
                        sprintf("%.1f", km_final),
                        sprintf("%.1f", period$under5),
                        "41.0"),
        check.names = FALSE
      )
    }, striped = TRUE, spacing = "xs", width = "100%")

    output$followup_plot <- renderPlotly({
      d <- des()$variables
      bins <- data.frame(months = d$surv_time) |>
        mutate(band = cut(months, c(-1, 0, 5, 11, 23, 35, 47, 59),
                          labels = c("0", "1-5", "6-11", "12-23", "24-35", "36-47", "48-59"))) |>
        filter(!is.na(band)) |>
        count(band, name = "children")
      p <- ggplot(bins, aes(x = band, y = children,
                            text = paste0(band, " months<br>", fmt_n(children), " children"))) +
        geom_col(fill = KDHS_COLORS$neutral, width = 0.75) +
        labs(x = "Months observed", y = "Children",
             title = "Exposure is uneven by design") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- 2. KAPLAN-MEIER ---------------------------------------------------
    output$km_title <- renderText({
      v <- strata_var()
      if (is.null(v)) "Survival to age 5 — all children"
      else paste0("Survival to age 5 — by ",
                  names(u5cm_covariate_choices())[match(v, u5cm_covariate_choices())])
    })

    km_data <- reactive({
      k <- u5cm_km(des(), by = strata_var(), ci = is.null(strata_var()))
      validate(need(nrow(k) > 0, "Not enough events to estimate a survival curve."))
      k
    })

    output$km_plot <- renderPlotly({
      k     <- km_data()
      ncol_ <- dplyr::n_distinct(k$group)
      has_ci <- all(!is.na(k$lower))

      p <- ggplot(k, aes(x = time, y = surv, colour = group, group = group,
                         text = paste0(group, "<br>", time, " months<br>",
                                       "survival ", sprintf("%.3f", surv)))) +
        geom_step(linewidth = 0.9)

      # Declare the fill scale ONLY when a fill aesthetic actually exists.
      # A stray scale_fill_manual() with nothing mapped to fill makes ggplotly
      # build legend entries from the interaction of both aesthetics, so the
      # legend read "(No,1)" and "(Yes,1)" instead of "No" and "Yes".
      if (has_ci) {
        p <- p +
          geom_ribbon(aes(ymin = lower, ymax = upper, fill = group),
                      alpha = 0.15, colour = NA) +
          scale_fill_manual(values = kdhs_palette(ncol_)) +
          labs(fill = "")
      }

      p <- p +
        scale_colour_manual(values = kdhs_palette(ncol_)) +
        coord_cartesian(ylim = c(min(k$surv, na.rm = TRUE) * 0.995, 1)) +
        labs(x = "Age (months)", y = "Probability of survival", colour = "") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    output$cummort_plot <- renderPlotly({
      k <- km_data()
      p <- ggplot(k, aes(x = time, y = cum_mort, colour = group, group = group,
                         text = paste0(group, "<br>", time, " months<br>",
                                       sprintf("%.1f", cum_mort), " per 1,000"))) +
        geom_step(linewidth = 0.9) +
        scale_colour_manual(values = kdhs_palette(dplyr::n_distinct(k$group))) +
        labs(x = "Age (months)", y = "Deaths per 1,000", colour = "") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    output$logrank_note <- renderUI({
      lr <- u5cm_logrank(des(), strata_var())
      if (is.null(lr) || is.na(lr$p)) {
        return(p(style = "font-size:0.8rem; color:#718096; margin-bottom:0;",
                 "Pick a grouping variable above to compare curves with a log-rank test."))
      }
      verdict <- if (lr$p < 0.05)
        "The curves differ more than sampling noise would explain."
      else
        "No statistically detectable difference between these curves."
      p(style = "font-size:0.85rem; margin-bottom:0;",
        strong("Weighted log-rank test: "),
        sprintf("p = %s. ", if (lr$p < 0.001) "<0.001" else sprintf("%.3f", lr$p)),
        verdict)
    })

    output$risk_tbl <- renderTable({
      rt <- u5cm_risk_table(des(), by = strata_var())
      out <- rt |>
        mutate(cell = paste0(fmt_n(at_risk), "  (", events, " deaths)")) |>
        select(group, time, cell) |>
        tidyr::pivot_wider(names_from = time, values_from = cell,
                           names_prefix = "month ")
      names(out)[1] <- "Group"
      as.data.frame(out)
    }, striped = TRUE, spacing = "xs", width = "100%")

    # ---- 3. HAZARD ---------------------------------------------------------
    output$hazard_plot <- renderPlotly({
      grp <- strata_var()
      h <- u5cm_life_table(des(), by = grp)
      validate(need(!is.null(h) && nrow(h) > 0,
                    "Not enough children to estimate segment hazards."))
      p <- ggplot(h, aes(x = segment, y = q * 1000, fill = group,
                         text = paste0(group, "<br>", segment, " months<br>",
                                       sprintf("%.1f", q * 1000), " per 1,000<br>",
                                       fmt_n(n_deaths), " deaths of ", fmt_n(n_entered)))) +
        geom_col(width = 0.75, position = if (is.null(grp)) "stack" else "dodge") +
        scale_fill_manual(values = kdhs_palette(dplyr::n_distinct(h$group))) +
        labs(x = "Age segment (months)", y = "Deaths per 1,000 entering segment",
             fill = "") +
        theme_kdhs()
      if (is.null(grp)) p <- p + theme(legend.position = "none")
      ggplotly(p, tooltip = "text")
    })

    output$cumhaz_plot <- renderPlotly({
      k <- u5cm_km(des(), by = strata_var(), ci = FALSE)
      k$cumhaz <- -log(pmax(k$surv, 1e-12))
      p <- ggplot(k, aes(x = time, y = cumhaz, colour = group, group = group,
                         text = paste0(group, "<br>", time, " months<br>H = ",
                                       sprintf("%.4f", cumhaz)))) +
        geom_step(linewidth = 0.9) +
        scale_colour_manual(values = kdhs_palette(dplyr::n_distinct(k$group))) +
        labs(x = "Age (months)", y = "Cumulative hazard H(t)", colour = "") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- 4. COX ------------------------------------------------------------
    selected_covariates <- reactive({
      unlist(lapply(names(U5CM_COVARIATES), function(grp)
        input[[paste0("cox_", gsub("[^A-Za-z]", "", grp))]]), use.names = FALSE)
    })

    # NOT an eventReactive. eventReactive ISOLATES its value expression, so des()
    # changing never retriggered the fit: after changing a filter the panel kept
    # showing a model fitted to a different cohort than the filter bar described,
    # with nothing to say so. A plain reactive that depends on des() and on the
    # button refits when the cohort changes, while isolate() keeps ticking a
    # covariate box from firing a fit before the user asks for one. The fit costs
    # ~0.2s, so there is nothing to protect against here.
    cox_fit <- reactive({
      des_now <- des()
      input$run_cox
      cv <- isolate(selected_covariates())
      validate(need(length(cv) > 0, "Select at least one covariate, then fit the model."))
      # Deaths are rare (694 in the full cohort), so a tight filter can leave too
      # few events to support any regression. The rule of thumb is ~10 events per
      # estimated parameter; below 20 events in total nothing is worth fitting.
      n_ev <- sum(des_now$variables$dead == 1, na.rm = TRUE)
      validate(need(n_ev >= 20, paste0(
        "Only ", n_ev, " deaths in this selection — too few to fit a Cox model. ",
        "Widen the filters above.")))
      f <- u5cm_cox(des_now, cv)
      validate(need(!is.null(f), "The model did not converge with these covariates."))
      f
    })

    cox_tidy <- reactive({
      fit <- cox_fit()
      broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE)
    })

    output$cox_forest <- renderPlotly({
      co <- cox_tidy()
      validate(need(nrow(co) > 0, "No terms to plot."))
      co <- order_levels(co, "term", "estimate")
      p <- ggplot(co, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high,
                          text = paste0(term, "<br>HR ", sprintf("%.2f", estimate),
                                        " (", sprintf("%.2f", conf.low), "-",
                                        sprintf("%.2f", conf.high), ")"))) +
        geom_pointrange(colour = KDHS_COLORS$primary, linewidth = 0.6) +
        geom_hline(yintercept = 1, linetype = "dashed", colour = "gray50") +
        coord_flip() +
        scale_y_log10() +
        labs(x = "", y = "Adjusted hazard ratio (log scale)") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    output$cox_tbl <- renderTable({
      co <- cox_tidy()
      data.frame(
        Term      = co$term,
        HR        = sprintf("%.2f", co$estimate),
        `95% CI`  = sprintf("%.2f - %.2f", co$conf.low, co$conf.high),
        `p-value` = ifelse(co$p.value < 0.001, "<0.001", sprintf("%.3f", co$p.value)),
        check.names = FALSE
      )
    }, striped = TRUE, spacing = "xs", width = "100%")

    ph_data <- reactive(u5cm_ph_test(cox_fit()))

    output$ph_tbl <- renderTable({
      z <- ph_data()
      validate(need(!is.null(z), "The Schoenfeld test could not be computed for this model."))
      data.frame(
        Term      = z$term,
        `Chi-sq`  = sprintf("%.2f", z$chisq),
        df        = z$df,
        `p-value` = ifelse(z$p < 0.001, "<0.001", sprintf("%.3f", z$p)),
        check.names = FALSE
      )
    }, striped = TRUE, spacing = "xs", width = "100%")

    output$ph_note <- renderUI({
      z <- ph_data()
      if (is.null(z)) return(NULL)
      g <- z$p[z$term == "GLOBAL"]
      if (length(g) == 0 || is.na(g)) return(NULL)
      if (g < 0.05)
        div(class = "alert alert-warning", style = "font-size:0.82rem; margin-bottom:0;",
            sprintf("Global p = %s: proportional hazards is violated. ",
                    if (g < 0.001) "<0.001" else sprintf("%.3f", g)),
            "At least one effect changes with the child's age, so a single hazard ratio ",
            "is an average over a moving target. This is the concrete limitation that ",
            "random survival forests and neural survival models are meant to relax.")
      else
        div(class = "alert alert-success", style = "font-size:0.82rem; margin-bottom:0;",
            sprintf("Global p = %.3f: no evidence against proportional hazards. ", g),
            "The Cox hazard ratios above are reasonable summaries.")
    })

    # ---- 5. COUNTY FRAILTY -------------------------------------------------
    # Same reasoning as cox_fit: this must follow the filters, or the map shades
    # counties using a cohort the user is no longer looking at.
    frailty_fit <- reactive({
      des_now <- des()
      input$run_cox
      cv <- isolate(selected_covariates())
      # A county frailty needs more than one county. Filtering to a single county
      # leaves the random effect with nothing to vary over, and the fit is either
      # degenerate or meaningless — say so instead of drawing a one-colour map.
      n_cty <- dplyr::n_distinct(des_now$variables$county, na.rm = TRUE)
      validate(need(n_cty >= 5, paste0(
        "County frailty needs several counties to estimate variation between them — ",
        "this selection has ", n_cty,
        ". Widen or clear the County filter above.")))
      u5cm_frailty_cox(des_now, cv)
    })

    frailty_df <- reactive({
      f <- frailty_fit()
      validate(need(!is.null(f), "Fit the Cox model first (tab 4) to estimate frailties."))
      fv <- u5cm_frailty_values(f)
      validate(need(!is.null(fv), "This model produced no county frailty estimates."))
      fv
    })

    output$frailty_summary <- renderUI({
      f <- frailty_fit()
      validate(need(!is.null(f), "Fit the Cox model first (tab 4)."))
      theta <- u5cm_frailty_theta(f)
      fv <- u5cm_frailty_values(f)
      tagList(
        p(style = "font-size:0.85rem;",
          strong("Frailty variance (theta): "),
          if (is.na(theta)) "not reported" else sprintf("%.4f", theta)),
        p(style = "font-size:0.85rem;",
          strong("Counties: "), nrow(fv), br(),
          strong("Hazard-ratio spread: "),
          sprintf("%.2f to %.2f", min(fv$hr), max(fv$hr))),
        p(style = "font-size:0.8rem; color:#4a5568; margin-bottom:0;",
          "A theta near zero means county adds little beyond the covariates; a larger ",
          "value means real unexplained geographic variation — the case for the ",
          "spatial models in the thesis.")
      )
    })

    output$frailty_plot <- renderPlotly({
      fv <- frailty_df()
      fv$county_lab <- fmt_label(fv$county)
      fv <- fv[order(-fv$hr), ]
      fv <- head(fv, 20)
      fv <- order_levels(fv, "county_lab", "hr")
      p <- ggplot(fv, aes(x = county_lab, y = hr,
                          text = paste0(county_lab, "<br>frailty HR ",
                                        sprintf("%.3f", hr)))) +
        geom_col(fill = KDHS_COLORS$secondary, width = 0.78) +
        geom_hline(yintercept = 1, linetype = "dashed", colour = "gray40") +
        coord_flip() +
        labs(x = "", y = "Frailty hazard ratio", title = "20 highest-risk counties") +
        theme_kdhs() +
        theme(axis.text.y = element_text(size = 8))
      ggplotly(p, tooltip = "text")
    })

    # ---- spatial diagnostics (spdep) ---------------------------------------
    spatial <- reactive({
      req(raw_data$counties)
      s <- u5cm_spatial_join(frailty_df(), raw_data$counties)
      validate(need(!is.null(s), "Could not build the county neighbour list."))
      s
    })

    output$moran_result <- renderUI({
      m <- u5cm_moran(spatial())
      validate(need(!is.null(m), "Moran's I could not be computed."))
      sig <- m$p < 0.05
      tagList(
        p(style = "font-size:0.9rem; margin-bottom:6px;",
          strong(sprintf("Moran's I = %.4f", m$I)),
          sprintf("  (expected under no structure: %.4f)   p = %s",
                  m$expected, if (m$p < 0.001) "<0.001" else sprintf("%.3f", m$p)),
          sprintf("   ·  %.1f neighbours per county on average", m$n_nb),
          if (m$islands > 0) sprintf(", %d with none", m$islands) else ""),
        if (sig)
          div(class = "alert alert-warning", style = "font-size:0.83rem; margin-bottom:0;",
              strong("Spatial structure detected. "),
              "Neighbouring counties have similar residual risk, so an ICAR prior — ",
              "which borrows strength across borders — should improve on the ",
              "exchangeable frailty. This is the empirical case for the thesis's ",
              "spatial Cox model.")
        else
          div(class = "alert alert-info", style = "font-size:0.83rem; margin-bottom:0;",
              strong("No spatial autocorrelation detected at county level. "),
              "The residual county effects are scattered rather than clustered, so an ",
              "ICAR prior would add little over the exchangeable frailty already fitted. ",
              "Three readings worth separating before concluding anything: the covariates ",
              "may already have absorbed the spatial signal (wealth and education are ",
              "themselves geographically clustered in Kenya); 47 counties may be too ",
              "coarse a grid, with structure visible only at DHS cluster level; or there ",
              "may genuinely be none. The first two are testable — the third is a finding.")
      )
    })

    lisa_df <- reactive({
      l <- u5cm_lisa(spatial())
      validate(need(!is.null(l), "Local Moran's I could not be computed."))
      l
    })

    output$lisa_tbl <- renderTable({
      l <- lisa_df()
      tab <- as.data.frame(table(l$cluster))
      names(tab) <- c("Class", "Counties")
      tab[tab$Counties > 0, ]
    }, striped = TRUE, spacing = "xs", width = "100%")

    output$frailty_map <- renderLeaflet({
      # Draw the unshaded basemap when the frailty cannot be estimated, rather
      # than letting validate() short-circuit. leaflet KEEPS its previous widget
      # when a render stops early, so a single-county selection otherwise left 47
      # shaded counties on screen captioned by a filter the shading did not
      # describe — the same stale-display problem as the eventReactive bug, in the
      # one output that fails silently instead of blanking.
      n_cty <- dplyr::n_distinct(des()$variables$county, na.rm = TRUE)
      if (n_cty < 5) {
        req(raw_data$counties)
        msg <- paste0(
          '<div style="background:#fff;padding:6px 9px;font-size:0.8rem;',
          'border-left:3px solid #b7791f;">No frailty estimated — this ',
          'selection has ', n_cty,
          ifelse(n_cty == 1, ' county', ' counties'),
          '. Widen or clear the County filter.</div>')
        return(
          leaflet(raw_data$counties) |>
            addTiles(attribution = "&copy; OpenStreetMap contributors") |>
            setView(lng = 37.9, lat = 0.02, zoom = 6) |>
            addPolygons(fillColor = "#e2e8f0", fillOpacity = 0.6,
                        color = "white", weight = 1, label = ~NAME_1) |>
            addControl(msg, position = "topright")
        )
      }

      s <- spatial()
      map_sf <- s$sf
      base <- leaflet(map_sf) |>
        addTiles(attribution = "&copy; OpenStreetMap contributors") |>
        setView(lng = 37.9, lat = 0.02, zoom = 6)

      if (identical(input$map_mode, "lisa")) {
        l <- lisa_df()
        map_sf$cluster <- l$cluster[match(map_sf$NAME_1, l$county)]
        map_sf$pval    <- l$p[match(map_sf$NAME_1, l$county)]
        pal <- colorFactor(LISA_COLOURS, domain = names(LISA_COLOURS),
                           na.color = "#c8c8c8")
        leaflet(map_sf) |>
          addTiles(attribution = "&copy; OpenStreetMap contributors") |>
          setView(lng = 37.9, lat = 0.02, zoom = 6) |>
          addPolygons(
            fillColor = ~pal(as.character(cluster)), fillOpacity = 0.85,
            color = "white", weight = 1,
            label = ~paste0(NAME_1, ": ", as.character(cluster),
                            ifelse(is.na(pval), "",
                                   sprintf(" (p = %.3f)", pval))),
            highlightOptions = highlightOptions(color = "black", weight = 2,
                                                bringToFront = TRUE)
          ) |>
          addLegend("bottomright", pal = pal, values = names(LISA_COLOURS),
                    title = "LISA class", opacity = 1)
      } else {
        pal <- colorNumeric("RdYlGn", domain = map_sf$hr, na.color = "#c8c8c8",
                            reverse = TRUE)
        base |>
          addPolygons(
            fillColor = ~pal(hr), fillOpacity = 0.85, color = "white", weight = 1,
            label = ~ifelse(is.na(hr), paste0(NAME_1, ": no estimate"),
                            paste0(NAME_1, ": frailty HR ", sprintf("%.3f", hr))),
            highlightOptions = highlightOptions(color = "black", weight = 2,
                                                bringToFront = TRUE)
          ) |>
          addLegend("bottomright", pal = pal, values = ~hr,
                    title = "Frailty HR", opacity = 1)
      }
    })

    # ---- 6. LIFE TABLE -----------------------------------------------------
    output$life_tbl <- renderTable({
      grp <- strata_var()
      h <- u5cm_life_table(des(), by = grp)
      validate(need(!is.null(h) && nrow(h) > 0,
                    "Not enough children to build a life table."))
      out <- data.frame(
        `Age segment (months)` = as.character(h$segment),
        `Entering segment`     = fmt_n(h$n_entered),
        Deaths                 = fmt_n(h$n_deaths),
        `q (per 1,000)`        = sprintf("%.2f", h$q_per_1000),
        `Cumulative survival`  = sprintf("%.4f", h$cum_surv),
        `Cumulative deaths per 1,000` = sprintf("%.1f", h$cum_mort_1000),
        check.names = FALSE
      )
      # When grouped, the group has to lead the table or the repeated age
      # segments read as duplicated rows.
      if (!is.null(grp)) out <- cbind(Group = as.character(h$group), out)
      out
    }, striped = TRUE, spacing = "xs", width = "100%")

    output$life_note <- renderUI({
      # Deliberately ungrouped: this note reconciles the cohort figure with the
      # period figure for the whole selection. With a grouping active,
      # tail(..., 1) would silently pick whichever group sorted last.
      h <- u5cm_life_table(des(), by = NULL)
      final <- tail(h$cum_mort_1000, 1)
      period <- dhs_child_mortality(des_br()$variables)
      p(style = "font-size:0.83rem; color:#4a5568; margin-bottom:0;",
        sprintf("Chaining these segments gives %.1f deaths per 1,000 for this cohort. ", final),
        sprintf("The Overview card reports %.1f, ", period$under5),
        "because it uses the DHS period method over the birth history — the whole ",
        "5-year window rather than only children currently under five. Both are correct ",
        "for what they measure; the published figure is 41.",
        if (!is.null(strata_var())) " The table above is split by the grouping variable; this sentence describes the whole selection."
      )
    })
  })
}
