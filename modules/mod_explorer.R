# =============================================================================
# modules/mod_explorer.R
# Dashboard 6: Research Explorer — for PhD students and researchers.
# Custom cross-tabulation, variable explorer, regression output, data download.
# =============================================================================

# ---- Variable dictionary for Explorer dropdowns ----------------------------
IR_VARS <- list(
  # Demographics
  "Age group (5yr)"              = "v013",
  "Region"                       = "v024",
  "Residence (Urban/Rural)"      = "v025",
  "Education level"              = "v106",
  "Wealth quintile"              = "v190",
  "Marital status"               = "v502",
  "Religion"                     = "v130",
  "Ethnicity"                    = "v131",
  # Fertility & FP
  "Children ever born"           = "v201",
  "Contraceptive method (any)"   = "v313",
  "Modern method used"           = "v312",
  "Unmet need for FP"            = "v624",
  "Ideal number of children"     = "v613",
  # Maternal health
  "ANC visits (last birth)"      = "m14",
  "Skilled birth attendance"     = "m3a",
  "Place of delivery"            = "m15",
  "C-section delivery"           = "m17",
  "Postnatal check (mother)"     = "m62",
  # Insurance & health
  "Has health insurance"         = "v481",
  "Insurance type"               = "sh44",
  "Out-of-pocket expenditure"    = "sh509",
  # HIV
  "Heard of AIDS"                = "v751",
  "HIV test (last 12 mo)"        = "v781",
  "Comprehensive HIV knowledge"  = "v774b",
  # Gender
  "Decision on own healthcare"   = "v743a",
  "Experienced physical violence"= "d105a",
  "FGM/C status"                 = "g102"
)

mod_explorer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      
      # ---- SUB-TAB 1: Cross-Tabulation Builder ------------------------------
      tabPanel("Cross-Tabulation", br(),
               fluidRow(
                 column(4, wellPanel(h6("Step 1: Dataset"),
                                     selectInput(ns("dataset"), NULL, choices = c("Women (IR)"="ir","Men (MR)"="mr","Children (KR)"="kr","Households (HR)"="hr"))
                 )),
                 column(4, wellPanel(h6("Step 2: Row Variable (Outcome)"),
                                     selectInput(ns("row_var"), NULL, choices = IR_VARS)
                 )),
                 column(4, wellPanel(h6("Step 3: Column Variable (Breakdown)"),
                                     selectInput(ns("col_var"), NULL, choices = c("None"="none", IR_VARS))
                 ))
               ),
               wellPanel(
                 div(style="display:flex; justify-content:space-between; align-items:center;",
                     h5("Weighted Cross-Tabulation Results"),
                     downloadButton(ns("download_crosstab"), "Download CSV", class="btn-sm btn-outline-primary")
                 ),
                 br(),
                 DT::DTOutput(ns("crosstab_table")) |> withSpinner()
               ),
               wellPanel(
                 h5("Chart"),
                 plotlyOutput(ns("crosstab_chart"), height="340px") |> withSpinner()
               )
      ),
      
      # ---- SUB-TAB 2: Variable Explorer -------------------------------------
      tabPanel("Variable Explorer", br(),
               fluidRow(
                 column(4,
                        wellPanel(
                          selectInput(ns("explore_var"), "Choose a variable", choices = IR_VARS),
                          hr(),
                          h6("Variable Info"),
                          uiOutput(ns("var_info"))
                        )
                 ),
                 column(8,
                        wellPanel(
                          h5("Distribution"),
                          plotlyOutput(ns("var_dist"), height="300px") |> withSpinner(),
                          hr(),
                          h6("Summary Statistics"),
                          tableOutput(ns("var_summary"))
                        )
                 )
               )
      ),
      
      # ---- SUB-TAB 3: Logistic Regression -----------------------------------
      tabPanel("Regression", br(),
               fluidRow(
                 column(4,
                        wellPanel(
                          h5("Model Setup"),
                          selectInput(ns("reg_outcome"), "Outcome (binary)",
                                      choices = list(
                                        `Child mortality (children's file)` = list(
                                          "Died before 1 month (neonatal)"  = "died_neonatal",
                                          "Died before 12 months (infant)"  = "died_infant",
                                          "Died before 24 months"           = "died_24m"
                                        ),
                                        `Women 15-49 (women's file)` = list(
                                          "Contraceptive use (modern)" = "v312_modern",
                                          "ANC 4+ visits"              = "m14_4plus",
                                          "Skilled birth attendance"   = "m3a_skilled",
                                          "HIV tested (12 mo)"         = "v781_yes"
                                        )
                                      )),
                          checkboxGroupInput(ns("reg_predictors"), "Predictors",
                                             choices = list(
                                               "Age group"="v013","Residence"="v025","Education"="v106",
                                               "Wealth"="v190","Region"="v024","Marital status"="v502","Parity"="v218"
                                             ),
                                             selected = c("v025","v106","v190")),
                          actionButton(ns("run_regression"), "Run Regression", class="btn-success btn-sm", icon=icon("play"))
                        )
                 ),
                 column(8,
                        wellPanel(
                          h5("Model Output: Adjusted Odds Ratios"),
                          p(style="font-size:0.8rem; color:#718096;",
                            "Survey-weighted logistic regression using svyglm(). All estimates use the DHS complex survey design."),
                          uiOutput(ns("reg_cohort")),
                          plotlyOutput(ns("forest_plot"), height="360px") |> withSpinner(),
                          hr(),
                          tableOutput(ns("reg_table"))
                        )
                 )
               )
      ),
      
      # ---- SUB-TAB 4: Merge Viewer ------------------------------------------
      tabPanel("Merge Viewer", br(),
               wellPanel(
                 h5("Merge IR (Women) + HR (Household)"),
                 fluidRow(
                   column(6,
                          h6("From IR (Women's Recode)"),
                          checkboxGroupInput(ns("ir_merge_vars"), NULL,
                                             choices = c("v025 (Residence)","v190 (Wealth)","v106 (Education)",
                                                         "v313 (FP Method)","m14 (ANC Visits)","v481 (Insurance)"),
                                             selected = c("v025 (Residence)","v190 (Wealth)"))
                   ),
                   column(6,
                          h6("From HR (Household Recode)"),
                          checkboxGroupInput(ns("hr_merge_vars"), NULL,
                                             choices = c("hv201 (Water source)","hv205 (Sanitation)",
                                                         "hv206 (Electricity)","hv009 (HH members)",
                                                         "sh44 (Insurance type)","sh109 (Chronic disease)"),
                                             selected = c("hv201 (Water source)","hv206 (Electricity)"))
                   )
                 ),
                 actionButton(ns("do_merge"), "Preview Merged Data", class="btn-primary btn-sm"),
                 hr(),
                 p(style="font-size:0.8rem; color:#718096;",
                   strong("Merge key: "), "v001 (cluster) + v002 (household number)"),
                 DT::DTOutput(ns("merge_preview")) |> withSpinner()
               )
      ),
      
      # ---- SUB-TAB 5: Download ----------------------------------------------
      tabPanel("Download", br(),
               wellPanel(
                 h5("Download Filtered Dataset"),
                 fluidRow(
                   column(4, selectInput(ns("dl_dataset"),   "Dataset",        choices=c("Women (IR)"="ir","Men (MR)"="mr","Children (KR)"="kr"))),
                   column(4, selectInput(ns("dl_residence"), "Residence",      choices=c("All","Urban","Rural"))),
                   column(4, selectInput(ns("dl_wealth"),    "Wealth Quintile",choices=c("All","Poorest","Poorer","Middle","Richer","Richest")))
                 ),
                 checkboxGroupInput(ns("dl_vars"), "Select Variables to Include",
                                    choices=IR_VARS, selected=c("v013","v024","v025","v190","v313"), inline=TRUE),
                 br(),
                 downloadButton(ns("download_data"), "Download as CSV", class="btn-success"),
                 p(style="font-size:0.78rem; color:#718096; margin-top:0.5rem;",
                   "Downloaded data includes survey weights (v005, v021, v023) for use in external tools.")
               )
      )
    )
  )
}

mod_explorer_server <- function(id, ir_design, mr_design, kr_design, hr_design, raw_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Select active design based on dataset choice -----------------------
    active_design <- reactive({
      switch(input$dataset,
             "ir" = ir_design(),
             "mr" = mr_design(),
             "kr" = kr_design(),
             "hr" = hr_design())
    })
    
    # The row/column dropdowns were hard-coded to IR_VARS, but the other recodes
    # use different column names (mv013 in MR, hv024 in HR...). Picking "Age group
    # (5yr)" = v013 with the Men or Household dataset selected crashed the panel:
    #   Can't subset elements that don't exist. Element `v013` doesn't exist.
    # Rebuild the choices from whatever the selected design actually contains,
    # keeping the friendly IR labels where they apply.
    crosstab_choices <- reactive({
      design <- active_design()
      req(design)
      vars <- names(design$variables)
      # Categorical columns only — a crosstab on a continuous variable is useless
      keep <- vars[vapply(design$variables, function(x) {
        (is.factor(x) || is.character(x)) && dplyr::n_distinct(x, na.rm = TRUE) %in% 2:25
      }, logical(1))]
      keep <- sort(keep)
      pretty <- vapply(keep, function(v) {
        hit <- names(IR_VARS)[unlist(IR_VARS) == v]
        if (length(hit)) sprintf("%s (%s)", hit[1], v) else v
      }, character(1))
      stats::setNames(keep, pretty)
    })

    observeEvent(crosstab_choices(), {
      ch <- crosstab_choices()
      keep_sel <- function(cur) if (!is.null(cur) && cur %in% ch) cur else unname(ch[1])
      updateSelectInput(session, "row_var", choices = ch,
                        selected = keep_sel(input$row_var))
      updateSelectInput(session, "col_var", choices = c("None" = "none", ch),
                        selected = if (!is.null(input$col_var) &&
                                       (input$col_var == "none" || input$col_var %in% ch))
                                     input$col_var else "none")
    })

    # ---- Cross-tabulation ---------------------------------------------------
    crosstab_data <- reactive({
      req(active_design(), input$row_var)
      design <- active_design()

      # Belt and braces: the dropdown update above is asynchronous, so a stale
      # selection can arrive for one cycle right after the dataset changes.
      vars <- names(design$variables)
      req(input$row_var %in% vars)
      if (!identical(input$col_var, "none")) req(input$col_var %in% vars)

      if (input$col_var == "none") {
        design |>
          group_by(across(all_of(input$row_var))) |>
          summarise(
            n      = survey_total(vartype = NULL),
            pct    = survey_prop(vartype = "ci") * 100
          )
      } else {
        design |>
          group_by(across(all_of(c(input$col_var, input$row_var)))) |>
          summarise(
            n   = survey_total(vartype = NULL),
            pct = survey_prop(vartype = "ci") * 100
          )
      }
    })
    
    output$crosstab_table <- DT::renderDT({
      req(crosstab_data())
      crosstab_data() |>
        mutate(across(where(is.numeric), ~round(., 1))) |>
        DT::datatable(rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })
    
    output$crosstab_chart <- renderPlotly({
      req(crosstab_data())
      data <- crosstab_data()
      row_var <- input$row_var
      col_var <- input$col_var
      
      if (col_var == "none") {
        p <- ggplot(data, aes(x = !!sym(row_var), y = pct)) +
          geom_col(fill = KDHS_COLORS$primary, width = 0.7) +
          coord_flip() +
          scale_y_continuous(labels = label_percent(scale = 1)) +
          labs(x = "", y = "Percentage (%)") +
          theme_kdhs()
      } else {
        p <- ggplot(data, aes(x = !!sym(row_var), y = pct, fill = !!sym(col_var))) +
          geom_col(position = "dodge", width = 0.7) +
          coord_flip() +
          scale_y_continuous(labels = label_percent(scale = 1)) +
          labs(x = "", y = "Percentage (%)", fill = "") +
          theme_kdhs()
      }
      ggplotly(p)
    })
    
    output$download_crosstab <- downloadHandler(
      filename = function() paste0("kdhs2022_crosstab_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(crosstab_data(), file, row.names = FALSE)
    )
    
    # ---- Variable Explorer --------------------------------------------------
    output$var_info <- renderUI({
      var <- input$explore_var
      label <- names(IR_VARS)[IR_VARS == var]
      tagList(
        p(strong("Variable code: "), code(var)),
        p(strong("Label: "), label),
        p(strong("Dataset: "), "IR (Women's Recode)"),
        p(strong("Type: "), "Categorical (factor)")
      )
    })
    
    output$var_dist <- renderPlotly({
      req(ir_design(), input$explore_var)
      var <- input$explore_var
      data <- ir_design() |>
        group_by(across(all_of(var))) |>
        summarise(pct = survey_prop(vartype = NULL) * 100)
      
      # KDHS_COLORS has no "accent4" — that returned NULL and dropped the fill.
      p <- ggplot(data, aes(x = !!sym(var), y = pct)) +
        geom_col(fill = KDHS_COLORS$accent, width = 0.7) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "Weighted %") +
        theme_kdhs()
      ggplotly(p)
    })

    # ---- Variable summary table --------------------------------------------
    output$var_summary <- renderTable({
      req(ir_design(), input$explore_var)
      var <- input$explore_var
      raw <- ir_design()$variables[[var]]

      tab <- ir_design() |>
        group_by(across(all_of(var))) |>
        summarise(pct = survey_prop(vartype = "ci") * 100,
                  n   = unweighted(n()),
                  .groups = "drop")

      data.frame(
        Category      = as.character(tab[[var]]),
        `Unweighted n` = fmt_n(tab$n),
        `Weighted %`   = fmt_pct(tab$pct),
        `95% CI`       = paste0(round(tab$pct_low, 1), " - ", round(tab$pct_upp, 1)),
        check.names   = FALSE
      ) |>
        rbind(data.frame(
          Category       = "TOTAL (non-missing)",
          `Unweighted n` = fmt_n(sum(!is.na(raw))),
          `Weighted %`   = "100%",
          `95% CI`       = paste0("missing: ", fmt_n(sum(is.na(raw)))),
          check.names    = FALSE
        ))
    }, striped = TRUE, spacing = "xs", width = "100%")
    
    # ---- Logistic Regression (survey-weighted) ------------------------------
    # Which recode each outcome lives on. Child-death outcomes come from the
    # children's file; the rest are woman-level and stay on the women's file.
    CHILD_OUTCOMES <- c("died_neonatal", "died_infant", "died_24m")

    # Predictor menus differ by unit of analysis — v502 or v218 mean nothing on a
    # child record, and birth order means nothing on a woman record.
    WOMAN_PREDICTORS <- list(
      "Age group" = "v013", "Residence" = "v025", "Education" = "v106",
      "Wealth" = "v190", "County" = "v024", "Marital status" = "v502",
      "Parity" = "v218"
    )
    CHILD_PREDICTORS <- list(
      "Child sex" = "sex", "Twin" = "twin", "Birth order" = "birth_order_grp",
      "Preceding birth interval" = "short_birth_interval",
      "Size at birth" = "size_at_birth",
      "Mother's age group" = "maternal_age_grp",
      "Mother's education" = "maternal_education",
      "ANC 4+ visits" = "anc_4plus", "Facility birth" = "facility_birth",
      "Wealth" = "v190", "Residence" = "v025",
      "Improved water" = "improved_water", "Clean cooking fuel" = "clean_fuel"
    )

    # Swap the predictor list when the unit of analysis changes.
    observeEvent(input$reg_outcome, {
      is_child <- input$reg_outcome %in% CHILD_OUTCOMES
      ch  <- if (is_child) CHILD_PREDICTORS else WOMAN_PREDICTORS
      def <- if (is_child) c("sex", "short_birth_interval", "maternal_education", "v190")
             else          c("v025", "v106", "v190")
      updateCheckboxGroupInput(session, "reg_predictors",
                               choices = ch, selected = def)
    })

    reg_model <- eventReactive(input$run_regression, {
      is_child <- input$reg_outcome %in% CHILD_OUTCOMES

      if (is_child) {
        req(kr_design())
        # died_* / elig_* are derived in preprocess.R. Each binary outcome is NA
        # for children who have not lived through its horizon, so filtering on
        # !is.na() automatically applies the correct exposure denominator —
        # without it the "alive" group is padded with children who simply have
        # not had time to die, biasing the odds ratios toward the null.
        oc <- input$reg_outcome
        design <- kr_design() |>
          mutate(.y = as.numeric(.data[[oc]])) |>
          filter(!is.na(.y))
      } else {
        req(ir_design())
        design <- ir_design()
        # Every one of these arrives as a labelled factor, so compare to labels.
        # The old code used as.numeric() on factors (level indices), referenced the
        # non-existent bare `m14` (the IR file is wide: m14_1), had no branch at all
        # for "m3a_skilled", and used v481 — which KDHS 2022 never collected.
        design <- switch(input$reg_outcome,
          "v312_modern" = design |>
            mutate(.y = as.numeric(as.character(v313) == "modern method")),
          "m14_4plus"   = design |>
            mutate(.y = as.numeric(
              suppressWarnings(as.numeric(as.character(m14_1))) >= 4)),
          "m3a_skilled" = design |>
            mutate(.y = as.numeric(as.character(m3a_1) == "yes" |
                                   as.character(m3b_1) == "yes")),
          "v781_yes"    = design |>
            mutate(.y = as.numeric(as.character(v781) == "yes")),
          design |> mutate(.y = NA_real_)
        )
      }
      validate(need(sum(!is.na(design$variables$.y)) > 0,
                    "This outcome has no data in KDHS 2022 — pick another."))
      validate(need(sum(design$variables$.y == 1, na.rm = TRUE) >= 20,
                    "Fewer than 20 events in this selection — the model would be unstable."))

      predictors <- intersect(input$reg_predictors, names(design$variables))
      validate(need(length(predictors) > 0, "Select at least one predictor."))
      formula_str <- paste(".y ~", paste(predictors, collapse = " + "))

      # Note: svydesign object needs to be extracted from srvyr
      svy_design <- survey::svydesign(
        ids     = ~v021,
        strata  = ~v023,
        weights = ~wt,
        data    = design$variables,
        nest    = TRUE
      )

      survey::svyglm(as.formula(formula_str), design = svy_design, family = quasibinomial())
    })

    # What the model is actually fitted on — shown above the results so the
    # denominator is never a mystery.
    output$reg_cohort <- renderUI({
      req(reg_model())
      d <- reg_model()$survey.design$variables
      n <- nrow(d); ev <- sum(d$.y == 1, na.rm = TRUE)
      is_child <- input$reg_outcome %in% CHILD_OUTCOMES
      expl <- switch(input$reg_outcome,
        died_neonatal = "Children who lived at least 1 month (so the first month was fully observed).",
        died_infant   = "Children aged 12 months or older (so the whole first year was observed).",
        died_24m      = "Children aged 24 months or older (so the first two years were observed).",
        "Women age 15-49 with non-missing outcome.")
      tagList(
        p(style = "font-size:0.82rem; margin-bottom:4px;",
          strong("Cohort: "), fmt_n(n), " records, ", fmt_n(ev), " events (",
          sprintf("%.2f%%", 100 * ev / n), "). ", expl),
        if (is_child) div(class = "alert alert-info",
          style = "font-size:0.8rem; padding:8px; margin-bottom:6px;",
          strong("There is no 60-month option, and that is not an oversight. "),
          "This file only contains children born in the last 5 years, so not one of ",
          "them has completed 60 months of exposure — a binary \"died before age 5\" ",
          "outcome has an empty denominator. Use the ", strong("Child Survival"),
          " tab, which uses every child's actual follow-up instead of discarding them.")
        else NULL
      )
    })
    
    output$forest_plot <- renderPlotly({
      req(reg_model())
      model <- reg_model()
      coefs <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term != "(Intercept)")
      
      coefs <- order_levels(coefs, "term", "estimate")

      
      p <- ggplot(coefs, aes(x = term, y = estimate,
                             ymin = conf.low, ymax = conf.high)) +
        geom_pointrange(color = KDHS_COLORS$primary, size = 0.8) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
        coord_flip() +
        scale_y_log10() +
        labs(x = "", y = "Adjusted Odds Ratio (log scale)",
             title = "Forest Plot: Adjusted Odds Ratios") +
        theme_kdhs()
      ggplotly(p)
    })

    # ---- Regression coefficient table --------------------------------------
    output$reg_table <- renderTable({
      req(reg_model())
      model <- reg_model()
      coefs <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term != "(Intercept)")

      data.frame(
        Term       = coefs$term,
        `Adj. OR`  = sprintf("%.2f", coefs$estimate),
        `95% CI`   = sprintf("%.2f - %.2f", coefs$conf.low, coefs$conf.high),
        `p-value`  = ifelse(coefs$p.value < 0.001, "<0.001",
                            sprintf("%.3f", coefs$p.value)),
        Signif     = cut(coefs$p.value,
                         breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                         labels = c("***", "**", "*", "")),
        check.names = FALSE
      )
    }, striped = TRUE, spacing = "xs", width = "100%")

    # ---- Merge viewer -------------------------------------------------------
    # IR is one row per woman, HR one row per household. The DHS merge key is
    # cluster + household number: v001/v002 on the women's side, hv001/hv002 on
    # the household side. Many women can live in one household, so this is a
    # many-to-one left join onto IR.
    merged_data <- eventReactive(input$do_merge, {
      req(ir_design(), hr_design())

      # The checkbox labels look like "v025 (Residence)" — take the code only.
      codes <- function(x) sub(" .*$", "", x)
      ir_vars <- codes(input$ir_merge_vars)
      hr_vars <- codes(input$hr_merge_vars)

      ir_df <- ir_design()$variables
      hr_df <- hr_design()$variables

      ir_vars <- intersect(ir_vars, names(ir_df))
      hr_vars <- intersect(hr_vars, names(hr_df))
      validate(need(all(c("v001","v002") %in% names(ir_df)) &&
                    all(c("hv001","hv002") %in% names(hr_df)),
                    "Merge keys (v001/v002, hv001/hv002) are not in the data."))

      left <- ir_df |>
        select(any_of(c("v001", "v002", ir_vars))) |>
        mutate(across(c(v001, v002), ~ suppressWarnings(as.numeric(as.character(.)))))

      right <- hr_df |>
        select(any_of(c("hv001", "hv002", hr_vars))) |>
        mutate(across(c(hv001, hv002), ~ suppressWarnings(as.numeric(as.character(.))))) |>
        rename(v001 = hv001, v002 = hv002) |>
        distinct(v001, v002, .keep_all = TRUE)

      out <- left |> left_join(right, by = c("v001", "v002"))
      attr(out, "matched") <- sum(!is.na(out[[ncol(out)]]))
      out
    })

    output$merge_preview <- DT::renderDT({
      df <- merged_data()
      matched <- attr(df, "matched")
      DT::datatable(
        head(df, 500),
        rownames = FALSE,
        caption = sprintf(
          "%s women joined to household records; %s rows matched. Showing first 500.",
          fmt_n(nrow(df)), fmt_n(matched %||% 0)),
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
      )
    })
    
    # ---- Data Download ------------------------------------------------------
    output$download_data <- downloadHandler(
      filename = function() paste0("kdhs2022_", input$dl_dataset, "_", Sys.Date(), ".csv"),
      content = function(file) {
        design <- switch(input$dl_dataset,
                         "ir" = ir_design(), "mr" = mr_design(), "kr" = kr_design())
        req(design)
        vars_to_keep <- c("v001", "v002", "v003", "v021", "v023", "v005",  # always include keys + weights
                          input$dl_vars)
        
        data_out <- design$variables |>
          select(any_of(vars_to_keep))
        
        if (input$dl_residence != "All")
          data_out <- data_out |> filter(as.character(v025) == tolower(input$dl_residence))
        # Same Title-Case vs lower-case mismatch as the chart filters — this one
        # silently exported an empty CSV rather than erroring.
        if (input$dl_wealth != "All")
          data_out <- data_out |>
            filter(tolower(as.character(v190)) == tolower(input$dl_wealth))
        
        write.csv(data_out, file, row.names = FALSE)
      }
    )
  })
}
