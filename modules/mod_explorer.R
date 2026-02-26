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
                                        "Contraceptive use (modern)" = "v312_modern",
                                        "ANC 4+ visits"              = "m14_4plus",
                                        "Skilled birth attendance"   = "m3a_skilled",
                                        "Health insurance"           = "v481_yes",
                                        "HIV tested (12 mo)"         = "v781_yes"
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
    
    # ---- Cross-tabulation ---------------------------------------------------
    crosstab_data <- reactive({
      req(active_design(), input$row_var)
      design <- active_design()
      
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
      
      p <- ggplot(data, aes(x = !!sym(var), y = pct)) +
        geom_col(fill = KDHS_COLORS$accent4, width = 0.7) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "Weighted %") +
        theme_kdhs()
      ggplotly(p)
    })
    
    # ---- Logistic Regression (survey-weighted) ------------------------------
    reg_model <- eventReactive(input$run_regression, {
      req(ir_design())
      
      # Build outcome flag based on selection
      design <- ir_design()
      outcome_col <- switch(input$reg_outcome,
                            "v312_modern" = { design <- design |> mutate(.y = as.numeric(as.numeric(v312) > 0)); ".y" },
                            "m14_4plus"   = { design <- design |> mutate(.y = as.numeric(as.numeric(m14) >= 4)); ".y" },
                            "v481_yes"    = { design <- design |> mutate(.y = as.numeric(v481 == "yes")); ".y" },
                            "v781_yes"    = { design <- design |> mutate(.y = as.numeric(v781 == "yes")); ".y" }
      )
      
      predictors <- input$reg_predictors
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
    
    output$forest_plot <- renderPlotly({
      req(reg_model())
      model <- reg_model()
      coefs <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term != "(Intercept)")
      
      p <- ggplot(coefs, aes(x = reorder(term, estimate), y = estimate,
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
        if (input$dl_wealth != "All")
          data_out <- data_out |> filter(as.character(v190) == input$dl_wealth)
        
        write.csv(data_out, file, row.names = FALSE)
      }
    )
  })
}
