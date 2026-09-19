# =============================================================================
# R/server.R
# Main server — loads data once, sets up survey designs, passes to modules.
# =============================================================================

server <- function(input, output, session) {

  # ---- LOAD DATA (once, on startup) -----------------------------------------
  # Using reactive() with caching so data loads once per session.
  # In production: move data loading outside server() for multi-user efficiency.

  raw_data <- reactiveValues(
    ir       = NULL,
    mr       = NULL,
    hr       = NULL,
    kr       = NULL,
    br       = NULL,
    cr       = NULL,
    ge       = NULL,
    counties = NULL,
    loaded   = FALSE
  )

  # Load on session start (with progress notification)
  observe({
    withProgress(message = "Loading KDHS 2022 data...", value = 0, {

      tryCatch({
        incProgress(0.1, detail = "Loading Women's Recode (IR)...")
        raw_data$ir <- load_recode("ir")

        incProgress(0.2, detail = "Loading Household Recode (HR)...")
        raw_data$hr <- load_recode("hr")

        incProgress(0.3, detail = "Loading Children's Recode (KR)...")
        raw_data$kr <- load_recode("kr")

        incProgress(0.4, detail = "Loading Men's Recode (MR)...")
        raw_data$mr <- load_recode("mr")

        incProgress(0.5, detail = "Loading Birth Recode (BR)...")
        raw_data$br <- load_recode("br")

        incProgress(0.6, detail = "Loading Household Members (PR)...")
        raw_data$pr <- load_recode("pr")

        incProgress(0.7, detail = "Loading GPS data...")
        raw_data$ge <- load_gps()

        incProgress(0.9, detail = "Loading county boundaries...")
        raw_data$counties <- load_county_shapefile()

        raw_data$loaded <- TRUE
        incProgress(1, detail = "Done!")
        showNotification("KDHS 2022 data loaded successfully.", type = "message")

      }, error = function(e) {
        showNotification(
          paste("Data loading error:", e$message),
          type = "error", duration = NULL
        )
      })
    })
  })

  # ---- SURVEY DESIGNS -------------------------------------------------------
  # Reactive — only created after data is loaded
  ir_design <- reactive({
    req(raw_data$ir)
    create_ir_design(raw_data$ir)
  })

  mr_design <- reactive({
    req(raw_data$mr)
    create_mr_design(raw_data$mr)
  })

  kr_design <- reactive({
    req(raw_data$kr)
    create_kr_design(raw_data$kr)
  })

  hr_design <- reactive({
    req(raw_data$hr)
    create_hr_design(raw_data$hr)
  })

  br_design <- reactive({
    req(raw_data$br)
    create_br_design(raw_data$br)
  })

  pr_design <- reactive({
    req(raw_data$pr)
    create_pr_design(raw_data$pr)
  })

  # ---- GLOBAL KPIs (for About tab) ------------------------------------------
  output$global_kpis <- renderUI({
    req(ir_design())
    tagList(
      stat_box(
        value   = "32,156",
        label   = "Women surveyed (15–49)",
        subtext = "Weighted to represent all Kenyan women"
      ),
      stat_box(
        value   = "14,453",
        label   = "Men surveyed (15–54)",
        subtext = "Half-sample (1 in 2 households)"
      ),
      stat_box(
        value   = "47",
        label   = "Counties covered",
        subtext = "1,692 sampling clusters",
        color   = "red"
      ),
      stat_box(
        value   = "2022",
        label   = "7th Kenya DHS",
        subtext = "Previous: 2014 KDHS",
        color   = "red"
      )
    )
  })

  # ---- CALL MODULE SERVERS --------------------------------------------------
  # kr/br/pr designs added so the child-mortality, stunting and health-insurance
  # cards can be computed from their proper source recodes instead of being
  # hard-coded national strings.
  mod_overview_server("overview", ir_design, hr_design,
                      kr_design, br_design, pr_design, raw_data)
  mod_survival_server("survival", kr_design, br_design, raw_data)
  # br_design added: the child-mortality panels need the birth history.
  mod_maternal_server("maternal", ir_design, kr_design, br_design, raw_data)
  mod_gender_server("gender", ir_design, raw_data)
  mod_hiv_server("hiv", ir_design, mr_design, raw_data)
  # kr_design (ECDI), mr_design (chronic disease in men) and pr_design
  # (disability, insurance type) added for the KDHS 2022 modules.
  mod_new_server("new_modules", hr_design, ir_design, kr_design, mr_design,
                 pr_design, raw_data)
  mod_explorer_server("explorer", ir_design, mr_design, kr_design, hr_design, raw_data)
}
