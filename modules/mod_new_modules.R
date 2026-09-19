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
                 column(6, wellPanel(h5("COVID-19 Testing Reach by County"),
                                     plotlyOutput(ns("covid_knowledge"), height="300px") |> withSpinner()))
               )
      )
    )
  )
}

mod_new_server <- function(id, hr_design, ir_design, kr_design, mr_design,
                           pr_design, raw_data) {
  moduleServer(id, function(input, output, session) {

    # ---- ECDI by domain -----------------------------------------------------
    # Real ECDI items (ecd21-ecd40 in the KR recode), scored in preprocess.R.
    # NOT the official UNICEF ECDI2030 index — see the note there. Each score is
    # the share of that domain's items the child can do.
    output$ecdi_domain <- renderPlotly({
      req(kr_design())
      cols <- grep("^ecdi_", names(kr_design()$variables), value = TRUE)
      cols <- setdiff(cols, "ecdi_overall")
      validate(need(length(cols) > 0, "ECDI items not available in kr.rds."))

      ecdi_data <- bind_rows(lapply(cols, function(cl) {
        kr_design() |>
          filter(!is.na(.data[[cl]])) |>
          summarise(pct_on_track = survey_mean(.data[[cl]], na.rm = TRUE)) |>
          mutate(domain = fmt_label(gsub("_", " ", sub("^ecdi_", "", cl))))
      }))
      validate(need(nrow(ecdi_data) > 0, "No ECDI data for this selection."))

      ecdi_data <- order_levels(ecdi_data, "domain", "pct_on_track")


      p <- ggplot(ecdi_data, aes(x = domain, y = pct_on_track,
                                 fill = domain)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = fmt_pct(pct_on_track)), hjust = -0.15, size = 3.2) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 110)) +
        labs(x = "", y = "Mean % of domain items the child can do",
             title = "ECDI Domains (children 24-59 months)") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- ECDI overall by county --------------------------------------------
    output$ecdi_county <- renderPlotly({
      req(kr_design())
      validate(need("ecdi_overall" %in% names(kr_design()$variables),
                    "ECDI items not available in kr.rds."))
      data <- kr_design() |>
        filter(!is.na(ecdi_overall)) |>
        group_by(v024) |>
        summarise(score = survey_mean(ecdi_overall, na.rm = TRUE, vartype = NULL)) |>
        mutate(county = fmt_label(v024)) |>
        filter(!is.na(score)) |>
        arrange(desc(score)) |>
        slice_head(n = 20)
      validate(need(nrow(data) > 0, "No ECDI data for this selection."))

      data <- order_levels(data, "county", "score")


      p <- ggplot(data, aes(x = county, y = score, text = paste0(county, ": ", round(score, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.78) +
        coord_flip() +
        labs(x = "", y = "Mean ECDI score (% of items)",
             title = "20 highest-scoring counties") +
        theme_kdhs() +
        theme(axis.text.y = element_text(size = 8))
      ggplotly(p, tooltip = "text")
    })

    # ---- Insurance by type --------------------------------------------------
    # Real per-person data from the PR recode: sh27 (covered) and sh28a-x (type).
    # The type questions are only asked of people who said they are covered.
    output$ins_type <- renderPlotly({
      req(pr_design())
      d <- pr_design()
      one <- function(var, label) {
        if (!var %in% names(d$variables)) return(NULL)
        d |>
          filter(!is.na(.data[[var]])) |>
          mutate(.x = as.numeric(as.character(.data[[var]]) == "yes")) |>
          summarise(pct = survey_mean(.x, na.rm = TRUE) * 100) |>
          mutate(type = label)
      }
      ins_data <- bind_rows(
        one("sh28a", "NHIF"),
        one("sh28b", "Private / commercial"),
        one("sh28c", "Community-based"),
        one("sh28x", "Other")
      )
      validate(need(nrow(ins_data) > 0, "Insurance type data not available."))

      ins_data <- order_levels(ins_data, "type", "pct")


      p <- ggplot(ins_data, aes(x = type, y = pct, text = paste0(type, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.7) +
        geom_text(aes(label = fmt_pct(pct)), hjust = -0.15, size = 3.2) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1),
                           expand = expansion(mult = c(0, 0.18))) +
        labs(x = "", y = "% of insured people holding this type",
             title = "Among people reporting insurance cover") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- Insurance coverage by county --------------------------------------
    output$ins_county <- renderPlotly({
      req(pr_design())
      validate(need("insured" %in% names(pr_design()$variables),
                    "Insurance variable not available in pr.rds."))
      data <- pr_design() |>
        filter(!is.na(insured)) |>
        group_by(hv024) |>
        summarise(pct = survey_mean(insured, na.rm = TRUE, vartype = NULL) * 100) |>
        mutate(county = fmt_label(hv024)) |>
        filter(!is.na(pct)) |>
        arrange(desc(pct)) |>
        slice_head(n = 20)
      validate(need(nrow(data) > 0, "No insurance data for this selection."))

      data <- order_levels(data, "county", "pct")


      p <- ggplot(data, aes(x = county, y = pct, text = paste0(county, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.78) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of people covered by health insurance",
             title = "20 highest-coverage counties") +
        theme_kdhs() +
        theme(axis.text.y = element_text(size = 8))
      ggplotly(p, tooltip = "text")
    })

    # ---- Chronic disease by age and sex ------------------------------------
    # chronic_any is derived in preprocess.R from the NCD module: chd02/07/11/13/
    # 17/18/20 for women and the matching mchd* for men (hypertension, diabetes,
    # heart disease, lung disease, depression, anxiety, arthritis).
    output$chronic_age <- renderPlotly({
      req(ir_design(), mr_design())
      side <- function(design, agevar, label) {
        if (!"chronic_any" %in% names(design$variables)) return(NULL)
        design |>
          filter(!is.na(chronic_any)) |>
          group_by(.grp = .data[[agevar]]) |>
          summarise(pct = survey_mean(chronic_any, na.rm = TRUE, vartype = NULL) * 100) |>
          mutate(sex = label, age_grp = fmt_label(.grp))
      }
      data <- bind_rows(side(ir_design(), "v013", "Women"),
                        side(mr_design(), "mv013", "Men")) |>
        filter(!is.na(pct))
      validate(need(nrow(data) > 0,
                    "Chronic disease module not available in this extract."))

      p <- ggplot(data, aes(x = age_grp, y = pct, fill = sex, text = paste0(age_grp, " — ", sex, "<br>", round(pct, 1)))) +
        geom_col(position = "dodge", width = 0.75) +
        scale_fill_manual(values = c(Women = KDHS_COLORS$female,
                                     Men   = KDHS_COLORS$male)) +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% ever diagnosed with any chronic condition", fill = "") +
        theme_kdhs() +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))
      ggplotly(p, tooltip = "text")
    })

    # ---- Disability by domain ----------------------------------------------
    # Real Washington Group short-set data from the PR recode (hdis2-hdis8),
    # replacing the placeholder numbers that were here. Severity levels come
    # straight from the response categories.
    output$disability_type <- renderPlotly({
      req(pr_design())
      domains <- c(hdis2 = "Seeing",  hdis4 = "Hearing",
                   hdis5 = "Communication", hdis6 = "Cognition",
                   hdis7 = "Walking", hdis8 = "Self-care")
      vars <- intersect(names(domains), names(pr_design()$variables))
      validate(need(length(vars) > 0,
                    "Washington Group disability items not available in pr.rds."))

      dis_data <- bind_rows(lapply(vars, function(v) {
        pr_design() |>
          filter(!is.na(.data[[v]])) |>
          mutate(.sev = case_when(
            grepl("^some difficulty", as.character(.data[[v]]))     ~ "Some difficulty",
            grepl("^a lot of difficulty", as.character(.data[[v]])) ~ "A lot of difficulty",
            grepl("^cannot", as.character(.data[[v]]))              ~ "Cannot do at all",
            TRUE                                                     ~ NA_character_)) |>
          filter(!is.na(.sev)) |>
          group_by(.sev) |>
          summarise(pct = survey_prop(vartype = NULL) * 100,
                    .groups = "drop") |>
          mutate(domain = unname(domains[v]))
      }))
      validate(need(nrow(dis_data) > 0, "No disability data for this selection."))

      dis_data <- dis_data |>
        mutate(severity = factor(.sev, levels = c("Some difficulty",
                                                  "A lot of difficulty",
                                                  "Cannot do at all")))

      p <- ggplot(dis_data, aes(x = domain, y = pct, fill = severity, text = paste0(domain, " — ", severity, "<br>", round(pct, 1)))) +
        geom_col(position = "stack", width = 0.7) +
        scale_fill_manual(values = c("Some difficulty"     = "#fca5a5",
                                     "A lot of difficulty" = "#ef4444",
                                     "Cannot do at all"    = "#7f1d1d")) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of people asked the module", fill = "Severity",
             title = "Washington Group short set") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- COVID-19 vaccination ----------------------------------------------
    # sh135l counts household members vaccinated against COVID-19; covid_vax_hh
    # flags households with at least one. Asked of the half-sample only.
    output$covid_vax <- renderPlotly({
      req(hr_design())
      validate(need("covid_vax_hh" %in% names(hr_design()$variables),
                    "COVID-19 items not available in hr.rds."))
      data <- hr_design() |>
        filter(!is.na(covid_vax_hh)) |>
        group_by(hv025) |>
        summarise(pct = survey_mean(covid_vax_hh, na.rm = TRUE, vartype = "ci") * 100) |>
        mutate(residence = fmt_label(hv025))
      validate(need(nrow(data) > 0, "No COVID-19 data for this selection."))

      p <- ggplot(data, aes(x = residence, y = pct, text = paste0(residence, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$primary, width = 0.5) +
        geom_errorbar(aes(ymin = pct_low, ymax = pct_upp), width = 0.12) +
        geom_text(aes(label = fmt_pct(pct)), vjust = -0.9, size = 3.2) +
        scale_y_continuous(labels = label_percent(scale = 1),
                           expand = expansion(mult = c(0, 0.25))) +
        labs(x = "", y = "% of households with >=1 member vaccinated") +
        theme_kdhs()
      ggplotly(p, tooltip = "text")
    })

    # ---- COVID-19 testing by county ----------------------------------------
    # NOTE: KDHS 2022 asked NO COVID-19 knowledge question — the only COVID items
    # are the household counts sh135f/h/j/l (tested, positive, died, vaccinated).
    # This panel therefore reports testing reach, and the tab label says so.
    output$covid_knowledge <- renderPlotly({
      req(hr_design())
      validate(need("covid_tested_hh" %in% names(hr_design()$variables),
                    "COVID-19 items not available in hr.rds."))
      data <- hr_design() |>
        filter(!is.na(covid_tested_hh)) |>
        group_by(hv024) |>
        summarise(pct = survey_mean(covid_tested_hh, na.rm = TRUE, vartype = NULL) * 100) |>
        mutate(county = fmt_label(hv024)) |>
        filter(!is.na(pct)) |>
        arrange(desc(pct)) |>
        slice_head(n = 20)
      validate(need(nrow(data) > 0, "No COVID-19 data for this selection."))

      data <- order_levels(data, "county", "pct")


      p <- ggplot(data, aes(x = county, y = pct, text = paste0(county, ": ", round(pct, 1)))) +
        geom_col(fill = KDHS_COLORS$neutral, width = 0.78) +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(x = "", y = "% of households with >=1 member ever tested",
             title = "20 counties with the widest testing reach") +
        theme_kdhs() +
        theme(axis.text.y = element_text(size = 8))
      ggplotly(p, tooltip = "text")
    })
  })
}
