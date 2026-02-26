# =============================================================================
# R/helpers.R
# Reusable functions for computing, formatting, and plotting KDHS indicators.
# Used by all modules — keeps code DRY across the dashboard.
# =============================================================================

library(srvyr)
library(ggplot2)
library(scales)
library(plotly)

# ---- COLORS -----------------------------------------------------------------
KDHS_COLORS <- list(
  primary   = "#1a6b4a",   # Kenya green
  secondary = "#b5121b",   # Kenya red
  accent    = "#e8c547",   # Gold
  neutral   = "#4a5568",
  urban     = "#3b82f6",
  rural     = "#10b981",
  female    = "#ec4899",
  male      = "#6366f1",
  q1 = "#fee2e2", q2 = "#fca5a5", q3 = "#f87171", q4 = "#ef4444", q5 = "#b91c1c"
)

REGION_LABELS <- c(
  "1" = "Nairobi", "2" = "Central", "3" = "Coast",
  "4" = "Eastern", "5" = "Nyanza", "6" = "Rift Valley",
  "7" = "Western", "8" = "North Eastern"
)

WEALTH_LABELS <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")

# ---- COMPUTE PROPORTION -----------------------------------------------------
#' Compute a weighted proportion from a survey design.
#' Returns a data frame with estimate, CI, and (optionally) group breakdown.
#'
#' @param design srvyr design object
#' @param var Variable to compute proportion for (character)
#' @param value Target value/level to measure
#' @param by Optional grouping variable (character)
#' @return tibble with columns: [by], pct, pct_low, pct_upp, n
compute_proportion <- function(design, var, value = NULL, by = NULL) {
  if (!is.null(by)) {
    design <- design |> group_by(across(all_of(by)))
  }

  if (!is.null(value)) {
    design <- design |> mutate(.flag = as.numeric(!!sym(var) == value))
    result <- design |>
      summarise(
        pct     = survey_mean(.flag, na.rm = TRUE, vartype = "ci") * 100,
        n       = survey_total(.flag, na.rm = TRUE, vartype = NULL)
      )
  } else {
    # Return all levels
    result <- design |>
      group_by(across(all_of(c(by, var)))) |>
      summarise(pct = survey_prop(na.rm = TRUE, vartype = "ci") * 100)
  }
  result
}

# ---- STANDARD BAR CHART -----------------------------------------------------
#' Create a standard horizontal bar chart for KDHS indicators.
kdhs_bar <- function(data, x, y, fill = NULL,
                     title = "", subtitle = "",
                     xlab = "Percentage (%)", ylab = "",
                     palette = "green") {

  fill_col <- if (palette == "green") KDHS_COLORS$primary else KDHS_COLORS$secondary

  p <- ggplot(data, aes(x = reorder(!!sym(x), !!sym(y)), y = !!sym(y))) +
    geom_col(fill = fill_col, alpha = 0.85, width = 0.7) +
    geom_errorbar(
      aes(ymin = pct_low, ymax = pct_upp),
      width = 0.25, color = KDHS_COLORS$neutral, linewidth = 0.5
    ) +
    coord_flip() +
    scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, subtitle = subtitle, x = ylab, y = xlab) +
    theme_kdhs()

  ggplotly(p, tooltip = c("x", "y")) |>
    layout(hoverlabel = list(bgcolor = "white"))
}

# ---- GROUPED BAR CHART (e.g., Urban vs Rural) -------------------------------
kdhs_grouped_bar <- function(data, x, y, group,
                              title = "", subtitle = "") {
  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(group))) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.85) +
    scale_fill_manual(values = c(KDHS_COLORS$urban, KDHS_COLORS$rural)) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(title = title, subtitle = subtitle, fill = "") +
    theme_kdhs()

  ggplotly(p, tooltip = c("x", "y", "fill"))
}

# ---- TREND LINE CHART -------------------------------------------------------
kdhs_trend <- function(data, x, y, group = NULL,
                        title = "", ylab = "Percentage (%)") {
  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y),
                         color = if (!is.null(group)) !!sym(group) else NULL,
                         group = if (!is.null(group)) !!sym(group) else 1)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    scale_color_manual(values = unlist(KDHS_COLORS[1:5])) +
    labs(title = title, y = ylab, x = "Survey Year") +
    theme_kdhs()

  ggplotly(p)
}

# ---- STAT VALUE BOX ---------------------------------------------------------
#' A visually clear KPI card (used in overview panel)
stat_box <- function(value, label, subtext = NULL, color = "green") {
  col <- if (color == "green") KDHS_COLORS$primary else KDHS_COLORS$secondary
  div(
    class = "stat-box",
    style = glue::glue("border-left: 4px solid {col}; padding: 1rem 1.25rem;
                         background: #f9fafb; border-radius: 6px; margin-bottom: 0.75rem;"),
    div(style = glue::glue("font-size: 2rem; font-weight: 700; color: {col};"), value),
    div(style = "font-size: 0.9rem; font-weight: 600; color: #1a202c;", label),
    if (!is.null(subtext)) div(style = "font-size: 0.78rem; color: #718096; margin-top: 0.2rem;", subtext)
  )
}

# ---- GGPLOT THEME -----------------------------------------------------------
theme_kdhs <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, color = "#1a202c"),
      plot.subtitle    = element_text(size = 11, color = "#4a5568"),
      panel.grid.major = element_line(color = "#e2e8f0"),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(color = "#4a5568"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      plot.margin      = margin(10, 15, 10, 10)
    )
}

# ---- MERGE HELPERS ----------------------------------------------------------
#' Merge women (IR) with household (HR) data.
#' Returns IR data with selected HR columns appended.
merge_ir_hr <- function(ir, hr, hr_vars = c("hv201", "hv205", "hv206")) {
  hr_sub <- hr |> select(hv001, hv002, all_of(hr_vars))
  ir |>
    rename(hv001 = v001, hv002 = v002) |>
    left_join(hr_sub, by = c("hv001", "hv002"))
}

#' Merge children (KR) with mothers (IR).
merge_kr_ir <- function(kr, ir, ir_vars = c("v106", "v190", "v501")) {
  ir_sub <- ir |>
    select(v001, v002, v003, all_of(ir_vars)) |>
    rename(v003m = v003)  # mother's line number

  kr |>
    left_join(ir_sub, by = c("v001", "v002", "v003m"))
}

#' Merge any recode with GPS clusters for mapping.
merge_with_gps <- function(df, gps, id_col = "v001") {
  gps_sub <- gps |> select(DHSCLUST, LATNUM, LONGNUM, URBAN_RURA)
  df |> left_join(gps_sub, by = setNames("DHSCLUST", id_col))
}

# ---- FORMAT HELPERS ---------------------------------------------------------
fmt_pct   <- function(x, digits = 1) paste0(round(x, digits), "%")
fmt_n     <- function(x) scales::comma(x)
fmt_label <- function(x) stringr::str_to_title(as.character(x))
