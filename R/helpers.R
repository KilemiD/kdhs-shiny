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

# ---- CATEGORICAL PALETTE ----------------------------------------------------
#' n distinct colours for a grouped chart.
#'
#' KDHS_COLORS holds 13 colours. Passing it straight to scale_colour_manual()
#' fails the moment a chart has more groups than that — grouping a survival curve
#' by county needs 47 and produced
#'   "Insufficient values in manual scale. 47 needed but only 13 provided."
#' which ggplotly surfaced as a silently blank panel. Always size the palette to
#' the data instead of assuming it fits.
#'
#' Up to 13 groups keep the KDHS house colours; above that, fall back to an
#' evenly spaced hue wheel, which stays distinguishable and never runs out.
kdhs_palette <- function(n) {
  if (!is.finite(n) || n < 1) return(character(0))
  house <- unname(unlist(KDHS_COLORS))
  if (n <= length(house)) return(house[seq_len(n)])
  grDevices::hcl(h = seq(15, 375, length.out = n + 1)[seq_len(n)],
                 c = 100, l = 62)
}

# ---- AXIS ORDERING ----------------------------------------------------------
#' Order a column's factor levels by another column.
#'
#' Use this instead of reorder() inside aes(). ggplotly turns the aes EXPRESSION
#' into the hover label and the axis title, so aes(x = reorder(county, pct))
#' shows the reader a tooltip reading "reorder(county, pct): Mandera" and an axis
#' titled "reorder(county, pct)". Ordering the factor up front means aes() can
#' name the column directly and the label stays clean.
#'
#' @param data  A data frame
#' @param var   Column whose levels should be reordered (character name)
#' @param by    Column to order by (character name)
#' @param desc  TRUE to put the largest value first
order_levels <- function(data, var, by, desc = FALSE) {
  if (!all(c(var, by) %in% names(data)) || nrow(data) == 0) return(data)
  vals <- as.character(data[[var]])
  ord  <- order(data[[by]], decreasing = desc, na.last = TRUE)
  data[[var]] <- factor(vals, levels = unique(vals[ord]))
  data
}

# ---- EMPTY-SELECTION GUARD --------------------------------------------------
#' Does this survey design still have rows?
#'
#' srvyr's summarise() on an EMPTY grouped design fails with an opaque
#' "subscript out of bounds" from cur_svy_env$split[[cur_group_id()]]. Call this
#' before summarising anything derived from a user filter so the panel can show
#' a readable message instead.
svy_has_rows <- function(design) {
  !is.null(design) && !is.null(design$variables) && nrow(design$variables) > 0
}

#' Match a filter choice to a data value, ignoring case.
#'
#' The filter dropdowns are written in Title Case ("Poorest", "No education")
#' while the DHS value labels are lower case ("poorest", "no education"), so a
#' plain == comparison matched nothing, emptied the design and crashed the panel.
#'
#' Works on BOTH design flavours in this app, which is not cosmetic:
#'   - srvyr `tbl_svy`      — what create_*_design() returns; dplyr::filter works
#'   - `survey.design2`     — what u5cm_design() returns; dplyr::filter does NOT,
#'                            it fails with "no applicable method for 'filter'"
#' Row-subsetting via `[` is defined for survey designs and correctly carries the
#' strata and PSU information, so use that for the plain-survey case.
svy_filter_eq <- function(design, var, choice) {
  if (is.null(choice) || identical(choice, "All")) return(design)
  if (is.null(design) || !var %in% names(design$variables)) return(design)

  if (inherits(design, "tbl_svy")) {
    return(design |> filter(tolower(as.character(.data[[var]])) == tolower(choice)))
  }
  keep <- tolower(as.character(design$variables[[var]])) == tolower(choice)
  keep[is.na(keep)] <- FALSE
  design[keep, ]
}

# ---- STANDARD BAR CHART -----------------------------------------------------
#' Create a standard horizontal bar chart for KDHS indicators.
kdhs_bar <- function(data, x, y, fill = NULL,
                     title = "", subtitle = "",
                     xlab = "Percentage (%)", ylab = "",
                     palette = "green") {

  fill_col <- if (palette == "green") KDHS_COLORS$primary else KDHS_COLORS$secondary
  data <- order_levels(data, x, y)

  # An explicit `text` aesthetic plus tooltip = "text" below. Without it ggplotly
  # labels the hover with the raw column names and full precision — readers got
  # "v190: Poorest / pct: 34.03149" instead of "Poorest: 34.0%".
  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y),
                        text = paste0(!!sym(x), ": ", round(!!sym(y), 1), "%"))) +
    geom_col(fill = fill_col, alpha = 0.85, width = 0.7)

  # Only draw the interval when the caller actually asked for vartype = "ci".
  if (all(c("pct_low", "pct_upp") %in% names(data))) {
    p <- p + geom_errorbar(
      aes(ymin = pct_low, ymax = pct_upp),
      width = 0.25, color = KDHS_COLORS$neutral, linewidth = 0.5
    )
  }

  p <- p +
    coord_flip() +
    scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, subtitle = subtitle, x = ylab, y = xlab) +
    theme_kdhs()

  ggplotly(p, tooltip = "text") |>
    layout(hoverlabel = list(bgcolor = "white"))
}

# ---- GROUPED BAR CHART (e.g., Urban vs Rural) -------------------------------
kdhs_grouped_bar <- function(data, x, y, group,
                              title = "", subtitle = "",
                              xlab = "", ylab = "Percentage (%)") {
  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(group),
                        text = paste0(!!sym(x), " — ", !!sym(group), "<br>",
                                      round(!!sym(y), 1), "%"))) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.85) +
    scale_fill_manual(values = c(KDHS_COLORS$urban, KDHS_COLORS$rural)) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    # Without these the axes are titled with the raw DHS column names (v024, pct)
    labs(title = title, subtitle = subtitle, x = xlab, y = ylab, fill = "") +
    theme_kdhs() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

  ggplotly(p, tooltip = "text")
}

# ---- TREND LINE CHART -------------------------------------------------------
kdhs_trend <- function(data, x, y, group = NULL,
                        title = "", ylab = "Percentage (%)",
                        xlab = "Survey Year", grouplab = "") {
  # The old version put `if (!is.null(group)) !!sym(group) else NULL` INSIDE aes(),
  # so ggplotly used that whole expression as the series name — the legend and
  # every tooltip read "if (!is.null(group)) indicator else NULL: anc4".
  # Build the mapping outside aes() instead.
  mapping <- if (!is.null(group)) {
    aes(x = !!sym(x), y = !!sym(y), colour = !!sym(group), group = !!sym(group),
        text = paste0(.data[[group]], "<br>", !!sym(x), ": ", round(!!sym(y), 1), "%"))
  } else {
    aes(x = !!sym(x), y = !!sym(y), group = 1,
        text = paste0(!!sym(x), ": ", round(!!sym(y), 1), "%"))
  }

  p <- ggplot(data, mapping) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    # unname(): unlist() keeps the KDHS_COLORS names (primary, secondary, ...),
    # and ggplot2 then tries to match those against the data's group levels,
    # finds none, warns "No shared levels found" and drops the palette.
    # Unnamed values are matched positionally, which is what is wanted here.
    scale_colour_manual(values = kdhs_palette(if (!is.null(group)) dplyr::n_distinct(data[[group]]) else 1)) +
    labs(title = title, y = ylab, x = xlab, colour = grouplab) +
    theme_kdhs()

  ggplotly(p, tooltip = "text")
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
