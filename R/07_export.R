# ============================================================
# Export functions
# ============================================================

library(ggplot2)
library(openxlsx)

#' Write a data frame to a CSV file
#'
#' @param df Data frame to export
#' @param name File name without extension
#' @param dir Output directory
export_table <- function(df, name, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  write.csv(df, file.path(dir, paste0(name, ".csv")), row.names = FALSE)
}

#' Save a ggplot object as a PNG file
#'
#' @param plot A ggplot object
#' @param name File name without extension
#' @param dir Output directory
#' @param width Figure width in inches
#' @param height Figure height in inches
export_figure <- function(plot, name, dir, width = 8, height = 5) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  ggsave(file.path(dir, paste0(name, ".png")), plot = plot, width = width, height = height)
}

# Internal name -> presentation column label, in display order, for
# the rebuilt league table (see R/05_league_table.R)
league_table_display_columns <- c(
  rank_nhp                 = "Rank (Net Health Benefit, full implementation)",
  intervention              = "Intervention",
  main_category              = "Category",
  sub_category                = "Sub-category",
  gbd_cause                    = "GBD cause",
  net_dalys_full                = "Net DALYs averted (full implementation)",
  net_dalys_realistic             = "Net DALYs averted (realistic implementation)",
  diff_net_dalys                    = "Difference (full - realistic)",
  health_system_value_usd             = "$ value to health system",
  icer_usd                              = "ICER ($ per DALY averted)",
  icer_rank                              = "Rank (ICER)",
  effectiveness_status                    = "Effectiveness source",
  dalys_final                              = "DALYs averted per patient",
  cost_status                               = "Cost source",
  unit_cost_final_usd                        = "Unit cost ($)",
  cases_scaleup_2023                          = "Cases (realistic implementation)",
  cases_full_2023                              = "Cases (full implementation)",
  total_cost_realistic_usd                      = "Total cost - realistic ($)",
  total_dalys_realistic                          = "Total DALYs averted - realistic",
  total_cost_full_usd                             = "Total cost - full implementation ($)",
  total_dalys_full                                 = "Total DALYs averted - full implementation"
)

#' Export the rebuilt league table and the intervention funnel log as a
#' single, presentation-ready Excel workbook (two sheets), matching
#' the look of the original workbook's LeagueTable_Final: bold header
#' row, frozen header, currency/number formatting, auto column width.
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param funnel_log Output of build_intervention_funnel()$funnel_log
#' @param name File name without extension
#' @param dir Output directory
export_league_table_xlsx <- function(league_table, funnel_log, name, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  display <- as.data.frame(league_table[, names(league_table_display_columns)])
  names(display) <- unname(league_table_display_columns)
  funnel_log <- as.data.frame(funnel_log)

  wb <- createWorkbook()

  header_style <- createStyle(
    textDecoration = "bold", fgFill = "#1F4E78", fontColour = "#FFFFFF",
    halign = "center", valign = "center", wrapText = TRUE, border = "TopBottom"
  )
  usd_style   <- createStyle(numFmt = "#,##0")
  daly_style  <- createStyle(numFmt = "#,##0.0")
  icer_style  <- createStyle(numFmt = "#,##0.00")

  addWorksheet(wb, "League table")
  writeData(wb, "League table", display, headerStyle = header_style)
  freezePane(wb, "League table", firstActiveRow = 2, firstActiveCol = 3)
  setColWidths(wb, "League table", cols = seq_along(display), widths = "auto")

  usd_cols  <- which(names(display) %in% c(
    "$ value to health system", "Unit cost ($)", "Total cost - realistic ($)",
    "Total cost - full implementation ($)"
  ))
  daly_cols <- which(names(display) %in% c(
    "Net DALYs averted (full implementation)", "Net DALYs averted (realistic implementation)",
    "Difference (full - realistic)", "DALYs averted per patient",
    "Total DALYs averted - realistic", "Total DALYs averted - full implementation"
  ))
  icer_col  <- which(names(display) == "ICER ($ per DALY averted)")

  n <- nrow(display) + 1
  addStyle(wb, "League table", usd_style,  rows = 2:n, cols = usd_cols,  gridExpand = TRUE)
  addStyle(wb, "League table", daly_style, rows = 2:n, cols = daly_cols, gridExpand = TRUE)
  addStyle(wb, "League table", icer_style, rows = 2:n, cols = icer_col,  gridExpand = TRUE)

  addWorksheet(wb, "Intervention funnel log")
  writeData(wb, "Intervention funnel log", funnel_log, headerStyle = header_style)
  freezePane(wb, "Intervention funnel log", firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, "Intervention funnel log", cols = seq_along(funnel_log), widths = "auto")

  saveWorkbook(wb, file.path(dir, paste0(name, ".xlsx")), overwrite = TRUE)
}
