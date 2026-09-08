# ============================================================
# Export functions
#
# Every table this pipeline produces is written as a formatted .xlsx
# workbook - never CSV, to avoid the character-encoding issues CSV
# causes when Excel opens it (accented characters in intervention
# names getting mangled). openxlsx writes proper UTF-8 throughout, so
# this is not a concern.
# ============================================================

library(ggplot2)
library(openxlsx)

# LISER graphic-charter blue (darkest primary tint) - see the
# liser-style skill for the full palette. Used here as the one colour
# accent on an otherwise plain white table; every structural line is
# plain black, matching a classic "three-line" academic table (a rule
# above the header, a rule below the header, and a rule below the last
# row) rather than a filled/banded grid.
liser_bleu <- "#000066"

xlsx_header_style <- function() {
  createStyle(
    textDecoration = "bold", fontColour = liser_bleu, fgFill = "#FFFFFF",
    fontSize = 11, halign = "center", valign = "center", wrapText = TRUE,
    border = "TopBottom", borderColour = "#000000", borderStyle = "thin"
  )
}

# Every body cell wraps its text and centers it (both horizontally and
# vertically) so a fixed, narrow column width never truncates or
# misaligns a value - long text wraps onto extra lines instead of
# spilling or being cut off. White fill (not "no fill") so a cell
# retains its plain background even where Excel's own alternating
# shading or theme would otherwise show through.
xlsx_body_style <- function() createStyle(wrapText = TRUE, halign = "center", valign = "center", fgFill = "#FFFFFF")

# The closing rule of the three-line table: a plain black line under
# the very last row, applied on its own (not part of xlsx_body_style,
# since only the final row gets it).
xlsx_bottom_rule_style <- function() createStyle(border = "Bottom", borderColour = "#000000", borderStyle = "thin")

# Fixed column width, in Excel character-width units: wide enough for
# an intervention name to read comfortably, uniformly narrow (~95px)
# everywhere else - so every sheet is scannable at a glance instead of
# each column auto-sizing to its longest value.
xlsx_intervention_col_width <- 45
xlsx_default_col_width <- 15

#' Write one data frame to one styled worksheet of an (already
#' created) workbook, in a plain "three-line" academic table style:
#' white background throughout, no worksheet gridlines, no cell
#' borders except a black rule above the header, another below the
#' header, and a closing one below the last row. Column headers are
#' bold and set in the LISER blue accent colour; body cells are
#' wrapped and centered with fixed column widths so nothing truncates
#' or spills. Number formats are applied for the named columns.
#'
#' @param wb An openxlsx Workbook (from createWorkbook())
#' @param sheet_name Name for the new worksheet
#' @param df Data frame to write
#' @param freeze_col Number of leading columns to keep visible when
#'   scrolling right (1 keeps just the first column, e.g. intervention)
#' @param currency_cols,decimal_cols,integer_cols Column names to give
#'   a "#,##0", "#,##0.00", or "#,##0" number format respectively
write_xlsx_sheet <- function(wb, sheet_name, df, freeze_col = 1,
                              currency_cols = character(0),
                              decimal_cols = character(0),
                              integer_cols = character(0)) {
  df <- as.data.frame(df)
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  writeData(wb, sheet_name, df, headerStyle = xlsx_header_style())
  freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = freeze_col + 1)

  wide_cols <- names(df) == "Intervention" | grepl("reference|source", names(df), ignore.case = TRUE)
  col_widths <- ifelse(wide_cols, xlsx_intervention_col_width, xlsx_default_col_width)
  setColWidths(wb, sheet_name, cols = seq_along(df), widths = col_widths)

  n <- nrow(df) + 1
  if (n >= 2) {
    addStyle(wb, sheet_name, xlsx_body_style(), rows = 2:n, cols = seq_along(df), gridExpand = TRUE, stack = TRUE)
    # Closing rule of the three-line table, on the last row only.
    addStyle(wb, sheet_name, xlsx_bottom_rule_style(), rows = n, cols = seq_along(df), gridExpand = TRUE, stack = TRUE)
  }

  apply_fmt <- function(cols, style) {
    idx <- which(names(df) %in% cols)
    if (length(idx) > 0 && n >= 2) {
      addStyle(wb, sheet_name, style, rows = 2:n, cols = idx, gridExpand = TRUE, stack = TRUE)
    }
  }
  apply_fmt(currency_cols, createStyle(numFmt = "#,##0"))
  apply_fmt(decimal_cols, createStyle(numFmt = "#,##0.00"))
  apply_fmt(integer_cols, createStyle(numFmt = "#,##0"))

  invisible(wb)
}

#' Save a workbook to <dir>/<name>.xlsx and clean up its archive
#'
#' @param wb An openxlsx Workbook
#' @param name File name without extension
#' @param dir Output directory
#' @return The path written to
save_xlsx <- function(wb, name, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  path <- file.path(dir, paste0(name, ".xlsx"))
  tryCatch(
    saveWorkbook(wb, path, overwrite = TRUE),
    error = function(e) {
      stop(
        "Could not write '", path, "' (", conditionMessage(e), "). ",
        "If it is open in Excel, close it and re-run - otherwise this file ",
        "is left holding stale data from a previous run rather than this one.",
        call. = FALSE
      )
    }
  )
  strip_unused_drawing_refs(path)
  path
}

#' Add a "Sources & methodology" worksheet listing the bibliographic
#' references behind this pipeline's methods, so every delivered
#' workbook is traceable back to its source literature on its own,
#' without depending on an accompanying note. Kept as one shared list
#' (rather than repeated ad hoc per script) so a citation fixed here is
#' fixed everywhere it appears.
#'
#' @param wb An openxlsx Workbook
#' @param topics Character vector of which reference rows to include
#'   (subset of names(pipeline_references())); default is all of them
add_sources_sheet <- function(wb, topics = names(pipeline_references())) {
  refs <- pipeline_references()[topics]
  df <- data.frame(
    Topic     = names(refs),
    Reference = vapply(refs, `[[`, character(1), "citation"),
    `Used for` = vapply(refs, `[[`, character(1), "used_for"),
    check.names = FALSE
  )
  write_xlsx_sheet(wb, "Sources & methodology", df, freeze_col = 0)
}

#' The pipeline's methodology references, keyed by topic. Describes
#' each method in generic terms (approach + what it is used for)
#' rather than naming a specific source publication or country, since
#' this deliverable is Senegal's own analysis - not a replication
#' credited to another country's study. Centralised here (rather than
#' repeated per script) so there is exactly one place to refine a
#' description.
pipeline_references <- function() {
  list(
    "Cost-effectiveness threshold (CET)" = list(
      citation = "Health-opportunity-cost cost-effectiveness threshold, estimated from Senegal's own GBD 2023 disease-burden data. See config.R for the current working value and how to revise it.",
      used_for = "Senegal reference CET of $485/DALY averted (config.R); league-table ICER cut-off and affordable-package definition (R/09_priority_setting_analysis.R)"
    ),
    "Constrained optimization" = list(
      citation = "Budget-constrained coverage-optimization approach: linear programming that maximises net health benefit (DALYs averted net of opportunity cost) subject to a consumables-budget constraint, a standard method in health-benefits-package prioritization analysis.",
      used_for = "Scenario comparison and program-inclusion-rate tables (R/18_constrained_optimization.R, main_optimization_senegal.R)"
    ),
    "Distributional cost-effectiveness analysis (DCEA)" = list(
      citation = "Equity-stratified distributional cost-effectiveness analysis (wealth-quintile and urban/rural strata), following standard DCEA methodology.",
      used_for = "Equity module (R/10-17_dcea_*.R, main_dcea.R) - deferred this round pending input-data refinement"
    )
  )
}

#' Save a ggplot object as a PNG file
#'
#' @param plot A ggplot object
#' @param name File name without extension
#' @param dir Output directory
#' @param width Figure width in inches
#' @param height Figure height in inches
export_figure <- function(plot, name, dir, width = 10, height = 6) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  ggsave(file.path(dir, paste0(name, ".png")), plot = plot, width = width, height = height, dpi = 150)
}

# Internal name -> presentation column label, in display order, for
# the league table (see R/05_league_table.R). Kept to the core metrics
# a reader needs for the benefits package itself - cost, effectiveness
# rate, ICER, cumulative spend, and net health benefit at full and
# realistic implementation. The evidence behind each effectiveness
# figure (study, author, year, methodology) is traceable in full in
# the "3 - Effectiveness evidence" sheet of pipeline_steps.xlsx instead
# of being repeated here.
league_table_display_columns <- c(
  rank_nhp                       = "# (rank by net DALYs averted, full implementation)",
  intervention                   = "Intervention",
  main_category                  = "Category",
  sub_category                   = "Sub-category",
  icer_usd                       = "ICER ($)",
  icer_rank                      = "Rank (ICER)",
  included_in_package            = "Included in package (ICER <= CET)?",
  dalys_per_1000usd              = "DALYs averted per $1,000",
  cases_full_2023                = "Cases per annum",
  implementation_level_pct       = "Implementation level (%)",
  total_cost_full_usd            = "Total cost, full implementation ($)",
  cumulative_cost_full_usd       = "Cumulative cost, full implementation ($)",
  total_cost_realistic_usd       = "Total cost, realistic implementation ($)",
  cumulative_cost_realistic_usd  = "Cumulative cost, realistic implementation ($)",
  total_dalys_full                = "Total DALYs averted, full implementation",
  total_dalys_realistic            = "Total DALYs averted, realistic implementation",
  net_dalys_full                    = "Net DALYs averted, full implementation",
  net_dalys_realistic                = "Net DALYs averted, realistic implementation",
  diff_net_dalys                      = "Difference in net DALYs averted",
  health_system_value_usd               = "$ value to the health system of implementation"
)

#' Build a league-table-shaped worksheet inside an existing workbook
#'
#' @param sheet_name Worksheet name (default "League table"); used for
#'   variants such as the CET-affordability subset, which share the
#'   same columns
add_league_table_sheet <- function(wb, league_table, sheet_name = "League table") {
  display <- league_table[, names(league_table_display_columns)]
  names(display) <- unname(league_table_display_columns)

  write_xlsx_sheet(
    wb, sheet_name, display, freeze_col = 2,
    currency_cols = c(
      "$ value to the health system of implementation",
      "Total cost, full implementation ($)", "Cumulative cost, full implementation ($)",
      "Total cost, realistic implementation ($)", "Cumulative cost, realistic implementation ($)"
    ),
    decimal_cols = c(
      "ICER ($)", "DALYs averted per $1,000", "Implementation level (%)",
      "Total DALYs averted, full implementation", "Total DALYs averted, realistic implementation",
      "Net DALYs averted, full implementation", "Net DALYs averted, realistic implementation",
      "Difference in net DALYs averted"
    )
  )
}

# Extra column -> label mappings, for tables other than the league
# table (which uses league_table_display_columns above for both
# labels and column order). Merged into one lookup used by
# prettify_names() below.
extra_display_labels <- c(
  main_category            = "Category",
  sub_category             = "Sub-category",
  gbd_cause                = "GBD cause",
  top20_dalys_flag         = "Top 20 DALYs flag",
  cost_note                = "Cost note",
  effectiveness_note       = "Effectiveness note",
  article_id               = "Tufts article ID",
  ratio_number             = "Tufts ratio number",
  confidence               = "Confidence",
  step_excluded            = "Step excluded (0 = included)",
  step_label               = "Step",
  reason_excluded          = "Reason excluded",
  no_target_population_flag = "Alert: no target population",
  zero_case_volume_flag    = "Alert: zero case volume/coverage",
  step                     = "Step",
  label                    = "Step label",
  n_entering               = "N entering",
  n_excluded               = "N excluded",
  n_passed                 = "N passed",
  scenario                 = "CET scenario",
  cet_usd_per_daly         = "CET ($ per DALY averted)",
  n_included               = "N interventions affordable",
  total_net_dalys_full     = "Total net DALYs averted",
  level                    = "Grouping level",
  name                     = "Category / sub-category / GBD cause",
  n_master_list            = "N in master list (389 interventions)",
  n_league_table           = "N in league table"
)

column_labels <- c(league_table_display_columns, extra_display_labels)
column_labels <- column_labels[!duplicated(names(column_labels))]

#' Relabel a data frame's columns for display, using the shared
#' column_labels dictionary where a column is known, and a generic
#' Title Case fallback (snake_case -> "Snake Case") otherwise. Column
#' order is left untouched.
prettify_names <- function(df) {
  names(df) <- vapply(names(df), function(col_name) {
    if (col_name %in% names(column_labels)) {
      unname(column_labels[[col_name]])
    } else {
      tools::toTitleCase(gsub("_", " ", col_name))
    }
  }, character(1))
  df
}

#' Remove openxlsx's unused drawing/vmlDrawing relationship declarations
#'
#' Some openxlsx versions declare a drawing + legacyDrawing (vml)
#' relationship for every worksheet - to support later comments or
#' images - without writing the file the declaration points at,
#' leaving the worksheet's .rels and [Content_Types].xml referencing a
#' part that doesn't exist in the archive. Stricter readers than Excel
#' (e.g. Python's openpyxl) reject the file outright over this, so any
#' such declaration whose target is genuinely absent from the archive
#' is removed here. A declaration whose target does exist (e.g. an
#' actual embedded chart) is left untouched.
#'
#' A no-op if the workbook has no orphaned reference.
#'
#' @param path Path to the .xlsx file to fix, in place
strip_unused_drawing_refs <- function(path) {
  # Resolve to an absolute path first: zip::zip() below resolves a
  # relative zipfile against `root` (the temp extraction dir), not
  # the working directory, which would silently write nowhere useful.
  path <- normalizePath(path, mustWork = TRUE)

  tmp_dir <- tempfile("xlsx_fix_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = tmp_dir)

  rels_files <- list.files(
    file.path(tmp_dir, "xl", "worksheets", "_rels"),
    pattern = "\\.rels$", full.names = TRUE
  )
  orphaned_targets <- character(0)
  changed <- FALSE
  for (f in rels_files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "")
    rel_dir <- dirname(dirname(f))  # xl/worksheets
    matches <- gregexpr(
      '<Relationship[^>]*Type="[^"]*/(drawing|vmlDrawing)"[^>]*Target="([^"]+)"[^>]*/>',
      txt
    )
    found <- regmatches(txt, matches)[[1]]
    for (rel_tag in found) {
      target <- sub('.*Target="([^"]+)".*', "\\1", rel_tag)
      target_path <- normalizePath(file.path(rel_dir, target), mustWork = FALSE)
      if (!file.exists(target_path)) {
        txt <- sub(rel_tag, "", txt, fixed = TRUE)
        orphaned_targets <- c(orphaned_targets, basename(target))
        changed <- TRUE
      }
    }
    writeLines(txt, f)
  }

  ct_file <- file.path(tmp_dir, "[Content_Types].xml")
  if (changed && file.exists(ct_file) && length(orphaned_targets) > 0) {
    txt <- paste(readLines(ct_file, warn = FALSE), collapse = "")
    for (target_name in unique(orphaned_targets)) {
      txt <- gsub(
        paste0('<Override[^>]*PartName="[^"]*/', target_name, '"[^>]*/>'),
        "", txt
      )
    }
    writeLines(txt, ct_file)
  }

  if (changed) {
    # Use the 'zip' package (a hard dependency of openxlsx, so always
    # available) rather than utils::zip(), which shells out to a
    # system 'zip' binary that plain R on Windows does not ship with.
    all_files <- list.files(tmp_dir, recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
    if (file.exists(path)) file.remove(path)
    zip::zip(zipfile = path, files = all_files, root = tmp_dir, mode = "mirror")
  }

  invisible(path)
}
