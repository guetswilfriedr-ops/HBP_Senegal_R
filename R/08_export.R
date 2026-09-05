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

xlsx_header_style <- function() {
  createStyle(
    textDecoration = "bold", fgFill = "#1F4E78", fontColour = "#FFFFFF",
    fontSize = 11, halign = "center", valign = "center", wrapText = TRUE,
    border = "TopBottom"
  )
}

xlsx_band_style <- function() createStyle(fgFill = "#EEF2F8")

#' Write one data frame to one styled worksheet of an (already
#' created) workbook: bold coloured header, frozen header row, light
#' banding on alternating rows, auto column width, and number formats
#' for the named columns.
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
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df, headerStyle = xlsx_header_style())
  freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = freeze_col + 1)
  setColWidths(wb, sheet_name, cols = seq_along(df), widths = "auto")

  n <- nrow(df) + 1
  if (n >= 3) {
    band_rows <- seq(3, n, by = 2)
    addStyle(wb, sheet_name, xlsx_band_style(), rows = band_rows, cols = seq_along(df), gridExpand = TRUE)
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
  saveWorkbook(wb, path, overwrite = TRUE)
  strip_unused_drawing_refs(path)
  path
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
# the league table (see R/05_league_table.R)
league_table_display_columns <- c(
  rank_nhp                 = "Rank (Net Health Benefit, full implementation)",
  intervention              = "Intervention",
  main_category              = "Category",
  sub_category                = "Sub-category",
  gbd_cause                    = "GBD cause",
  zero_case_volume_flag          = "Alert: zero case volume/coverage",
  no_target_population_flag        = "Alert: no target population",
  net_dalys_full                     = "Net DALYs averted (full implementation)",
  net_dalys_realistic                  = "Net DALYs averted (realistic implementation)",
  diff_net_dalys                         = "Difference (full - realistic)",
  health_system_value_usd                  = "$ value to health system",
  icer_usd                                   = "ICER ($ per DALY averted)",
  icer_rank                                    = "Rank (ICER)",
  effectiveness_status                           = "Effectiveness source",
  dalys_final                                      = "DALYs averted per patient",
  title                                              = "Study title",
  primary_author                                       = "Study author",
  issue_year                                             = "Study year",
  journal_name                                             = "Journal",
  publication_date                                           = "Publication date",
  target_countries                                             = "Study country/countries",
  comparator_modality                                            = "Comparator",
  time_horizon                                                     = "Study time horizon",
  perspective_author                                                 = "Study analytic perspective",
  costs_discounted                                                     = "Costs discounted",
  outcome_discounted                                                     = "Outcomes discounted",
  total_quality_score                                                      = "Study quality score",
  cost_status                                                                = "Cost source",
  unit_cost_final_usd                                                          = "Unit cost ($)",
  cases_scaleup_2023                                                             = "Cases (realistic implementation)",
  cases_full_2023                                                                  = "Cases (full implementation)",
  total_cost_realistic_usd                                                           = "Total cost - realistic ($)",
  total_dalys_realistic                                                                = "Total DALYs averted - realistic",
  total_cost_full_usd                                                                    = "Total cost - full implementation ($)",
  total_dalys_full                                                                         = "Total DALYs averted - full implementation"
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
      "$ value to health system", "Unit cost ($)",
      "Total cost - realistic ($)", "Total cost - full implementation ($)"
    ),
    decimal_cols = c(
      "Net DALYs averted (full implementation)", "Net DALYs averted (realistic implementation)",
      "Difference (full - realistic)", "DALYs averted per patient",
      "Total DALYs averted - realistic", "Total DALYs averted - full implementation",
      "ICER ($ per DALY averted)"
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
  total_net_dalys_full     = "Total net DALYs averted"
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
