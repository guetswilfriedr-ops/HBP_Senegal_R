# ============================================================
# Import functions
# Load only the Excel sheets listed in config$sheets_to_load,
# not the entire workbook. Some sheets have a title or a merged
# group-header row above the real column names; config$sheet_header_row
# gives the number of rows to skip for those (see config.R).
# ============================================================

library(readxl)

#' Read the requested sheets from the source workbook
#'
#' @param path Path to the .xlsx workbook
#' @param sheets Character vector of sheet names to load
#' @param header_row Named list of sheet_name -> rows to skip before
#'   the header (defaults to 0, i.e. header on row 1, for any sheet
#'   not listed)
#' @return A named list of data frames, one per sheet
load_raw_data <- function(path, sheets, header_row = list()) {
  if (!file.exists(path)) {
    stop("Workbook not found: ", path)
  }

  available_sheets <- excel_sheets(path)
  missing_sheets <- setdiff(sheets, available_sheets)
  if (length(missing_sheets) > 0) {
    stop("Sheet(s) not found in workbook: ", paste(missing_sheets, collapse = ", "))
  }

  data_list <- lapply(sheets, function(sheet_name) {
    skip_n <- header_row[[sheet_name]]
    if (is.null(skip_n)) skip_n <- 0
    read_excel(path, sheet = sheet_name, skip = skip_n)
  })
  names(data_list) <- sheets

  data_list
}
