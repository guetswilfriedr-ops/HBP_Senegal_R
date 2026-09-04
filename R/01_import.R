# ============================================================
# Import functions
# Load only the Excel sheets listed in config$sheets_to_load,
# not the entire workbook.
# ============================================================

library(readxl)

#' Read the requested sheets from the source workbook
#'
#' @param path Path to the .xlsx workbook
#' @param sheets Character vector of sheet names to load
#' @return A named list of data frames, one per sheet
load_raw_data <- function(path, sheets) {
  if (!file.exists(path)) {
    stop("Workbook not found: ", path)
  }

  available_sheets <- excel_sheets(path)
  missing_sheets <- setdiff(sheets, available_sheets)
  if (length(missing_sheets) > 0) {
    stop("Sheet(s) not found in workbook: ", paste(missing_sheets, collapse = ", "))
  }

  data_list <- lapply(sheets, function(sheet_name) {
    read_excel(path, sheet = sheet_name)
  })
  names(data_list) <- sheets

  data_list
}
