# ============================================================
# Cleaning / harmonization functions
# ============================================================

library(dplyr)
library(janitor)

#' Apply generic cleaning to a single sheet's data frame
#'
#' @param df A raw data frame, as returned by load_raw_data()
#' @return The cleaned data frame
clean_sheet <- function(df) {
  df %>%
    clean_names() %>%
    remove_empty(c("rows", "cols"))
}

# ------------------------------------------------------------
# Sheet-specific renames
#
# janitor::clean_names() handles most headers fine, but a few
# (acronyms like "DALYs", symbols like "ICER (in $)") produce an
# unpredictable snake_case result. For sheets referenced by name
# in R/03_indicators.R, rename the columns explicitly first, using
# the exact original header text from the workbook, so downstream
# code does not depend on clean_names()'s exact output.
# ------------------------------------------------------------

rename_league_table <- function(df) {
  df %>%
    rename(
      intervention    = `Interventions`,
      intervention_fr = `Intervention (FR)`,
      icer_usd        = `ICER (in $)`,
      cases_per_annum = `Cases per annum`
    )
}

rename_gbd_tier3 <- function(df) {
  df %>%
    rename(
      category     = `Category`,
      cause        = `Cause`,
      dalys        = `DALYs`,
      pct_of_dalys = `Percentage of DALYs lost by disease`,
      category_l1  = `Category (L1)`
    )
}

# Sheet name -> rename function, applied before clean_sheet()
rename_overrides <- list(
  "LeagueTable_Final" = rename_league_table,
  "GBD_TIER3"          = rename_gbd_tier3
)

#' Apply clean_sheet() (and, where defined, a sheet-specific rename)
#' to every element of a named list of data frames
#'
#' @param data_list Named list of raw data frames, as returned by
#'   load_raw_data()
#' @return Named list of cleaned data frames
clean_all <- function(data_list) {
  cleaned <- lapply(names(data_list), function(sheet_name) {
    df <- data_list[[sheet_name]]
    rename_fn <- rename_overrides[[sheet_name]]
    if (!is.null(rename_fn)) {
      df <- rename_fn(df)
    }
    clean_sheet(df)
  })
  names(cleaned) <- names(data_list)
  cleaned
}
