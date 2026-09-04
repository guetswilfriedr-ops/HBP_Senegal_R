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

#' Rename columns by their position (1 = column A, 2 = column B, ...)
#'
#' Several source sheets repeat the same header text across multiple
#' year blocks (e.g. "2023" appears at columns G, N, V, AD and AK in
#' "OHT Case data"), which makes renaming by name ambiguous. Renaming
#' by position, using the exact column letters from the workbook, is
#' unambiguous and documented inline below.
#'
#' @param df A data frame
#' @param position_map Named list: target column name -> 1-based
#'   column index
#' @return df with the listed columns renamed; all other columns
#'   (and their order) are left untouched
rename_at_position <- function(df, position_map) {
  for (new_name in names(position_map)) {
    idx <- position_map[[new_name]]
    names(df)[idx] <- new_name
  }
  df
}

# ------------------------------------------------------------
# Sheet-specific renames
#
# janitor::clean_names() handles most headers fine, but a few
# (acronyms like "DALYs", symbols like "ICER (in $)", or headers
# repeated across year blocks) produce an unpredictable or ambiguous
# snake_case result. For sheets referenced by name in R/03-05, rename
# the columns explicitly first, using the exact original position
# from the workbook, so downstream code does not depend on
# clean_names()'s exact output.
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

#' "OHT Case data": master list of 389 interventions, demand data
rename_oht_case_data <- function(df) {
  rename_at_position(df, list(
    intervention        = 4,  # D: Interventions
    case_status          = 5,  # E: Full case status - 2023
    target_2023           = 7,  # G: Target population, 2023
    pin_2023                = 14, # N: Population in need, 2023
    coverage_2023             = 22, # V: Coverage, 2023
    cases_scaleup_2023          = 30, # AD: Cases_scaleup, 2023 (realistic implementation)
    cases_full_2023               = 37  # AK: Cases_full, 2023 (full implementation)
  ))
}

#' "OHT Drug supply costs": unit cost per case
rename_oht_drug_supply_costs <- function(df) {
  rename_at_position(df, list(
    intervention      = 3,  # C: Intervention
    max_avg_cost_usd    = 14, # N: Max_avg_cost (max across delivery channels)
    unit_cost_xof          = 36, # AJ: Final unit cost in XOF
    unit_cost_usd             = 37  # AK: Final unit cost in $ per case in Senegal OHT
  ))
}

#' "Senegal HBP Tool - Top20 Causes": 141 interventions linked to a
#' Top-20-DALY-burden GBD cause
rename_top20_causes <- function(df) {
  rename_at_position(df, list(
    id                     = 1,  # A: #
    main_category           = 2,  # B
    sub_category              = 3,  # C
    intervention                = 4,  # D
    intervention_fr                = 5,  # E
    gbd_cause_code                    = 7,  # G
    gbd_cause                           = 8,  # H
    gbd_category_l1                       = 9,  # I
    gbd_rank                                = 10, # J: Rank (out of 176, by % DALYs)
    top20_dalys_flag                          = 11, # K: Top 20 DALYs (yes/no)
    pct_dalys_lost                              = 12, # L
    cost_manual_input_usd                         = 36  # AJ: Cost if manual input
  ))
}

#' "id_Ratio": analyst's choice of Tufts article/ratio per intervention
rename_id_ratio <- function(df) {
  rename_at_position(df, list(
    intervention = 4, # D
    article_id     = 6, # F: Tufts Article ID
    ratio_number      = 7, # G: Tufts Ratio #
    confidence          = 9  # I
  ))
}

#' "Tufts_Ratios": cost-effectiveness literature database
rename_tufts_ratios <- function(df) {
  rename_at_position(df, list(
    article_id                = 2,  # B: Article ID
    ratio_number                 = 18, # R: Ratio #
    dalys_per_patient_delta         = 93  # CO: DALY Per Patient Delta
  ))
}

#' "Uganda HBP Tool": fallback DALYs-averted/patient
rename_uganda_hbp <- function(df) {
  rename_at_position(df, list(
    old_intervention_name                = 7,  # G: Old intervention name
    dalys_averted_per_patient_uganda        = 91  # CM: DALYs averted per patient (Uganda)
  ))
}

#' "OHT Int name mapping recent-old": bridges Senegal intervention
#' names to Uganda's "old intervention name"
rename_name_mapping <- function(df) {
  rename_at_position(df, list(
    recent_intervention = 1, # A
    old_code               = 2, # B
    old_intervention          = 3, # C
    match_type                   = 4, # D
    confidence                      = 5, # E
    note                                = 6  # F
  ))
}

# Sheet name -> rename function, applied before clean_sheet()
rename_overrides <- list(
  "LeagueTable_Final"                = rename_league_table,
  "GBD_TIER3"                         = rename_gbd_tier3,
  "OHT Case data"                       = rename_oht_case_data,
  "OHT Drug supply costs"                 = rename_oht_drug_supply_costs,
  "Senegal HBP Tool - Top20 Causes"         = rename_top20_causes,
  "id_Ratio"                                  = rename_id_ratio,
  "Tufts_Ratios"                                 = rename_tufts_ratios,
  "Uganda HBP Tool"                                 = rename_uganda_hbp,
  "OHT Int name mapping recent-old"                    = rename_name_mapping
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
