# ============================================================
# HBP indicator calculations
# Add one function per indicator; keep each function focused
# on a single, well-named computation.
#
# LeagueTable_Final and GBD_TIER3 already carry computed figures
# (ICER, DALYs, ranks) rather than raw counts, so the functions
# below mostly select/reshape rather than recompute from scratch.
# Column names match the renames applied in R/02_cleaning.R
# (rename_league_table(), rename_gbd_tier3()).
# ============================================================

library(dplyr)

#' Rank interventions by cost-effectiveness (ICER)
#'
#' @param league_table Cleaned "LeagueTable_Final" data frame
#' @param n Number of top interventions to keep
#' @return A data frame with the n most cost-effective interventions
top_interventions_by_icer <- function(league_table, n = 20) {
  league_table %>%
    arrange(icer_usd) %>%
    slice_head(n = n)
}

#' Summarize the burden of disease by GBD category
#'
#' @param gbd_tier3 Cleaned "GBD_TIER3" data frame
#' @return A data frame with total DALYs and share per category (L1)
dalys_by_category <- function(gbd_tier3) {
  gbd_tier3 %>%
    group_by(category_l1) %>%
    summarise(
      total_dalys  = sum(dalys, na.rm = TRUE),
      pct_of_dalys = sum(pct_of_dalys, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_dalys))
}
