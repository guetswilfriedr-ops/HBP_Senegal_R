# ============================================================
# DCEA Method Stage 2: baseline distribution of health (SIMPLIFIED)
#
# Arnold et al. (2020) build baseline HALE by quintile from sibling-
# and children-mortality histories in the DHS (life tables via the
# Sullivan method) combined with a GBD-based YLD adjustment for 15
# linked diseases. That requires DHS birth/sibling-history microdata
# we do not yet have access to for Senegal (see tab 6 of the DCEA prep
# workbook - EDS-Continue microdata request pending).
#
# Until that access materializes, this file builds a much simpler
# PROXY baseline: national HALE (config$dcea$national_hale_years) is
# reweighted across quintiles by a "relative burden index" built from
# data already in tab 2 of the prep workbook -
#   (a) the neonatal-mortality-derived quintile shares (a genuine,
#       Senegal-sourced mortality gradient), and
#   (b) the mean disease-share ratio across all 14 GBD causes (a crude
#       general-morbidity gradient) -
# blended with config$dcea$baseline_mortality_weight.
#
# This is NOT a replication of Arnold et al.'s method and should be
# replaced once sibling-history life tables can be built directly from
# EDS-Continue microdata (see tab 6). It exists so the pipeline has a
# working, internally consistent Stage 2 end to end today, per the
# project's "start simple, enrich progressively" approach.
# ============================================================

library(dplyr)

quintile_ids <- c("q1", "q2", "q3", "q4", "q5")

#' Build a relative burden index by quintile (mean 1 across quintiles;
#' >1 = more burden / less health than average) from tab 2 of the DCEA
#' prep workbook
#'
#' @param d_table Output of read_dcea_prep()$d_table
#' @param neonatal_cause_name Exact "GBD cause" text used for the
#'   mortality-based component (default: "Neonatal disorders")
#' @param mortality_weight Weight (0-1) given to the neonatal-mortality
#'   component vs. the mean-disease-share component (config$dcea$baseline_mortality_weight)
#' @return A named numeric vector, one value per quintile in
#'   `quintile_ids`, averaging to 1
build_relative_burden_index <- function(d_table, neonatal_cause_name = "Neonatal disorders",
                                         mortality_weight = 0.5) {
  equal_share <- 100 / length(quintile_ids)

  mean_share <- d_table %>%
    summarise(across(all_of(quintile_ids), ~ mean(.x, na.rm = TRUE))) %>%
    as.numeric()
  general_index <- mean_share / equal_share

  neonatal_row <- d_table %>% filter(gbd_cause_matches(cause_gbd, neonatal_cause_name))
  if (nrow(neonatal_row) == 0) {
    warning(
      "'", neonatal_cause_name, "' not found in tab 2 - baseline burden index falls back to the ",
      "general (all-cause) index only.", call. = FALSE
    )
    mortality_index <- general_index
  } else {
    mortality_index <- as.numeric(neonatal_row[1, quintile_ids]) / equal_share
  }

  combined <- mortality_weight * mortality_index + (1 - mortality_weight) * general_index
  names(combined) <- quintile_ids
  combined / mean(combined)
}

# Small helper kept separate so a future swap to a different neonatal
# label (or a vector of several mortality-linked causes) is a one-line
# change rather than a rewrite.
gbd_cause_matches <- function(cause_gbd, target) cause_gbd == target

#' Build the simplified baseline HALE-by-quintile proxy
#'
#' @param d_table Output of read_dcea_prep()$d_table
#' @param national_hale_years config$dcea$national_hale_years
#' @param mortality_weight config$dcea$baseline_mortality_weight
#' @return A one-row data frame with columns q1..q5 (HALE in years),
#'   plus `relative_index` list-column for transparency/debugging
build_baseline_hale <- function(d_table, national_hale_years, mortality_weight = 0.5) {
  burden_index <- build_relative_burden_index(d_table, mortality_weight = mortality_weight)
  relative_health <- 1 / burden_index
  relative_health <- relative_health / mean(relative_health)
  hale_by_quintile <- national_hale_years * relative_health

  as.data.frame(as.list(hale_by_quintile))
}
