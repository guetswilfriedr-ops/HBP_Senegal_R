# ============================================================
# DCEA Method Stage 2: baseline distribution of health (SIMPLIFIED)
#
# Arnold et al. (2020) build baseline HALE by quintile AND by
# residence from sibling- and children-mortality histories in the DHS
# (life tables via the Sullivan method) combined with a GBD-based YLD
# adjustment for 15 linked diseases. That requires DHS birth/sibling-
# history microdata we do not yet have access to for Senegal (see tab
# 6 of the DCEA prep workbook - EDS-Continue microdata request
# pending).
#
# Until that access materializes, this file builds a much simpler
# PROXY baseline, for both stratifiers: national HALE
# (config$dcea$national_hale_years) is reweighted across groups by a
# "relative burden index" built from data already in tab 2 of the prep
# workbook -
#   (a) the neonatal-mortality-derived group shares (a genuine,
#       Senegal-sourced mortality gradient), and
#   (b) the mean disease-share ratio across all 14 GBD causes (a crude
#       general-morbidity gradient) -
# blended with config$dcea$baseline_mortality_weight.
#
# For wealth quintiles, each group's "expected" population share is
# 20% (equal by construction). For residence, it is NOT equal - it is
# config$dcea$national_urban_share (proxied from survey sample
# composition, see config.R) - so the relative-burden-index formula
# divides each group's share by ITS OWN expected population share,
# not by a fixed 1/n.
#
# This is NOT a replication of Arnold et al.'s method and should be
# replaced once sibling-history life tables can be built directly from
# EDS-Continue microdata (see tab 6). It exists so the pipeline has a
# working, internally consistent Stage 2 end to end today, per the
# project's "start simple, enrich progressively" approach.
# ============================================================

library(dplyr)

#' Build a relative burden index by group (population-weighted mean 1;
#' >1 = more burden / less health than expected for that group's
#' population share) from tab 2 of the DCEA prep workbook
#'
#' @param d_table Output of read_dcea_prep()$d_table
#' @param group_ids Character vector (wealth_group_ids or residence_group_ids, R/10)
#' @param expected_share Named numeric vector, same names as
#'   `group_ids`, giving each group's expected population share as a
#'   percentage (e.g. c(q1=20,...,q5=20) for wealth; c(urban=39.6,
#'   rural=60.4) for residence)
#' @param neonatal_cause_name Exact "GBD cause" text used for the
#'   mortality-based component (default: "Neonatal disorders")
#' @param mortality_weight Weight (0-1) given to the neonatal-mortality
#'   component vs. the mean-disease-share component (config$dcea$baseline_mortality_weight)
#' @return A named numeric vector, one value per group in `group_ids`,
#'   with a population-weighted mean of 1
build_relative_burden_index <- function(d_table, group_ids, expected_share,
                                         neonatal_cause_name = "Neonatal disorders",
                                         mortality_weight = 0.5) {
  mean_share <- d_table %>%
    summarise(across(all_of(group_ids), ~ mean(.x, na.rm = TRUE))) %>%
    as.numeric()
  names(mean_share) <- group_ids
  general_index <- mean_share / expected_share[group_ids]

  neonatal_row <- d_table %>% filter(cause_gbd == neonatal_cause_name)
  if (nrow(neonatal_row) == 0) {
    warning(
      "'", neonatal_cause_name, "' not found in tab 2 - baseline burden index falls back to the ",
      "general (all-cause) index only.", call. = FALSE
    )
    mortality_index <- general_index
  } else {
    mortality_share <- as.numeric(neonatal_row[1, group_ids])
    names(mortality_share) <- group_ids
    mortality_index <- mortality_share / expected_share[group_ids]
  }

  combined <- mortality_weight * mortality_index + (1 - mortality_weight) * general_index
  pop_weights <- expected_share[group_ids] / sum(expected_share[group_ids])
  combined / sum(combined * pop_weights)
}

#' Build the simplified baseline HALE-by-group proxy, for BOTH
#' stratifiers (wealth quintile and residence) at once
#'
#' @param d_table Output of read_dcea_prep()$d_table
#' @param national_hale_years config$dcea$national_hale_years
#' @param national_urban_share config$dcea$national_urban_share (a
#'   proportion, e.g. 0.396 - NOT a percentage)
#' @param mortality_weight config$dcea$baseline_mortality_weight
#' @return A long data frame, one row per group, columns: group_type
#'   ("wealth"/"residence"), group_id, hale (years), pop_weight
#'   (proportion, sums to 1 within each group_type)
build_baseline_hale <- function(d_table, national_hale_years, national_urban_share,
                                 mortality_weight = 0.5) {
  wealth_expected_share <- setNames(rep(100 / length(wealth_group_ids), length(wealth_group_ids)), wealth_group_ids)
  residence_expected_share <- setNames(
    c(national_urban_share, 1 - national_urban_share) * 100, residence_group_ids
  )

  build_one <- function(group_ids, expected_share, group_type_label) {
    burden_index <- build_relative_burden_index(
      d_table, group_ids, expected_share,
      mortality_weight = mortality_weight
    )
    pop_weights <- expected_share[group_ids] / sum(expected_share[group_ids])
    relative_health <- 1 / burden_index
    relative_health <- relative_health / sum(relative_health * pop_weights)
    data.frame(
      group_type = group_type_label,
      group_id = group_ids,
      hale = national_hale_years * relative_health[group_ids],
      pop_weight = as.numeric(pop_weights[group_ids]),
      row.names = NULL
    )
  }

  bind_rows(
    build_one(wealth_group_ids, wealth_expected_share, "wealth"),
    build_one(residence_group_ids, residence_expected_share, "residence")
  )
}
