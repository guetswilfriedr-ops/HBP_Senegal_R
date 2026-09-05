# ============================================================
# DCEA Method Stage 1: distributional impact of interventions
#
# Reproduces the Box 1 worked example of Arnold, Nkhoma & Griffin
# (2020) for every intervention in the Senegal league table at once:
#
#   eligible_q  = cases * D_q                  (D = disease/eligibility
#                                                 share by quintile, tab 2)
#   treated_q   = eligible_q * E_q             (E = coverage/uptake
#                                                 rate by quintile, tab 3)
#   direct_benefit_q   = treated_q * dalys_per_patient
#   opportunity_cost_q = F_q * (total_cost / opportunity_cost_usd_per_daly)
#                                               (F = national opportunity-
#                                                cost share, tab 4)
#   net_benefit_q = direct_benefit_q - opportunity_cost_q
#
# Computed for both the "full implementation" and "realistic
# implementation" case-volume scenarios already in the league table
# (cases_full_2023 / cases_scaleup_2023), so the same before/after
# comparison Arnold et al. make in their Figure 3 is available here.
# ============================================================

library(dplyr)
library(tidyr)

quintile_ids <- c("q1", "q2", "q3", "q4", "q5")

#' Build the long-format (intervention x quintile) distributional
#' impact table for every intervention in the league table
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#'   (R/05_league_table.R)
#' @param interventions_mapped Output of assign_e_indicator()
#'   (R/11_dcea_mapping.R)
#' @param d_table Output of read_dcea_prep()$d_table
#' @param e_table Output of read_dcea_prep()$e_table
#' @param f_row Output of read_dcea_prep()$f_row
#' @param opportunity_cost_usd_per_daly Output of
#'   resolve_opportunity_cost_rate() (R/10_dcea_import.R)
#' @return One row per intervention x quintile, with:
#'   d_share, e_rate, f_share (as proportions, 0-1)
#'   population_eligible/_treated, direct_benefit, opportunity_cost,
#'   net_benefit - each computed for both the full-implementation
#'   scenario (no suffix) and the realistic-implementation scenario
#'   (_realistic suffix)
build_dcea_distribution <- function(league_table, interventions_mapped, d_table, e_table, f_row,
                                     opportunity_cost_usd_per_daly) {

  intervention_lookup <- interventions_mapped %>%
    select(intervention = intervention_en, e_indicator_id, e_indicator_source)

  lt <- league_table %>%
    select(
      intervention, main_category, sub_category, gbd_cause,
      dalys_final, cases_full_2023, cases_scaleup_2023,
      total_cost_full_usd, total_cost_realistic_usd
    ) %>%
    left_join(intervention_lookup, by = "intervention")

  unmatched <- lt %>% filter(is.na(e_indicator_id))
  if (nrow(unmatched) > 0) {
    warning(
      nrow(unmatched), " league-table intervention(s) have no E-indicator mapping ",
      "(not found in the DCEA prep workbook's tab 1) and will get NA distributional results: ",
      paste(head(unmatched$intervention, 5), collapse = "; "),
      if (nrow(unmatched) > 5) ", ..." else "",
      call. = FALSE
    )
  }

  d_long <- d_table %>%
    select(cause_gbd, all_of(quintile_ids)) %>%
    pivot_longer(all_of(quintile_ids), names_to = "quintile", values_to = "d_share_pct")

  e_long <- e_table %>%
    select(indicator_id, all_of(quintile_ids)) %>%
    pivot_longer(all_of(quintile_ids), names_to = "quintile", values_to = "e_rate_pct")

  f_long <- f_row %>%
    select(all_of(quintile_ids)) %>%
    pivot_longer(everything(), names_to = "quintile", values_to = "f_share_pct")

  crossing(lt, quintile = quintile_ids) %>%
    left_join(d_long, by = c("gbd_cause" = "cause_gbd", "quintile")) %>%
    left_join(e_long, by = c("e_indicator_id" = "indicator_id", "quintile")) %>%
    left_join(f_long, by = "quintile") %>%
    mutate(
      d_share = d_share_pct / 100,
      e_rate  = e_rate_pct / 100,
      f_share = f_share_pct / 100,

      population_eligible = cases_full_2023 * d_share,
      population_treated  = population_eligible * e_rate,
      direct_benefit       = population_treated * dalys_final,
      opportunity_cost      = f_share * (total_cost_full_usd / opportunity_cost_usd_per_daly),
      net_benefit            = direct_benefit - opportunity_cost,

      population_eligible_realistic = cases_scaleup_2023 * d_share,
      population_treated_realistic  = population_eligible_realistic * e_rate,
      direct_benefit_realistic       = population_treated_realistic * dalys_final,
      opportunity_cost_realistic      = f_share * (total_cost_realistic_usd / opportunity_cost_usd_per_daly),
      net_benefit_realistic            = direct_benefit_realistic - opportunity_cost_realistic
    ) %>%
    select(
      intervention, main_category, sub_category, gbd_cause,
      e_indicator_id, e_indicator_source, quintile,
      d_share, e_rate, f_share,
      population_eligible, population_treated, direct_benefit, opportunity_cost, net_benefit,
      population_eligible_realistic, population_treated_realistic,
      direct_benefit_realistic, opportunity_cost_realistic, net_benefit_realistic
    )
}

#' Aggregate the distributional impact across every intervention, by
#' quintile - the Senegal equivalent of Arnold et al.'s Table 1 /
#' Figure 3b/3d (direct benefit, opportunity cost, net benefit, per
#' quintile, summed over the whole modelled set of interventions)
#'
#' @param distribution Output of build_dcea_distribution()
#' @param interventions Optionally restrict to a subset of intervention
#'   names (e.g. only those affordable at the reference CET, from
#'   build_affordability_table() in R/09_ochalek_analysis.R). NULL (the
#'   default) uses every intervention in `distribution`.
#' @return One row per quintile, with total direct_benefit,
#'   opportunity_cost and net_benefit for both the full and realistic
#'   implementation scenarios
aggregate_dcea_by_quintile <- function(distribution, interventions = NULL) {
  if (!is.null(interventions)) {
    distribution <- distribution %>% filter(intervention %in% interventions)
  }
  distribution %>%
    group_by(quintile) %>%
    summarise(
      direct_benefit = sum(direct_benefit, na.rm = TRUE),
      opportunity_cost = sum(opportunity_cost, na.rm = TRUE),
      net_benefit = sum(net_benefit, na.rm = TRUE),
      direct_benefit_realistic = sum(direct_benefit_realistic, na.rm = TRUE),
      opportunity_cost_realistic = sum(opportunity_cost_realistic, na.rm = TRUE),
      net_benefit_realistic = sum(net_benefit_realistic, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(match(quintile, quintile_ids))
}
