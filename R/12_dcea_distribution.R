# ============================================================
# DCEA Method Stage 1: distributional impact of interventions
#
# Reproduces the Box 1 worked example of Arnold, Nkhoma & Griffin
# (2020) for every intervention in the Senegal league table, for BOTH
# equity-relevant stratifiers they use: wealth quintile (q1..q5) and
# residence (urban/rural) - WITH ONE ADAPTATION, explained below.
#
# Arnold et al.'s "A" (total eligible population) is a raw disease-
# burden denominator, independent of how many people actually get
# treated - so their D (disease share) and E (uptake rate) apply as
# two SEQUENTIAL filters: eligible = A * D, treated = eligible * E.
#
# Our league table's `cases_full_2023` / `cases_scaleup_2023` are NOT
# that: they are the OHT model's own already-coverage-adjusted case
# volumes (the number of people the intervention is modelled to reach
# at a given implementation scenario), and `total_dalys_full` /
# `net_dalys_full` in the league table are computed directly from
# them. Applying D and E as sequential filters on top of an already-
# coverage-adjusted number would double-count coverage and understate
# every intervention's benefit relative to the (already validated)
# league table.
#
# So here, D and E are combined into a single per-group WEIGHT,
# normalized to sum to 1 across the groups of a given stratifier
# (quintiles, or residence), and used to REDISTRIBUTE the league
# table's own case volume - not to shrink it:
#
#   weight_g = (D_g * E_g) / sum_g(D_g * E_g)
#   treated_g = cases * weight_g
#   direct_benefit_g   = treated_g * dalys_per_patient
#   opportunity_cost_g = F_g * (total_cost / opportunity_cost_usd_per_daly)
#   net_benefit_g = direct_benefit_g - opportunity_cost_g
#
# This guarantees sum_g(direct_benefit_g) == total_dalys_full and
# sum_g(net_benefit_g) == net_dalys_full exactly (when
# opportunity_cost_usd_per_daly equals the league table's own CET),
# FOR EACH STRATIFIER SEPARATELY - i.e. the wealth-quintile view and
# the residence view are two different ways of slicing the exact same
# intervention total, not two different totals.
#
# Computed for both the "full implementation" and "realistic
# implementation" case-volume scenarios already in the league table
# (cases_full_2023 / cases_scaleup_2023), so the same before/after
# comparison Arnold et al. make in their Figure 3 is available here.
# ============================================================

library(dplyr)
library(tidyr)

#' Build the long-format (intervention x group) distributional impact
#' table for one stratifier (wealth quintile or residence)
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param interventions_mapped Output of assign_e_indicator()
#' @param d_table,e_table,f_row Output of read_dcea_prep()
#' @param opportunity_cost_usd_per_daly Output of resolve_opportunity_cost_rate()
#' @param group_ids Character vector of column-name suffixes for this
#'   stratifier (wealth_group_ids or residence_group_ids, R/10)
#' @param group_type_label "wealth" or "residence" - tagged onto every
#'   output row so a downstream filter(group_type == ...) can pick one
#'   stratifier's rows out of the combined table
#' @return One row per intervention x group, tagged with group_type
build_dcea_distribution_for_stratifier <- function(league_table, interventions_mapped,
                                                     d_table, e_table, f_row,
                                                     opportunity_cost_usd_per_daly,
                                                     group_ids, group_type_label) {

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
    select(cause_gbd, all_of(group_ids)) %>%
    pivot_longer(all_of(group_ids), names_to = "group_id", values_to = "d_share_pct")

  e_long <- e_table %>%
    select(indicator_id, all_of(group_ids)) %>%
    pivot_longer(all_of(group_ids), names_to = "group_id", values_to = "e_rate_pct")

  f_long <- f_row %>%
    select(all_of(group_ids)) %>%
    pivot_longer(everything(), names_to = "group_id", values_to = "f_share_pct")

  crossing(lt, group_id = group_ids) %>%
    left_join(d_long, by = c("gbd_cause" = "cause_gbd", "group_id")) %>%
    left_join(e_long, by = c("e_indicator_id" = "indicator_id", "group_id")) %>%
    left_join(f_long, by = "group_id") %>%
    group_by(intervention) %>%
    mutate(
      group_type = group_type_label,
      d_share = d_share_pct / 100,
      e_rate  = e_rate_pct / 100,
      f_share = f_share_pct / 100,

      # Combined D*E weight, normalized across this stratifier's
      # groups within each intervention (see file header comment)
      de_raw = d_share * e_rate,
      de_weight = ifelse(rep(sum(de_raw, na.rm = TRUE) > 0, n()), de_raw / sum(de_raw, na.rm = TRUE), NA_real_),

      population_eligible = cases_full_2023 * d_share,
      population_treated  = cases_full_2023 * de_weight,
      direct_benefit       = population_treated * dalys_final,
      opportunity_cost      = f_share * (total_cost_full_usd / opportunity_cost_usd_per_daly),
      net_benefit            = direct_benefit - opportunity_cost,

      population_eligible_realistic = cases_scaleup_2023 * d_share,
      population_treated_realistic  = cases_scaleup_2023 * de_weight,
      direct_benefit_realistic       = population_treated_realistic * dalys_final,
      opportunity_cost_realistic      = f_share * (total_cost_realistic_usd / opportunity_cost_usd_per_daly),
      net_benefit_realistic            = direct_benefit_realistic - opportunity_cost_realistic
    ) %>%
    ungroup() %>%
    select(
      intervention, main_category, sub_category, gbd_cause,
      e_indicator_id, e_indicator_source, group_type, group_id,
      d_share, e_rate, f_share,
      population_eligible, population_treated, direct_benefit, opportunity_cost, net_benefit,
      population_eligible_realistic, population_treated_realistic,
      direct_benefit_realistic, opportunity_cost_realistic, net_benefit_realistic
    )
}

#' Build the combined distributional impact table for BOTH stratifiers
#' (wealth quintile and residence) at once
#'
#' @inheritParams build_dcea_distribution_for_stratifier
#' @return Rows for every intervention x quintile AND every
#'   intervention x residence group, distinguished by `group_type`
build_dcea_distribution <- function(league_table, interventions_mapped, d_table, e_table, f_row,
                                     opportunity_cost_usd_per_daly) {
  wealth <- build_dcea_distribution_for_stratifier(
    league_table, interventions_mapped, d_table, e_table, f_row,
    opportunity_cost_usd_per_daly, wealth_group_ids, "wealth"
  )
  residence <- build_dcea_distribution_for_stratifier(
    league_table, interventions_mapped, d_table, e_table, f_row,
    opportunity_cost_usd_per_daly, residence_group_ids, "residence"
  )
  bind_rows(wealth, residence)
}

#' Aggregate the distributional impact across every intervention, by
#' group, for one stratifier - the Senegal equivalent of Arnold et
#' al.'s Table 1 / Figure 3 (direct benefit, opportunity cost, net
#' benefit, per group, summed over the whole modelled set of
#' interventions)
#'
#' @param distribution Output of build_dcea_distribution()
#' @param group_type "wealth" or "residence"
#' @param interventions Optionally restrict to a subset of intervention
#'   names (e.g. only those affordable at the reference CET, from
#'   build_affordability_table() in R/09_ochalek_analysis.R). NULL (the
#'   default) uses every intervention in `distribution`.
#' @return One row per group, with total direct_benefit,
#'   opportunity_cost and net_benefit for both the full and realistic
#'   implementation scenarios
aggregate_dcea_by_group <- function(distribution, group_type = c("wealth", "residence"), interventions = NULL) {
  group_type <- match.arg(group_type)
  group_ids <- if (group_type == "wealth") wealth_group_ids else residence_group_ids

  df <- distribution %>% filter(.data$group_type == !!group_type)
  if (!is.null(interventions)) {
    df <- df %>% filter(intervention %in% interventions)
  }
  df %>%
    group_by(group_id) %>%
    summarise(
      direct_benefit = sum(direct_benefit, na.rm = TRUE),
      opportunity_cost = sum(opportunity_cost, na.rm = TRUE),
      net_benefit = sum(net_benefit, na.rm = TRUE),
      direct_benefit_realistic = sum(direct_benefit_realistic, na.rm = TRUE),
      opportunity_cost_realistic = sum(opportunity_cost_realistic, na.rm = TRUE),
      net_benefit_realistic = sum(net_benefit_realistic, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(match(group_id, group_ids))
}
