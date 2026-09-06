# ============================================================
# DCEA Method Stage 4: sensitivity analyses
#
# Mirrors the five sensitivity analyses in Arnold, Nkhoma & Griffin
# (2020):
#   SA1 - equal vs. more-unequal disease prevalence (tab 2 / D)
#   SA2 - equal vs. more-unequal service uptake (tab 3 / E)
#   SA3 - alternative opportunity-cost rate, and equal vs. more-unequal
#         opportunity-cost distribution (tab 5 / tab 4)
#   SA4 - alternative baseline (not replicated here - see R/13's header
#         comment on why the baseline itself is already a first-pass
#         simplification; re-running SA4 properly needs the same DHS
#         sibling-history microdata the baseline itself is waiting on)
#   SA5 - inequality-aversion parameter (epsilon)
#
# Each scenario function returns a MODIFIED COPY of the relevant table
# (d_table / e_table / f_row) - the original tier-1/2/3 values loaded
# from the workbook are never mutated in place, so re-running the base
# case after a sensitivity analysis always reproduces the same numbers.
# ============================================================

library(dplyr)

quintile_ids <- c("q1", "q2", "q3", "q4", "q5")

#' SA1a: equal prevalence/eligibility across all quintiles
#' @param d_table Output of read_dcea_prep()$d_table
scenario_equal_prevalence <- function(d_table) {
  d_table %>% mutate(across(all_of(quintile_ids), ~ 100 / length(quintile_ids)))
}

#' SA1b: more-unequal prevalence - shift `shift_points` (percentage
#' points, split evenly) from the two richest to the two poorest
#' quintiles, then clip at 0 and renormalize each row back to 100
#' @param d_table Output of read_dcea_prep()$d_table
#' @param shift_points Total percentage-point shift (config$dcea$sensitivity_shift_points)
scenario_unequal_prevalence <- function(d_table, shift_points = 10) {
  half <- shift_points / 2
  d_table %>%
    mutate(
      q1 = q1 + half, q2 = q2 + half,
      q4 = q4 - half, q5 = q5 - half,
      across(all_of(quintile_ids), ~ pmax(.x, 0))
    ) %>%
    mutate(row_sum = q1 + q2 + q3 + q4 + q5) %>%
    mutate(across(all_of(quintile_ids), ~ .x / row_sum * 100)) %>%
    select(-row_sum)
}

#' SA2a: equal uptake across all quintiles, at each row's own
#' (unweighted) mean rate - since quintiles are equal-sized, this is
#' also the population-weighted mean
#' @param e_table Output of read_dcea_prep()$e_table
scenario_equal_uptake <- function(e_table) {
  e_table %>%
    rowwise() %>%
    mutate(row_mean = mean(c_across(all_of(quintile_ids)), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(across(all_of(quintile_ids), ~ row_mean)) %>%
    select(-row_mean)
}

#' SA2b: more-unequal uptake - shift `shift_points` (percentage points,
#' split evenly) from the two richest to the two poorest quintiles,
#' clipped to [0, 100] (uptake is a rate, not a share, so no renormalization)
#' @param e_table Output of read_dcea_prep()$e_table
#' @param shift_points config$dcea$sensitivity_shift_points
scenario_unequal_uptake <- function(e_table, shift_points = 10) {
  half <- shift_points / 2
  e_table %>%
    mutate(
      q1 = pmin(pmax(q1 + half, 0), 100),
      q2 = pmin(pmax(q2 + half, 0), 100),
      q4 = pmin(pmax(q4 - half, 0), 100),
      q5 = pmin(pmax(q5 - half, 0), 100)
    )
}

#' SA3b: equal opportunity-cost distribution across all quintiles
#' @param f_row Output of read_dcea_prep()$f_row
scenario_equal_opportunity_cost <- function(f_row) {
  f_row %>% mutate(across(all_of(quintile_ids), ~ 100 / length(quintile_ids)))
}

#' SA3c: more-unequal opportunity-cost distribution (same +/- shift
#' logic as SA1b, renormalized to 100)
#' @param f_row Output of read_dcea_prep()$f_row
#' @param shift_points config$dcea$sensitivity_shift_points
scenario_unequal_opportunity_cost <- function(f_row, shift_points = 10) {
  scenario_unequal_prevalence(f_row, shift_points)
}

#' Run the full sensitivity battery and return one summary row per
#' scenario, in the shape of Arnold et al.'s Table 2: scenario label,
#' package-level total net benefit, delta EDE, inequality impact, and
#' (per-intervention) equity-plane quadrant counts
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param interventions_mapped Output of assign_e_indicator()
#' @param d_table,e_table,f_row,cet_params Output of read_dcea_prep()
#' @param reference_cet config$cet_usd_per_daly
#' @param baseline_hale Output of build_baseline_hale()
#' @param national_population config$dcea$national_population
#' @param epsilon_reference config$dcea$inequality_aversion_epsilon
#' @param epsilon_sensitivity config$dcea$inequality_aversion_sensitivity
#'   (named numeric vector)
#' @param cet_multipliers config$cet_sensitivity_multipliers, reused
#'   here for the opportunity-cost-rate sensitivity in the absence of a
#'   Senegal-specific set of alternative Ochalek-style estimates
#' @param shift_points config$dcea$sensitivity_shift_points
#' @param package_interventions Character vector of intervention names
#'   defining "the package" for the package-level metrics (e.g. from
#'   build_affordability_table() in R/09_ochalek_analysis.R)
#' @return A data frame, one row per scenario
build_dcea_sensitivity_table <- function(league_table, interventions_mapped,
                                          d_table, e_table, f_row, cet_params,
                                          reference_cet, baseline_hale, national_population,
                                          epsilon_reference, epsilon_sensitivity,
                                          cet_multipliers, shift_points,
                                          package_interventions) {

  base_opp_cost_rate <- resolve_opportunity_cost_rate(cet_params, reference_cet)

  run_scenario <- function(label, d = d_table, e = e_table, f = f_row,
                            opp_cost_rate = base_opp_cost_rate, epsilon = epsilon_reference) {
    dist <- build_dcea_distribution(league_table, interventions_mapped, d, e, f, opp_cost_rate)
    pkg <- compute_package_equity(dist, baseline_hale, national_population, epsilon, package_interventions)
    eq <- compute_equity_metrics(dist, baseline_hale, national_population, epsilon)$per_intervention
    quad_counts <- table(factor(eq$quadrant, levels = c("++", "+-", "-+", "--")))
    data.frame(
      scenario = label,
      n_interventions_package = pkg$n_interventions,
      total_net_benefit = pkg$total_net_benefit,
      delta_ede = pkg$delta_ede,
      inequality_impact_dalys = pkg$inequality_impact,
      quadrant_pp = as.integer(quad_counts["++"]),
      quadrant_pm = as.integer(quad_counts["+-"]),
      quadrant_mp = as.integer(quad_counts["-+"]),
      quadrant_mm = as.integer(quad_counts["--"])
    )
  }

  scenarios <- list(
    run_scenario("Base case"),
    run_scenario("SA1a - Equal prevalence", d = scenario_equal_prevalence(d_table)),
    run_scenario("SA1b - More unequal prevalence", d = scenario_unequal_prevalence(d_table, shift_points)),
    run_scenario("SA2a - Equal uptake", e = scenario_equal_uptake(e_table)),
    run_scenario("SA2b - More unequal uptake", e = scenario_unequal_uptake(e_table, shift_points)),
    run_scenario("SA3a - Equal opportunity cost distribution", f = scenario_equal_opportunity_cost(f_row)),
    run_scenario("SA3b - More unequal opportunity cost distribution", f = scenario_unequal_opportunity_cost(f_row, shift_points))
  )

  for (scenario_label in names(cet_multipliers)) {
    scenarios[[length(scenarios) + 1]] <- run_scenario(
      paste0("SA3c - Opportunity cost rate: ", scenario_label),
      opp_cost_rate = base_opp_cost_rate * cet_multipliers[[scenario_label]]
    )
  }

  for (epsilon_label in names(epsilon_sensitivity)) {
    scenarios[[length(scenarios) + 1]] <- run_scenario(
      paste0("SA5 - Inequality aversion: ", epsilon_label),
      epsilon = epsilon_sensitivity[[epsilon_label]]
    )
  }

  bind_rows(scenarios)
}
