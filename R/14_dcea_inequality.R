# ============================================================
# DCEA Method Stage 3: measurement of inequality
#
# Implements the Atkinson equally-distributed-equivalent (EDE) health
# index used by Arnold et al. (2020) and Asaria, Griffin & Cookson
# (2016):
#
#   h_EDE = [ (1/n) * sum(h_i ^ (1-e)) ] ^ (1 / (1-e))     for e != 1
#   h_EDE = exp[ (1/n) * sum(log(h_i)) ]                    for e == 1
#
# where e is the inequality-aversion parameter and h_i is the health
# level of group i. With equal population weights (our five wealth
# quintiles are equal-sized by construction), (1/n) * sum(...) is just
# the population-weighted mean.
#
# An intervention's impact on inequality is then:
#   delta_ede = EDE(baseline + net_benefit_per_capita) - EDE(baseline)
#   inequality_impact = delta_ede - total_net_health_benefit
# A positive inequality_impact means the intervention reduces health
# inequality (its EDE gain exceeds its raw population health gain);
# negative means it increases inequality.
# ============================================================

library(dplyr)

quintile_ids <- c("q1", "q2", "q3", "q4", "q5")

#' Atkinson equally-distributed-equivalent (EDE) of a health
#' distribution
#'
#' @param health Numeric vector of health levels by group (must be > 0)
#' @param pop_weights Numeric vector of population shares, same length
#'   as `health`, summing to 1 (default: equal weights, i.e. wealth
#'   quintiles)
#' @param epsilon Inequality-aversion parameter (0 = no aversion, i.e.
#'   EDE = arithmetic mean; higher = more aversion to inequality)
#' @return A single number, in the same units as `health`. Returns
#'   NA_real_ (with a warning naming the offending value) rather than
#'   erroring when a health value is non-positive - e.g. an
#'   intervention whose opportunity cost swamps its direct benefit can
#'   push baseline HALE + net_benefit_per_capita below zero for one
#'   quintile, in a batch computed over 100+ interventions at once. A
#'   hard stop() here would abort every other intervention's result
#'   along with the one bad row.
atkinson_ede <- function(health, pop_weights = rep(1 / length(health), length(health)), epsilon) {
  if (length(health) != length(pop_weights)) {
    stop("`health` and `pop_weights` must be the same length")
  }
  if (any(is.na(health))) {
    return(NA_real_)
  }
  if (any(health <= 0)) {
    warning(
      "atkinson_ede() got a non-positive health value (", paste(round(health, 3), collapse = ", "),
      ") - returning NA for this group.", call. = FALSE
    )
    return(NA_real_)
  }
  if (isTRUE(all.equal(epsilon, 1))) {
    exp(sum(pop_weights * log(health)))
  } else {
    (sum(pop_weights * health^(1 - epsilon)))^(1 / (1 - epsilon))
  }
}

#' Per-intervention equity metrics: how each intervention's net health
#' benefit, added to the baseline HALE distribution, changes the
#' Atkinson EDE - the Senegal equivalent of Arnold et al.'s
#' Supplementary Table S5 / Figure 2 (health equity impact plane)
#'
#' @param distribution Output of build_dcea_distribution() (R/12)
#' @param baseline_hale One-row data frame with columns q1..q5, output
#'   of build_baseline_hale() (R/13)
#' @param national_population config$dcea$national_population, used
#'   only to convert a quintile's total net benefit (in DALYs) into a
#'   per-capita figure comparable to baseline HALE (in years);
#'   quintiles are assumed equal-sized (population / 5) by construction
#' @param epsilon Inequality-aversion parameter (config$dcea$inequality_aversion_epsilon)
#' @param scenario "full" (cases_full_2023-based columns) or
#'   "realistic" (cases_scaleup_2023-based columns)
#' @return A list with:
#'   baseline_ede - the Atkinson EDE of the baseline distribution alone
#'   per_intervention - one row per intervention: total_net_benefit,
#'     delta_ede, inequality_impact, quadrant ("++"/"+-"/"-+"/"--"
#'     following net-health-benefit sign x inequality-impact sign,
#'     as in Arnold et al.'s Figure 2/Table 2)
compute_equity_metrics <- function(distribution, baseline_hale, national_population,
                                    epsilon, scenario = c("full", "realistic")) {
  scenario <- match.arg(scenario)
  net_col <- if (scenario == "full") "net_benefit" else "net_benefit_realistic"

  quintile_population <- national_population / length(quintile_ids)
  baseline_vec <- as.numeric(baseline_hale[1, quintile_ids])
  baseline_ede <- atkinson_ede(baseline_vec, epsilon = epsilon)

  per_intervention <- distribution %>%
    group_by(intervention, main_category, gbd_cause, quintile) %>%
    summarise(net_benefit = sum(.data[[net_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(net_benefit_per_capita = net_benefit / quintile_population) %>%
    group_by(intervention, main_category, gbd_cause) %>%
    summarise(
      total_net_benefit = sum(net_benefit),
      post_hale = list(
        baseline_vec + net_benefit_per_capita[match(quintile_ids, quintile)]
      ),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      post_ede = atkinson_ede(unlist(post_hale), epsilon = epsilon),
      delta_ede = post_ede - baseline_ede,
      inequality_impact = delta_ede - total_net_benefit,
      quadrant = case_when(
        is.na(total_net_benefit) | is.na(inequality_impact) ~ NA_character_,
        total_net_benefit >= 0 & inequality_impact >= 0 ~ "++",
        total_net_benefit >= 0 & inequality_impact <  0 ~ "+-",
        total_net_benefit <  0 & inequality_impact >= 0 ~ "-+",
        TRUE ~ "--"
      )
    ) %>%
    ungroup() %>%
    select(-post_hale)

  list(baseline_ede = baseline_ede, per_intervention = per_intervention)
}

#' Package-level equity metric: sum the net benefit of a chosen set of
#' interventions (e.g. those affordable at the reference CET) and
#' compute ONE EDE change for the package as a whole - the Senegal
#' equivalent of the "current 51 EHP interventions" row in Arnold et
#' al.'s Table 2
#'
#' @param distribution Output of build_dcea_distribution() (R/12)
#' @param baseline_hale Output of build_baseline_hale() (R/13)
#' @param national_population config$dcea$national_population
#' @param epsilon Inequality-aversion parameter
#' @param interventions Character vector of intervention names making
#'   up "the package" (e.g. build_affordability_table()$table$intervention)
#' @param scenario "full" or "realistic"
#' @return A one-row data frame: total_net_benefit, baseline_ede,
#'   post_ede, delta_ede, inequality_impact
compute_package_equity <- function(distribution, baseline_hale, national_population,
                                    epsilon, interventions, scenario = c("full", "realistic")) {
  scenario <- match.arg(scenario)
  net_col <- if (scenario == "full") "net_benefit" else "net_benefit_realistic"

  quintile_population <- national_population / length(quintile_ids)
  baseline_vec <- as.numeric(baseline_hale[1, quintile_ids])
  baseline_ede <- atkinson_ede(baseline_vec, epsilon = epsilon)

  by_quintile <- distribution %>%
    filter(intervention %in% interventions) %>%
    group_by(quintile) %>%
    summarise(net_benefit = sum(.data[[net_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(net_benefit_per_capita = net_benefit / quintile_population)

  post_vec <- baseline_vec + by_quintile$net_benefit_per_capita[match(quintile_ids, by_quintile$quintile)]
  post_ede <- atkinson_ede(post_vec, epsilon = epsilon)
  total_net_benefit <- sum(by_quintile$net_benefit, na.rm = TRUE)

  data.frame(
    n_interventions = length(interventions),
    total_net_benefit = total_net_benefit,
    baseline_ede = baseline_ede,
    post_ede = post_ede,
    delta_ede = post_ede - baseline_ede,
    inequality_impact = (post_ede - baseline_ede) - total_net_benefit
  )
}
