# ============================================================
# DCEA Method Stage 3: measurement of inequality
#
# Implements the Atkinson equally-distributed-equivalent (EDE) health
# index used by Arnold et al. (2020) and Asaria, Griffin & Cookson
# (2016):
#
#   h_EDE = [ sum(w_i * h_i ^ (1-e)) ] ^ (1 / (1-e))     for e != 1
#   h_EDE = exp[ sum(w_i * log(h_i)) ]                    for e == 1
#
# where e is the inequality-aversion parameter, h_i is the health level
# of group i, and w_i is group i's population weight (summing to 1).
# For wealth quintiles w_i = 0.2 for every group (equal-sized by
# construction); for residence w_i is NOT equal
# (config$dcea$national_urban_share) - this file takes population
# weights from R/13_dcea_baseline.R's own `pop_weight` column rather
# than assuming equal weights, so both stratifiers use the correct
# weighting automatically.
#
# An intervention's impact on inequality is then:
#   delta_ede = EDE(baseline + net_benefit_per_capita) - EDE(baseline)
#   inequality_impact = delta_ede * population - total_net_health_benefit
# delta_ede is a PER-CAPITA quantity (same units as baseline HALE:
# years per person), while total_net_health_benefit is a POPULATION-
# TOTAL quantity (DALYs summed over everyone) - so delta_ede must be
# scaled up by the total population before the two are compared, per
# Arnold et al.'s own worked example (baseline EDE HALE vs average
# HALE differ by "0.24 DALYs per person"; scaled by total population,
# this becomes the multi-hundred-thousand to million-DALY "cost of
# inequality" figures in their Table 2). One healthy life year gained
# is treated as equivalent to one DALY averted, as throughout this
# literature.
# A positive inequality_impact means the intervention reduces health
# inequality (its (population-scaled) EDE gain exceeds its raw
# population health gain); negative means it increases inequality.
# ============================================================

library(dplyr)

#' Atkinson equally-distributed-equivalent (EDE) of a health
#' distribution
#'
#' @param health Numeric vector of health levels by group (must be > 0)
#' @param pop_weights Numeric vector of population shares, same length
#'   as `health`, summing to 1
#' @param epsilon Inequality-aversion parameter (0 = no aversion, i.e.
#'   EDE = population-weighted mean; higher = more aversion to inequality)
#' @return A single number, in the same units as `health`. Returns
#'   NA_real_ (with a warning naming the offending value) rather than
#'   erroring when a health value is non-positive - e.g. an
#'   intervention whose opportunity cost swamps its direct benefit can
#'   push baseline HALE + net_benefit_per_capita below zero for one
#'   group, in a batch computed over 100+ interventions at once. A
#'   hard stop() here would abort every other intervention's result
#'   along with the one bad row.
atkinson_ede <- function(health, pop_weights, epsilon) {
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

#' Extract one stratifier's baseline health vector and population
#' weights, in group_id order, from build_baseline_hale()'s output
#'
#' @param baseline_hale Output of build_baseline_hale() (R/13)
#' @param group_type "wealth" or "residence"
#' @param group_ids wealth_group_ids or residence_group_ids (R/10)
#' @return A list with `health` and `pop_weights`, both named numeric
#'   vectors in `group_ids` order
get_baseline_vectors <- function(baseline_hale, group_type, group_ids) {
  df <- baseline_hale %>% filter(.data$group_type == !!group_type)
  df <- df[match(group_ids, df$group_id), ]
  list(health = setNames(df$hale, group_ids), pop_weights = setNames(df$pop_weight, group_ids))
}

#' Per-intervention equity metrics: how each intervention's net health
#' benefit, added to the baseline HALE distribution, changes the
#' Atkinson EDE - the Senegal equivalent of Arnold et al.'s
#' Supplementary Table S5 / Figure 2 (health equity impact plane)
#'
#' @param distribution Output of build_dcea_distribution() (R/12)
#' @param baseline_hale Output of build_baseline_hale() (R/13)
#' @param national_population config$dcea$national_population, used
#'   only to convert a group's total net benefit (in DALYs) into a
#'   per-capita figure comparable to baseline HALE (in years)
#' @param epsilon Inequality-aversion parameter (config$dcea$inequality_aversion_epsilon)
#' @param group_type "wealth" or "residence"
#' @param scenario "full" (cases_full_2023-based columns) or
#'   "realistic" (cases_scaleup_2023-based columns)
#' @return A list with:
#'   baseline_ede - the Atkinson EDE of the baseline distribution alone
#'   per_intervention - one row per intervention: total_net_benefit,
#'     delta_ede, inequality_impact, quadrant ("++"/"+-"/"-+"/"--"
#'     following net-health-benefit sign x inequality-impact sign,
#'     as in Arnold et al.'s Figure 2/Table 2)
compute_equity_metrics <- function(distribution, baseline_hale, national_population,
                                    epsilon, group_type = c("wealth", "residence"),
                                    scenario = c("full", "realistic")) {
  group_type <- match.arg(group_type)
  scenario <- match.arg(scenario)
  group_ids <- if (group_type == "wealth") wealth_group_ids else residence_group_ids
  net_col <- if (scenario == "full") "net_benefit" else "net_benefit_realistic"

  base <- get_baseline_vectors(baseline_hale, group_type, group_ids)
  group_population <- base$pop_weights * national_population
  baseline_ede <- atkinson_ede(base$health, base$pop_weights, epsilon)

  per_intervention <- distribution %>%
    filter(.data$group_type == !!group_type) %>%
    group_by(intervention, main_category, gbd_cause, group_id) %>%
    summarise(net_benefit = sum(.data[[net_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(net_benefit_per_capita = net_benefit / group_population[group_id]) %>%
    group_by(intervention, main_category, gbd_cause) %>%
    summarise(
      total_net_benefit = sum(net_benefit),
      post_hale = list(
        base$health + net_benefit_per_capita[match(group_ids, group_id)]
      ),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      post_ede = atkinson_ede(unlist(post_hale), base$pop_weights, epsilon),
      delta_ede = post_ede - baseline_ede,
      inequality_impact = delta_ede * national_population - total_net_benefit,
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
#' @param group_type "wealth" or "residence"
#' @param scenario "full" or "realistic"
#' @return A one-row data frame: total_net_benefit, baseline_ede,
#'   post_ede, delta_ede, inequality_impact
compute_package_equity <- function(distribution, baseline_hale, national_population,
                                    epsilon, interventions, group_type = c("wealth", "residence"),
                                    scenario = c("full", "realistic")) {
  group_type <- match.arg(group_type)
  scenario <- match.arg(scenario)
  group_ids <- if (group_type == "wealth") wealth_group_ids else residence_group_ids
  net_col <- if (scenario == "full") "net_benefit" else "net_benefit_realistic"

  base <- get_baseline_vectors(baseline_hale, group_type, group_ids)
  group_population <- base$pop_weights * national_population
  baseline_ede <- atkinson_ede(base$health, base$pop_weights, epsilon)

  by_group <- distribution %>%
    filter(.data$group_type == !!group_type, intervention %in% interventions) %>%
    group_by(group_id) %>%
    summarise(net_benefit = sum(.data[[net_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(net_benefit_per_capita = net_benefit / group_population[group_id])

  post_vec <- base$health + by_group$net_benefit_per_capita[match(group_ids, by_group$group_id)]
  post_ede <- atkinson_ede(post_vec, base$pop_weights, epsilon)
  total_net_benefit <- sum(by_group$net_benefit, na.rm = TRUE)

  data.frame(
    n_interventions = length(interventions),
    total_net_benefit = total_net_benefit,
    baseline_ede = baseline_ede,
    post_ede = post_ede,
    delta_ede = post_ede - baseline_ede,
    inequality_impact = (post_ede - baseline_ede) * national_population - total_net_benefit
  )
}
