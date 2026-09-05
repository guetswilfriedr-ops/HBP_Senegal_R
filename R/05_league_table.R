# ============================================================
# Intervention funnel and league table
#
# Tracks every intervention in the master list through a sequence of
# steps, so every inclusion or exclusion is logged with a reason:
#
#   Step 1: linked to a Top-20-DALY-burden GBD cause
#   Step 2: has a unit cost                          (R/03_costs.R)
#   Step 3: has an effectiveness figure               (R/04_effectiveness.R)
#   Step 4: has case-volume data to compute totals from
#   -> included in the league table
#
# An intervention with a recorded case volume of exactly zero (as
# opposed to no case-volume data at all) is NOT excluded at step 4: it
# is computed normally (giving zero cost and zero DALYs averted) and
# flagged for attention via zero_case_volume_flag, so the intervention
# stays visible rather than silently disappearing.
#
# The league table itself is never filtered by the sign of the net
# health benefit: it ranks every intervention that reached step 4,
# regardless of whether that benefit is positive or negative, so a
# reader can see directly where each one falls relative to zero.
# ============================================================

library(dplyr)

step_labels <- c(
  "1" = "Linked to a Top-20-DALY-burden GBD cause",
  "2" = "Has a unit cost",
  "3" = "Has an effectiveness figure",
  "4" = "Has case-volume data to compute totals",
  "0" = "Included in the league table"
)

#' Build the intervention funnel and the final league table
#'
#' @param oht_case_data Cleaned "OHT Case data" data frame (the master
#'   intervention list, with demand/case-volume figures)
#' @param top20_causes Cleaned "Senegal HBP Tool - Top20 Causes" data frame
#' @param cost_table Output of build_cost_table()
#' @param effectiveness_table Output of build_effectiveness_table()
#' @param cet_usd_per_daly Cost-effectiveness threshold (config$cet_usd_per_daly)
#' @return A list with:
#'   funnel_log        - one row per master-list intervention, with a
#'                        numeric `step_excluded` (0 if included),
#'                        `reason_excluded`, and the zero-case-volume
#'                        alert flag
#'   funnel_summary     - one row per step, with how many interventions
#'                        entered, passed, and were excluded there
#'   step1_top20        - interventions passing step 1
#'   step2_cost         - interventions passing step 1-2, with cost
#'   step3_effectiveness - interventions passing step 1-3, with the
#'                        effectiveness figure and its study documentation
#'   league_table       - interventions passing step 1-4, with computed
#'                        costs, DALYs, ICER, and net health benefit
build_intervention_funnel <- function(oht_case_data, top20_causes,
                                       cost_table, effectiveness_table,
                                       cet_usd_per_daly) {

  oht_case_data <- ensure_columns(oht_case_data, c(
    "intervention", "case_status", "target_2023", "pin_2023",
    "coverage_2023", "cases_scaleup_2023", "cases_full_2023"
  ), "OHT Case data")
  top20_causes <- ensure_columns(top20_causes, c(
    "intervention", "main_category", "sub_category", "gbd_cause", "top20_dalys_flag"
  ), "Senegal HBP Tool - Top20 Causes")

  to_numeric <- function(x) suppressWarnings(as.numeric(x))

  master_list <- oht_case_data %>%
    distinct(intervention, .keep_all = TRUE) %>%
    select(intervention, case_status, target_2023, pin_2023, coverage_2023,
           cases_scaleup_2023, cases_full_2023) %>%
    mutate(across(
      c(target_2023, pin_2023, coverage_2023, cases_scaleup_2023, cases_full_2023),
      to_numeric
    ))

  top20_set <- top20_causes %>%
    distinct(intervention, .keep_all = TRUE) %>%
    select(intervention, main_category, sub_category, gbd_cause, top20_dalys_flag)

  funnel <- master_list %>%
    left_join(top20_set, by = "intervention") %>%
    left_join(cost_table, by = "intervention") %>%
    left_join(effectiveness_table, by = "intervention") %>%
    mutate(
      passed_1 = !is.na(main_category),
      passed_2 = passed_1 & !is.na(unit_cost_final_usd),
      passed_3 = passed_2 & !is.na(dalys_final),
      passed_4 = passed_3 & !is.na(cases_full_2023),
      step_excluded = case_when(
        !passed_1 ~ 1L,
        !passed_2 ~ 2L,
        !passed_3 ~ 3L,
        !passed_4 ~ 4L,
        TRUE       ~ 0L
      ),
      reason_excluded = case_when(
        step_excluded == 1 ~
          "Not linked to a Top-20-DALY-burden GBD cause",
        step_excluded == 2 ~ coalesce(cost_note, "No cost data"),
        step_excluded == 3 ~ coalesce(effectiveness_note, "No effectiveness data"),
        step_excluded == 4 ~ "No case-volume data available for this intervention",
        TRUE                 ~ NA_character_
      ),
      no_target_population_flag = passed_1 & (is.na(target_2023) | target_2023 == 0),
      zero_case_volume_flag = passed_4 & (
        (!is.na(cases_full_2023) & cases_full_2023 == 0) |
        (!is.na(coverage_2023) & coverage_2023 == 0)
      )
    )

  league_table <- funnel %>%
    filter(step_excluded == 0) %>%
    mutate(
      total_cost_realistic_usd  = unit_cost_final_usd * cases_scaleup_2023,
      total_dalys_realistic     = dalys_final * cases_scaleup_2023,
      total_dalys_full          = dalys_final * cases_full_2023,
      total_cost_full_usd       = unit_cost_final_usd * cases_full_2023,
      icer_usd                  = if_else(total_dalys_full > 0, total_cost_full_usd / total_dalys_full, NA_real_),
      net_dalys_realistic       = total_dalys_realistic - total_cost_realistic_usd / cet_usd_per_daly,
      net_dalys_full            = total_dalys_full - total_cost_full_usd / cet_usd_per_daly,
      diff_net_dalys            = net_dalys_full - net_dalys_realistic,
      health_system_value_usd   = diff_net_dalys * cet_usd_per_daly,
      # DALYs averted per $1,000 spent is the reciprocal of the ICER,
      # expressed at a more legible scale for reading and charting
      dalys_per_1000usd         = if_else(!is.na(icer_usd) & icer_usd != 0, 1000 / icer_usd, NA_real_),
      implementation_level_pct  = round(100 * cases_scaleup_2023 / cases_full_2023, 1)
    ) %>%
    arrange(icer_usd) %>%
    mutate(icer_rank = row_number()) %>%
    arrange(desc(net_dalys_full)) %>%
    mutate(
      rank_nhp = row_number(),
      cumulative_cost_full_usd      = cumsum(coalesce(total_cost_full_usd, 0)),
      cumulative_cost_realistic_usd = cumsum(coalesce(total_cost_realistic_usd, 0))
    ) %>%
    select(
      rank_nhp, intervention, main_category, sub_category, gbd_cause,
      zero_case_volume_flag, no_target_population_flag,
      effectiveness_status, dalys_final, title, primary_author, issue_year,
      journal_name, publication_date, target_countries, comparator_modality,
      time_horizon, perspective_author, costs_discounted, outcome_discounted,
      total_quality_score,
      cost_status, unit_cost_final_usd,
      cases_scaleup_2023, cases_full_2023, implementation_level_pct,
      total_cost_realistic_usd, total_dalys_realistic,
      total_cost_full_usd, total_dalys_full,
      cumulative_cost_full_usd, cumulative_cost_realistic_usd,
      icer_usd, icer_rank, dalys_per_1000usd,
      net_dalys_realistic, net_dalys_full, diff_net_dalys,
      health_system_value_usd
    )

  funnel_log <- funnel %>%
    mutate(step_label = step_labels[as.character(step_excluded)]) %>%
    select(intervention, main_category, sub_category,
           step_excluded, step_label, reason_excluded,
           no_target_population_flag, zero_case_volume_flag)

  n_total <- nrow(funnel)
  funnel_summary <- lapply(1:4, function(s) {
    n_entering <- sum(funnel$step_excluded == 0 | funnel$step_excluded >= s)
    n_excluded <- sum(funnel$step_excluded == s)
    data.frame(
      step = s,
      label = step_labels[[as.character(s)]],
      n_entering = n_entering,
      n_excluded = n_excluded,
      n_passed = n_entering - n_excluded
    )
  }) %>% bind_rows()
  funnel_summary <- bind_rows(
    data.frame(step = 0, label = "Total interventions considered",
               n_entering = n_total, n_excluded = 0, n_passed = n_total),
    funnel_summary
  )

  step1_top20 <- funnel %>%
    filter(passed_1) %>%
    select(intervention, main_category, sub_category, gbd_cause, top20_dalys_flag)

  step2_cost <- funnel %>%
    filter(passed_2) %>%
    select(intervention, main_category, sub_category,
           cost_status, unit_cost_final_usd, cost_note)

  step3_effectiveness <- funnel %>%
    filter(passed_3) %>%
    select(intervention, main_category, sub_category,
           cost_status, unit_cost_final_usd,
           effectiveness_status, dalys_final,
           article_id, ratio_number, confidence,
           title, primary_author, issue_year, journal_name, publication_date,
           target_countries, comparator_modality,
           time_horizon, perspective_author, costs_discounted, outcome_discounted,
           total_quality_score,
           effectiveness_note)

  list(
    funnel_log = funnel_log,
    funnel_summary = funnel_summary,
    step1_top20 = step1_top20,
    step2_cost = step2_cost,
    step3_effectiveness = step3_effectiveness,
    league_table = league_table
  )
}
