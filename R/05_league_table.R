# ============================================================
# League table "machine": rebuilds LeagueTable_Final from first
# principles, tracking every one of the 389 master-list interventions
# through the funnel so nothing is silently dropped.
#
# Funnel (mirrors the workbook, see README.md for the full formula
# trace):
#   389 interventions (OHT Case data)
#     -> Stage 1: linked to a Top-20-DALY-burden GBD cause
#                 (present in 'Senegal HBP Tool - Top20 Causes')
#     -> Stage 2: has an effectiveness figure (R/03_effectiveness.R)
#     -> Stage 3: has a unit cost (R/04_costs.R)
#     -> Stage 4: has demand data (case volume > 0)
#     -> included in the league table, with cost/DALY/ICER computed
#        and ranked.
#
# Presentation order (confirmed by the analyst): the league table
# returned here is sorted by rank_nhp - descending net health benefit
# (net DALYs averted, full/100% implementation scenario), i.e. the
# intervention with the largest net benefit is rank 1. This differs
# from the original workbook's own "Ranking of NHP" column, which
# ranked ascending (worst net benefit first); ascending order is not
# used anywhere here. icer_rank (ascending ICER = best value for
# money first) is kept as a secondary column for reference.
# ============================================================

library(dplyr)

#' Build the full intervention funnel: audit log for all 389
#' interventions, plus the final league table for those with
#' complete data.
#'
#' @param oht_case_data Cleaned "OHT Case data" data frame (389 rows)
#' @param top20_causes Cleaned "Senegal HBP Tool - Top20 Causes" data frame
#' @param effectiveness_table Output of build_effectiveness_table()
#' @param cost_table Output of build_cost_table()
#' @param cet_usd_per_daly Cost-effectiveness threshold (config$cet_usd_per_daly)
#' @return A list with:
#'   funnel_log     - one row per of the 389 interventions, with the
#'                    stage it was excluded at (or "0_included") and
#'                    a human-readable reason
#'   league_table   - interventions with complete data, with computed
#'                    costs, DALYs, ICER, net benefit and ranks
build_intervention_funnel <- function(oht_case_data, top20_causes,
                                       effectiveness_table, cost_table,
                                       cet_usd_per_daly) {

  oht_case_data <- ensure_columns(oht_case_data, c(
    "intervention", "case_status", "target_2023", "pin_2023",
    "coverage_2023", "cases_scaleup_2023", "cases_full_2023"
  ), "OHT Case data")
  top20_causes <- ensure_columns(top20_causes, c(
    "intervention", "main_category", "sub_category", "gbd_cause", "top20_dalys_flag"
  ), "Senegal HBP Tool - Top20 Causes")

  # These columns are expected to be numeric, but a stray text value
  # anywhere in the column (e.g. "N/A", "-", a leftover Excel error
  # like "#N/A") makes readxl import the whole column as character.
  # Force numeric here; any non-numeric text becomes NA (with a
  # warning, suppressed here since it's expected/handled downstream
  # as "no data").
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
    left_join(effectiveness_table, by = "intervention") %>%
    left_join(cost_table, by = "intervention") %>%
    mutate(
      in_top20     = !is.na(main_category),
      has_effect   = !is.na(dalys_final),
      has_cost     = !is.na(unit_cost_final_usd),
      has_demand   = !is.na(cases_full_2023) & cases_full_2023 > 0,
      stage_excluded = case_when(
        !in_top20   ~ "1_top20_causes",
        !has_effect ~ "2_effectiveness",
        !has_cost   ~ "3_cost",
        !has_demand ~ "4_demand",
        TRUE         ~ "0_included"
      ),
      reason_excluded = case_when(
        stage_excluded == "1_top20_causes" ~
          "Not linked to a Top-20-DALY-burden GBD cause (absent from 'Senegal HBP Tool - Top20 Causes')",
        stage_excluded == "2_effectiveness" ~ coalesce(effectiveness_note, "No effectiveness data"),
        stage_excluded == "3_cost"          ~ coalesce(cost_note, "No cost data"),
        stage_excluded == "4_demand" & !is.na(target_2023) & target_2023 > 0 ~
          "Target population is set in 'OHT Case data' but coverage/case volume is still 0 - scale-up data entry not yet completed",
        stage_excluded == "4_demand"        ~ "No target population or case data in 'OHT Case data'",
        TRUE                                  ~ NA_character_
      )
    )

  league_table <- funnel %>%
    filter(stage_excluded == "0_included") %>%
    mutate(
      total_cost_realistic_usd  = unit_cost_final_usd * cases_scaleup_2023,
      total_dalys_realistic     = dalys_final * cases_scaleup_2023,
      total_dalys_full          = dalys_final * cases_full_2023,
      total_cost_full_usd       = unit_cost_final_usd * cases_full_2023,
      icer_usd                  = if_else(total_dalys_full > 0, total_cost_full_usd / total_dalys_full, NA_real_),
      net_dalys_realistic       = total_dalys_realistic - total_cost_realistic_usd / cet_usd_per_daly,
      net_dalys_full            = total_dalys_full - total_cost_full_usd / cet_usd_per_daly,
      diff_net_dalys            = net_dalys_full - net_dalys_realistic,
      health_system_value_usd   = diff_net_dalys * cet_usd_per_daly
    ) %>%
    arrange(icer_usd) %>%
    mutate(icer_rank = row_number()) %>%
    arrange(desc(net_dalys_full)) %>%
    mutate(rank_nhp = row_number()) %>%
    select(
      rank_nhp, intervention, main_category, sub_category, gbd_cause,
      net_dalys_full, net_dalys_realistic, diff_net_dalys, health_system_value_usd,
      icer_usd, icer_rank,
      effectiveness_status, dalys_final,
      cost_status, unit_cost_final_usd,
      cases_scaleup_2023, cases_full_2023,
      total_cost_realistic_usd, total_dalys_realistic,
      total_cost_full_usd, total_dalys_full
    )

  funnel_log <- funnel %>%
    select(intervention, main_category, sub_category,
           in_top20, has_effect, has_cost, has_demand,
           stage_excluded, reason_excluded)

  list(funnel_log = funnel_log, league_table = league_table)
}
