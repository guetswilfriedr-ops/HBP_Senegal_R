# ============================================================
# DCEA export
#
# Reuses the export helpers from R/08_export.R (write_xlsx_sheet,
# save_xlsx, export_figure) so DCEA outputs land in output/tables and
# output/figures alongside the league-table outputs, in the same
# formatted-workbook style.
# ============================================================

library(openxlsx)

dcea_display_labels <- c(
  intervention              = "Intervention",
  main_category             = "Category",
  sub_category              = "Sub-category",
  gbd_cause                 = "GBD cause",
  e_indicator_id            = "Coverage indicator used",
  e_indicator_source        = "Indicator source (manual/sub_category/main_category/fallback)",
  quintile                  = "Wealth quintile",
  d_share                   = "Disease/eligibility share",
  e_rate                    = "Coverage/uptake rate",
  f_share                   = "Opportunity-cost share",
  population_eligible       = "Eligible population (full implementation)",
  population_treated        = "Population treated (full implementation)",
  direct_benefit            = "Direct benefit (DALYs averted, full implementation)",
  opportunity_cost          = "Opportunity cost (DALYs, full implementation)",
  net_benefit               = "Net benefit (DALYs, full implementation)",
  population_eligible_realistic = "Eligible population (realistic implementation)",
  population_treated_realistic  = "Population treated (realistic implementation)",
  direct_benefit_realistic      = "Direct benefit (DALYs averted, realistic implementation)",
  opportunity_cost_realistic    = "Opportunity cost (DALYs, realistic implementation)",
  net_benefit_realistic         = "Net benefit (DALYs, realistic implementation)",
  total_net_benefit         = "Total net health benefit (DALYs)",
  delta_ede                 = "Change in EDE health (DALY-equivalent)",
  inequality_impact         = "Inequality impact (DALYs averted-equivalent)",
  quadrant                  = "Equity-plane quadrant",
  baseline_ede              = "Baseline EDE (years)",
  post_ede                  = "Post-package EDE (years)",
  n_interventions_package   = "N interventions in package",
  n_interventions           = "N interventions in package",
  scenario                  = "Sensitivity scenario",
  inequality_impact_dalys   = "Inequality impact (DALYs averted-equivalent)",
  quadrant_pp               = "N interventions: ++ (health-improving, equity-improving)",
  quadrant_pm               = "N interventions: +- (health-improving, equity-worsening)",
  quadrant_mp               = "N interventions: -+ (health-worsening, equity-improving)",
  quadrant_mm               = "N interventions: -- (health-worsening, equity-worsening)"
)

#' Relabel a DCEA output data frame's columns for display, using
#' dcea_display_labels where known and prettify_names()'s generic
#' Title Case fallback otherwise (R/08_export.R)
prettify_dcea_names <- function(df) {
  names(df) <- vapply(names(df), function(col_name) {
    if (col_name %in% names(dcea_display_labels)) {
      unname(dcea_display_labels[[col_name]])
    } else {
      tools::toTitleCase(gsub("_", " ", col_name))
    }
  }, character(1))
  df
}

#' Table S4-style export: per-intervention population distribution by
#' quintile (eligible and treated population, in thousands and as a
#' % share), alongside the incremental benefit/cost figures - the
#' Senegal equivalent of Arnold et al.'s Supplementary Table S4
#'
#' @param distribution Output of build_dcea_distribution() (R/12)
#' @param league_table Output of build_intervention_funnel()$league_table
#' @return One row per intervention, wide-format by quintile
build_table_s4_style <- function(distribution, league_table) {
  quintile_ids <- c("q1", "q2", "q3", "q4", "q5")
  quintile_labels <- c(q1 = "Poorest", q2 = "Poorer", q3 = "Middle", q4 = "Richer", q5 = "Richest")

  wide <- distribution %>%
    group_by(intervention) %>%
    mutate(
      eligible_thousands = population_eligible / 1000,
      eligible_share_pct = population_eligible / sum(population_eligible, na.rm = TRUE) * 100,
      users_thousands = population_treated / 1000,
      users_share_pct = population_treated / sum(population_treated, na.rm = TRUE) * 100
    ) %>%
    ungroup() %>%
    mutate(quintile = unname(quintile_labels[quintile])) %>%
    select(intervention, main_category, sub_category, quintile,
           eligible_thousands, eligible_share_pct, users_thousands, users_share_pct) %>%
    tidyr::pivot_wider(
      names_from = quintile,
      values_from = c(eligible_thousands, eligible_share_pct, users_thousands, users_share_pct),
      names_glue = "{.value}_{quintile}"
    )

  league_table %>%
    select(intervention, net_dalys_full, dalys_final, unit_cost_final_usd, cases_full_2023) %>%
    mutate(pop_thousands = cases_full_2023 / 1000) %>%
    select(-cases_full_2023) %>%
    left_join(wide, by = "intervention") %>%
    arrange(desc(net_dalys_full))
}

#' Table S5-style export: net population impact and distributional
#' impact per intervention, ranked by improvement in EDE health - the
#' Senegal equivalent of Arnold et al.'s Supplementary Table S5
#'
#' @param equity_metrics The `per_intervention` element of
#'   compute_equity_metrics()'s output (R/14)
#' @param league_table Output of build_intervention_funnel()$league_table
#'   (supplies the ICER-based cost-effectiveness rank)
#' @return One row per intervention, ranked by delta_ede (descending)
build_table_s5_style <- function(equity_metrics, league_table) {
  equity_metrics %>%
    left_join(league_table %>% select(intervention, icer_rank), by = "intervention") %>%
    arrange(desc(delta_ede)) %>%
    mutate(rank_delta_ede = row_number()) %>%
    select(
      main_category, intervention, rank_delta_ede, delta_ede,
      icer_rank, total_net_benefit, inequality_impact, quadrant
    ) %>%
    rename(rank_cost_effectiveness = icer_rank)
}

#' Write every DCEA table (distribution, quintile summary,
#' per-intervention equity metrics, package equity, sensitivity table,
#' and the Table S4/S5-style supplementary exports) to one formatted
#' workbook: output/tables/dcea_results.xlsx
#'
#' @param distribution Output of build_dcea_distribution() (R/12)
#' @param quintile_summary Output of aggregate_dcea_by_quintile() (R/12)
#' @param equity_metrics The `per_intervention` element of
#'   compute_equity_metrics()'s output (R/14)
#' @param package_equity Output of compute_package_equity() (R/14)
#' @param sensitivity_table Output of build_dcea_sensitivity_table() (R/15)
#' @param league_table Output of build_intervention_funnel()$league_table,
#'   needed to build the Table S4/S5-style sheets
#' @param output_dir config$output_tables_dir
export_dcea_tables <- function(distribution, quintile_summary, equity_metrics,
                                package_equity, sensitivity_table, league_table, output_dir) {
  wb <- createWorkbook()
  write_xlsx_sheet(wb, "Distribution by intervention", prettify_dcea_names(distribution), freeze_col = 4)
  write_xlsx_sheet(wb, "Quintile summary", prettify_dcea_names(quintile_summary), freeze_col = 1)
  write_xlsx_sheet(
    wb, "Equity plane (per intervention)", prettify_dcea_names(equity_metrics), freeze_col = 1,
    decimal_cols = c("Total net health benefit (DALYs)", "Change in EDE health (DALY-equivalent)",
                      "Inequality impact (DALYs averted-equivalent)")
  )
  write_xlsx_sheet(wb, "Package equity summary", prettify_dcea_names(package_equity), freeze_col = 0)
  write_xlsx_sheet(wb, "Sensitivity analysis", prettify_dcea_names(sensitivity_table), freeze_col = 1)
  write_xlsx_sheet(
    wb, "Table S4 style - population dist.", build_table_s4_style(distribution, league_table), freeze_col = 2
  )
  write_xlsx_sheet(
    wb, "Table S5 style - EDE ranking", build_table_s5_style(equity_metrics, league_table), freeze_col = 2
  )
  save_xlsx(wb, "dcea_results", output_dir)
}
