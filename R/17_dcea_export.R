# ============================================================
# DCEA export
#
# Reuses the export helpers from R/08_export.R (write_xlsx_sheet,
# save_xlsx, export_figure) so DCEA outputs land in output/tables and
# output/figures alongside the league-table outputs, in the same
# formatted-workbook style.
# ============================================================

library(openxlsx)
library(dplyr)

dcea_display_labels <- c(
  intervention              = "Intervention",
  main_category             = "Category",
  sub_category              = "Sub-category",
  gbd_cause                 = "GBD cause",
  e_indicator_id            = "Coverage indicator used",
  e_indicator_source        = "Indicator source (manual/sub_category/main_category/fallback)",
  group_type                = "Stratifier (wealth quintile / residence)",
  group_id                  = "Group",
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
  delta_ede                 = "Change in EDE health, per capita (years)",
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
  quadrant_mm               = "N interventions: -- (health-worsening, equity-worsening)",
  icer_usd                  = "ICER ($/DALY)",
  net_dalys_full            = "Net benefit, full implementation (DALYs)",
  included_in_dcea          = "Included in DCEA?",
  exclusion_reason          = "Reason if excluded"
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

#' Table 1-style export: population, disease cases and health-service
#' utilization by group, for both stratifiers side by side - the
#' Senegal equivalent of Arnold et al.'s Table 1. Aggregates the
#' per-intervention distributional data (R/12) across every
#' DCEA-mapped intervention rather than computing a single country-
#' wide disease/utilization figure from scratch, since that is what
#' the D/E/F inputs already encode per intervention.
#'
#' @param distribution Output of build_dcea_distribution() (R/12) -
#'   contains BOTH stratifiers, tagged by group_type
#' @param f_row Output of read_dcea_prep()$f_row
#' @param national_population config$dcea$national_population
#' @param wealth_pop_weights,residence_pop_weights Named numeric
#'   vectors (population share, 0-1) for wealth_group_ids /
#'   residence_group_ids - e.g. get_baseline_vectors(...)$pop_weights (R/14)
#' @return One row per metric (population size, disease cases,
#'   service utilized, uptake, opportunity cost), one column per
#'   group across both stratifiers plus a "Total" column
build_table1_style <- function(distribution, f_row, national_population,
                                wealth_pop_weights, residence_pop_weights) {
  build_group_stats <- function(group_type, group_ids, pop_weights) {
    agg <- distribution %>%
      filter(.data$group_type == !!group_type) %>%
      group_by(group_id) %>%
      summarise(
        disease_cases    = sum(population_eligible, na.rm = TRUE),
        service_utilized = sum(population_treated, na.rm = TRUE),
        .groups = "drop"
      )
    agg <- agg[match(group_ids, agg$group_id), ]
    data.frame(
      group_id             = group_ids,
      label                = unname(group_labels_for(group_type)[group_ids]),
      population_size      = pop_weights[group_ids] * national_population,
      population_share_pct = pop_weights[group_ids] * 100,
      disease_cases        = agg$disease_cases,
      service_utilized     = agg$service_utilized,
      opportunity_cost_pct = as.numeric(f_row[1, group_ids]),
      stringsAsFactors = FALSE
    )
  }

  wealth_stats    <- build_group_stats("wealth", wealth_group_ids, wealth_pop_weights)
  residence_stats <- build_group_stats("residence", residence_group_ids, residence_pop_weights)

  total_disease_cases    <- sum(wealth_stats$disease_cases)
  total_service_utilized <- sum(wealth_stats$service_utilized)

  # Each group's share of the DALY-generating disease burden / of
  # service use is a share WITHIN its own stratifier (residence shares
  # sum to 100 across urban+rural; wealth shares sum to 100 across the
  # 5 quintiles) - both stratifiers describe the same national totals,
  # just split two different ways, matching Arnold et al.'s Table 1.
  add_shares <- function(stats) {
    stats %>%
      mutate(
        disease_cases_share_pct    = 100 * disease_cases / total_disease_cases,
        service_utilized_share_pct = 100 * service_utilized / total_service_utilized,
        uptake_pct                 = 100 * service_utilized / disease_cases
      )
  }
  all_stats <- bind_rows(add_shares(residence_stats), add_shares(wealth_stats))

  fmt_n_pct <- function(n, pct) sprintf("%s (%.0f%%)", format(round(n), big.mark = ",", scientific = FALSE), pct)
  fmt_pct   <- function(pct) sprintf("%.0f%%", pct)

  one_row <- function(label, total, values) {
    row <- as.data.frame(as.list(setNames(values, all_stats$label)), stringsAsFactors = FALSE)
    cbind(data.frame(Metric = label, Total = total, stringsAsFactors = FALSE), row)
  }

  bind_rows(
    one_row(
      "Population size, n (%)", fmt_n_pct(national_population, 100),
      fmt_n_pct(all_stats$population_size, all_stats$population_share_pct)
    ),
    one_row(
      "Disease cases (prevalence), n (%)", fmt_n_pct(total_disease_cases, 100),
      fmt_n_pct(all_stats$disease_cases, all_stats$disease_cases_share_pct)
    ),
    one_row(
      "Health service utilized (utilization), n (%)", fmt_n_pct(total_service_utilized, 100),
      fmt_n_pct(all_stats$service_utilized, all_stats$service_utilized_share_pct)
    ),
    one_row(
      "Uptake (services/diseases), %", fmt_pct(100 * total_service_utilized / total_disease_cases),
      fmt_pct(all_stats$uptake_pct)
    ),
    one_row("Opportunity cost, % of total", "", fmt_pct(all_stats$opportunity_cost_pct))
  )
}

#' Table S4-style export: per-intervention population distribution by
#' WEALTH QUINTILE (eligible and treated population, in thousands and
#' as a % share), alongside the incremental benefit/cost figures - the
#' Senegal equivalent of Arnold et al.'s Supplementary Table S4
#'
#' @param distribution Output of build_dcea_distribution() (R/12)
#' @param league_table Output of build_intervention_funnel()$league_table
#' @return One row per intervention, wide-format by quintile
build_table_s4_style <- function(distribution, league_table) {
  wide <- distribution %>%
    filter(.data$group_type == "wealth") %>%
    group_by(intervention) %>%
    mutate(
      eligible_thousands = population_eligible / 1000,
      eligible_share_pct = population_eligible / sum(population_eligible, na.rm = TRUE) * 100,
      users_thousands = population_treated / 1000,
      users_share_pct = population_treated / sum(population_treated, na.rm = TRUE) * 100
    ) %>%
    ungroup() %>%
    mutate(group_id = unname(wealth_group_labels[group_id])) %>%
    select(intervention, main_category, sub_category, group_id,
           eligible_thousands, eligible_share_pct, users_thousands, users_share_pct) %>%
    tidyr::pivot_wider(
      names_from = group_id,
      values_from = c(eligible_thousands, eligible_share_pct, users_thousands, users_share_pct),
      names_glue = "{.value}_{group_id}"
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
#'   compute_equity_metrics()'s output (R/14), for ONE group_type
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

#' Pipeline traceability, DCEA phase: for every league-table
#' intervention, whether it made it into the DCEA distributional
#' analysis and, if not, why - continuing the funnel-log tradition
#' from the 4-phase league-table funnel (R/05) into this later stage.
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param interventions_mapped Output of assign_e_indicator() (R/11)
#' @param distribution Output of build_dcea_distribution() (R/12),
#'   WEALTH-quintile rows only checked (both stratifiers share the
#'   same E-indicator mapping and D/E/F availability upstream)
#' @return One row per league-table intervention
build_dcea_inclusion_table <- function(league_table, interventions_mapped, distribution) {
  mapped_lookup <- interventions_mapped %>%
    select(intervention = intervention_en, e_indicator_id, e_indicator_source)

  data_check <- distribution %>%
    filter(.data$group_type == "wealth") %>%
    group_by(intervention) %>%
    summarise(has_missing_distributional_data = any(is.na(direct_benefit)), .groups = "drop")

  league_table %>%
    select(intervention, main_category, sub_category, icer_usd, net_dalys_full) %>%
    left_join(mapped_lookup, by = "intervention") %>%
    left_join(data_check, by = "intervention") %>%
    mutate(
      has_missing_distributional_data = coalesce(has_missing_distributional_data, TRUE),
      included_in_dcea = !is.na(e_indicator_id) & !has_missing_distributional_data,
      exclusion_reason = case_when(
        included_in_dcea ~ "Included",
        is.na(e_indicator_id) ~ "Excluded: no coverage (E-)indicator mapping found in the DCEA prep workbook",
        TRUE ~ "Excluded: missing D (prevalence) or F (opportunity-cost) data for its GBD cause"
      )
    ) %>%
    select(-has_missing_distributional_data) %>%
    arrange(desc(included_in_dcea), desc(net_dalys_full))
}

#' Write every DCEA table - distribution, per-stratifier group
#' summaries, per-stratifier equity planes, per-stratifier package
#' equity, sensitivity table, and the Table S4/S5-style supplementary
#' exports (wealth quintile only, as in the paper's own appendix) - to
#' one formatted workbook: output/tables/dcea_results.xlsx
#'
#' @param distribution Output of build_dcea_distribution() (R/12) -
#'   contains BOTH stratifiers, tagged by group_type
#' @param wealth_summary,residence_summary Output of
#'   aggregate_dcea_by_group() for group_type "wealth"/"residence" (R/12)
#' @param wealth_equity,residence_equity The `per_intervention` element
#'   of compute_equity_metrics() for each group_type (R/14)
#' @param wealth_package_equity,residence_package_equity Output of
#'   compute_package_equity() for each group_type (R/14)
#' @param sensitivity_table Output of build_dcea_sensitivity_table() (R/15)
#' @param league_table Output of build_intervention_funnel()$league_table,
#'   needed to build the Table S4/S5-style sheets
#' @param interventions_mapped Output of assign_e_indicator() (R/11),
#'   needed for the DCEA-phase inclusion/exclusion tracking sheet
#' @param f_row Output of read_dcea_prep()$f_row, needed for the
#'   Table 1-style input-data summary sheet
#' @param national_population config$dcea$national_population
#' @param wealth_pop_weights,residence_pop_weights Named numeric
#'   vectors (population share, 0-1), e.g.
#'   get_baseline_vectors(baseline_hale, "wealth"/"residence", ...)$pop_weights
#' @param output_dir config$output_tables_dir
export_dcea_tables <- function(distribution, wealth_summary, residence_summary,
                                wealth_equity, residence_equity,
                                wealth_package_equity, residence_package_equity,
                                sensitivity_table, league_table, interventions_mapped,
                                f_row, national_population,
                                wealth_pop_weights, residence_pop_weights, output_dir) {
  wb <- createWorkbook()
  write_xlsx_sheet(
    wb, "Table 1 style - population",
    build_table1_style(distribution, f_row, national_population, wealth_pop_weights, residence_pop_weights),
    freeze_col = 1
  )
  write_xlsx_sheet(
    wb, "DCEA inclusion tracking",
    prettify_dcea_names(build_dcea_inclusion_table(league_table, interventions_mapped, distribution)),
    freeze_col = 1
  )
  write_xlsx_sheet(wb, "Distribution by intervention", prettify_dcea_names(distribution), freeze_col = 4)
  write_xlsx_sheet(wb, "Wealth quintile summary", prettify_dcea_names(wealth_summary), freeze_col = 1)
  write_xlsx_sheet(wb, "Residence summary", prettify_dcea_names(residence_summary), freeze_col = 1)
  write_xlsx_sheet(
    wb, "Equity plane - wealth", prettify_dcea_names(wealth_equity), freeze_col = 1,
    decimal_cols = c("Total net health benefit (DALYs)", "Change in EDE health, per capita (years)",
                      "Inequality impact (DALYs averted-equivalent)")
  )
  write_xlsx_sheet(
    wb, "Equity plane - residence", prettify_dcea_names(residence_equity), freeze_col = 1,
    decimal_cols = c("Total net health benefit (DALYs)", "Change in EDE health, per capita (years)",
                      "Inequality impact (DALYs averted-equivalent)")
  )
  write_xlsx_sheet(
    wb, "Package equity summary",
    prettify_dcea_names(bind_rows(
      cbind(data.frame(stratifier = "wealth"), wealth_package_equity),
      cbind(data.frame(stratifier = "residence"), residence_package_equity)
    )),
    freeze_col = 1
  )
  write_xlsx_sheet(wb, "Sensitivity analysis (wealth)", prettify_dcea_names(sensitivity_table), freeze_col = 1)
  write_xlsx_sheet(
    wb, "Table S4 style - population", build_table_s4_style(distribution, league_table), freeze_col = 2
  )
  write_xlsx_sheet(
    wb, "Table S5 style - EDE rank", build_table_s5_style(wealth_equity, league_table), freeze_col = 2
  )
  save_xlsx(wb, "dcea_results", output_dir)
}
