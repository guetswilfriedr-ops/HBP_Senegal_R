# ============================================================
# Entry point
# Sources every script in R/ and runs the pipeline end to end.
# Run with: source("main.R") or Rscript main.R
# ============================================================

source("config.R")
source("R/01_import.R")
source("R/02_cleaning.R")
source("R/03_costs.R")
source("R/04_effectiveness.R")
source("R/05_league_table.R")
source("R/06_charts.R")
source("R/08_export.R")
source("R/09_ochalek_analysis.R")

raw_data     <- load_raw_data(config$raw_data_path, config$sheets_to_load, config$sheet_header_row)
cleaned_data <- clean_all(raw_data)

# ------------------------------------------------------------
# Intervention funnel: cost -> effectiveness -> case-volume data,
# for the interventions linked to a Top-20-DALY-burden GBD cause.
# See R/03_costs.R, R/04_effectiveness.R, R/05_league_table.R.
# ------------------------------------------------------------
cost_table <- build_cost_table(
  cleaned_data[["Senegal HBP Tool - Top20 Causes"]],
  cleaned_data[["OHT Drug supply costs"]]
)

interventions_with_cost <- cost_table$intervention[!is.na(cost_table$unit_cost_final_usd)]

effectiveness_table <- build_effectiveness_table(
  interventions_with_cost,
  cleaned_data[["id_Ratio"]],
  cleaned_data[["Tufts_Ratios"]],
  cleaned_data[["Tufts_Methods"]],
  cleaned_data[["Uganda HBP Tool"]],
  cleaned_data[["OHT Int name mapping recent-old"]],
  config$tufts_ratio_plausibility_bound
)

funnel <- build_intervention_funnel(
  cleaned_data[["OHT Case data"]],
  cleaned_data[["Senegal HBP Tool - Top20 Causes"]],
  cost_table,
  effectiveness_table,
  config$cet_usd_per_daly
)

# ------------------------------------------------------------
# Charts
# ------------------------------------------------------------
funnel_flow_plot <- build_funnel_flow_plot(funnel$funnel_summary)

export_figure(funnel_flow_plot, "funnel_flow", config$output_figures_dir, width = 11, height = 7)

# ------------------------------------------------------------
# Step-by-step trace, one workbook with one sheet per step
# ------------------------------------------------------------
wb_steps <- createWorkbook()
write_xlsx_sheet(
  wb_steps, "1 - Top20 causes", prettify_names(funnel$step1_top20),
  decimal_cols = "GBD cause, % of total DALYs"
)
write_xlsx_sheet(
  wb_steps, "2 - Cost", prettify_names(funnel$step2_cost),
  currency_cols = "Unit cost ($)",
  decimal_cols = "GBD cause, % of total DALYs"
)
write_xlsx_sheet(
  wb_steps, "3 - Effectiveness evidence", prettify_names(funnel$step3_effectiveness),
  currency_cols = "Unit cost ($)",
  decimal_cols = c("GBD cause, % of total DALYs", "DALYs averted per patient", "Study quality score")
)
write_xlsx_sheet(
  wb_steps, "4 - Case volume", prettify_names(funnel$step4_case_volume),
  currency_cols = "Unit cost ($)",
  decimal_cols = c("GBD cause, % of total DALYs", "DALYs averted per patient")
)
write_xlsx_sheet(
  wb_steps, "Category summary", prettify_names(funnel$category_summary), freeze_col = 1,
  decimal_cols = "GBD cause, % of total DALYs"
)
save_xlsx(wb_steps, "pipeline_steps", config$processed_data_dir)

# ------------------------------------------------------------
# Main deliverable: the league table on its own - every intervention
# that reached step 4, ranked by net health benefit
# ------------------------------------------------------------
wb_final <- createWorkbook()
add_league_table_sheet(wb_final, funnel$league_table)
save_xlsx(wb_final, "league_table_final", config$output_tables_dir)

# ------------------------------------------------------------
# Funnel audit trail: at which step, and why, each of the 389
# master-list interventions was included or excluded
# ------------------------------------------------------------
wb_funnel <- createWorkbook()
write_xlsx_sheet(wb_funnel, "Funnel log", prettify_names(funnel$funnel_log))
write_xlsx_sheet(wb_funnel, "Funnel summary", prettify_names(funnel$funnel_summary), freeze_col = 0)
save_xlsx(wb_funnel, "funnel_tracking", config$output_tables_dir)

# ------------------------------------------------------------
# Efficiency, affordability, and CET-sensitivity analysis,
# structured after Ochalek et al. (2016)'s Figures 5-7 and Tables
# 4/7/8. See R/09_ochalek_analysis.R
# ------------------------------------------------------------
efficiency_frontier_plot <- build_efficiency_frontier_plot(funnel$league_table, config$cet_usd_per_daly)
fig6_plot                <- build_fig6_plot(funnel$league_table)
fig7_plot                <- build_fig7_plot(funnel$league_table, config$cet_usd_per_daly)

export_figure(efficiency_frontier_plot, "efficiency_frontier", config$output_figures_dir, width = 11, height = 6)
export_figure(fig6_plot, "fig6_net_benefit_ranked", config$output_figures_dir, width = 11, height = 6)
export_figure(fig7_plot, "fig7_full_vs_realistic", config$output_figures_dir, width = 11, height = 6)

table4_icer_ranking <- build_table4_icer_ranking(funnel$league_table)
table5_nhp_ranking  <- build_table5_net_benefit_ranking(funnel$league_table)
table6_net_benefit  <- build_table6_net_benefit_summary(funnel$league_table, config$cet_usd_per_daly)
budget_reallocation <- build_budget_reallocation_table(funnel$league_table, config$cet_usd_per_daly)
table8_sensitivity <- build_table8_ehp_scale_sensitivity(
  funnel$league_table, config$cet_usd_per_daly, config$cet_sensitivity_multipliers
)

wb_findings <- createWorkbook()
write_xlsx_sheet(
  wb_findings, "Table 4 - ICER ranking", table4_icer_ranking, freeze_col = 2,
  currency_cols = c("Total cost (full implementation) [$]", "Cumulative cost [$]"),
  decimal_cols = c("ICER [$]", "DALYs averted per $1,000", "Total DALYs averted (full implementation)")
)
write_xlsx_sheet(
  wb_findings, "Table 5 - NHB ranking", table5_nhp_ranking, freeze_col = 2,
  currency_cols = c("Total cost (full implementation) [$]", "Cumulative cost [$]"),
  decimal_cols = c(
    "ICER [$]", "DALYs averted per $1,000",
    "Total DALYs averted (full implementation)", "Net DALYs averted (full implementation)"
  )
)
write_xlsx_sheet(
  wb_findings, "Table 6 - Net benefit summary", table6_net_benefit, freeze_col = 2,
  currency_cols = c(
    "Total cost (full implementation) [$]", "Cumulative cost (full implementation) [$]",
    "Total cost (realistic implementation) [$]", "Cumulative cost (realistic implementation) [$]",
    "$ value to the health system of implementation"
  ),
  decimal_cols = c(
    "ICER [$]", "DALYs averted per $1,000", "Implementation level (%)",
    "Total DALYs averted (full implementation)", "Total DALYs averted (realistic implementation)",
    "Net DALYs averted (full implementation)", "Net DALYs averted (realistic implementation)",
    "Difference in net DALYs averted"
  )
)
write_xlsx_sheet(
  wb_findings, "Table 7 - Budget reallocation", budget_reallocation$additional, freeze_col = 2,
  currency_cols = c(
    "Total cost (realistic implementation) [$]",
    "Cumulative cost (additional interventions, realistic implementation) [$]"
  ),
  decimal_cols = c(
    "ICER [$]", "DALYs averted per $1,000", "Implementation level (%)",
    "Total DALYs averted (realistic implementation)"
  )
)
write_xlsx_sheet(
  wb_findings, "Table 8 - EHP scale sensitivity", table8_sensitivity, freeze_col = 0,
  currency_cols = c(
    "How much can Senegal afford to pay to avert a DALY? [$]",
    "Full implementation: total spend [$]", "Realistic implementation: total spend [$]",
    "Money left in the budget [$]", "Extended package: budget [$]"
  ),
  decimal_cols = c(
    "Full implementation: total DALYs averted", "Realistic implementation: total DALYs averted",
    "Max DALYs from moving realistic to full implementation",
    "Extended package: additional DALYs averted from the underspend"
  )
)
save_xlsx(wb_findings, "detailed_findings", config$output_tables_dir)
