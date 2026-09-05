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
source("R/07_indicators.R")
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
nhb_ranking_plot  <- build_nhb_ranking_plot(funnel$league_table)

export_figure(funnel_flow_plot, "funnel_flow", config$output_figures_dir, width = 11, height = 7)
export_figure(
  nhb_ranking_plot, "nhb_ranking", config$output_figures_dir,
  width = 9, height = max(6, nrow(funnel$league_table) * 0.14)
)

# ------------------------------------------------------------
# Step-by-step trace, one workbook with one sheet per step
# ------------------------------------------------------------
wb_steps <- createWorkbook()
write_xlsx_sheet(wb_steps, "1 - Top20 causes", prettify_names(funnel$step1_top20))
write_xlsx_sheet(
  wb_steps, "2 - Cost", prettify_names(funnel$step2_cost),
  currency_cols = "Unit cost ($)"
)
write_xlsx_sheet(
  wb_steps, "3 - Effectiveness evidence", prettify_names(funnel$step3_effectiveness),
  currency_cols = "Unit cost ($)",
  decimal_cols = c("DALYs averted per patient", "Study quality score")
)
save_xlsx(wb_steps, "pipeline_steps", config$processed_data_dir)

# ------------------------------------------------------------
# Main deliverable: league table + funnel log + funnel summary,
# one formatted workbook
# ------------------------------------------------------------
wb_final <- createWorkbook()
add_league_table_sheet(wb_final, funnel$league_table)
write_xlsx_sheet(wb_final, "Funnel log", prettify_names(funnel$funnel_log))
write_xlsx_sheet(wb_final, "Funnel summary", prettify_names(funnel$funnel_summary), freeze_col = 0)
save_xlsx(wb_final, "league_table_final", config$output_tables_dir)

# ------------------------------------------------------------
# Efficiency, affordability, and CET-sensitivity analysis
# See R/09_ochalek_analysis.R
# ------------------------------------------------------------
efficiency_frontier_plot <- build_efficiency_frontier_plot(funnel$league_table, config$cet_usd_per_daly)
net_benefit_curve_plot   <- build_net_benefit_curve_plot(funnel$league_table)

export_figure(efficiency_frontier_plot, "efficiency_frontier", config$output_figures_dir, width = 11, height = 6)
export_figure(net_benefit_curve_plot, "net_benefit_curve", config$output_figures_dir, width = 10, height = 6)

affordability    <- build_affordability_table(funnel$league_table, config$cet_usd_per_daly)
cet_sensitivity  <- build_cet_sensitivity_table(
  funnel$league_table, config$cet_usd_per_daly, config$cet_sensitivity_multipliers
)

wb_ochalek <- createWorkbook()
add_league_table_sheet(wb_ochalek, affordability$table, sheet_name = "Affordable at reference CET")
write_xlsx_sheet(
  wb_ochalek, "CET sensitivity", prettify_names(cet_sensitivity), freeze_col = 0,
  currency_cols = c("CET ($ per DALY averted)", "Total cost - full implementation ($)"),
  decimal_cols = c("Total DALYs averted - full implementation", "Total net DALYs averted")
)
save_xlsx(wb_ochalek, "ochalek_style_analysis", config$output_tables_dir)

# ------------------------------------------------------------
# Reference tables read directly from the source workbook
# ------------------------------------------------------------
top_20_cost_effective <- top_interventions_by_icer(cleaned_data[["LeagueTable_Final"]], n = 20)
burden_by_category    <- dalys_by_category(cleaned_data[["GBD_TIER3"]])

wb_reference <- createWorkbook()
write_xlsx_sheet(wb_reference, "Top 20 by ICER", prettify_names(top_20_cost_effective), decimal_cols = "ICER ($ per DALY averted)")
write_xlsx_sheet(wb_reference, "Burden by category", prettify_names(burden_by_category), freeze_col = 0)
save_xlsx(wb_reference, "source_workbook_reference", config$output_tables_dir)
