# ============================================================
# Entry point
# Sources every script in R/ and runs the pipeline end to end.
# Run with: source("main.R") or Rscript main.R
# ============================================================

source("config.R")
source("R/01_import.R")
source("R/02_cleaning.R")
source("R/03_effectiveness.R")
source("R/04_costs.R")
source("R/05_league_table.R")
source("R/06_indicators.R")
source("R/07_export.R")

raw_data     <- load_raw_data(config$raw_data_path, config$sheets_to_load, config$sheet_header_row)
cleaned_data <- clean_all(raw_data)

# ------------------------------------------------------------
# Rebuilt league table machine (see R/03-05 and README.md for the
# full logic). Produces two outputs:
#   - league_table_rebuilt: interventions with complete data, ranked
#   - intervention_funnel_log: all 389 interventions, with the stage
#     and reason for exclusion for those that did not make it in
# ------------------------------------------------------------
effectiveness_table <- build_effectiveness_table(
  cleaned_data[["Senegal HBP Tool - Top20 Causes"]][["intervention"]],
  cleaned_data[["id_Ratio"]],
  cleaned_data[["Tufts_Ratios"]],
  cleaned_data[["Uganda HBP Tool"]],
  cleaned_data[["OHT Int name mapping recent-old"]],
  config$tufts_ratio_plausibility_bound
)

cost_table <- build_cost_table(
  cleaned_data[["Senegal HBP Tool - Top20 Causes"]],
  cleaned_data[["OHT Drug supply costs"]]
)

funnel <- build_intervention_funnel(
  cleaned_data[["OHT Case data"]],
  cleaned_data[["Senegal HBP Tool - Top20 Causes"]],
  effectiveness_table,
  cost_table,
  config$cet_usd_per_daly
)

export_table(funnel$league_table, "league_table_rebuilt", config$output_tables_dir)
export_table(funnel$funnel_log, "intervention_funnel_log", config$output_tables_dir)

# Main deliverable: a single formatted .xlsx (league table + funnel
# log as a second sheet), matching the presentation of the original
# workbook's LeagueTable_Final.
export_league_table_xlsx(funnel$league_table, funnel$funnel_log, "league_table_final", config$output_tables_dir)

# ------------------------------------------------------------
# Baseline from the original workbook (sanity-check / comparison)
# ------------------------------------------------------------
top_20_cost_effective <- top_interventions_by_icer(cleaned_data[["LeagueTable_Final"]], n = 20)
burden_by_category    <- dalys_by_category(cleaned_data[["GBD_TIER3"]])

export_table(top_20_cost_effective, "original_top_20_cost_effective", config$output_tables_dir)
export_table(burden_by_category, "original_burden_by_category", config$output_tables_dir)
