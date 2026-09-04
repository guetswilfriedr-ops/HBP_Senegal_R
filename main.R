# ============================================================
# Entry point
# Sources every script in R/ and runs the pipeline end to end.
# Run with: source("main.R") or Rscript main.R
# ============================================================

source("config.R")
source("R/01_import.R")
source("R/02_cleaning.R")
source("R/03_indicators.R")
source("R/04_export.R")

raw_data     <- load_raw_data(config$raw_data_path, config$sheets_to_load, config$sheet_header_row)
cleaned_data <- clean_all(raw_data)

# ------------------------------------------------------------
# Example indicators, ready to run once the workbook is in place
# ------------------------------------------------------------
top_20_cost_effective <- top_interventions_by_icer(cleaned_data[["LeagueTable_Final"]], n = 20)
burden_by_category    <- dalys_by_category(cleaned_data[["GBD_TIER3"]])

export_table(top_20_cost_effective, "top_20_cost_effective", config$output_tables_dir)
export_table(burden_by_category, "burden_by_category", config$output_tables_dir)
