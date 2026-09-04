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

raw_data     <- load_raw_data(config$raw_data_path, config$sheets_to_load)
cleaned_data <- clean_all(raw_data)

# Indicators and exports are added here as they are implemented, e.g.:
# coverage <- compute_coverage(cleaned_data)
# export_table(coverage, "coverage", config$output_tables_dir)
