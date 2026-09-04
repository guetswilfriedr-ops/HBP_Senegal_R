# ============================================================
# Project configuration
# Edit this file to point to your data and to choose which
# sheets to load. Nothing else in the project should contain
# hard-coded paths or parameters.
# ============================================================

config <- list(

  # Path to the source Excel workbook
  raw_data_path = "data/raw/hbp_senegal_data.xlsx",

  # Only the sheets actually used by the pipeline are loaded.
  # Add or remove sheet names here as the analysis evolves.
  sheets_to_load = c(
    "demography",
    "morbidity",
    "financing"
  ),

  # Output locations
  processed_data_dir = "data/processed",
  output_tables_dir   = "output/tables",
  output_figures_dir  = "output/figures"
)
