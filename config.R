# ============================================================
# Project configuration
# Edit this file to point to your data and to choose which
# sheets to load. Nothing else in the project should contain
# hard-coded paths or parameters.
#
# To use a workbook stored elsewhere on your machine (outside
# data/raw/), do NOT edit raw_data_path below. Instead create
# config_local.R (git-ignored, see .gitignore) with:
#
#   config_local <- list(
#     raw_data_path = "C:/Users/you/Documents/hbp_senegal_data.xlsx"
#   )
#
# It is sourced automatically if present and overrides only the
# fields it defines, so the default stays valid for everyone else.
# ============================================================

config <- list(

  # Path to the source Excel workbook (default: inside the project)
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

if (file.exists("config_local.R")) {
  source("config_local.R")
  config <- modifyList(config, config_local)
}
