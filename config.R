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

  # Sheets actually used by the pipeline, grouped by purpose.
  # The workbook has 34 sheets in total; sheets that are pure
  # bibliographic references (Tufts CE literature), other-country
  # comparators (Uganda/Zambia/Malawi), or section dividers are
  # intentionally left out. See README.md for the full rationale.
  sheets_to_load = c(

    # -- HBP intervention list & prioritization (Senegal) --------
    "SEN_Cartography_insur_scheme",
    "Senegal HBP Tool - Top20 Causes",
    "Initial draft Sen",
    "LeagueTable_Final",
    "id_Ratio",
    "OHT - GBD",
    "OHT Int name mapping recent-old",

    # -- Burden of disease (GBD/IHME, Senegal) -------------------
    "GBD_TIER3",
    "IHME_DATA_ALL_AGE_FINAL",

    # -- Costing engine (OHT, 2023-2028 horizon) -----------------
    "OHT Case data",
    "OHT Intervention overview",
    "OHT Avg medical personnel minut",
    "OHT Delivery channels",
    "OHT Drug supply costs",

    # -- Cost-effectiveness reference (DCP3) ---------------------
    "DCP3 - GBD",

    # -- Demographic & macroeconomic parameters (Senegal) --------
    "Population",
    "PPP"
  ),

  # Row at which the real header sits for sheets whose first
  # row(s) are a title or a merged group header rather than
  # column names (0 = header is row 1, readxl default).
  # Sheets not listed here default to 0.
  sheet_header_row = list(
    "Senegal HBP Tool - Top20 Causes" = 2,
    "Initial draft Sen"               = 2,
    "IHME_DATA_ALL_AGE_FINAL"         = 1,
    "OHT Case data"                   = 1,
    "OHT Intervention overview"       = 1,
    "OHT Drug supply costs"           = 1
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
