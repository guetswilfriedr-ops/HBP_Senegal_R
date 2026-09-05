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

    # -- League table machine: builds the intervention funnel --------
    # -- (389 interventions -> ranked league table) -------------------
    "OHT Case data",                    # master list: 389 interventions, demand (case volumes)
    "Senegal HBP Tool - Top20 Causes",  # 141 interventions linked to a Top-20-DALY GBD cause
    "id_Ratio",                         # analyst's choice of Tufts article/ratio per intervention
    "Tufts_Ratios",                     # cost-effectiveness literature database (DALYs averted/patient)
    "Tufts_Methods",                    # study-level methodology fields (time horizon, perspective, etc.)
    "Uganda HBP Tool",                  # fallback DALYs-averted/patient when no Senegal/Tufts ratio
    "OHT Int name mapping recent-old",  # bridges Senegal intervention names to Uganda's old names
    "OHT Drug supply costs",            # unit cost per case (Senegal OHT costing)

    # -- Broader HBP Senegal context (not used by the league --------
    # -- table machine yet, kept available for other analyses) ------
    "SEN_Cartography_insur_scheme",
    "Initial draft Sen",
    "OHT - GBD",
    "GBD_TIER3",
    "IHME_DATA_ALL_AGE_FINAL",
    "OHT Intervention overview",
    "OHT Avg medical personnel minut",
    "OHT Delivery channels",
    "DCP3 - GBD",
    "Population",
    "PPP"
  ),

  # Row at which the real header sits for sheets whose first
  # row(s) are a title or a merged group header rather than
  # column names (0 = header is row 1, readxl default).
  # Sheets not listed here default to 0.
  sheet_header_row = list(
    "Senegal HBP Tool - Top20 Causes" = 1,
    "Initial draft Sen"               = 2,
    "IHME_DATA_ALL_AGE_FINAL"         = 1,
    "OHT Case data"                   = 1,
    "OHT Intervention overview"       = 1,
    "OHT Drug supply costs"           = 1,
    "Uganda HBP Tool"                 = 2
  ),

  # Cost-effectiveness threshold (CET, $ per DALY averted), used to
  # convert cost into DALY-equivalent terms in the net-benefit
  # calculation. Source: 'Data sources'!B15 in the workbook. Update
  # this value here (not by re-adding the 'Data sources' sheet to
  # sheets_to_load) if the analyst revises the threshold.
  cet_usd_per_daly = 435,

  # Alternative CET scenarios for the sensitivity analysis
  # (R/09_ochalek_analysis.R), mirroring the way Ochalek et al. (2016)
  # show how the affordable package changes under different threshold
  # estimates. Senegal has no published set of alternative CET
  # estimates, so scenarios are expressed as a proportion of the
  # reference threshold above instead of alternative fixed values.
  cet_sensitivity_multipliers = c(
    "Low (50% of reference)"  = 0.5,
    "Reference"               = 1,
    "High (150% of reference)" = 1.5,
    "High (200% of reference)" = 2
  ),

  # A Tufts ratio (DALY averted per patient) is discarded as
  # implausible if its absolute value exceeds this bound, mirroring
  # the check in 'Senegal HBP Tool - Top20 Causes'!BE.
  tufts_ratio_plausibility_bound = 6,

  # Output locations
  processed_data_dir = "data/processed",
  output_tables_dir   = "output/tables",
  output_figures_dir  = "output/figures"
)

if (file.exists("config_local.R")) {
  source("config_local.R")
  config <- modifyList(config, config_local)
}
