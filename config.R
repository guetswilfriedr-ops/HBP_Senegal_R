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
    "LeagueTable_Final",                # the workbook's own final table

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
  output_figures_dir  = "output/figures",

  # ----------------------------------------------------------
  # DCEA (Distributional Cost-Effectiveness Analysis) parameters
  # See main_dcea.R and R/10-17_dcea_*.R. This mirrors Arnold,
  # Nkhoma & Griffin (2020) for Malawi, applied to Senegal with a
  # tiered fallback (Senegal EDS-Continue data > Malawi published
  # values > plausible WHO/regional assumption) documented cell by
  # cell in data/dcea_prep/DCEA_preparatory_data.xlsx.
  # ----------------------------------------------------------
  dcea_prep_path = "data/dcea_prep/DCEA_preparatory_data.xlsx",

  dcea = list(

    # PLACEHOLDER - verify against WHO Global Health Estimates
    # (healthy life expectancy at birth, Senegal, latest available
    # year) before treating baseline HALE figures as reliable.
    national_hale_years = 58,

    # Sourced from the workbook's own 'Population' sheet (Senegal,
    # 2023, all ages, both sexes) - see cleaned_data[["Population"]].
    # Only affects the denominator used to turn total net benefit into
    # a per-capita figure for the Atkinson/EDE calculation; the
    # league-table DALY totals themselves do not depend on it.
    national_population = 18126390,

    # PLACEHOLDER - proxied from the EDS-Continue Senegal 2023 <5-year-
    # old sample composition (Tables 10.8/10.9: 3,762 urban / 5,743
    # rural = 39.6% urban), NOT a whole-population census figure.
    # Verify against RGPH-5 (2023) before treating this as reliable.
    # Used only for the urban/rural (residence) equity dimension, since
    # unlike wealth quintiles (equal-sized by construction) the two
    # residence groups are not equally sized.
    national_urban_share = 0.396,

    # Weight given to the neonatal-mortality-derived quintile ratio
    # (vs. the mean disease-share ratio across the 14 GBD causes in
    # tab 2) when building the simplified baseline health index in
    # R/13_dcea_baseline.R. See that file's header comment for why
    # this is a simplification of Arnold et al.'s full sibling-history
    # life-table method, not a replication of it.
    baseline_mortality_weight = 0.5,

    # Atkinson inequality-aversion parameter (epsilon). 10 is Arnold
    # et al.'s own starting value for Malawi, in the absence of a
    # Senegal-specific estimate (e.g. via Robson et al.-style
    # elicitation).
    inequality_aversion_epsilon = 10,

    # Alternative epsilon values for the sensitivity analysis
    # (R/15_dcea_sensitivity.R), mirroring Arnold et al.'s Sensitivity
    # Analysis 5 (low = 2, high = 25).
    inequality_aversion_sensitivity = c("Low (e=2)" = 2, "Reference (e=10)" = 10, "High (e=25)" = 25),

    # Percentage-point shift applied to the poorest two / richest two
    # quintiles in the "unequal prevalence" and "unequal uptake"
    # sensitivity scenarios, mirroring Arnold et al.'s Sensitivity
    # Analyses 1-2 (+/-10 percentage points, split evenly across the
    # two quintiles on each side).
    sensitivity_shift_points = 10
  )
)

if (file.exists("config_local.R")) {
  source("config_local.R")
  config <- modifyList(config, config_local)
}
