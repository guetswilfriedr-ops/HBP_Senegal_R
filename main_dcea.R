# ============================================================
# DCEA entry point
#
# Runs the Distributional Cost-Effectiveness Analysis (DCEA) for the
# Senegal HBP league table, following Arnold, Nkhoma & Griffin (2020)
# ("Distributional impact of the Malawian Essential Health Package",
# Health Policy and Planning 35(6):646-656) applied to Senegal.
#
# Prerequisite: data/dcea_prep/DCEA_preparatory_data.xlsx must exist
# and have its D (tab 2) / E (tab 3) / F (tab 4) tables filled in -
# every row is filled as of this version, using a tiered fallback
# (Senegal EDS-Continue actual > Malawi published values > plausible
# WHO/regional assumption), colour-coded and documented cell by cell
# in that workbook's "Data tier" column. Replace tier-2/3 values with
# Senegal-sourced ones as they become available (see that workbook's
# tab 6 for how to access the underlying microdata) - nothing else in
# this script needs to change when you do.
#
# Run with: source("main_dcea.R") or Rscript main_dcea.R
# (Run main.R first, or just source this file - it sources main.R's
# dependencies itself, up to and including the league table.)
# ============================================================

source("config.R")
source("R/01_import.R")
source("R/02_cleaning.R")
source("R/03_costs.R")
source("R/04_effectiveness.R")
source("R/05_league_table.R")
source("R/09_ochalek_analysis.R")
source("R/10_dcea_import.R")
source("R/11_dcea_mapping.R")
source("R/12_dcea_distribution.R")
source("R/13_dcea_baseline.R")
source("R/14_dcea_inequality.R")
source("R/15_dcea_sensitivity.R")
source("R/16_dcea_figures.R")
source("R/17_dcea_export.R")

# ------------------------------------------------------------
# Rebuild the league table (same as main.R) - the DCEA operates on top
# of it, so it must exist before anything below can run.
# ------------------------------------------------------------
raw_data     <- load_raw_data(config$raw_data_path, config$sheets_to_load, config$sheet_header_row)
cleaned_data <- clean_all(raw_data)

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
league_table <- funnel$league_table

# ------------------------------------------------------------
# DCEA data prep: read the workbook, assign a coverage indicator to
# every intervention (manual choice if given, else a category-level
# default - see R/11_dcea_mapping.R)
# ------------------------------------------------------------
dcea_prep <- read_dcea_prep(config$dcea_prep_path)
interventions_mapped <- assign_e_indicator(dcea_prep$interventions)

cat(
  "\nE-indicator assignment: ",
  sum(interventions_mapped$e_indicator_source == "manual"), " manual, ",
  sum(interventions_mapped$e_indicator_source == "sub_category"), " by sub-category default, ",
  sum(interventions_mapped$e_indicator_source == "main_category"), " by main-category default, ",
  sum(interventions_mapped$e_indicator_source == "fallback"), " by universal fallback.\n",
  sep = ""
)

opportunity_cost_rate <- resolve_opportunity_cost_rate(dcea_prep$cet_params, config$cet_usd_per_daly)

# ------------------------------------------------------------
# Stage 1: distributional impact of every intervention
# ------------------------------------------------------------
distribution <- build_dcea_distribution(
  league_table, interventions_mapped,
  dcea_prep$d_table, dcea_prep$e_table, dcea_prep$f_row,
  opportunity_cost_rate
)
quintile_summary <- aggregate_dcea_by_quintile(distribution)

# ------------------------------------------------------------
# Stage 2: baseline HALE by quintile (simplified - see R/13)
# ------------------------------------------------------------
baseline_hale <- build_baseline_hale(
  dcea_prep$d_table,
  national_hale_years = config$dcea$national_hale_years,
  mortality_weight = config$dcea$baseline_mortality_weight
)

# ------------------------------------------------------------
# Stage 3: inequality metrics, per intervention and for "the package"
# (defined here as every intervention affordable at the reference CET,
# reusing R/09_ochalek_analysis.R's own definition of that set)
# ------------------------------------------------------------
affordability <- build_affordability_table(league_table, config$cet_usd_per_daly)
package_interventions <- affordability$table$intervention

equity <- compute_equity_metrics(
  distribution, baseline_hale, config$dcea$national_population,
  epsilon = config$dcea$inequality_aversion_epsilon
)
package_equity <- compute_package_equity(
  distribution, baseline_hale, config$dcea$national_population,
  epsilon = config$dcea$inequality_aversion_epsilon,
  interventions = package_interventions
)

cat(
  "\nPackage-level equity summary (", length(package_interventions), " interventions affordable at $",
  config$cet_usd_per_daly, "/DALY):\n",
  "  Total net health benefit: ", round(package_equity$total_net_benefit), " DALYs\n",
  "  Inequality impact:        ", round(package_equity$inequality_impact), " DALYs averted-equivalent\n",
  sep = ""
)

# ------------------------------------------------------------
# Stage 4: sensitivity analyses
# ------------------------------------------------------------
sensitivity_table <- build_dcea_sensitivity_table(
  league_table, interventions_mapped,
  dcea_prep$d_table, dcea_prep$e_table, dcea_prep$f_row, dcea_prep$cet_params,
  config$cet_usd_per_daly, baseline_hale, config$dcea$national_population,
  epsilon_reference = config$dcea$inequality_aversion_epsilon,
  epsilon_sensitivity = config$dcea$inequality_aversion_sensitivity,
  cet_multipliers = config$cet_sensitivity_multipliers,
  shift_points = config$dcea$sensitivity_shift_points,
  package_interventions = package_interventions
)

# ------------------------------------------------------------
# Figures
# ------------------------------------------------------------
equity_plane_plot <- build_equity_plane_plot(equity$per_intervention)
benefit_breakdown_full_plot <- build_benefit_breakdown_plot(quintile_summary, scenario = "full")
benefit_breakdown_realistic_plot <- build_benefit_breakdown_plot(quintile_summary, scenario = "realistic")
hale_plot <- build_hale_plot(
  baseline_hale, distribution, package_interventions,
  config$dcea$national_population, scenario = "full"
)

export_figure(equity_plane_plot, "dcea_equity_plane", config$output_figures_dir, width = 9, height = 7)
export_figure(benefit_breakdown_full_plot, "dcea_benefit_breakdown_full", config$output_figures_dir, width = 8, height = 6)
export_figure(benefit_breakdown_realistic_plot, "dcea_benefit_breakdown_realistic", config$output_figures_dir, width = 8, height = 6)
export_figure(hale_plot, "dcea_hale_by_quintile", config$output_figures_dir, width = 8, height = 6)

# ------------------------------------------------------------
# Tables
# ------------------------------------------------------------
export_dcea_tables(
  distribution, quintile_summary, equity$per_intervention,
  package_equity, sensitivity_table, league_table, config$output_tables_dir
)

cat("\nDCEA outputs written to ", config$output_tables_dir, "/dcea_results.xlsx and ",
    config$output_figures_dir, "/dcea_*.png\n", sep = "")
