# ============================================================
# Mohan et al. (2023)-style optimization - Stage 1 demo
#
# Runs the constrained-optimization function (R/18_mohan_optimization.R)
# against the same league table as main.R, in "Stage 1" mode: CET +
# consumables budget only, no health-workforce constraints (those
# need Senegal-specific HR-need-per-intervention and workforce-
# capacity data not yet collected - see R/18_mohan_optimization.R's
# hr_needs/hr_capacity_minutes arguments for Stage 2).
#
# This script's purpose right now is the audit check, not a
# deliverable: it demonstrates that with only a budget constraint,
# the LP optimum exactly reproduces the ICER-ranking package already
# reported in Table 6/detailed_findings.xlsx - the two methods are
# provably equivalent in that special case (Mohan et al. 2023), and
# only diverge once a workforce cadre becomes a binding constraint.
# Run with: Rscript main_optimization.R
# ============================================================

source("config.R")
source("R/01_import.R")
source("R/02_cleaning.R")
source("R/03_costs.R")
source("R/04_effectiveness.R")
source("R/05_league_table.R")
source("R/18_mohan_optimization.R")

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

# ------------------------------------------------------------
# Audit 1: no budget constraint at all. With objective = "nethealth"
# and no other constraint, the LP optimum is simply "cover every
# intervention with a positive net health benefit at the CET" - i.e.
# the same 86 interventions as Table 6, at whatever that costs.
# ------------------------------------------------------------
audit_unconstrained <- find_optimal_package(
  funnel$league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = Inf
)

# ------------------------------------------------------------
# Audit 2: budget set to exactly Table 6's core-package cost. Should
# reproduce Table 6's package exactly (same n, same total DALYs, same
# total cost) - the direct cross-check against the existing
# ICER-ranking pipeline.
# ------------------------------------------------------------
core_package_cost <- sum(funnel$league_table$total_cost_full_usd[
  funnel$league_table$included_in_package == "To be included"
], na.rm = TRUE)

audit_matched_budget <- find_optimal_package(
  funnel$league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = core_package_cost
)

# ------------------------------------------------------------
# Audit 3: a binding budget below that cutoff. Demonstrates genuine
# optimization behaviour - interventions funded in ascending-ICER
# order until the budget runs out, with (at most) one intervention
# funded at a fractional coverage share at the margin.
# ------------------------------------------------------------
audit_binding_budget <- find_optimal_package(
  funnel$league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = core_package_cost / 2
)

cat("\n=== Stage 1 audit: LP optimization vs. ICER-ranking (Table 6) ===\n\n")
cat("Table 6 (ICER-ranking) core package: n =",
    sum(funnel$league_table$included_in_package == "To be included"),
    "; cost = $", format(core_package_cost, big.mark = ","),
    "; DALYs = ", format(sum(funnel$league_table$total_dalys_full[
      funnel$league_table$included_in_package == "To be included"
    ], na.rm = TRUE), big.mark = ","), "\n\n", sep = "")

for (label in c("audit_unconstrained", "audit_matched_budget", "audit_binding_budget")) {
  res <- get(label)
  cat(label, ": n =", res$summary$n_interventions_in_package,
      "; cost = $", format(round(res$summary$budget_used_usd), big.mark = ","),
      "; DALYs = ", format(round(res$summary$total_dalys_averted), big.mark = ","),
      "; net DALYs (LP objective) = ", format(round(res$summary$net_dalys_averted), big.mark = ","),
      "; highest ICER in package = $", round(res$summary$highest_icer_in_package, 2),
      "\n", sep = "")
}
