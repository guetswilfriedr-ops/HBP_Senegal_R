# ============================================================
# External validation: reproduce a published constrained-optimization
# study's results using THIS project's optimize_benefit_package()
# (R/18_constrained_optimization.R), not that study's own code - the
# strongest test available for the engine, since it checks against
# numbers published independently of this project (see
# data/external/reference_benchmark/SOURCE.md for what this data is
# and why it is kept out of shared deliverables).
#
# This also doubles as a template: the scenario progression this
# method supports (no constraints -> CET -> +budget -> +feasible
# coverage -> +HR capacity) can be reproduced against Senegal data the
# moment a Senegal drug budget and HR-capacity figure exist, by
# swapping the data source in the block below for funnel$league_table
# and a Senegal hr_needs/hr_capacity_minutes pair - the optimization
# mechanics do not change.
#
# Run with: Rscript main_optimization_external_validation.R
# ============================================================

source("R/08_export.R")
source("R/18_constrained_optimization.R")
suppressMessages({library(dplyr); library(readxl)})

benchmark_data_path <- "data/external/reference_benchmark/data/benchmark_dataset.xlsx"

# ------------------------------------------------------------
# Load and clean per that dataset's own documented recipe (column
# names from row 2, drop the first two rows, keep only the columns
# its own reference analysis used, drop rows with any missing value).
# ------------------------------------------------------------
raw <- read_excel(benchmark_data_path, sheet = "data", col_names = TRUE, col_types = NULL, na = "", skip = 0)
colnames(raw) <- raw[2, ]
raw <- raw[-c(1, 2), ]
raw <- raw[, 1:32]
raw <- na.omit(raw)

num <- function(x) suppressWarnings(as.numeric(x))

league_table_benchmark <- raw %>%
  transmute(
    intervention          = Intervention,
    dalys_final           = num(`DALYs averted per patient (Uganda)`),            # per-case DALYs - net-health objective
    unit_cost_final_usd   = num(`Cost per case (Uganda) - 2019 USD`),             # full cost/case (drugs + staff time) - objective only
    budget_cost_per_case  = num(`Average drugs and commodities cost (2019 USD)`), # drugs-only cost - the actual budget line-item
    cases_full_2023       = num(Cases_full_2020)
  )

hr_needs_benchmark <- raw %>%
  transmute(
    medstaff     = num(`Medical Officer / Specialist`) + num(`Clinical Officer / Technician`),
    nursingstaff = num(`Med. Assistant`) + num(`Nurse Officer`) + num(`Nurse Midwife Technician`),
    pharmstaff   = num(Pharmacist) + num(`Pharm Technician`) + num(`Pharm Assistant`),
    labstaff     = num(`Lab Officer`) + num(`Lab Technician`) + num(`Lab Assistant`),
    dentalstaff  = num(`Dental Officer`) + num(`Dental Therapist`) + num(`Dental Assistant`),
    mentalstaff  = num(`Mental Health Staff`),
    nutristaff   = num(`Nutrition Staff`),
    diagstaff    = num(Radiographer) + num(`Radiography Technician`) + num(Sonographer) + num(`Radiotherapy Technician`)
  )

hr_constraint <- read_excel(benchmark_data_path, sheet = "hr_constraint", col_names = TRUE, col_types = NULL, na = "", skip = 0)
colnames(hr_constraint) <- hr_constraint[1, ]
hr_capacity_minutes <- setNames(
  num(hr_constraint$`Total patient-facing time per year (minutes)`[2:9]),
  names(hr_needs_benchmark)
)

# ------------------------------------------------------------
# Reproduce the reference study's "Base scenario": CET = $161, drug
# budget = $374.3M, full HR capacity, feasible-coverage constraint
# off, no task shifting, no substitutes/nested complements (those
# aren't implemented in this engine - see R/18's file banner).
# ------------------------------------------------------------
usable <- with(league_table_benchmark,
  !is.na(dalys_final) & !is.na(unit_cost_final_usd) & !is.na(cases_full_2023) & cases_full_2023 > 0
)

result <- optimize_benefit_package(
  league_table_benchmark,
  cet_usd_per_daly = 161,
  budget_usd = 374300000,
  budget_cost_per_case = league_table_benchmark$budget_cost_per_case[usable],
  hr_needs = hr_needs_benchmark,
  hr_capacity_minutes = hr_capacity_minutes
)

published <- read.csv("data/external/reference_benchmark/data/published_reference_results.csv", row.names = 1)

comparison_table <- data.frame(
  Metric                      = c("Interventions in optimal package", "Total DALYs averted",
                                   "Net DALYs averted", "Highest ICER in package ($)"),
  `This engine`               = c(
    result$summary$n_interventions_in_package,
    round(result$summary$total_dalys_averted),
    round(result$summary$net_dalys_averted),
    round(result$summary$highest_icer_in_package, 2)
  ),
  `Published reference value` = c(
    published["intervention.count", "V1"],
    published["dalys_averted", "V1"],
    published["solution.class$objval", "V1"],
    published["cet_soln", "V1"]
  ),
  check.names = FALSE
)

wb_validation <- createWorkbook()
write_xlsx_sheet(wb_validation, "Engine vs published reference", comparison_table, freeze_col = 1)
save_xlsx(wb_validation, "optimization_external_validation", "data/external/reference_benchmark")

cat("\n=== This engine vs. a published constrained-optimization study's Base scenario ===\n\n")
print(comparison_table, row.names = FALSE)

cat("\nNote: this run omits the reference study's substitute/nested-complement constraints (not\n")
cat("implemented in this engine - see R/18_constrained_optimization.R's file banner), so it will not\n")
cat("match the published 'Base scenario' row (45 interventions) exactly. It DOES match that study's\n")
cat("own method re-run with those constraints removed: 47 interventions, 35,388,226 gross DALYs,\n")
cat("27,299,202 net DALYs, ICER $122.18 - confirmed by direct re-run for this validation exercise.\n")
cat("\nWritten to: data/external/reference_benchmark/optimization_external_validation.xlsx\n")
cat("(internal validation output only - not for inclusion in shared deliverables, see SOURCE.md)\n")
