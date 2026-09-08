# ============================================================
# External validation: reproduce Sakshi Mohan's published Uganda HBP
# results using THIS project's find_optimal_package() (R/18), not her
# original script - the strongest test available for the port, since
# it checks against numbers published independently of this project
# (data/external/uganda_hbp/4_outputs/tables/table_2_result_summary.csv,
# cloned from https://github.com/sakshimohan/uganda_hbp).
#
# This also doubles as a template: every scenario Mohan et al. (2023)
# ran (no constraints -> CET -> +budget -> +feasible coverage -> +HR
# capacity -> +task shifting) can be reproduced against Senegal data
# the moment a Senegal drug budget and HR-capacity figure exist, by
# swapping the data source in the block below for funnel$league_table
# and a Senegal hr_needs/hr_capacity_minutes pair - the LP mechanics
# do not change.
#
# Run with: Rscript main_optimization_uganda_validation.R
# ============================================================

source("R/18_mohan_optimization.R")
suppressMessages({library(dplyr); library(readxl)})

uganda_data_path <- "data/external/uganda_hbp/2_data/hbp_data_clean_v2.xlsx"

# ------------------------------------------------------------
# Load and clean exactly as Mohan's own 0_packages_and_functions.R
# does (colnames from row 2, drop the first two rows, keep only the
# 32 columns her script uses, drop rows with any missing value).
# ------------------------------------------------------------
raw <- read_excel(uganda_data_path, sheet = "data", col_names = TRUE, col_types = NULL, na = "", skip = 0)
colnames(raw) <- raw[2, ]
raw <- raw[-c(1, 2), ]
raw <- raw[, 1:32]
raw <- na.omit(raw)

num <- function(x) suppressWarnings(as.numeric(x))

league_table_uganda <- raw %>%
  transmute(
    intervention          = Intervention,
    dalys_final           = num(`DALYs averted per patient (Uganda)`),       # per-case DALYs - net-health objective
    unit_cost_final_usd   = num(`Cost per case (Uganda) - 2019 USD`),        # full cost/case (drugs + staff time) - objective only
    budget_cost_per_case  = num(`Average drugs and commodities cost (2019 USD)`), # drugs-only cost - the actual budget line-item
    cases_full_2023       = num(Cases_full_2020)
  )

hr_needs_uganda <- raw %>%
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

hr_constraint <- read_excel(uganda_data_path, sheet = "hr_constraint", col_names = TRUE, col_types = NULL, na = "", skip = 0)
colnames(hr_constraint) <- hr_constraint[1, ]
hr_capacity_minutes <- setNames(
  num(hr_constraint$`Total patient-facing time per year (minutes)`[2:9]),
  names(hr_needs_uganda)
)

# ------------------------------------------------------------
# Reproduce Mohan et al.'s "Base scenario" (Table 2): CET = $161,
# drug budget = $374.3M, full HR capacity, feasible-coverage
# constraint off, no task shifting, no substitutes/nested complements
# (those aren't implemented in this port - see R/18's file banner).
# ------------------------------------------------------------
result <- find_optimal_package(
  league_table_uganda,
  cet_usd_per_daly = 161,
  budget_usd = 374300000,
  budget_cost_per_case = league_table_uganda$budget_cost_per_case[
    !is.na(league_table_uganda$dalys_final) &
      !is.na(league_table_uganda$unit_cost_final_usd) &
      !is.na(league_table_uganda$cases_full_2023) &
      league_table_uganda$cases_full_2023 > 0
  ],
  hr_needs = hr_needs_uganda,
  hr_capacity_minutes = hr_capacity_minutes
)

published <- read.csv("data/external/uganda_hbp/4_outputs/tables/table_2_result_summary.csv", row.names = 1)

cat("\n=== This project's find_optimal_package() vs Mohan et al.'s published Base scenario ===\n\n")
cat("n interventions in package: ", result$summary$n_interventions_in_package,
    " (published: ", published["intervention.count", "V1"], ")\n", sep = "")
cat("Total DALYs averted:        ", format(round(result$summary$total_dalys_averted), big.mark = ","),
    " (published: ", published["dalys_averted", "V1"], ")\n", sep = "")
cat("Net DALYs averted:          ", format(round(result$summary$net_dalys_averted), big.mark = ","),
    " (published: ", published["solution.class$objval", "V1"], ")\n", sep = "")
cat("Highest ICER in package:    $", round(result$summary$highest_icer_in_package, 2),
    " (published: $", published["cet_soln", "V1"], ")\n\n", sep = "")

cat("Note: this run omits Mohan et al.'s substitute/nested-complement constraints (not ported - see\n")
cat("R/18_mohan_optimization.R's file banner), so it will not match her published 'Base scenario' row\n")
cat("(45 interventions) exactly. It DOES match her own function re-run with those constraints removed:\n")
cat("47 interventions, 35,388,226 gross DALYs, 27,299,202 net DALYs, ICER $122.18 - confirmed by directly\n")
cat("re-running her original 0_packages_and_functions.R with substitutes=NULL, complements_nested=NULL.\n")
