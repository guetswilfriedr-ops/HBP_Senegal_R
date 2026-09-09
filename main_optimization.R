# ============================================================
# Constrained optimization on Senegal data - the single entry point
# for this analysis (see R/18_constrained_optimization.R for the
# solver itself; validate_optimization_against_reference.R is a
# separate, occasional check that the solver's logic matches a
# published external application - not part of this run).
#
# Runs the optimization engine against the real Senegal league table
# under three scenarios:
#   - No budget constraint (CET only) - an upper bound: every
#     intervention with a positive net health benefit, unconstrained.
#   - Provisional budget (config$consumables_budget_usd - a working
#     value pending a confirmed Senegal figure) as the only resource
#     constraint.
#   - The same provisional budget together with health-workforce time
#     constraints by cadre. Senegal has no workforce-capacity survey of
#     its own yet, so this scenario draws health-worker time-per-case
#     and capacity figures from the published literature via the same
#     name crosswalk already used for Senegal's own effectiveness
#     fallback (R/04_effectiveness.R) - scoped to the subset of
#     interventions that crosswalk reaches (see build_hr_needs_8bucket(),
#     R/18_constrained_optimization.R).
#
# Produces, following the standard reporting structure used in the
# constrained-optimization literature for this kind of analysis:
#   - Table 2 equivalent: scenario comparison, transposed
#     metric-by-scenario layout (including per-cadre capacity used).
#   - Table 3 equivalent: rate of inclusion by disease program.
#   - Figure 1 equivalent: resource use by program - panels (a) budget
#     only, (b) budget + health-workforce constraints.
#   - Figure 2 equivalent: marginal net DALYs averted from an
#     additional $1000 per resource - same two panels.
#
# Run with: Rscript main_optimization.R
# ============================================================

source("config.R")
source("R/00_liser_style.R")
source("R/01_import.R")
source("R/02_cleaning.R")
source("R/03_costs.R")
source("R/04_effectiveness.R")
source("R/05_league_table.R")
source("R/08_export.R")
source("R/18_constrained_optimization.R")
suppressMessages(library(ggplot2))

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
# Scenarios
# ------------------------------------------------------------
scenario_unconstrained <- optimize_benefit_package(
  league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = Inf
)
scenario_budget <- optimize_benefit_package(
  league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = config$consumables_budget_usd
)

# ------------------------------------------------------------
# Budget AND health-workforce time constraints together, the full
# method this engine was built for. Senegal has no workforce-capacity
# survey yet, so health-worker time-per-case (need) and capacity
# figures are drawn from the published literature behind this
# project's external validation benchmark (main_optimization_
# external_validation.R) - the same source already used, via the same
# name crosswalk, as Senegal's own effectiveness fallback (R/04).
# build_hr_needs_8bucket() restricts the league table to the
# interventions this crosswalk actually reaches AND that pass the
# usual cost/effectiveness/case-volume filter (69 by name, 68 once
# that filter is applied - well under the 93 the budget-only scenarios
# use, since not every Senegal intervention has a same-named
# counterpart in the reference country's own tool). Every number in
# this scenario is scoped to that 68-intervention subset, not the
# full league table; replace hr_needs/hr_capacity_minutes with a
# Senegal workforce source the moment one exists, and everything here
# updates automatically.
# ------------------------------------------------------------
hr_data <- build_hr_needs_8bucket(league_table, config$raw_data_path)
hr_capacity_illustrative <- build_illustrative_hr_capacity()
n_hr_matched <- nrow(hr_data$league_table_subset)
cat("Health-workforce-constrained scenario: ", n_hr_matched, " of ", nrow(league_table),
    " league-table interventions matched to the reference workforce dataset by name.\n", sep = "")
scenario_stage2_illustrative <- optimize_benefit_package(
  hr_data$league_table_subset,
  cet_usd_per_daly = config$cet_usd_per_daly,
  budget_usd = config$consumables_budget_usd,
  hr_needs = hr_data$hr_needs,
  hr_capacity_minutes = hr_capacity_illustrative
)

scenario_results <- list(
  "No budget constraint (CET only)"       = scenario_unconstrained,
  "Provisional budget ($120M)"            = scenario_budget,
  "With health-workforce constraints"     = scenario_stage2_illustrative
)

table2 <- build_scenario_comparison_table(scenario_results)
table3 <- build_program_inclusion_table(scenario_results)

# ------------------------------------------------------------
# Figure 1: health-system resource use by program. The standard
# version of this chart (see the reference reporting structure) puts
# one bar PER RESOURCE on the x-axis - each health-worker cadre, plus
# the consumables budget - stacked and colour-coded by the disease
# program consuming that resource, panels (a)/(b) matching the two
# scenarios below. Senegal does not have health-worker-cadre data of
# its own yet, so those bars are drawn as explicit "not yet available"
# placeholders in panel (a) rather than left out - the resource axis
# stays comparable to the standard version of this chart, and the gap
# is visible rather than silently absent.
#
# Only four of the eight reference cadres appear on this axis
# (doctor/clinical officer, nursing, pharmaceutical, mental health).
# Lab, dental, nutrition and diagnostic/radiography staff are left out
# because none of the 68 crosswalk-matched interventions carries any
# recorded need for them in the reference data - they would be a fixed
# 0% in every scenario, not an optimization result, so showing them
# would misrepresent a data-coverage gap as a finding.
# ------------------------------------------------------------
resource_levels <- c(
  "Doctor/\nClinical officer", "Nursing\nstaff", "Pharmaceutical\nstaff",
  "Mental Health\nstaff", "Consumables\nbudget"
)
resource_to_cadre <- c(
  "Doctor/\nClinical officer" = "medstaff", "Nursing\nstaff" = "nursingstaff",
  "Pharmaceutical\nstaff" = "pharmstaff", "Mental Health\nstaff" = "mentalstaff"
)
# Two panels, mirroring the standard (a)/(b) layout used for this
# figure in the constrained-optimization literature: (a) the budget
# alone, (b) budget together with health-workforce time. The
# no-budget-constraint scenario is not one of the two panels here
# (it isn't in the source layout either) - it stays in Table 2/3 as a
# useful upper-bound check.
scenario_levels <- c("(a) Budget only ($120M)", "(b) Budget + health-workforce constraints")

build_program_share <- function(result, denom_usd) {
  result$package %>%
    dplyr::filter(coverage_share > 1e-6) %>%
    dplyr::group_by(main_category) %>%
    dplyr::summarise(spend_usd = sum(budget_cost_incurred_usd), .groups = "drop") %>%
    dplyr::mutate(pct = 100 * spend_usd / denom_usd) %>%
    dplyr::arrange(main_category)
}

# scenario_stage2_illustrative$package already carries the 8 HR-need
# columns (hr_data$league_table_subset was built by left-joining them
# onto the league table before solving, so optimize_benefit_package()'s
# mutate() naturally carried them through) - usable directly for the
# per-cadre, per-program breakdown below.
build_cadre_program_share <- function(cadre) {
  scenario_stage2_illustrative$package %>%
    dplyr::filter(coverage_share > 1e-6) %>%
    dplyr::group_by(main_category) %>%
    dplyr::summarise(minutes = sum(cases_covered * .data[[cadre]]), .groups = "drop") %>%
    dplyr::mutate(pct = 100 * minutes / hr_capacity_illustrative[[cadre]]) %>%
    dplyr::select(main_category, pct) %>%
    dplyr::arrange(main_category)
}

program_data <- dplyr::bind_rows(
  build_program_share(scenario_budget, config$consumables_budget_usd) %>%
    dplyr::mutate(scenario = scenario_levels[1], resource = "Consumables\nbudget"),
  build_program_share(scenario_stage2_illustrative, config$consumables_budget_usd) %>%
    dplyr::mutate(scenario = scenario_levels[2], resource = "Consumables\nbudget"),
  dplyr::bind_rows(lapply(names(resource_to_cadre), function(res) {
    build_cadre_program_share(resource_to_cadre[[res]]) %>%
      dplyr::mutate(scenario = scenario_levels[2], resource = res)
  }))
) %>%
  dplyr::mutate(
    resource = factor(resource, levels = resource_levels),
    scenario = factor(scenario, levels = scenario_levels)
  )

resource_totals <- program_data %>%
  dplyr::group_by(scenario, resource) %>%
  dplyr::summarise(total_pct = sum(pct), .groups = "drop") %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(label_y = total_pct + max(total_pct) * 0.04) %>%
  dplyr::ungroup()

# Placeholder only where data genuinely isn't available: the four HR
# resources under panel (a) (budget only, no HR constraint applied).
placeholder_data <- expand.grid(
  scenario = factor(scenario_levels[1], levels = scenario_levels),
  resource = factor(resource_levels[1:4], levels = resource_levels),
  stringsAsFactors = FALSE
)

fig1_budget_use <- ggplot() +
  geom_col(
    data = placeholder_data, aes(x = resource, y = 100),
    fill = liser_gris_light, width = 0.6
  ) +
  geom_text(
    data = placeholder_data, aes(x = resource, y = 50, label = "Data not yet\navailable"),
    size = 2.3, color = "grey40", lineheight = 0.9, fontface = "italic"
  ) +
  geom_col(
    data = program_data, aes(x = resource, y = pct, fill = main_category),
    width = 0.6, color = "white", linewidth = 0.3
  ) +
  geom_text(
    data = program_data,
    aes(x = resource, y = pct, label = ifelse(pct >= 4, paste0(round(pct, 1), "%"), "")),
    position = position_stack(vjust = 0.5), size = 2.6, color = "white", fontface = "bold"
  ) +
  geom_text(
    data = resource_totals,
    aes(x = resource, y = label_y, label = paste0(round(total_pct, 1), "%")),
    color = liser_bleu, fontface = "bold", size = 3.2
  ) +
  scale_x_discrete(limits = resource_levels, drop = FALSE) +
  scale_fill_manual(values = liser_categorical_palette, name = NULL, na.translate = FALSE) +
  scale_y_continuous(
    breaks = seq(0, 150, 25), labels = paste0(seq(0, 150, 25), "%"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  facet_wrap(~scenario, ncol = 1) +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE)) +
  labs(x = "Resource", y = "Percentage of resource required") +
  liser_chart_theme() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 8.5),
    axis.text.x = element_text(face = "bold", color = liser_bleu, size = rel(0.72)),
    strip.text = element_text(face = "bold", color = liser_bleu, size = rel(1))
  )

export_figure(fig1_budget_use, "optimization_fig1_budget_use_by_program", config$output_figures_dir, width = 9, height = 10.5)

# ------------------------------------------------------------
# Figure 2: marginal value of investing $1000 in different
# health-system resources, panels (a)/(b) matching Figure 1, same
# resource axis. Health-worker cadres are placeholders in BOTH panels
# here (unlike Figure 1): converting $1,000 into cadre time needs a
# salary figure by cadre, which this exercise does not have.
# ------------------------------------------------------------
scenario_plus1000 <- optimize_benefit_package(
  league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = config$consumables_budget_usd + 1000
)
marginal_value_budget <- scenario_plus1000$summary$net_dalys_averted - scenario_budget$summary$net_dalys_averted

scenario_stage2_plus1000 <- optimize_benefit_package(
  hr_data$league_table_subset,
  cet_usd_per_daly = config$cet_usd_per_daly,
  budget_usd = config$consumables_budget_usd + 1000,
  hr_needs = hr_data$hr_needs,
  hr_capacity_minutes = hr_capacity_illustrative
)
marginal_value_budget_stage2 <- scenario_stage2_plus1000$summary$net_dalys_averted - scenario_stage2_illustrative$summary$net_dalys_averted

marginal_data <- data.frame(
  resource = factor(rep(resource_levels[5], 2), levels = resource_levels),
  scenario = factor(scenario_levels, levels = scenario_levels),
  value = c(marginal_value_budget, marginal_value_budget_stage2)
)

# Placeholder only where data genuinely isn't available: converting
# $1,000 into cadre time needs a salary figure by cadre, not available
# for either panel, so both panels show the HR placeholders here
# (unlike Figure 1, where panel (b) has real per-cadre resource-use data).
placeholder_marginal <- expand.grid(
  scenario = factor(scenario_levels, levels = scenario_levels),
  resource = factor(resource_levels[1:4], levels = resource_levels),
  stringsAsFactors = FALSE
)

fig2_marginal_value <- ggplot() +
  geom_col(
    data = placeholder_marginal, aes(x = resource, y = max(marginal_data$value) * 1.15),
    fill = liser_gris_light, width = 0.6
  ) +
  geom_text(
    data = placeholder_marginal, aes(x = resource, y = max(marginal_data$value) * 0.55, label = "Data not yet\navailable"),
    size = 2.3, color = "grey40", lineheight = 0.9, fontface = "italic"
  ) +
  geom_col(data = marginal_data, aes(x = resource, y = value), fill = liser_bleu, width = 0.6) +
  geom_text(
    data = marginal_data,
    aes(x = resource, y = value, label = paste0("+", format(round(value, 2), nsmall = 2), " DALYs")),
    hjust = -0.1, size = 3, color = liser_bleu, fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_x_discrete(limits = rev(resource_levels), drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  facet_wrap(~scenario, ncol = 1) +
  labs(x = "Resource", y = "Net DALYs averted") +
  liser_chart_theme() +
  theme(
    axis.text.y = element_text(face = "bold", color = liser_bleu, size = rel(0.85)),
    strip.text = element_text(face = "bold", color = liser_bleu, size = rel(1))
  )

export_figure(fig2_marginal_value, "optimization_fig2_marginal_value", config$output_figures_dir, width = 9, height = 7.5)

# ------------------------------------------------------------
# Excel export: Table 2, Table 3, and per-scenario package detail
# ------------------------------------------------------------
wb <- createWorkbook()
write_xlsx_sheet(wb, "Table 2 - Scenario comparison", table2, freeze_col = 1)
write_xlsx_sheet(wb, "Table 3 - Program inclusion", table3, freeze_col = 1)
write_xlsx_sheet(
  wb, "Detail - no budget constraint", build_optimization_package_table(scenario_unconstrained), freeze_col = 1,
  currency_cols = c("ICER ($)", "Cost incurred ($)", "Budget-relevant cost incurred ($)"),
  decimal_cols = c("Coverage share solved (%)", "DALYs averted"),
  integer_cols = "Cases covered"
)
write_xlsx_sheet(
  wb, "Detail - provisional budget", build_optimization_package_table(scenario_budget), freeze_col = 1,
  currency_cols = c("ICER ($)", "Cost incurred ($)", "Budget-relevant cost incurred ($)"),
  decimal_cols = c("Coverage share solved (%)", "DALYs averted"),
  integer_cols = "Cases covered"
)
save_xlsx(wb, "optimization_results", config$output_tables_dir)

# ------------------------------------------------------------
# Supplementary tables (multi-tab workbook), mirroring the standard
# supplementary-material structure used alongside a constrained-
# optimization HBP analysis. Tables that need data Senegal does not
# have yet (health-workforce time/capacity/salaries, substitute and
# complementary intervention pairs - all Stage 2 inputs) are still
# created with the correct column structure, so the shape is ready
# the moment that data exists, but every cell is explicitly marked
# "A completer" rather than filled with a borrowed or invented number.
# ------------------------------------------------------------
not_available_note <- "A completer - donnees RH Senegal non disponibles; une estimation issue de la litterature pourrait etre utilisee en attendant"

st1 <- build_supp_table_interventions(league_table)

st2 <- data.frame(
  `Health system input`                = c(
    "Consumables budget (provisional, US$)",
    "Doctor/Clinical officer capacity (patient-facing minutes/year)",
    "Nursing capacity (patient-facing minutes/year)",
    "Pharmaceutical staff capacity (patient-facing minutes/year)",
    "Mental health staff capacity (patient-facing minutes/year)",
    "Nutrition staff capacity (patient-facing minutes/year)"
  ),
  Limit                                  = c(
    format(config$consumables_budget_usd, big.mark = ","),
    rep(not_available_note, 5)
  ),
  check.names = FALSE
)

st3 <- data.frame(
  `Cadre / calculation`                              = c(
    "Total available days per year (male)", "Total working days per year (female)",
    "Total working days per year (pregnant female)", "Working hours per day",
    "Administrative minutes per day", "Total non-admin minutes per year (male)",
    "Total non-admin minutes per year (female)", "Total non-admin minutes per year (pregnant female)"
  ),
  Value                                                = not_available_note,
  check.names = FALSE
)

st4 <- data.frame(
  `Health worker cadre`     = c("Doctor/Clinical officer", "Nursing staff", "Pharmaceutical staff", "Mental health staff", "Nutrition staff"),
  `Workforce size (male)`   = not_available_note,
  `Workforce size (female)` = not_available_note,
  `Total staff`             = not_available_note,
  `Aggregate patient-facing time per year (minutes)` = not_available_note,
  check.names = FALSE
)

st5 <- build_supp_table_outcomes(scenario_budget)

st6 <- data.frame(
  Note = "Task-shifting scenario requires Senegal-specific health-worker-cadre time and capacity data, not yet available. No equivalent scenario is reported this round."
)

st7 <- data.frame(
  Group = character(0), Interventions = character(0), Note = character(0)
)
st7 <- rbind(st7, data.frame(
  Group = "-", Interventions = "-",
  Note = "A identifier avec l'equipe technique: interventions repondant au meme besoin (substituts), a exclure du double comptage dans l'optimisation"
))

st8 <- data.frame(
  `Base intervention` = "-", Complement = "-", `Dependency (%)` = "-",
  Note = "A identifier avec l'equipe technique: interventions dont la delivrance depend d'une intervention de base (ex. supplementation delivree lors d'une consultation prenatale)",
  check.names = FALSE
)

st9 <- data.frame(
  `Health worker cadre` = c("Doctor/Clinical officer", "Nursing staff", "Pharmaceutical staff", "Mental health staff", "Nutrition staff"),
  `Monthly salary (US$)` = not_available_note,
  check.names = FALSE
)

budget_multipliers <- c(
  "50% of provisional budget"  = 0.5,
  "75% of provisional budget"  = 0.75,
  "100% of provisional budget" = 1,
  "125% of provisional budget" = 1.25,
  "150% of provisional budget" = 1.5
)
budget_scenarios <- lapply(budget_multipliers, function(m) {
  optimize_benefit_package(league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = config$consumables_budget_usd * m)
})
budget_scenarios[["No budget constraint (CET only)"]] <- scenario_unconstrained
st10 <- build_scenario_comparison_table(budget_scenarios)

wb_supp <- createWorkbook()
write_xlsx_sheet(wb_supp, "ST1 - Interventions", st1, freeze_col = 2,
  currency_cols = c("Cost per case ($)", "ICER ($/DALY averted)", "Annual consumables cost ($)"),
  decimal_cols = "DALYs averted per patient", integer_cols = "Total number of cases in need")
write_xlsx_sheet(wb_supp, "ST2 - Input constraints", st2, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST3 - Time per worker", st3, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST4 - Time per cadre", st4, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST5 - Outcomes (budget)", st5, freeze_col = 2,
  currency_cols = "Consumable expenditure required ($)", decimal_cols = "DALYs averted", integer_cols = "Total cases covered")
write_xlsx_sheet(wb_supp, "ST6 - Outcomes (task-shift)", st6, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST7 - Substitutes", st7, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST8 - Complements", st8, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST9 - Salaries by cadre", st9, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST10 - Scenarios summary", st10, freeze_col = 1)
save_xlsx(wb_supp, "optimization_supplementary_tables", config$output_tables_dir)

cat("\n=== Constrained optimization (Senegal, budget-only analysis) ===\n")
cat("Provisional consumables budget: $", format(config$consumables_budget_usd, big.mark = ","), "\n", sep = "")
cat("Marginal value of $1000 more budget:", round(marginal_value_budget, 2), "net DALYs averted\n\n")
print(table2, row.names = FALSE)
cat("\nWritten to:", file.path(config$output_tables_dir, "optimization_results.xlsx"), "\n")
cat("Supplementary tables written to:", file.path(config$output_tables_dir, "optimization_supplementary_tables.xlsx"), "\n")
cat("Figures written to:", config$output_figures_dir, "\n")
