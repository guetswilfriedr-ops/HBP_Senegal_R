# ============================================================
# Constrained optimization on Senegal data - Stage 1 (budget only)
#
# Runs the optimization engine (R/18_constrained_optimization.R)
# against the real Senegal league table, with a national consumables
# budget (config$consumables_budget_usd - a PROVISIONAL working value,
# not yet a confirmed Senegal figure, see config.R) as the only
# resource constraint. Health-workforce constraints are not applied
# here: Senegal-specific HR-capacity data (staff counts and
# patient-facing minutes per year, by cadre) does not exist yet - see
# R/18_constrained_optimization.R's hr_needs/hr_capacity_minutes
# arguments for how to add that once it does (Stage 2).
#
# Produces, following the standard reporting structure used in the
# constrained-optimization literature for this kind of analysis:
#   - Table 2 equivalent: scenario comparison (no budget constraint vs
#     the provisional budget), transposed metric-by-scenario layout.
#   - Table 3 equivalent: rate of inclusion by disease program.
#   - Figure 1 equivalent: consumables-budget use by disease program
#     (single-resource version - a health-workforce breakdown awaits
#     Stage 2 data).
#   - Figure 2 equivalent: marginal net DALYs averted from investing
#     an additional $1000 in the consumables budget (single-resource
#     version, same reason).
#
# Run with: Rscript main_optimization_senegal.R
# ============================================================

source("config.R")
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

scenario_results <- list(
  "No budget constraint (CET only)" = scenario_unconstrained,
  "Provisional budget ($120M)"      = scenario_budget
)

table2 <- build_scenario_comparison_table(scenario_results)
table3 <- build_program_inclusion_table(scenario_results)

# ------------------------------------------------------------
# Figure 1 equivalent: consumables-budget use by disease program,
# provisional-budget scenario. Single-resource version of the
# multi-resource chart in the literature (no HR data yet).
# ------------------------------------------------------------
budget_pkg <- scenario_budget$package %>%
  dplyr::filter(coverage_share > 1e-6) %>%
  dplyr::group_by(main_category) %>%
  dplyr::summarise(spend_usd = sum(budget_cost_incurred_usd), .groups = "drop") %>%
  dplyr::mutate(pct_of_budget = 100 * spend_usd / config$consumables_budget_usd) %>%
  dplyr::arrange(desc(pct_of_budget))

# LISER palette (see the liser-style skill): step through the Bleu,
# Cyan, and Rouge ramps rather than an arbitrary categorical palette,
# so this chart reads as one system with the rest of this project's
# LISER-aligned outputs.
liser_categorical_palette <- c(
  "#000066", "#0099FF", "#E30613", "#4A3D8B", "#00B0E9",
  "#EB4B30", "#7366A4", "#56C4EF", "#F07E5D", "#9D93C1"
)

# Shared LISER chart theme: white canvas, no border box, light horizontal
# guides only, bold LISER-blue titles - used by both figures below so the
# two read as one visual family.
liser_chart_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", color = "#000066", size = rel(1.15), margin = margin(b = 4)),
      plot.subtitle = element_text(color = "grey35", size = rel(0.85), margin = margin(b = 14)),
      plot.caption = element_text(color = "grey55", size = rel(0.68), hjust = 0, margin = margin(t = 12)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(color = "grey25", size = rel(0.85)),
      axis.text = element_text(color = "grey25", size = rel(0.85)),
      axis.text.y = element_text(face = "bold", color = "#000066"),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.35),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.margin = margin(16, 20, 12, 16)
    )
}

fig1_budget_use <- ggplot(budget_pkg, aes(x = reorder(main_category, pct_of_budget), y = pct_of_budget, fill = pct_of_budget)) +
  geom_col(width = 0.62) +
  geom_text(
    aes(label = paste0(format(round(pct_of_budget, 1), nsmall = 1), "%")),
    hjust = -0.18, size = 3.5, color = "#000066", fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_fill_gradient(low = "#7FB3E8", high = "#000066") +
  scale_y_continuous(limits = c(0, max(budget_pkg$pct_of_budget) * 1.18), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Consumables-budget use by disease program",
    subtitle = paste0(
      "Share of the provisional $", format(config$consumables_budget_usd / 1e6, big.mark = ","),
      "M consumables budget absorbed by each program in the optimal package"
    ),
    x = NULL, y = "Share of the consumables budget (%)",
    caption = "Provisional budget scenario — placeholder value, to be revised once a confirmed MSAS figure is available."
  ) +
  liser_chart_theme()

export_figure(fig1_budget_use, "optimization_fig1_budget_use_by_program", config$output_figures_dir, width = 9.5, height = 6)

# ------------------------------------------------------------
# Figure 2 equivalent: marginal net DALYs averted from an additional
# $1000 in the consumables budget. Single-resource version (no HR
# data yet, so no cadre-level bars to compute).
# ------------------------------------------------------------
scenario_plus1000 <- optimize_benefit_package(
  league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = config$consumables_budget_usd + 1000
)
marginal_value_budget <- scenario_plus1000$summary$net_dalys_averted - scenario_budget$summary$net_dalys_averted

fig2_marginal_value <- ggplot(
  data.frame(Resource = "Consumables\nbudget", Value = marginal_value_budget),
  aes(x = Resource, y = Value)
) +
  geom_col(fill = "#000066", width = 0.45) +
  geom_text(aes(label = paste0("+", format(round(Value, 2), nsmall = 2), " DALYs")), hjust = -0.12, size = 4, color = "#000066", fontface = "bold") +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, marginal_value_budget * 1.35), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Marginal value of investing $1000 in the consumables budget",
    subtitle = "Net DALYs averted gained from an additional $1000 added to the optimal package's budget",
    x = NULL, y = "Net DALYs averted",
    caption = "Health-worker-cadre resources (nurses, pharmacy staff, etc.) await Senegal-specific workforce-capacity data (Stage 2)."
  ) +
  liser_chart_theme() +
  theme(axis.text.y = element_text(face = "bold", color = "#000066", size = rel(1)))

export_figure(fig2_marginal_value, "optimization_fig2_marginal_value", config$output_figures_dir, width = 9.5, height = 4.2)

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
add_sources_sheet(wb, c("Cost-effectiveness threshold (CET)", "Constrained optimization"))
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
not_available_note <- "A completer - donnees RH Senegal non disponibles a ce stade (Stage 2)"

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
  Note = "Task-shifting scenario requires Senegal-specific health-worker-cadre time and capacity data, not yet available (Stage 2). No equivalent scenario is reported this round."
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
add_sources_sheet(wb_supp, c("Cost-effectiveness threshold (CET)", "Constrained optimization"))
save_xlsx(wb_supp, "optimization_supplementary_tables", config$output_tables_dir)

cat("\n=== Constrained optimization (Senegal, Stage 1: budget only) ===\n")
cat("Provisional consumables budget: $", format(config$consumables_budget_usd, big.mark = ","), "\n", sep = "")
cat("Marginal value of $1000 more budget:", round(marginal_value_budget, 2), "net DALYs averted\n\n")
print(table2, row.names = FALSE)
cat("\nWritten to:", file.path(config$output_tables_dir, "optimization_results.xlsx"), "\n")
cat("Supplementary tables written to:", file.path(config$output_tables_dir, "optimization_supplementary_tables.xlsx"), "\n")
cat("Figures written to:", config$output_figures_dir, "\n")
