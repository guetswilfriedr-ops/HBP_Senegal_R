# ============================================================
# Constrained optimization on Senegal data - the single entry point
# for this analysis (see R/18_constrained_optimization.R for the
# solver itself; validate_optimization_against_reference.R is a
# separate, occasional check that the solver's logic - including the
# task-shifting mechanism below - matches a published external
# application, both its base and its task-shifting scenario).
#
# The two scenarios that drive Table 2/3 and Figures 1/2 both apply
# the same provisional budget AND health-workforce time constraints by
# cadre (Senegal has no workforce-capacity survey of its own yet, so
# health-worker time-per-case and capacity figures are drawn from the
# published literature via the same name crosswalk already used for
# Senegal's own effectiveness fallback - R/04_effectiveness.R - scoped
# to the subset of interventions that crosswalk reaches; see
# build_hr_needs_8bucket(), R/18_constrained_optimization.R). They
# differ only in whether task-shifting is allowed:
#   - Base scenario: each cadre's time need is as recorded.
#   - Task-shifting scenario: pharmaceutical-staff and nutrition-staff
#     task time is reassigned onto nursing staff (nurses take over
#     those tasks) - apply_task_shifting_to_nursing(), R/18. This is
#     the specific reassignment validated against the published
#     external benchmark (see validate_optimization_against_reference.R).
# Two further scenarios (no constraint at all; budget only, no HR) are
# kept in Table 2 as upper-bound/reference columns, over the full
# league table rather than the crosswalk-matched subset.
#
# Produces, following the standard reporting structure used in the
# constrained-optimization literature for this kind of analysis:
#   - Table 2 equivalent: scenario comparison, transposed
#     metric-by-scenario layout (including per-cadre capacity used).
#   - Table 3 equivalent: rate of inclusion by disease program, base
#     vs. task-shifting scenario.
#   - Figure 1 equivalent: resource use by program - panels (a) base
#     scenario, (b) task-shifting scenario.
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
# Reference/upper-bound scenarios, over the full league table (no HR
# constraint - kept in Table 2 only, not part of Table 3 or Figures 1/2).
# ------------------------------------------------------------
scenario_unconstrained <- optimize_benefit_package(
  league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = Inf
)
scenario_budget <- optimize_benefit_package(
  league_table, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = config$consumables_budget_usd
)

# ------------------------------------------------------------
# Base and task-shifting scenarios: budget AND health-workforce time
# constraints together, the full method this engine was built for.
# Senegal has no workforce-capacity survey yet, so health-worker
# time-per-case (need) and capacity figures are drawn from the
# published literature behind this project's external validation
# benchmark (validate_optimization_against_reference.R) - the same
# source already used, via the same name crosswalk, as Senegal's own
# effectiveness fallback (R/04). build_hr_needs_8bucket() restricts
# the league table to the interventions this crosswalk actually
# reaches AND that pass the usual cost/effectiveness/case-volume
# filter (69 by name, 68 once that filter is applied - well under the
# 93 the reference scenarios above use, since not every Senegal
# intervention has a same-named counterpart in the reference country's
# own tool). Every number in these two scenarios is scoped to that
# 68-intervention subset, not the full league table; replace
# hr_needs/hr_capacity_minutes with a Senegal workforce source the
# moment one exists, and everything here updates automatically.
# ------------------------------------------------------------
hr_data <- build_hr_needs_8bucket(league_table, config$raw_data_path)
hr_capacity_illustrative <- build_illustrative_hr_capacity()
n_hr_matched <- nrow(hr_data$league_table_subset)
cat("Health-workforce-constrained scenarios: ", n_hr_matched, " of ", nrow(league_table),
    " league-table interventions matched to the reference workforce dataset by name.\n", sep = "")

scenario_base <- optimize_benefit_package(
  hr_data$league_table_subset,
  cet_usd_per_daly = config$cet_usd_per_daly,
  budget_usd = config$consumables_budget_usd,
  hr_needs = hr_data$hr_needs,
  hr_capacity_minutes = hr_capacity_illustrative
)

# Task-shifting: pharmaceutical-staff and nutrition-staff task time
# reassigned onto nursing staff (see apply_task_shifting_to_nursing(),
# R/18_constrained_optimization.R, for why this specific pair of
# cadres and how it was validated).
hr_needs_task_shifted <- apply_task_shifting_to_nursing(hr_data$hr_needs)
scenario_task_shifting <- optimize_benefit_package(
  hr_data$league_table_subset,
  cet_usd_per_daly = config$cet_usd_per_daly,
  budget_usd = config$consumables_budget_usd,
  hr_needs = hr_needs_task_shifted,
  hr_capacity_minutes = hr_capacity_illustrative
)

scenario_results <- list(
  "No constraint (CET only)"       = scenario_unconstrained,
  "Budget only ($120M, no HR)"     = scenario_budget,
  "Base scenario"                  = scenario_base,
  "Task-shifting scenario"         = scenario_task_shifting
)

table2 <- build_scenario_comparison_table(scenario_results)
table3 <- build_program_inclusion_table(list(
  "Base scenario"           = scenario_base,
  "Task-shifting scenario"  = scenario_task_shifting
))

# ------------------------------------------------------------
# Resource axis shared by the two resource-use figures below: four of
# the eight reference cadres (doctor/clinical officer, nursing,
# pharmaceutical, mental health) plus the consumables budget. Lab,
# dental, nutrition and diagnostic/radiography staff are left out
# because none of the 68 crosswalk-matched interventions carries any
# recorded need for them in the reference data - they would be a fixed
# 0% in every scenario, not a finding, so showing them would
# misrepresent a data-coverage gap as a result.
# ------------------------------------------------------------
resource_levels <- c(
  "Doctor/\nClinical officer", "Nursing\nstaff", "Pharmaceutical\nstaff",
  "Mental Health\nstaff", "Consumables\nbudget"
)
resource_to_cadre <- c(
  "Doctor/\nClinical officer" = "medstaff", "Nursing\nstaff" = "nursingstaff",
  "Pharmaceutical\nstaff" = "pharmstaff", "Mental Health\nstaff" = "mentalstaff"
)

# ------------------------------------------------------------
# Figure: health-system resource use by program. The standard version
# of this chart (see the reference reporting structure) puts one bar
# PER RESOURCE on the x-axis - each health-worker cadre, plus the
# consumables budget - stacked and colour-coded by the disease program
# consuming that resource, panels (a) base scenario, (b) task-shifting
# scenario. Both panels have real health-worker-cadre bars (no
# placeholders): the health-workforce constraint applies in both, they
# differ only in the task-shifting reassignment. Uses the same
# resource_levels/resource_to_cadre axis defined above.
# ------------------------------------------------------------
scenario_levels <- c("(a) Base scenario", "(b) Task-shifting scenario")

build_program_share <- function(result, denom_usd) {
  result$package %>%
    dplyr::filter(coverage_share > 1e-6) %>%
    dplyr::group_by(main_category) %>%
    dplyr::summarise(spend_usd = sum(budget_cost_incurred_usd), .groups = "drop") %>%
    dplyr::mutate(pct = 100 * spend_usd / denom_usd) %>%
    dplyr::arrange(main_category)
}

# Per-cadre, per-program share of capacity used, for one scenario.
# hr_needs_used must be the SAME hr_needs matrix passed to
# optimize_benefit_package() for that scenario (aligned by row with
# hr_data$league_table_subset) - not read back off result$package,
# because the task-shifting scenario's package still carries the
# ORIGINAL (pre-shift) cadre columns from hr_data$league_table_subset;
# only the matrix actually solved over reflects the reassignment.
build_cadre_program_share <- function(result, hr_needs_used, cadre) {
  pkg <- result$package
  pkg$.cadre_minutes_percase <- hr_needs_used[[cadre]]
  pkg %>%
    dplyr::filter(coverage_share > 1e-6) %>%
    dplyr::group_by(main_category) %>%
    dplyr::summarise(minutes = sum(cases_covered * .cadre_minutes_percase), .groups = "drop") %>%
    dplyr::mutate(pct = 100 * minutes / hr_capacity_illustrative[[cadre]]) %>%
    dplyr::select(main_category, pct) %>%
    dplyr::arrange(main_category)
}

panel_hr_needs <- setNames(list(hr_data$hr_needs, hr_needs_task_shifted), scenario_levels)
panel_result <- setNames(list(scenario_base, scenario_task_shifting), scenario_levels)

program_data <- dplyr::bind_rows(
  build_program_share(scenario_base, config$consumables_budget_usd) %>%
    dplyr::mutate(scenario = scenario_levels[1], resource = "Consumables\nbudget"),
  build_program_share(scenario_task_shifting, config$consumables_budget_usd) %>%
    dplyr::mutate(scenario = scenario_levels[2], resource = "Consumables\nbudget"),
  dplyr::bind_rows(lapply(scenario_levels, function(sc) {
    dplyr::bind_rows(lapply(names(resource_to_cadre), function(res) {
      build_cadre_program_share(panel_result[[sc]], panel_hr_needs[[sc]], resource_to_cadre[[res]]) %>%
        dplyr::mutate(scenario = sc, resource = res)
    }))
  }))
) %>%
  dplyr::mutate(
    resource = factor(resource, levels = resource_levels),
    scenario = factor(scenario, levels = scenario_levels),
    # Explicit, identical factor levels across both panels - otherwise
    # each panel's own fill scale only picks up the programmes present
    # in ITS data, and patchwork's collected legend ends up with two
    # near-duplicate colour keys instead of one shared one.
    main_category = factor(main_category, levels = sort(unique(main_category)))
  )

resource_totals <- program_data %>%
  dplyr::group_by(scenario, resource) %>%
  dplyr::summarise(total_pct = sum(pct), .groups = "drop") %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(label_y = total_pct + max(total_pct) * 0.04) %>%
  dplyr::ungroup()

suppressMessages(library(patchwork))

# ------------------------------------------------------------
# Figure: health-system resource use without vs with constraints.
# Panel (a) sums, for every intervention with an ICER at or below the
# CET, the resource it would need at full (100%) target coverage - no
# capacity limit applied, so a resource can and does exceed 100%. This
# shows the size of the gap between a cost-effectiveness-only view of
# the benefit package and what the health system can actually deliver.
# Panel (b) is the base scenario's own resource use (budget + HR
# capacity jointly respected, so every bar is at most 100% by
# construction) - the same data as the base-scenario panel of the
# next figure, repeated here for direct visual comparison against the
# unconstrained gap in panel (a).
# ------------------------------------------------------------
ce_mask_hr <- hr_data$league_table_subset$icer_usd <= config$cet_usd_per_daly
ce_subset_hr <- hr_data$league_table_subset[ce_mask_hr, ]
ce_hr_needs <- hr_data$hr_needs[ce_mask_hr, ]
ce_subset_full <- league_table %>%
  dplyr::filter(!is.na(dalys_final), !is.na(unit_cost_final_usd), !is.na(cases_full_2023), cases_full_2023 > 0) %>%
  dplyr::filter(icer_usd <= config$cet_usd_per_daly)

unconstrained_data <- dplyr::bind_rows(
  build_unconstrained_budget_use(ce_subset_full, config$consumables_budget_usd) %>%
    dplyr::mutate(resource = "Consumables\nbudget"),
  dplyr::bind_rows(lapply(names(resource_to_cadre), function(res) {
    build_unconstrained_hr_use(ce_subset_hr, ce_hr_needs, hr_capacity_illustrative, resource_to_cadre[[res]]) %>%
      dplyr::mutate(resource = res)
  }))
) %>%
  dplyr::mutate(scenario = "(a) Without constraints", resource = factor(resource, levels = resource_levels))

constrained_data <- program_data %>%
  dplyr::filter(scenario == scenario_levels[1]) %>%
  dplyr::mutate(scenario = "(b) With constraints (base scenario)") %>%
  dplyr::select(main_category, pct, resource, scenario)

gap_data <- dplyr::bind_rows(unconstrained_data, constrained_data) %>%
  dplyr::mutate(
    main_category = factor(main_category, levels = sort(unique(main_category))),
    scenario = factor(scenario, levels = c("(a) Without constraints", "(b) With constraints (base scenario)"))
  )

gap_totals <- gap_data %>%
  dplyr::group_by(scenario, resource) %>%
  dplyr::summarise(total_pct = sum(pct), .groups = "drop") %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(label_y = total_pct + max(total_pct) * 0.04) %>%
  dplyr::ungroup()

build_gap_panel <- function(scenario_label, y_max) {
  pd <- gap_data %>% dplyr::filter(scenario == scenario_label)
  rt <- gap_totals %>% dplyr::filter(scenario == scenario_label)
  ggplot() +
    geom_hline(yintercept = 100, linetype = "dashed", color = liser_rouge, linewidth = 0.4) +
    geom_col(data = pd, aes(x = resource, y = pct, fill = main_category), width = 0.6, color = "white", linewidth = 0.3) +
    geom_text(data = rt, aes(x = resource, y = label_y, label = paste0(round(total_pct, 1), "%")),
              color = liser_bleu, fontface = "bold", size = 3.2) +
    scale_x_discrete(limits = resource_levels, drop = FALSE) +
    scale_fill_manual(values = liser_categorical_palette, name = NULL, na.translate = FALSE, drop = FALSE) +
    scale_y_continuous(breaks = seq(0, y_max, 50), labels = paste0(seq(0, y_max, 50), "%"),
                        limits = c(0, y_max), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Percentage of resource required", title = scenario_label) +
    liser_chart_theme() +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 8.5),
      axis.text.x = element_text(face = "bold", color = liser_bleu, size = rel(0.72)),
      plot.title = element_text(face = "bold", color = liser_bleu, size = rel(1), hjust = 0.5)
    )
}

y_max_common <- ceiling(max(gap_totals$total_pct) * 1.15 / 50) * 50
fig_gap <- (build_gap_panel(levels(gap_data$scenario)[1], y_max_common) /
              build_gap_panel(levels(gap_data$scenario)[2], y_max_common)) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

export_figure(fig_gap, "optimization_fig0_resource_gap", config$output_figures_dir, width = 9, height = 10.5)

# Built as two separate panels (not facet_wrap) and stacked with
# patchwork: facet_wrap only draws the shared x-axis category labels
# once, under the bottom panel, which left the top panel's resource
# names unlabelled. Two independent plots each keep their own full
# x-axis. The stacked-bar segments are colour-coded by programme but
# are not individually labelled (too many small segments to label
# without clutter/overlap) - only the bold total above each bar is
# labelled, since that is the number the text discusses.
suppressMessages(library(patchwork))

build_fig1_panel <- function(scenario_label) {
  pd <- program_data %>% dplyr::filter(scenario == scenario_label)
  rt <- resource_totals %>% dplyr::filter(scenario == scenario_label)
  ggplot() +
    geom_col(
      data = pd, aes(x = resource, y = pct, fill = main_category),
      width = 0.6, color = "white", linewidth = 0.3
    ) +
    geom_text(
      data = rt, aes(x = resource, y = label_y, label = paste0(round(total_pct, 1), "%")),
      color = liser_bleu, fontface = "bold", size = 3.2
    ) +
    scale_x_discrete(limits = resource_levels, drop = FALSE) +
    scale_fill_manual(values = liser_categorical_palette, name = NULL, na.translate = FALSE, drop = FALSE) +
    scale_y_continuous(
      breaks = seq(0, 150, 25), labels = paste0(seq(0, 150, 25), "%"),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(x = NULL, y = "Percentage of resource required", title = scenario_label) +
    liser_chart_theme() +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 8.5),
      axis.text.x = element_text(face = "bold", color = liser_bleu, size = rel(0.72)),
      plot.title = element_text(face = "bold", color = liser_bleu, size = rel(1), hjust = 0.5)
    )
}

fig1_budget_use <- (build_fig1_panel(scenario_levels[1]) / build_fig1_panel(scenario_levels[2])) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

export_figure(fig1_budget_use, "optimization_fig1_budget_use_by_program", config$output_figures_dir, width = 9, height = 10.5)

# ------------------------------------------------------------
# Figure 2: marginal value of investing $1000 in different
# health-system resources, panels (a)/(b) matching Figure 1, same
# resource axis. Health-worker cadres remain placeholders in both
# panels: converting $1,000 into cadre time uses each cadre's monthly
# salary (hr_salary_monthly_usd, R/18) - the reference study's own
# supplementary Table S9 figures (its own workbook has no salary data
# at all - checked directly) - the same literature source as the
# health-worker time-per-case and capacity figures above. Every cadre
# on the axis is real in both panels now; only the mechanism's
# validated limits differ by cadre (see build_hr_marginal_value(),
# R/18, and validate_optimization_against_reference.R).
# ------------------------------------------------------------
hr_workforce_size <- build_illustrative_hr_workforce_size()
marginal_cadres_fig2 <- c("medstaff", "nursingstaff", "pharmstaff", "mentalstaff")

marginal_value_base <- optimize_benefit_package(
  hr_data$league_table_subset,
  cet_usd_per_daly = config$cet_usd_per_daly,
  budget_usd = config$consumables_budget_usd + 1000,
  hr_needs = hr_data$hr_needs,
  hr_capacity_minutes = hr_capacity_illustrative
)$summary$net_dalys_averted - scenario_base$summary$net_dalys_averted

marginal_value_task_shifting <- optimize_benefit_package(
  hr_data$league_table_subset,
  cet_usd_per_daly = config$cet_usd_per_daly,
  budget_usd = config$consumables_budget_usd + 1000,
  hr_needs = hr_needs_task_shifted,
  hr_capacity_minutes = hr_capacity_illustrative
)$summary$net_dalys_averted - scenario_task_shifting$summary$net_dalys_averted

marginal_by_cadre_base <- build_hr_marginal_value(
  hr_data$league_table_subset, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = config$consumables_budget_usd,
  hr_needs = hr_data$hr_needs, hr_capacity_minutes = hr_capacity_illustrative, workforce_size = hr_workforce_size,
  salary_monthly_usd = hr_salary_monthly_usd, cadres = marginal_cadres_fig2, base_result = scenario_base
)
marginal_by_cadre_ts <- build_hr_marginal_value(
  hr_data$league_table_subset, cet_usd_per_daly = config$cet_usd_per_daly, budget_usd = config$consumables_budget_usd,
  hr_needs = hr_needs_task_shifted, hr_capacity_minutes = hr_capacity_illustrative, workforce_size = hr_workforce_size,
  salary_monthly_usd = hr_salary_monthly_usd, cadres = marginal_cadres_fig2, base_result = scenario_task_shifting
)

marginal_data <- rbind(
  data.frame(resource = resource_levels[5], scenario = scenario_levels[1], value = marginal_value_base),
  data.frame(resource = resource_levels[5], scenario = scenario_levels[2], value = marginal_value_task_shifting),
  data.frame(resource = names(resource_to_cadre), scenario = scenario_levels[1], value = marginal_by_cadre_base[resource_to_cadre[names(resource_to_cadre)]]),
  data.frame(resource = names(resource_to_cadre), scenario = scenario_levels[2], value = marginal_by_cadre_ts[resource_to_cadre[names(resource_to_cadre)]])
)
marginal_data$resource <- factor(marginal_data$resource, levels = resource_levels)
marginal_data$scenario <- factor(marginal_data$scenario, levels = scenario_levels)
rownames(marginal_data) <- NULL

fig2_marginal_value <- ggplot() +
  geom_col(data = marginal_data, aes(x = resource, y = value), fill = liser_bleu, width = 0.6) +
  geom_text(
    data = marginal_data,
    aes(x = resource, y = value, label = paste0("+", sprintf("%.2f", value), " DALYs")),
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
write_xlsx_sheet(
  wb, "Detail - base scenario", build_optimization_package_table(scenario_base), freeze_col = 1,
  currency_cols = c("ICER ($)", "Cost incurred ($)", "Budget-relevant cost incurred ($)"),
  decimal_cols = c("Coverage share solved (%)", "DALYs averted"),
  integer_cols = "Cases covered"
)
write_xlsx_sheet(
  wb, "Detail - task-shifting scenario", build_optimization_package_table(scenario_task_shifting), freeze_col = 1,
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

st5 <- build_supp_table_outcomes(scenario_base)

st6 <- build_supp_table_outcomes(scenario_task_shifting)

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
write_xlsx_sheet(wb_supp, "ST5 - Outcomes (base scenario)", st5, freeze_col = 2,
  currency_cols = "Consumable expenditure required ($)", decimal_cols = "DALYs averted", integer_cols = "Total cases covered")
write_xlsx_sheet(wb_supp, "ST6 - Outcomes (task-shift)", st6, freeze_col = 2,
  currency_cols = "Consumable expenditure required ($)", decimal_cols = "DALYs averted", integer_cols = "Total cases covered")
write_xlsx_sheet(wb_supp, "ST7 - Substitutes", st7, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST8 - Complements", st8, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST9 - Salaries by cadre", st9, freeze_col = 0)
write_xlsx_sheet(wb_supp, "ST10 - Scenarios summary", st10, freeze_col = 1)
save_xlsx(wb_supp, "optimization_supplementary_tables", config$output_tables_dir)

cat("\n=== Constrained optimization (Senegal): base and task-shifting scenarios ===\n")
cat("Provisional consumables budget: $", format(config$consumables_budget_usd, big.mark = ","), "\n", sep = "")
cat("Marginal value of $1000 more budget - base scenario:", round(marginal_value_base, 2), "net DALYs averted\n")
cat("Marginal value of $1000 more budget - task-shifting scenario:", round(marginal_value_task_shifting, 2), "net DALYs averted\n\n")
print(table2, row.names = FALSE)
cat("\nWritten to:", file.path(config$output_tables_dir, "optimization_results.xlsx"), "\n")
cat("Supplementary tables written to:", file.path(config$output_tables_dir, "optimization_supplementary_tables.xlsx"), "\n")
cat("Figures written to:", config$output_figures_dir, "\n")
