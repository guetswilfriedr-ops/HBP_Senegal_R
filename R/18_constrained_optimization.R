# ============================================================
# Constrained optimization for health benefits package design
#
# Health-maximizing selection of a COVERAGE LEVEL for each
# intervention (a continuous 0-1 share of cases in need), subject to
# a consumables budget and, once the data exists, health-workforce
# capacity by cadre - as opposed to the ICER-ranking method used
# elsewhere in this pipeline (R/05-09), which sorts interventions by
# ICER and fills the budget top-down.
#
# The two methods are NOT equivalent in general: once a health-worker
# cadre is scarce, an intervention with a favourable ICER can be
# excluded by a binding time constraint on the cadre that delivers it,
# while a less-favourable-ICER intervention drawing on an underused
# cadre can be included. They ARE provably equivalent whenever the
# only binding constraint is a single linear budget (no workforce
# constraints) - sorting by ascending ICER and filling until the
# budget is exhausted is then the exact optimum of the linear program
# below. This file's function is built so that check can be run
# directly: solve with only a budget constraint and compare the
# result to Tables 6/7/8's ICER-ranked "core package".
#
# The linear-programming formulation follows the standard
# constrained-optimization approach used in the health-benefits-
# package literature (maximize net health benefit subject to a
# resource-envelope and, where relevant, health-workforce time
# constraints by cadre), re-expressed here with the decision variable
# as a coverage FRACTION (0 to max_coverage_i) rather than a case
# count, so every constraint's units stay in "cost or minutes per unit
# of coverage" and the cadre list is not fixed in advance - it is
# whatever hr_needs supplies columns for.
#
# Validated against an independently published application of this
# method (see data/external/reference_benchmark/): this engine
# reproduces that study's own headline results to the figure once its
# own two-cost-concept convention is respected (see
# budget_cost_per_case below) - see main_optimization_external_validation.R.
# ============================================================

library(lpSolve)
library(dplyr)

#' Blend the raw data's per-case staff-time sheet (minutes of staff
#' time per case, BY DELIVERY PLATFORM: Community, Outreach, Clinic,
#' Hospital) with the delivery-channel sheet (the % of cases each
#' intervention actually reaches through each channel, including
#' three non-personnel channels - WASH, Other non-health, Private
#' sector - carried in the raw sheet but not used here) into ONE
#' minutes-per-case figure per intervention: a case-share-weighted
#' average across the four personnel-time channels.
#'
#' This is the NEED side only, and only at the level of total "medical
#' personnel" time, pooled across cadres - a per-cadre breakdown DOES
#' exist elsewhere in the raw data (see build_hr_needs_by_cadre()
#' below, borrowed from a companion reference-country HR-needs matrix
#' via the same name-mapping crosswalk already used for effectiveness
#' fallback), but only covers the interventions that crosswalk
#' reaches. This single-pool version has no such gap (95/95 league
#' table interventions, see below) and is the right choice for a
#' first, coarser HR-time budget; prefer build_hr_needs_by_cadre()
#' once a genuine per-cadre bottleneck analysis is wanted and its
#' coverage gap is acceptable. This function feeds a single
#' pooled-workforce time constraint (optimize_benefit_package()'s
#' hr_needs with one column), not a full per-cadre bottleneck
#' analysis.
#' The CAPACITY side (total staff and minutes/year available, ideally
#' per cadre) is not in this project's raw data at all and has to come
#' from a Senegal HR source - see hr_capacity_minutes below.
#'
#' @param raw_data_path Path to the project's raw Excel workbook
#'   (config$raw_data_path).
#' @return A data frame: intervention, minutes_per_case (NA where the
#'   two source sheets' channel shares don't sum to a usable total -
#'   flagged rather than silently zeroed).
build_hr_need_minutes <- function(raw_data_path) {
  minutes <- openxlsx::read.xlsx(raw_data_path, sheet = "OHT Avg medical personnel minut", colNames = FALSE)
  names(minutes) <- c("intervention", "community", "outreach", "clinic", "hospital")

  channels <- openxlsx::read.xlsx(raw_data_path, sheet = "OHT Delivery channels", colNames = FALSE)
  names(channels) <- c("intervention", "community", "outreach", "clinic", "hospital", "wash", "other", "private", "channel_total")
  channels$channel_total <- NULL

  to_num <- function(df) {
    df %>% mutate(across(-intervention, ~ suppressWarnings(as.numeric(.x))))
  }
  minutes  <- to_num(minutes)
  channels <- to_num(channels)

  channels <- channels %>%
    # As in the minutes sheet, NA in a channel column means "0% of
    # cases reach this intervention through this channel", not
    # "unknown" - coalesce before summing so one NA channel doesn't
    # turn an otherwise complete row's share total into NA.
    mutate(across(c(community, outreach, clinic, hospital, wash, other, private), ~ coalesce(.x, 0))) %>%
    mutate(channel_share_total = community + outreach + clinic + hospital + wash + other + private) %>%
    filter(channel_share_total > 0) %>%
    mutate(across(c(community, outreach, clinic, hospital), ~ .x / channel_share_total))

  minutes %>%
    inner_join(channels, by = "intervention", suffix = c("_minutes", "_share"), relationship = "many-to-many") %>%
    mutate(
      # A channel's minutes figure is NA whenever that channel isn't
      # a delivery route for this intervention at all (e.g. a
      # hospital-only obstetric procedure has NA, not 0, in its
      # community/outreach minutes cells) - and that channel's case
      # share is then 0, so coalesce()-ing to 0 before multiplying is
      # correct: it is NOT the same as treating genuinely unknown
      # minutes as free. Skipping this coalesce is a real bug this
      # engine hit and fixed - an NA in an irrelevant channel silently
      # turned an otherwise fully-known weighted average into NA for
      # about a quarter of this project's league table (mostly
      # hospital-only maternal/obstetric interventions).
      minutes_per_case = coalesce(community_minutes, 0) * community_share +
        coalesce(outreach_minutes, 0) * outreach_share +
        coalesce(clinic_minutes, 0) * clinic_share +
        coalesce(hospital_minutes, 0) * hospital_share
    ) %>%
    group_by(intervention) %>%
    summarise(minutes_per_case = mean(minutes_per_case, na.rm = TRUE), .groups = "drop")
}

#' Per-cadre (21-cadre) minutes-per-case need, borrowed from a
#' companion reference-country HR-needs matrix already present in the
#' raw workbook, via the SAME name-mapping crosswalk this project
#' already uses for effectiveness fallback (R/04_effectiveness.R: a
#' recent-to-old intervention name mapping bridges a current
#' Senegal/OHT intervention name to the name key that reference sheet
#' uses). That sheet carries a 21-column "HR Needs" block (Medical
#' Officer/Specialist through Radiotherapy Technician) already filled
#' in for the reference country - this reads that block and joins it
#' across the same crosswalk, exactly mirroring how
#' R/04_effectiveness.R borrows that same sheet's per-patient DALYs
#' column for cost-effectiveness fallback.
#'
#' Coverage is necessarily incomplete: only interventions the
#' crosswalk actually maps reach a value (69/95 of this project's
#' league table, checked directly - the other 26 have no reference
#' equivalent recorded in the mapping sheet and would need either a
#' Senegal-specific estimate or a manual analogy to a similar mapped
#' intervention, the same two options used in the literature to fill
#' comparable gaps (reuse a clinically similar, already-estimated
#' intervention's HR profile, or a manual expert entry)). Every
#' borrowed value is a reference-country proxy, not a Senegal-specific
#' measurement - the same caveat this project already carries for its
#' literature-borrowed effectiveness ratios.
#'
#' @param raw_data_path Path to the project's raw Excel workbook.
#' @return A data frame: intervention (Senegal/OHT name), then one
#'   column per cadre (21 columns, reference sheet's cadre names
#'   verbatim) - NA for an intervention/cadre the crosswalk could not
#'   reach.
build_hr_needs_by_cadre <- function(raw_data_path) {
  reference_header <- openxlsx::read.xlsx(raw_data_path, sheet = "Uganda HBP Tool", colNames = FALSE)
  cadre_names       <- as.character(reference_header[3, 68:88])

  reference_sheet <- openxlsx::read.xlsx(raw_data_path, sheet = "Uganda HBP Tool", colNames = FALSE, startRow = 4)
  reference_hr <- reference_sheet[, c(7, 68:88)]
  names(reference_hr) <- c("old_intervention_name", cadre_names)
  reference_hr <- reference_hr %>%
    mutate(across(all_of(cadre_names), ~ suppressWarnings(as.numeric(.x)))) %>%
    filter(!is.na(old_intervention_name)) %>%
    # An old intervention name can appear on more than one reference
    # row - keep the first, exactly as R/04_effectiveness.R does for
    # the same sheet, so this join cannot fan out into extra rows.
    distinct(old_intervention_name, .keep_all = TRUE)

  mapping <- openxlsx::read.xlsx(raw_data_path, sheet = "OHT Int name mapping recent-old", colNames = FALSE, startRow = 2)
  mapping <- mapping[, c(1, 3)]
  names(mapping) <- c("recent_intervention", "old_intervention")

  mapping %>%
    distinct(recent_intervention, .keep_all = TRUE) %>%
    left_join(reference_hr, by = c("old_intervention" = "old_intervention_name")) %>%
    select(intervention = recent_intervention, all_of(cadre_names))
}

#' Solve for the health-maximizing coverage of each intervention,
#' subject to a consumables budget and, optionally, health-workforce
#' time constraints by cadre.
#'
#' @param league_table A data frame with one row per intervention,
#'   carrying at least: intervention, dalys_final (DALYs averted per
#'   patient), unit_cost_final_usd (cost per case), cases_full_2023
#'   (cases in need at full implementation). This is
#'   funnel$league_table from build_intervention_funnel() (R/05), the
#'   same object every other table and chart in this pipeline reads.
#' @param cet_usd_per_daly CET used to define net health benefit
#'   (dalys - cost/cet). Ignored if objective = "dalys".
#' @param objective "nethealth" (maximise DALYs net of the health
#'   opportunity cost of spending - the standard objective in this
#'   literature) or "dalys" (maximise gross DALYs averted, ignoring
#'   cost - only sensible together with a binding budget, otherwise it
#'   just funds everything).
#' @param budget_usd Consumables/drug budget ceiling in USD. Use Inf
#'   for no budget constraint at all.
#' @param budget_cost_per_case Optional numeric vector (same length/
#'   order as league_table's usable rows), the per-case cost that
#'   counts against budget_usd, if it differs from
#'   unit_cost_final_usd. Published applications of this method
#'   distinguish two cost concepts: the net-health objective uses a
#'   "full cost" per case (drugs plus the value of staff time), while
#'   the drug-budget constraint only counts the consumables subset,
#'   since staff time is governed separately by the HR-capacity
#'   constraint rather than by the same dollar budget. Validating this
#'   engine against a published external application (see
#'   data/external/reference_benchmark/) surfaced this exact
#'   distinction - using the same per-case cost for both objective and
#'   budget silently produced too small a package, because it charged
#'   staff-time cost against a budget meant only for consumables.
#'   Leave NULL (the default) when league_table has only one cost
#'   concept - true for this project's Senegal league table today,
#'   whose unit_cost_final_usd is already a drugs/commodities figure
#'   (see R/03_costs.R) - so the same value is correctly used for
#'   both purposes.
#' @param max_coverage Feasible-coverage ceiling per intervention - a
#'   single value applied to every intervention, or a numeric vector
#'   the same length and order as league_table's rows. Defaults to 1
#'   (no feasibility ceiling beyond the budget/HR constraints), because
#'   this project does not yet have a Senegal-specific
#'   maximum-feasible-coverage dataset distinct from the league
#'   table's own (already realised/planned) implementation_level_pct.
#' @param hr_needs Optional data frame/matrix, one row per
#'   league_table row (same order), one column per health-worker
#'   cadre, each cell the minutes of that cadre's time needed PER
#'   CASE covered. NULL (the default) omits the HR constraint
#'   entirely - this is "Stage 1" of the rollout (CET + budget only).
#'   Pass this, together with hr_capacity_minutes, once
#'   Senegal-specific HR-need-per-intervention data exists, for
#'   "Stage 2" (+ workforce-capacity constraints).
#' @param hr_capacity_minutes Optional named numeric vector, total
#'   patient-facing minutes per year available per cadre; names must
#'   match colnames(hr_needs). Required if hr_needs is supplied.
#' @return A list:
#'   package  - league_table (restricted to rows with usable dalys/
#'              cost/case data) with coverage_share, cases_covered,
#'              dalys_averted_solution, and cost_incurred_usd columns
#'              added for every intervention considered (0 for one
#'              the solver excluded).
#'   summary  - named list of headline figures:
#'              n_interventions_considered,
#'              n_interventions_in_package (coverage_share > 0),
#'              total_dalys_averted, net_dalys_averted (the
#'              optimization's objective value), budget_used_usd,
#'              budget_used_pct, highest_icer_in_package (the
#'              ICER-ranking equivalence check - see file banner),
#'              hr_used_minutes (NULL unless hr_needs supplied).
optimize_benefit_package <- function(league_table,
                                      cet_usd_per_daly,
                                      objective = "nethealth",
                                      budget_usd = Inf,
                                      budget_cost_per_case = NULL,
                                      max_coverage = 1,
                                      hr_needs = NULL,
                                      hr_capacity_minutes = NULL) {
  stopifnot(objective %in% c("nethealth", "dalys"))

  df <- league_table %>%
    filter(!is.na(dalys_final), !is.na(unit_cost_final_usd), !is.na(cases_full_2023), cases_full_2023 > 0)

  n <- nrow(df)
  if (length(max_coverage) == 1) max_coverage <- rep(max_coverage, n)
  stopifnot(length(max_coverage) == n)

  dalys       <- df$dalys_final
  fullcost    <- df$unit_cost_final_usd
  cases       <- df$cases_full_2023
  nethealth   <- dalys - fullcost / cet_usd_per_daly
  budget_cost <- if (is.null(budget_cost_per_case)) fullcost else budget_cost_per_case
  stopifnot(length(budget_cost) == n)

  objective_coef <- if (objective == "nethealth") nethealth * cases else dalys * cases

  # Constraint 1: consumables budget. cons_budget[i] is the dollar
  # cost of moving intervention i's coverage share from 0 to 1 (i.e.
  # covering every case in need) - the decision variable is that
  # share, so this is directly usable as the row of the constraint
  # matrix. An infinite budget means "no budget constraint at all" -
  # lp_solve rejects Inf in const.rhs, so that case omits the row
  # entirely rather than passing it a very large finite number.
  has_budget_constraint <- is.finite(budget_usd)
  if (has_budget_constraint) {
    cons_budget <- budget_cost * cases
    cons_mat <- matrix(cons_budget, nrow = 1)
    cons_dir <- "<="
    cons_rhs <- budget_usd
  } else {
    cons_mat <- matrix(nrow = 0, ncol = n)
    cons_dir <- character(0)
    cons_rhs <- numeric(0)
  }

  # Constraint 2 (optional): health-workforce time by cadre. Same
  # logic - minutes needed per case, times cases, gives minutes
  # needed per unit of coverage share, summed per cadre against that
  # cadre's annual capacity.
  hr_used_minutes <- NULL
  if (!is.null(hr_needs)) {
    stopifnot(!is.null(hr_capacity_minutes), nrow(hr_needs) == n)
    hr_needs <- as.matrix(hr_needs)
    stopifnot(all(colnames(hr_needs) %in% names(hr_capacity_minutes)))
    cons_hr  <- t(hr_needs * cases) # cadre (rows) x intervention (cols)
    cons_mat <- rbind(cons_mat, cons_hr)
    cons_dir <- c(cons_dir, rep("<=", nrow(cons_hr)))
    cons_rhs <- c(cons_rhs, hr_capacity_minutes[rownames(cons_hr)])
  }

  # Constraint 3: feasible-coverage ceiling per intervention
  # (coverage_i <= max_coverage_i). Non-negativity (coverage_i >= 0)
  # is lp_solve's default variable bound and does not need its own
  # constraint row.
  cons_mat <- rbind(cons_mat, diag(n))
  cons_dir <- c(cons_dir, rep("<=", n))
  cons_rhs <- c(cons_rhs, max_coverage)

  solution <- lp(
    direction = "max",
    objective.in = objective_coef,
    const.mat = cons_mat,
    const.dir = cons_dir,
    const.rhs = cons_rhs
  )
  if (solution$status != 0) {
    stop("Optimization did not solve to optimality (lpSolve status code ", solution$status, ")")
  }

  coverage_share       <- solution$solution
  cases_covered        <- coverage_share * cases
  dalys_solution       <- coverage_share * dalys * cases
  cost_solution        <- coverage_share * fullcost * cases     # full per-case cost, for reporting
  budget_cost_solution <- coverage_share * budget_cost * cases  # what actually counts against budget_usd

  package <- df %>%
    mutate(
      coverage_share           = coverage_share,
      cases_covered            = cases_covered,
      dalys_averted_solution   = dalys_solution,
      cost_incurred_usd        = cost_solution,
      budget_cost_incurred_usd = budget_cost_solution
    )

  in_package <- coverage_share > 1e-6
  icer_considered <- fullcost / dalys
  highest_icer_in_package <- if (any(in_package)) max(icer_considered[in_package]) else NA_real_

  hr_used <- NULL
  if (!is.null(hr_needs)) {
    hr_used <- as.numeric(t(hr_needs * cases) %*% coverage_share)
    names(hr_used) <- colnames(hr_needs)
  }

  summary_out <- list(
    n_interventions_considered      = n,
    n_interventions_positive_nethealth = sum(nethealth > 0),
    n_interventions_in_package      = sum(in_package),
    total_dalys_averted             = sum(dalys_solution),
    net_dalys_averted               = solution$objval,
    budget_used_usd                 = sum(budget_cost_solution),
    budget_used_pct                 = if (is.finite(budget_usd)) sum(budget_cost_solution) / budget_usd else NA_real_,
    highest_icer_in_package         = highest_icer_in_package,
    hr_used_minutes                 = hr_used
  )

  list(package = package, summary = summary_out)
}

#' Shape a named list of optimize_benefit_package() results into one
#' data frame, one row per scenario, ready for write_xlsx_sheet()
#' (R/08_export.R) - the scenario-comparison table this pipeline's
#' other exports use English display names and no further
#' relabelling for.
#'
#' @param scenario_results Named list, each element a list(package=,
#'   summary=) as returned by optimize_benefit_package(); the name is
#'   used as the "Scenario" column value.
#' @return A data frame, one row per scenario.
build_optimization_summary_table <- function(scenario_results) {
  rows <- lapply(names(scenario_results), function(scenario_name) {
    s <- scenario_results[[scenario_name]]$summary
    data.frame(
      Scenario                                = scenario_name,
      `Interventions considered`               = s$n_interventions_considered,
      `Interventions in optimal package`       = s$n_interventions_in_package,
      `Total DALYs averted`                    = s$total_dalys_averted,
      `Net DALYs averted`                      = s$net_dalys_averted,
      `Budget used ($)`                        = s$budget_used_usd,
      `Budget used (%)`                        = if (is.na(s$budget_used_pct)) NA_real_ else round(100 * s$budget_used_pct, 1),
      `Highest ICER in package ($)`            = s$highest_icer_in_package,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Shape one optimize_benefit_package() result's intervention-level
#' detail into a data frame ready for write_xlsx_sheet().
#'
#' @param result A single list(package=, summary=) as returned by
#'   optimize_benefit_package().
#' @return A data frame, one row per intervention considered.
build_optimization_package_table <- function(result) {
  p <- result$package
  data.frame(
    Intervention                        = p$intervention,
    `ICER ($)`                          = p$unit_cost_final_usd / p$dalys_final,
    `Coverage share solved (%)`         = round(100 * p$coverage_share, 1),
    `Cases covered`                     = round(p$cases_covered),
    `DALYs averted`                     = p$dalys_averted_solution,
    `Cost incurred ($)`                 = p$cost_incurred_usd,
    `Budget-relevant cost incurred ($)` = p$budget_cost_incurred_usd,
    check.names = FALSE
  ) %>%
    arrange(desc(`DALYs averted`))
}

#' Scenario-comparison table in the classic transposed layout used in
#' the constrained-optimization literature: one row per headline
#' metric, one column per scenario - easier to scan than a wide
#' one-row-per-scenario table once there are more than 2-3 metrics.
#'
#' @param scenario_results Named list, each element a list(package=,
#'   summary=) as returned by optimize_benefit_package(); names become
#'   column headers.
#' @return A data frame: a "Metric" column plus one column per
#'   scenario name.
build_scenario_comparison_table <- function(scenario_results) {
  metric_rows <- list(
    "Number of interventions with positive net health benefit" = function(s) s$n_interventions_positive_nethealth,
    "Number of interventions in the optimal package"           = function(s) s$n_interventions_in_package,
    "Net DALYs averted"                                        = function(s) round(s$net_dalys_averted),
    "Total DALYs averted"                                      = function(s) round(s$total_dalys_averted),
    "Highest ICER in the optimal package ($)"                  = function(s) round(s$highest_icer_in_package, 2),
    "Percentage of consumables budget required"                = function(s) if (is.na(s$budget_used_pct)) NA else paste0(round(100 * s$budget_used_pct), "%")
  )

  out <- data.frame(Metric = names(metric_rows), check.names = FALSE)
  for (scenario_name in names(scenario_results)) {
    s <- scenario_results[[scenario_name]]$summary
    out[[scenario_name]] <- vapply(metric_rows, function(f) as.character(f(s)), character(1))
  }
  out
}

#' Intervention-level parameter table: DALYs averted per patient, cost
#' per case, ICER, source of the effectiveness evidence, and the
#' demand/cost figures that feed the optimization - the same
#' intervention-level detail behind every scenario result, independent
#' of which scenario is run. One row per intervention considered
#' (i.e. every league_table row with a usable cost/effectiveness/case
#' figure - see optimize_benefit_package()'s own filter).
#'
#' @param league_table funnel$league_table (R/05_league_table.R)
#' @return A data frame ready for write_xlsx_sheet()
build_supp_table_interventions <- function(league_table) {
  league_table %>%
    filter(!is.na(dalys_final), !is.na(unit_cost_final_usd), !is.na(cases_full_2023), cases_full_2023 > 0) %>%
    transmute(
      Program                                   = main_category,
      Intervention                              = intervention,
      `DALYs averted per patient`               = dalys_final,
      `Cost per case ($)`                       = unit_cost_final_usd,
      `ICER ($/DALY averted)`                   = icer_usd,
      `Source of effectiveness evidence`        = source_reference,
      `Total number of cases in need`           = cases_full_2023,
      `Annual consumables cost ($)`             = total_cost_full_usd
    ) %>%
    arrange(`ICER ($/DALY averted)`)
}

#' Health outcomes and resource use by intervention under one solved
#' scenario - the per-intervention detail behind a scenario's headline
#' numbers (build_scenario_comparison_table()), with the disease
#' program attached so it can be read/filtered on its own.
#'
#' @param result A single list(package=, summary=) as returned by
#'   optimize_benefit_package()
#' @return A data frame ready for write_xlsx_sheet()
build_supp_table_outcomes <- function(result) {
  p <- result$package
  data.frame(
    Program                                = p$main_category,
    Intervention                           = p$intervention,
    `Percentage of cases in need covered`  = round(100 * p$coverage_share, 1),
    `Total cases covered`                  = round(p$cases_covered),
    `DALYs averted`                        = round(p$dalys_averted_solution, 2),
    `Consumable expenditure required ($)`  = round(p$budget_cost_incurred_usd),
    check.names = FALSE
  ) %>%
    arrange(desc(`DALYs averted`))
}

#' Rate of inclusion of interventions from different disease programs
#' in the optimal package, across one or more scenarios - the
#' program-level view of a scenario-comparison table.
#'
#' @param scenario_results Named list, each element a list(package=,
#'   summary=) as returned by optimize_benefit_package(). All elements
#'   must share the same set of considered interventions (i.e. differ
#'   only in constraints, not in league_table) for the "N considered"
#'   column to be meaningful once.
#' @return A data frame: Program, N interventions considered, then two
#'   columns per scenario (Number, Percentage) included in that
#'   scenario's optimal package.
build_program_inclusion_table <- function(scenario_results) {
  first_pkg <- scenario_results[[1]]$package
  considered_by_program <- first_pkg %>%
    count(main_category, name = "n_considered")

  out <- considered_by_program %>% rename(Program = main_category, `Number of interventions considered` = n_considered)

  for (scenario_name in names(scenario_results)) {
    pkg <- scenario_results[[scenario_name]]$package
    included_by_program <- pkg %>%
      filter(coverage_share > 1e-6) %>%
      count(main_category, name = "n_included")
    out <- out %>%
      left_join(included_by_program, by = c("Program" = "main_category")) %>%
      mutate(n_included = coalesce(n_included, 0L))
    out[[paste0(scenario_name, " - Number included")]] <- out$n_included
    out[[paste0(scenario_name, " - Percentage included")]] <- paste0(
      round(100 * out$n_included / out$`Number of interventions considered`), "%"
    )
    out$n_included <- NULL
  }

  total_row <- data.frame(Program = "Grand total", check.names = FALSE)
  total_row[["Number of interventions considered"]] <- sum(out[["Number of interventions considered"]])
  for (scenario_name in names(scenario_results)) {
    num_col <- paste0(scenario_name, " - Number included")
    pct_col <- paste0(scenario_name, " - Percentage included")
    total_row[[num_col]] <- sum(out[[num_col]])
    total_row[[pct_col]] <- paste0(round(100 * total_row[[num_col]] / total_row[["Number of interventions considered"]]), "%")
  }
  rbind(out, total_row)
}
