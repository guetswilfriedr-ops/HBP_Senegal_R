# ============================================================
# Mohan et al. (2023)-style constrained optimization
#
# Health-maximizing selection of a COVERAGE LEVEL for each
# intervention (a continuous 0-1 share of cases in need), subject to
# a consumables budget and, once the data exists, health-workforce
# capacity by cadre - as opposed to the ICER-ranking method used
# elsewhere in this pipeline (R/05-09), which sorts interventions by
# ICER and fills the budget top-down.
#
# The two methods are NOT equivalent in general (Mohan et al. 2023,
# Health Policy and Planning): once a health-worker cadre is scarce,
# an intervention with a favourable ICER can be excluded by a binding
# time constraint on the cadre that delivers it, while a
# less-favourable-ICER intervention drawing on an underused cadre can
# be included. They ARE provably equivalent whenever the only binding
# constraint is a single linear budget (no workforce constraints) -
# sorting by ascending ICER and filling until the budget is exhausted
# is then the exact LP optimum. This file's function is built so that
# check can be run directly: solve with only a budget constraint and
# compare the result to Tables 6/7/8's ICER-ranked "core package".
#
# Ported from Sakshi Mohan's Uganda HBP repository
# (https://github.com/sakshimohan/uganda_hbp,
# 1_script/0_packages_and_functions.R), generalized to run against
# this project's league table instead of Uganda's
# hbp_data_clean_v2.xlsx, and re-expressed with the decision variable
# as a coverage FRACTION (0 to max_coverage_i) rather than a case
# count - this keeps every constraint's units in "cost or minutes per
# unit of coverage", and removes the need for Mohan's per-cadre
# column-index bookkeeping (this project's cadre list is not fixed in
# advance: it is whatever hr_needs supplies columns for).
# ============================================================

library(lpSolve)
library(dplyr)

#' Blend the raw data's "OHT Avg medical personnel minut(es)" sheet
#' (minutes of staff time per case, BY DELIVERY PLATFORM: Community,
#' Outreach, Clinic, Hospital) with "OHT Delivery channels" (the % of
#' cases each intervention actually reaches through each channel,
#' including three non-personnel channels - WASH, Other non-health,
#' Private sector - carried in the raw sheet but not used here) into
#' ONE minutes-per-case figure per intervention: a case-share-weighted
#' average across the four personnel-time channels.
#'
#' This is the NEED side only, and only at the level of total "medical
#' personnel" time, pooled across cadres - a per-cadre breakdown DOES
#' exist elsewhere in the raw data (see build_hr_needs_by_cadre()
#' below, borrowed from Uganda's own HR-needs matrix via the same
#' name-mapping crosswalk already used for effectiveness fallback),
#' but only covers the interventions that crosswalk reaches. This
#' single-pool version has no such gap (95/95 league table
#' interventions, see below) and is the right choice for a first,
#' coarser HR-time budget; prefer build_hr_needs_by_cadre() once a
#' genuine per-cadre bottleneck analysis is wanted and its coverage
#' gap is acceptable. This function feeds a single pooled-workforce
#' time constraint (find_optimal_package()'s hr_needs with one
#' column), not Mohan et al.'s per-cadre bottleneck analysis.
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
      # port hit and fixed - an NA in an irrelevant channel silently
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

#' Per-cadre (21-cadre) minutes-per-case need, borrowed from Sakshi
#' Mohan's Uganda HBP Tool via the SAME name-mapping crosswalk this
#' project already uses for effectiveness fallback
#' (R/04_effectiveness.R: "OHT Int name mapping recent-old" bridges a
#' current Senegal/OHT intervention name to an "old" Uganda-tool name,
#' which "Uganda HBP Tool" then keys on). The raw workbook's "Uganda
#' HBP Tool" sheet carries a 21-column "HR Needs" block (Medical
#' Officer/Specialist through Radiotherapy Technician) already filled
#' in for Uganda - this reads that block and joins it across the same
#' crosswalk, exactly mirroring how R/04_effectiveness.R borrows that
#' sheet's dalys_averted_per_patient_uganda column.
#'
#' Coverage is necessarily incomplete: only interventions the
#' crosswalk actually maps reach a value (69/95 of this project's
#' league table, checked directly - the other 26 have no Uganda
#' equivalent recorded in the mapping sheet and would need either a
#' Senegal-specific estimate or a manual analogy to a similar mapped
#' intervention, the same two options Mohan's own team used to fill
#' gaps in the Uganda/Malawi data - see her "Inputs from Uganda staff"
#' tab). Every borrowed value is a Uganda-context proxy, not a
#' Senegal-specific measurement - same caveat this project already
#' carries for Tufts/Uganda-borrowed effectiveness ratios.
#'
#' @param raw_data_path Path to the project's raw Excel workbook.
#' @return A data frame: intervention (Senegal/OHT name), then one
#'   column per cadre (21 columns, Uganda's cadre names verbatim) -
#'   NA for an intervention/cadre the crosswalk could not reach.
build_hr_needs_by_cadre <- function(raw_data_path) {
  uganda_header <- openxlsx::read.xlsx(raw_data_path, sheet = "Uganda HBP Tool", colNames = FALSE)
  cadre_names   <- as.character(uganda_header[3, 68:88])

  uganda <- openxlsx::read.xlsx(raw_data_path, sheet = "Uganda HBP Tool", colNames = FALSE, startRow = 4)
  uganda_hr <- uganda[, c(7, 68:88)]
  names(uganda_hr) <- c("old_intervention_name", cadre_names)
  uganda_hr <- uganda_hr %>%
    mutate(across(all_of(cadre_names), ~ suppressWarnings(as.numeric(.x)))) %>%
    filter(!is.na(old_intervention_name)) %>%
    # An old intervention name can appear on more than one Uganda HBP
    # Tool row - keep the first, exactly as R/04_effectiveness.R does
    # for the same sheet, so this join cannot fan out into extra rows.
    distinct(old_intervention_name, .keep_all = TRUE)

  mapping <- openxlsx::read.xlsx(raw_data_path, sheet = "OHT Int name mapping recent-old", colNames = FALSE, startRow = 2)
  mapping <- mapping[, c(1, 3)]
  names(mapping) <- c("recent_intervention", "old_intervention")

  mapping %>%
    distinct(recent_intervention, .keep_all = TRUE) %>%
    left_join(uganda_hr, by = c("old_intervention" = "old_intervention_name")) %>%
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
#'   opportunity cost of spending - the Mohan et al. objective) or
#'   "dalys" (maximise gross DALYs averted, ignoring cost - only
#'   sensible together with a binding budget, otherwise it just
#'   funds everything).
#' @param budget_usd Consumables/drug budget ceiling in USD. Use Inf
#'   for no budget constraint at all (Mohan's "no.drugbudget.limit"
#'   scenario).
#' @param max_coverage Feasible-coverage ceiling per intervention
#'   (Mohan's "maxcoverage") - a single value applied to every
#'   intervention, or a numeric vector the same length and order as
#'   league_table's rows. Defaults to 1 (no feasibility ceiling
#'   beyond the budget/HR constraints), because this project does not
#'   yet have a Senegal-specific maximum-feasible-coverage dataset
#'   distinct from the league table's own (already realised/planned)
#'   implementation_level_pct.
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
#'   summary  - named list mirroring Mohan et al.'s printed summary:
#'              n_interventions_considered,
#'              n_interventions_in_package (coverage_share > 0),
#'              total_dalys_averted, net_dalys_averted (the LP
#'              objective value), budget_used_usd,
#'              budget_used_pct, highest_icer_in_package (the
#'              ICER-ranking equivalence check - see file banner),
#'              hr_used_minutes (NULL unless hr_needs supplied).
find_optimal_package <- function(league_table,
                                  cet_usd_per_daly,
                                  objective = "nethealth",
                                  budget_usd = Inf,
                                  max_coverage = 1,
                                  hr_needs = NULL,
                                  hr_capacity_minutes = NULL) {
  stopifnot(objective %in% c("nethealth", "dalys"))

  df <- league_table %>%
    filter(!is.na(dalys_final), !is.na(unit_cost_final_usd), !is.na(cases_full_2023), cases_full_2023 > 0)

  n <- nrow(df)
  if (length(max_coverage) == 1) max_coverage <- rep(max_coverage, n)
  stopifnot(length(max_coverage) == n)

  dalys     <- df$dalys_final
  fullcost  <- df$unit_cost_final_usd
  cases     <- df$cases_full_2023
  nethealth <- dalys - fullcost / cet_usd_per_daly

  objective_coef <- if (objective == "nethealth") nethealth * cases else dalys * cases

  # Constraint 1: consumables budget. cons_budget[i] is the dollar
  # cost of moving intervention i's coverage share from 0 to 1 (i.e.
  # covering every case in need) - the LP's decision variable is that
  # share, so this is directly usable as the row of the constraint
  # matrix. An infinite budget means "no budget constraint at all" -
  # lp_solve rejects Inf in const.rhs, so that case omits the row
  # entirely rather than passing it a very large finite number.
  has_budget_constraint <- is.finite(budget_usd)
  if (has_budget_constraint) {
    cons_budget <- fullcost * cases
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
    stop("LP did not solve to optimality (lpSolve status code ", solution$status, ")")
  }

  coverage_share <- solution$solution
  cases_covered  <- coverage_share * cases
  dalys_solution <- coverage_share * dalys * cases
  cost_solution  <- coverage_share * fullcost * cases

  package <- df %>%
    mutate(
      coverage_share         = coverage_share,
      cases_covered          = cases_covered,
      dalys_averted_solution = dalys_solution,
      cost_incurred_usd      = cost_solution
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
    n_interventions_considered = n,
    n_interventions_in_package = sum(in_package),
    total_dalys_averted        = sum(dalys_solution),
    net_dalys_averted          = solution$objval,
    budget_used_usd            = sum(cost_solution),
    budget_used_pct            = if (is.finite(budget_usd)) sum(cost_solution) / budget_usd else NA_real_,
    highest_icer_in_package    = highest_icer_in_package,
    hr_used_minutes            = hr_used
  )

  list(package = package, summary = summary_out)
}
