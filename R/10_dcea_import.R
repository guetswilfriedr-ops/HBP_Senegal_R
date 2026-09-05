# ============================================================
# DCEA data import
#
# Reads data/dcea_prep/DCEA_preparatory_data.xlsx - the hand-curated
# (and script-populated) workbook that maps every intervention to a
# GBD-cause prevalence-by-quintile-AND-residence row (tab 2), a
# coverage/utilization indicator (tab 3), a national opportunity-cost
# distribution (tab 4), and the CET/opportunity-cost/inequality-
# aversion/urban-share parameters (tab 5).
#
# Columns are read BY POSITION, not by header text, following the same
# convention already used in R/02_cleaning.R for sheets with awkward or
# merged headers - the workbook's headers are written for a human
# reader (with units, footnote markers, etc.), not as stable machine
# keys.
#
# Two equity-relevant stratifiers are modelled throughout this DCEA
# module, mirroring Arnold, Nkhoma & Griffin (2020): wealth quintile
# (q1 = poorest .. q5 = richest, equal-sized by construction) and
# residence (urban/rural, NOT equal-sized - see
# config$dcea$national_urban_share). The two constant lookups below are
# shared by every DCEA script sourced after this one.
# ============================================================

library(readxl)

wealth_group_ids <- c("q1", "q2", "q3", "q4", "q5")
wealth_group_labels <- c(q1 = "Poorest", q2 = "Poorer", q3 = "Middle", q4 = "Richer", q5 = "Richest")

residence_group_ids <- c("urban", "rural")
residence_group_labels <- c(urban = "Urban", rural = "Rural")

#' Read every tab of the DCEA preparatory workbook needed by the
#' pipeline
#'
#' @param path Path to DCEA_preparatory_data.xlsx (config$dcea_prep_path)
#' @return A list with:
#'   interventions - one row per intervention (tab 1), columns:
#'     num, main_category, sub_category, intervention_en, intervention_fr,
#'     gbd_cause, target_population, cases_2023, effectiveness_status,
#'     cost_status, demand_status, eligible_league_table,
#'     e_indicator_manual, e_justification_manual, notes
#'   d_table - one row per GBD cause (tab 2), columns:
#'     cause_id, cause_gbd, n_interventions, q1..q5 and urban/rural
#'     (each a share of cases, %), sum_check, sum_check_residence,
#'     source, year, indicator, page, justification, status, tier
#'   e_table - one row per coverage indicator (tab 3), columns:
#'     indicator_id, indicator_name, description, q1..q5 and
#'     urban/rural (each a rate, %), source, year, page, justification,
#'     status, tier
#'   f_row - the single national opportunity-cost row (tab 4), columns:
#'     q1..q5 and urban/rural (each a share, %), sum_check,
#'     sum_check_residence, source, year, indicator, page,
#'     justification, status, tier
#'   cet_params - tab 5, columns: parameter, value, source, notes
read_dcea_prep <- function(path) {
  if (!file.exists(path)) {
    stop(
      "DCEA preparatory workbook not found: ", path, "\n",
      "Run the data-prep step first (see data/dcea_prep/) before main_dcea.R."
    )
  }

  interventions <- read_excel(path, sheet = "1_Interventions", skip = 0, col_types = "text")
  names(interventions) <- c(
    "num", "main_category", "sub_category", "intervention_en", "intervention_fr",
    "gbd_cause", "target_population", "cases_2023",
    "effectiveness_status", "cost_status", "demand_status", "eligible_league_table",
    "e_indicator_manual", "e_justification_manual", "notes"
  )
  interventions$cases_2023 <- suppressWarnings(as.numeric(interventions$cases_2023))
  interventions$e_indicator_manual <- ifelse(
    is.na(interventions$e_indicator_manual) | trimws(interventions$e_indicator_manual) == "",
    NA_character_,
    trimws(interventions$e_indicator_manual)
  )

  d_table <- read_excel(path, sheet = "2_D_prevalence_by_cause", skip = 0, col_types = "text")
  names(d_table) <- c(
    "cause_id", "cause_gbd", "n_interventions",
    "q1", "q2", "q3", "q4", "q5", "sum_check",
    "urban", "rural", "sum_check_residence",
    "source", "year", "indicator", "page", "justification", "status", "tier"
  )
  for (col in c(wealth_group_ids, residence_group_ids)) {
    d_table[[col]] <- suppressWarnings(as.numeric(d_table[[col]]))
  }

  e_table <- read_excel(path, sheet = "3_E_coverage_by_indicator", skip = 0, col_types = "text")
  names(e_table) <- c(
    "indicator_id", "indicator_name", "description",
    "q1", "q2", "q3", "q4", "q5", "urban", "rural",
    "source", "year", "page", "justification", "status", "tier"
  )
  for (col in c(wealth_group_ids, residence_group_ids)) {
    e_table[[col]] <- suppressWarnings(as.numeric(e_table[[col]]))
  }

  f_raw <- read_excel(path, sheet = "4_F_opportunity_cost_national", skip = 0, col_types = "text")
  names(f_raw) <- c(
    "q1", "q2", "q3", "q4", "q5", "sum_check",
    "urban", "rural", "sum_check_residence",
    "source", "year", "indicator", "page", "justification", "status", "tier"
  )
  for (col in c(wealth_group_ids, residence_group_ids)) {
    f_raw[[col]] <- suppressWarnings(as.numeric(f_raw[[col]]))
  }
  f_row <- f_raw[1, ]

  cet_params <- read_excel(path, sheet = "5_CET_parameters", skip = 0, col_types = "text")
  names(cet_params) <- c("parameter", "value", "source", "notes")
  cet_params$value <- suppressWarnings(as.numeric(cet_params$value))

  incomplete_d <- d_table$cause_gbd[is.na(d_table$q1) | is.na(d_table$q5) | is.na(d_table$urban) | is.na(d_table$rural)]
  if (length(incomplete_d) > 0) {
    warning(
      "2_D_prevalence_by_cause has incomplete quintile and/or residence shares for: ",
      paste(incomplete_d, collapse = ", "),
      ". Interventions linked to these causes will get NA distributional results.",
      call. = FALSE
    )
  }

  list(
    interventions = interventions,
    d_table = d_table,
    e_table = e_table,
    f_row = f_row,
    cet_params = cet_params
  )
}

#' Look up a named parameter from tab 5 (5_CET_parameters)
#'
#' @param cet_params Output of read_dcea_prep()$cet_params
#' @param parameter_name Exact text in the "Parameter" column
#' @param default Value to return if the parameter is missing/blank
get_cet_parameter <- function(cet_params, parameter_name, default = NA_real_) {
  match_row <- cet_params[cet_params$parameter == parameter_name, ]
  if (nrow(match_row) == 0 || is.na(match_row$value[1])) {
    return(default)
  }
  match_row$value[1]
}

#' Resolve the health opportunity-cost rate ($ per DALY averted) to use
#' for the opportunity-cost calculation.
#'
#' Prefers the dedicated Ochalek-type threshold in tab 5 if an analyst
#' has filled it in; otherwise falls back to the reference CET
#' (config$cet_usd_per_daly) with a message, since the CET used to rank
#' the league table is a willingness-to-pay threshold and not
#' necessarily the same as a supply-side opportunity-cost threshold
#' (see data/dcea_prep/DCEA_preparatory_data.xlsx, tab 5, row 2).
#'
#' @param cet_params Output of read_dcea_prep()$cet_params
#' @param reference_cet config$cet_usd_per_daly, used as the fallback
resolve_opportunity_cost_rate <- function(cet_params, reference_cet) {
  ochalek_value <- get_cet_parameter(
    cet_params, "Health opportunity-cost threshold (Ochalek-type)", default = NA_real_
  )
  if (is.na(ochalek_value)) {
    message(
      "No Senegal-specific health opportunity-cost threshold found in tab 5 - ",
      "falling back to the reference CET ($", reference_cet, " per DALY averted). ",
      "This is a known simplification: see data/dcea_prep/DCEA_preparatory_data.xlsx, ",
      "tab 5, and the rotavirus worked example discussed for this project."
    )
    return(reference_cet)
  }
  ochalek_value
}
