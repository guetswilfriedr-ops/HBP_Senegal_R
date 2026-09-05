# ============================================================
# Effectiveness (DALYs averted per patient) lookup
#
# For each intervention that has a cost figure, effectiveness is
# resolved in this order:
#   1. id_Ratio records the chosen Tufts article ID + ratio number for
#      the intervention - the point where an analyst selects or
#      changes which published study backs an intervention's
#      effectiveness figure. Edit id_Ratio and re-run to change it.
#   2. That (article_id, ratio_number) pair is looked up in
#      Tufts_Ratios to get DALYs averted per patient, along with the
#      study's own bibliographic details (author, year, title,
#      countries, comparator) and, from Tufts_Methods (joined by
#      article_id alone, since it is one row per study), its
#      methodology (time horizon, analytic perspective, discounting,
#      quality score) - so the figure stays fully traceable to its
#      source. A ratio whose absolute value exceeds
#      config$tufts_ratio_plausibility_bound is discarded as
#      implausible.
#   3. If no usable Tufts ratio exists - including when the
#      intervention has no id_Ratio entry at all - fall back to the
#      Uganda HBP Tool's own DALYs-averted-per-patient figure for the
#      same intervention, matched via the name bridge in
#      "OHT Int name mapping recent-old" (an intervention can be
#      renamed between tools, so its current name is matched to the
#      Uganda tool's own naming through that bridge before matching).
#   4. If neither source has a figure, effectiveness is "Missing" and
#      the intervention cannot proceed to the league table.
# ============================================================

library(dplyr)

#' Build the per-intervention effectiveness (DALYs averted/patient) table
#'
#' @param interventions Character vector (or single-column data frame)
#'   of every intervention that needs an effectiveness figure - pass
#'   the interventions that already have a cost (R/03_costs.R), not
#'   id_Ratio's own rows, so an intervention with no id_Ratio entry
#'   still gets a chance at the Uganda fallback
#' @param id_ratio Cleaned "id_Ratio" data frame
#' @param tufts_ratios Cleaned "Tufts_Ratios" data frame
#' @param tufts_methods Cleaned "Tufts_Methods" data frame
#' @param uganda_hbp Cleaned "Uganda HBP Tool" data frame
#' @param name_mapping Cleaned "OHT Int name mapping recent-old" data frame
#' @param plausibility_bound Max absolute DALYs-averted/patient value
#'   accepted from Tufts_Ratios (config$tufts_ratio_plausibility_bound)
#' @return One row per intervention in `interventions`, with:
#'   dalys_tufts, dalys_uganda, dalys_final, effectiveness_status
#'   ("Tufts ratio" / "Uganda fallback" / "Missing"), effectiveness_note,
#'   and the study documentation for a Tufts-sourced figure: title,
#'   primary_author, issue_year, target_countries, comparator_modality,
#'   journal_name, publication_date, time_horizon, perspective_author,
#'   costs_discounted, outcome_discounted, total_quality_score
build_effectiveness_table <- function(interventions, id_ratio, tufts_ratios, tufts_methods,
                                       uganda_hbp, name_mapping, plausibility_bound) {

  base <- data.frame(intervention = unique(interventions), stringsAsFactors = FALSE)

  id_ratio     <- ensure_columns(id_ratio, c("intervention", "article_id", "ratio_number", "confidence"), "id_Ratio")
  tufts_ratios <- ensure_columns(tufts_ratios, c(
    "article_id", "ratio_number", "dalys_per_patient_delta",
    "title", "primary_author", "issue_year", "target_countries", "comparator_modality"
  ), "Tufts_Ratios")
  tufts_methods <- ensure_columns(tufts_methods, c(
    "article_id", "journal_name", "publication_date", "time_horizon",
    "perspective_author", "costs_discounted", "outcome_discounted", "total_quality_score"
  ), "Tufts_Methods")
  name_mapping <- ensure_columns(name_mapping, c("recent_intervention", "old_intervention"), "OHT Int name mapping recent-old")
  uganda_hbp   <- ensure_columns(uganda_hbp, c("old_intervention_name", "dalys_averted_per_patient_uganda"), "Uganda HBP Tool")

  # article_id/ratio_number must have matching types on both sides of
  # the join below, and the DALY figures must be numeric - a stray
  # text value anywhere in a column (a note, "N/A", a leftover error
  # value) otherwise imports the whole column as character and either
  # breaks the join or errors on abs()/arithmetic further down.
  to_numeric <- function(x) suppressWarnings(as.numeric(x))
  id_ratio <- id_ratio %>%
    mutate(article_id = as.character(article_id), ratio_number = to_numeric(ratio_number))
  tufts_ratios <- tufts_ratios %>%
    mutate(
      article_id = as.character(article_id),
      ratio_number = to_numeric(ratio_number),
      dalys_per_patient_delta = to_numeric(dalys_per_patient_delta),
      issue_year = to_numeric(issue_year)
    )
  uganda_hbp <- uganda_hbp %>%
    mutate(dalys_averted_per_patient_uganda = to_numeric(dalys_averted_per_patient_uganda))
  tufts_methods <- tufts_methods %>%
    mutate(article_id = as.character(article_id)) %>%
    filter(!is.na(article_id)) %>%
    distinct(article_id, .keep_all = TRUE)

  tufts_lookup <- base %>%
    left_join(
      id_ratio %>% select(intervention, article_id, ratio_number, confidence),
      by = "intervention"
    ) %>%
    left_join(
      tufts_ratios %>% select(
        article_id, ratio_number, dalys_per_patient_delta,
        title, primary_author, issue_year, target_countries, comparator_modality
      ),
      by = c("article_id", "ratio_number")
    ) %>%
    left_join(
      tufts_methods %>% select(
        article_id, journal_name, publication_date, time_horizon,
        perspective_author, costs_discounted, outcome_discounted, total_quality_score
      ),
      by = "article_id"
    ) %>%
    mutate(
      dalys_tufts = if_else(
        !is.na(dalys_per_patient_delta) & abs(dalys_per_patient_delta) <= plausibility_bound,
        abs(dalys_per_patient_delta),
        NA_real_
      ),
      tufts_implausible = !is.na(dalys_per_patient_delta) & abs(dalys_per_patient_delta) > plausibility_bound
    )

  # An old intervention name can appear on more than one Uganda HBP
  # Tool row (e.g. reused across age bands or delivery channels); keep
  # only the first match for each name, so the join below cannot fan
  # out into extra rows for a single intervention.
  uganda_hbp_by_name <- uganda_hbp %>%
    filter(!is.na(old_intervention_name)) %>%
    distinct(old_intervention_name, .keep_all = TRUE)

  uganda_lookup <- tufts_lookup %>%
    left_join(
      name_mapping %>% select(recent_intervention, old_intervention),
      by = c("intervention" = "recent_intervention")
    ) %>%
    left_join(
      uganda_hbp_by_name %>% select(old_intervention_name, dalys_averted_per_patient_uganda),
      by = c("old_intervention" = "old_intervention_name")
    ) %>%
    rename(dalys_uganda = dalys_averted_per_patient_uganda)

  uganda_lookup %>%
    mutate(
      dalys_final = coalesce(dalys_tufts, dalys_uganda),
      effectiveness_status = case_when(
        !is.na(dalys_tufts)  ~ "Tufts ratio",
        !is.na(dalys_uganda) ~ "Uganda fallback",
        TRUE                  ~ "Missing"
      ),
      effectiveness_note = case_when(
        !is.na(dalys_tufts) ~ paste0("Tufts article ", article_id, ", ratio #", ratio_number),
        !is.na(dalys_uganda) ~ "No usable Tufts ratio; used Uganda HBP Tool fallback",
        tufts_implausible ~ paste0(
          "Tufts article ", article_id, ", ratio #", ratio_number,
          " found but |DALYs/patient| exceeds the plausibility bound (",
          plausibility_bound, ") and no Uganda fallback found"
        ),
        is.na(article_id) ~ "No Tufts article/ratio selected in id_Ratio, and no Uganda fallback found (no name match in 'OHT Int name mapping recent-old' / 'Uganda HBP Tool')",
        TRUE ~ "Tufts article/ratio selected but not found in Tufts_Ratios, and no Uganda fallback found"
      )
    ) %>%
    select(intervention, article_id, ratio_number, confidence,
           title, primary_author, issue_year, journal_name, publication_date,
           target_countries, comparator_modality,
           time_horizon, perspective_author, costs_discounted, outcome_discounted,
           total_quality_score,
           dalys_tufts, dalys_uganda, dalys_final,
           effectiveness_status, effectiveness_note)
}
