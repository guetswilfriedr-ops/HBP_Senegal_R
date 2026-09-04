# ============================================================
# Effectiveness (DALYs averted per patient) lookup
#
# Reconstructs the logic in 'Senegal HBP Tool - Top20 Causes'
# columns AY/AZ/BE/BF/BI/W/AM:
#
#   1. For each intervention, id_Ratio may record the analyst's
#      chosen Tufts article ID + ratio number (this is the manual
#      input point: to change which article/ratio an intervention
#      uses, edit id_Ratio in the workbook and re-run the pipeline).
#   2. That (article_id, ratio_number) pair is looked up in
#      Tufts_Ratios to get DALYs averted per patient. A ratio whose
#      absolute value exceeds config$tufts_ratio_plausibility_bound
#      is discarded as implausible (mirrors sheet column BE).
#   3. If no usable Tufts ratio exists - including when the
#      intervention has no id_Ratio entry at all - fall back to the
#      Uganda HBP Tool's own DALYs-averted-per-patient figure for the
#      same intervention (matched via the old/recent name bridge in
#      "OHT Int name mapping recent-old"). The workbook's own BF4
#      formula does this fallback independently of id_Ratio, so the
#      lookup here starts from every intervention that needs an
#      effectiveness figure, not just the ones id_Ratio covers - an
#      earlier version of this function started from id_Ratio itself
#      and silently lost interventions (including TB and adult
#      malaria treatment) that have no id_Ratio row but do have a
#      usable Uganda fallback. Always pass the full intervention list
#      here, not just id_Ratio's own rows.
#   4. If neither is available, the intervention has no effectiveness
#      figure and cannot enter the league table (status = "Missing").
# ============================================================

library(dplyr)

#' Build the per-intervention effectiveness (DALYs averted/patient) table
#'
#' @param interventions Character vector (or single-column data frame)
#'   of every intervention name that needs an effectiveness figure -
#'   pass the full "Senegal HBP Tool - Top20 Causes" intervention list,
#'   not id_Ratio's own rows, so interventions without an id_Ratio
#'   entry still get a chance at the Uganda fallback
#' @param id_ratio Cleaned "id_Ratio" data frame
#' @param tufts_ratios Cleaned "Tufts_Ratios" data frame
#' @param uganda_hbp Cleaned "Uganda HBP Tool" data frame
#' @param name_mapping Cleaned "OHT Int name mapping recent-old" data frame
#' @param plausibility_bound Max absolute DALYs-averted/patient value
#'   accepted from Tufts_Ratios (config$tufts_ratio_plausibility_bound)
#' @return One row per intervention in `interventions`, with:
#'   dalys_tufts, dalys_uganda, dalys_final, effectiveness_status
#'   ("Tufts ratio" / "Uganda fallback" / "Missing"), effectiveness_note
build_effectiveness_table <- function(interventions, id_ratio, tufts_ratios,
                                       uganda_hbp, name_mapping, plausibility_bound) {

  base <- data.frame(intervention = unique(interventions), stringsAsFactors = FALSE)

  id_ratio     <- ensure_columns(id_ratio, c("intervention", "article_id", "ratio_number", "confidence"), "id_Ratio")
  tufts_ratios <- ensure_columns(tufts_ratios, c("article_id", "ratio_number", "dalys_per_patient_delta"), "Tufts_Ratios")
  name_mapping <- ensure_columns(name_mapping, c("recent_intervention", "old_intervention"), "OHT Int name mapping recent-old")
  uganda_hbp   <- ensure_columns(uganda_hbp, c("old_intervention", "dalys_averted_per_patient_uganda"), "Uganda HBP Tool")

  tufts_lookup <- base %>%
    left_join(
      id_ratio %>% select(intervention, article_id, ratio_number, confidence),
      by = "intervention"
    ) %>%
    left_join(
      tufts_ratios %>% select(article_id, ratio_number, dalys_per_patient_delta),
      by = c("article_id", "ratio_number")
    ) %>%
    mutate(
      dalys_tufts = if_else(
        !is.na(dalys_per_patient_delta) & abs(dalys_per_patient_delta) <= plausibility_bound,
        abs(dalys_per_patient_delta),
        NA_real_
      ),
      tufts_implausible = !is.na(dalys_per_patient_delta) & abs(dalys_per_patient_delta) > plausibility_bound
    )

  uganda_lookup <- tufts_lookup %>%
    left_join(
      name_mapping %>% select(recent_intervention, old_intervention),
      by = c("intervention" = "recent_intervention")
    ) %>%
    left_join(
      uganda_hbp %>% select(old_intervention, dalys_averted_per_patient_uganda),
      by = "old_intervention"
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
           dalys_tufts, dalys_uganda, dalys_final,
           effectiveness_status, effectiveness_note)
}
