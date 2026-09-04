# ============================================================
# Unit cost lookup
#
# Reconstructs the logic in 'Senegal HBP Tool - Top20 Causes'
# columns AI/AJ/BG/BJ:
#
#   1. The workbook's own manual override point is column AJ,
#      "Cost if manual input" (kept here as cost_manual_input_usd),
#      filled in by the analyst when the OHT data has no cost for an
#      intervention. To supply or change a manual cost, edit that
#      column in 'Senegal HBP Tool - Top20 Causes' and re-run.
#   2. Otherwise, use the unit cost already resolved in
#      'OHT Drug supply costs' (column AK, "Final unit cost in $ per
#      case in Senegal OHT") for that intervention.
#   3. If neither is available, the intervention has no cost figure
#      and cannot enter the league table (status = "Missing").
# ============================================================

library(dplyr)

#' Build the per-intervention unit cost table
#'
#' @param top20_causes Cleaned "Senegal HBP Tool - Top20 Causes" data frame
#' @param oht_drug_costs Cleaned "OHT Drug supply costs" data frame
#' @return One row per intervention in top20_causes, with:
#'   cost_manual_usd, cost_oht_usd, unit_cost_final_usd, cost_status
#'   ("Manual override" / "Senegal OHT" / "Missing"), cost_note
build_cost_table <- function(top20_causes, oht_drug_costs) {
  top20_causes %>%
    select(intervention, cost_manual_input_usd) %>%
    left_join(
      oht_drug_costs %>% select(intervention, unit_cost_usd),
      by = "intervention"
    ) %>%
    rename(
      cost_manual_usd = cost_manual_input_usd,
      cost_oht_usd    = unit_cost_usd
    ) %>%
    mutate(
      cost_manual_usd = suppressWarnings(as.numeric(cost_manual_usd)),
      cost_oht_usd    = suppressWarnings(as.numeric(cost_oht_usd)),
      unit_cost_final_usd = coalesce(cost_manual_usd, cost_oht_usd),
      cost_status = case_when(
        !is.na(cost_manual_usd) ~ "Manual override",
        !is.na(cost_oht_usd)    ~ "Senegal OHT",
        TRUE                     ~ "Missing"
      ),
      cost_note = case_when(
        !is.na(cost_manual_usd) ~ "Manual cost entered in 'Senegal HBP Tool - Top20 Causes'!Cost if manual input",
        !is.na(cost_oht_usd)    ~ "Unit cost from 'OHT Drug supply costs'",
        TRUE                     ~ "No manual cost and no OHT unit cost found for this intervention"
      )
    ) %>%
    select(intervention, cost_manual_usd, cost_oht_usd, unit_cost_final_usd,
           cost_status, cost_note)
}
