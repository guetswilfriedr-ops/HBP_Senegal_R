# ============================================================
# Intervention -> coverage indicator (E) mapping
#
# Every intervention needs an E indicator (tab 3 of the DCEA prep
# workbook) describing how its utilization/coverage varies by wealth
# quintile. An analyst can fill this in by hand, per intervention, in
# tab 1's "E indicator to use" column - that manual choice always
# wins. Where it is left blank (which is every row, on a first pass),
# this file assigns a default automatically from the intervention's
# main/sub-category, so the pipeline runs end to end immediately and
# can be refined incrementally, one intervention at a time, without
# ever being blocked on a fully-completed spreadsheet.
#
# The heuristic below is deliberately coarse (category-level, not
# intervention-level) and every default it produces is visible in the
# exported distribution table (see R/17_dcea_export.R) alongside which
# rows used a manual override - so a reviewer can see exactly which
# assumption applies to which intervention and improve it later.
# ============================================================

library(dplyr)

# Sub-category (exact text from "Senegal HBP Tool - Top20 Causes") ->
# default E indicator id. Checked before the main-category fallback,
# since several main categories mix quite different service types.
default_e_by_subcategory <- c(
  "Family planning"                              = "E01",
  "Pregancy care - ANC"                          = "E02",
  "Childbirth care - Facility births"            = "E03",
  "Childbirth care - Other"                      = "E03",
  "Postpartum care - Other"                      = "E04",
  "Postpartum care - Treatment of newborn sepsis" = "E04",
  "Postpartum care - Treatment of sepsis"        = "E04",
  "Pregnancy care - Treatment of pregnancy complications" = "E02",
  "Management of abortion complications"         = "E03",
  "Management of ectopic pregnancy care"         = "E03",
  "Safe abortion"                                = "E03",
  "Diarrhea management"                          = "E06",
  "Pneumonia"                                    = "E07",
  "Child health"                                 = "E06",
  "Children"                                     = "E15",
  "Pregnant and lactating women"                 = "E15",
  "Immunization"                                 = "E05",
  "Measles"                                       = "E05",
  "Malaria"                                       = "E08",
  "Prevention"                                    = "E10",
  "Prevention - Other"                            = "E10",
  "Care and treatment"                            = "E11",
  "Collaborative HIV/AIDS and TB interventions"   = "E10",
  "Collaborative TB-HIV interventions"            = "E12",
  "Case management"                               = "E12",
  "First-line TB treatment"                       = "E12",
  "Second-line TB treatment"                      = "E12",
  "TB diagnosis with Xpert (molecular)"           = "E12",
  "TB diagnosis with culture"                     = "E12",
  "TB diagnosis with microscopy"                  = "E12",
  "TB diagnosis: Culture for DST"                 = "E12",
  "TB diagnosis: LPA (molecular)"                 = "E12",
  "TB preventive therapy (TPT)"                   = "E12",
  "TB screening with X-rays"                      = "E12",
  "CVD & diabetes"                                = "E13",
  "Anxiety disorders"                             = "E14",
  "Depression"                                    = "E14"
)

# Main category -> default E indicator id, used only when the
# sub-category has no entry above.
default_e_by_main_category <- c(
  "Child health"                                       = "E06",
  "HIV/AIDS"                                           = "E10",
  "Immunization"                                        = "E05",
  "Malaria"                                             = "E08",
  "Maternal/newborn and reproductive health"            = "E02",
  "Mental, neurological, and substance use disorders"   = "E14",
  "Non-communicable diseases"                            = "E13",
  "Nutrition"                                             = "E15",
  "TB"                                                     = "E12"
)

# Universal last-resort fallback: the national average public-facility
# care-seeking rate (also used to build tab 4's opportunity-cost
# distribution) - not disease-specific, but always defined.
default_e_fallback <- "E16"

#' Assign an E-indicator id to every intervention: the analyst's manual
#' choice (tab 1) if present, otherwise the sub-category default,
#' otherwise the main-category default, otherwise the universal
#' fallback.
#'
#' @param interventions Output of read_dcea_prep()$interventions
#' @return interventions, with an added `e_indicator_id` column and an
#'   `e_indicator_source` column ("manual", "sub_category",
#'   "main_category", or "fallback") so every assumption stays visible
assign_e_indicator <- function(interventions) {
  interventions %>%
    mutate(
      e_by_subcat = unname(default_e_by_subcategory[sub_category]),
      e_by_maincat = unname(default_e_by_main_category[main_category]),
      e_indicator_id = case_when(
        !is.na(e_indicator_manual) ~ e_indicator_manual,
        !is.na(e_by_subcat)        ~ e_by_subcat,
        !is.na(e_by_maincat)       ~ e_by_maincat,
        TRUE                       ~ default_e_fallback
      ),
      e_indicator_source = case_when(
        !is.na(e_indicator_manual) ~ "manual",
        !is.na(e_by_subcat)        ~ "sub_category",
        !is.na(e_by_maincat)       ~ "main_category",
        TRUE                       ~ "fallback"
      )
    ) %>%
    select(-e_by_subcat, -e_by_maincat)
}
