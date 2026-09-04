# ============================================================
# Cleaning / harmonization functions
# ============================================================

library(dplyr)
library(janitor)

#' Apply generic cleaning to a single sheet's data frame
#'
#' @param df A raw data frame, as returned by load_raw_data()
#' @return The cleaned data frame
clean_sheet <- function(df) {
  df %>%
    clean_names() %>%
    remove_empty(c("rows", "cols"))
}

#' Apply clean_sheet() to every element of a named list of data frames
#'
#' @param data_list Named list of raw data frames
#' @return Named list of cleaned data frames
clean_all <- function(data_list) {
  lapply(data_list, clean_sheet)
}
