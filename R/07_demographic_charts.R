# ============================================================
# Demographic context charts (child and maternal mortality trends)
# Descriptive context only - not inputs to the league table - so the
# data points are hardcoded from the cited DHS/EDS-Continue survey
# rounds rather than read from the pipeline's own workbook.
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)

#' Under-five, infant and neonatal mortality trend across DHS survey
#' rounds (source: ANDS, Continuous DHS, Senegal, 2023 - see the note
#' text for the underlying figures).
#'
#' @return A ggplot object
build_child_mortality_trend_plot <- function() {
  df <- data.frame(
    survey    = factor(c("DHS 2005", "DHS 2010-11", "DHS 2015", "DHS 2017", "Continuous\nDHS 2023"),
                        levels = c("DHS 2005", "DHS 2010-11", "DHS 2015", "DHS 2017", "Continuous\nDHS 2023")),
    `Under-five mortality`  = c(121, 72, 59, 37, 40),
    `Infant mortality`      = c(61, 47, 39, 29, 31),
    `Neonatal mortality`    = c(35, 29, 23, 21, 23),
    check.names = FALSE
  ) %>%
    pivot_longer(-survey, names_to = "indicator", values_to = "rate") %>%
    mutate(indicator = factor(indicator, levels = c("Under-five mortality", "Infant mortality", "Neonatal mortality")))

  ggplot(df, aes(x = survey, y = rate, color = indicator, group = indicator)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.2) +
    geom_text(aes(label = rate), vjust = -0.9, size = 3, show.legend = FALSE) +
    scale_color_manual(values = c(
      "Under-five mortality" = liser_bleu,
      "Infant mortality"     = liser_cyan,
      "Neonatal mortality"   = liser_rouge
    ), name = NULL) +
    scale_y_continuous(limits = c(0, max(df$rate) * 1.15), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Deaths per 1,000 live births") +
    liser_chart_theme(base_size = 11) +
    theme(legend.position = "bottom")
}

#' Maternal mortality ratio trend across DHS survey rounds (source:
#' ANDS, Continuous DHS, Senegal, 2023).
#'
#' @return A ggplot object
build_maternal_mortality_plot <- function() {
  df <- data.frame(
    survey = factor(c("DHS 2005", "DHS 2010-2011", "DHS 2013", "DHS 2017", "Continuous\nDHS 2023"),
                     levels = c("DHS 2005", "DHS 2010-2011", "DHS 2013", "DHS 2017", "Continuous\nDHS 2023")),
    mmr    = c(401, 484, 434, 273, 216)
  )

  ggplot(df, aes(x = survey, y = mmr)) +
    geom_col(fill = liser_bleu, width = 0.6) +
    geom_text(aes(label = mmr), vjust = -0.6, size = 3.2, color = liser_bleu, fontface = "bold") +
    scale_y_continuous(limits = c(0, max(df$mmr) * 1.15), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Maternal deaths per 100,000 live births") +
    liser_chart_theme(base_size = 11)
}
