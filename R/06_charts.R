# ============================================================
# Charts
# ============================================================

library(ggplot2)
library(dplyr)

#' Top 20 GBD causes of DALYs in Senegal, ranked, colour-coded by
#' Level-1 GBD category. Reads "GBD_TIER3" directly from the raw
#' workbook (not part of config$sheets_to_load - this chart is
#' descriptive disease-burden context, not an input to the league
#' table), mirroring how R/18_constrained_optimization.R reads its own
#' extra sheets directly for the same reason.
#'
#' @param raw_data_path Path to the project's raw Excel workbook
#'   (config$raw_data_path)
#' @return A ggplot object
build_top20_causes_plot <- function(raw_data_path) {
  gbd <- openxlsx::read.xlsx(raw_data_path, sheet = "GBD_TIER3")
  df <- gbd %>%
    filter(!is.na(Rank), as.numeric(Rank) <= 20) %>%
    transmute(
      category = `Category.(L1)`,
      cause = factor(Cause, levels = Cause[order(as.numeric(Percentage.of.DALYs.lost.by.disease))]),
      pct = as.numeric(Percentage.of.DALYs.lost.by.disease)
    )

  ggplot(df, aes(x = cause, y = pct, fill = category)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.15, size = 3, color = "grey25") +
    coord_flip(clip = "off") +
    scale_fill_manual(values = c(
      "Communicable, maternal, neonatal, and nutritional diseases" = liser_bleu,
      "Injuries" = liser_rouge,
      "Non-communicable diseases" = liser_cyan
    ), name = NULL) +
    scale_y_continuous(limits = c(0, max(df$pct) * 1.12), expand = expansion(mult = c(0, 0.05))) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = NULL, y = "Share of total DALYs (%)") +
    liser_chart_theme(base_size = 11) +
    theme(legend.position = "bottom", axis.text.y = element_text(size = rel(0.85)))
}

#' Flow diagram of the intervention funnel (steps and exclusion counts)
#'
#' @param funnel_summary Output of build_intervention_funnel()$funnel_summary
#' @return A ggplot object
build_funnel_flow_plot <- function(funnel_summary) {
  steps <- funnel_summary %>% filter(step >= 1)
  n_steps <- nrow(steps)

  box_width <- 6
  box_height <- 0.8
  gap <- 1.3
  y_top <- n_steps * gap

  main_boxes <- steps %>%
    mutate(
      y = y_top - (row_number() - 1) * gap,
      label = paste0(label, "\n", n_entering, " assessed -> ", n_passed, " retained")
    )

  exclusion_boxes <- steps %>%
    filter(n_excluded > 0) %>%
    mutate(
      y = y_top - (match(step, steps$step) - 1) * gap,
      label = paste0("Excluded: ", n_excluded, "\n", unname(reason_short_labels[as.character(step)]))
    )

  arrows <- data.frame(
    x = box_width / 2, xend = box_width / 2,
    y = head(main_boxes$y, -1) - box_height / 2,
    yend = tail(main_boxes$y, -1) + box_height / 2
  )

  side_arrows <- exclusion_boxes %>%
    transmute(
      x = box_width / 2 + 0.1, xend = box_width + 1.9,
      y = y, yend = y
    )

  ggplot() +
    geom_rect(
      data = main_boxes,
      aes(xmin = 0, xmax = box_width, ymin = y - box_height / 2, ymax = y + box_height / 2),
      fill = liser_bleu, color = "white"
    ) +
    geom_text(
      data = main_boxes, aes(x = box_width / 2, y = y, label = label),
      color = "white", size = 3.4, lineheight = 0.95
    ) +
    geom_rect(
      data = exclusion_boxes,
      aes(xmin = box_width + 2, xmax = box_width + 2 + 5.5, ymin = y - box_height / 2, ymax = y + box_height / 2),
      fill = liser_rouge_light, color = liser_rouge
    ) +
    geom_text(
      data = exclusion_boxes, aes(x = box_width + 2 + 2.75, y = y, label = label),
      color = liser_rouge_fonce, size = 3.1, lineheight = 0.95
    ) +
    geom_segment(
      data = arrows, aes(x = x, xend = xend, y = y, yend = yend),
      arrow = arrow(length = unit(0.2, "cm")), color = liser_bleu
    ) +
    geom_segment(
      data = side_arrows, aes(x = x, xend = xend, y = y, yend = yend),
      arrow = arrow(length = unit(0.15, "cm")), color = liser_rouge
    ) +
    coord_cartesian(xlim = c(-0.5, box_width + 2 + 6), clip = "off") +
    theme_void()
}

# Short label for the exclusion side-box at each step
reason_short_labels <- c(
  "1" = "not linked to a\nTop-20-DALY GBD cause",
  "2" = "no unit cost",
  "3" = "no effectiveness figure",
  "4" = "no case-volume data"
)

