# ============================================================
# Charts
# ============================================================

library(ggplot2)
library(dplyr)

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
      fill = "#1F4E78", color = "white"
    ) +
    geom_text(
      data = main_boxes, aes(x = box_width / 2, y = y, label = label),
      color = "white", size = 3.4, lineheight = 0.95
    ) +
    geom_rect(
      data = exclusion_boxes,
      aes(xmin = box_width + 2, xmax = box_width + 2 + 5.5, ymin = y - box_height / 2, ymax = y + box_height / 2),
      fill = "#F4E7E1", color = "#B5533C"
    ) +
    geom_text(
      data = exclusion_boxes, aes(x = box_width + 2 + 2.75, y = y, label = label),
      color = "#7A3324", size = 3.1, lineheight = 0.95
    ) +
    geom_segment(
      data = arrows, aes(x = x, xend = xend, y = y, yend = yend),
      arrow = arrow(length = unit(0.2, "cm")), color = "#1F4E78"
    ) +
    geom_segment(
      data = side_arrows, aes(x = x, xend = xend, y = y, yend = yend),
      arrow = arrow(length = unit(0.15, "cm")), color = "#B5533C"
    ) +
    coord_cartesian(xlim = c(-0.5, box_width + 2 + 6), clip = "off") +
    theme_void() +
    labs(title = "Intervention funnel") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

# Short label for the exclusion side-box at each step
reason_short_labels <- c(
  "1" = "not linked to a\nTop-20-DALY GBD cause",
  "2" = "no unit cost",
  "3" = "no effectiveness figure",
  "4" = "no case-volume data"
)

#' Bar chart ranking every league-table intervention by net health
#' benefit (full implementation), with a threshold line at zero
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @return A ggplot object
build_nhb_ranking_plot <- function(league_table) {
  df <- league_table %>%
    mutate(
      intervention = factor(intervention, levels = rev(intervention[order(net_dalys_full)])),
      above_zero = net_dalys_full >= 0
    )

  ggplot(df, aes(x = intervention, y = net_dalys_full, fill = above_zero)) +
    geom_col(width = 0.75) +
    geom_hline(yintercept = 0, color = "#C0392B", linewidth = 0.9) +
    coord_flip() +
    scale_fill_manual(values = c(`TRUE` = "#1F4E78", `FALSE` = "#B5533C"), guide = "none") +
    labs(
      title = "Interventions ranked by net health benefit (full implementation)",
      subtitle = "Red line marks zero net health benefit",
      x = NULL, y = "Net DALYs averted"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      panel.grid.minor = element_blank()
    )
}
