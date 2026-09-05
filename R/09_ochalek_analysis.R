# ============================================================
# Efficiency, affordability, and cost-effectiveness-threshold
# (CET) sensitivity analysis
#
# Three views of the league table, each answering a different
# planning question:
#
#   - Efficiency frontier: ordering every costed, effective
#     intervention by its ICER and stacking their cost gives a
#     declining curve of DALYs averted per $1,000 spent - the
#     rate at which additional spending buys additional health
#     as the budget is allocated to the best-value interventions
#     first. A threshold line at the CET marks where that rate
#     stops clearing the bar the health system is willing to pay.
#   - Cumulative net health benefit: adding interventions in
#     descending order of net health benefit (rank_nhp) traces
#     how total benefit accumulates, and where it would start
#     falling if lower-priority interventions were added anyway.
#     Comparing full and realistic implementation shows how much
#     benefit is left on the table by current coverage levels.
#   - CET sensitivity: net health benefit is a function of the
#     threshold itself, so the affordability picture (how many
#     interventions clear the bar, and at what total cost and
#     DALYs) changes with it. Senegal has no set of alternative
#     CET estimates comparable to Ochalek et al. (2016)'s
#     $38/$61/$120 range for Malawi, so scenarios here are
#     expressed as a proportion of the reference threshold
#     (config$cet_usd_per_daly) instead.
# ============================================================

library(dplyr)
library(ggplot2)

#' Recompute net health benefit, its full/realistic difference, and
#' the rank_nhp ordering for an alternative CET, from a league table
#' that already has totals computed (total_cost_full_usd,
#' total_dalys_full, total_cost_realistic_usd, total_dalys_realistic).
#' ICER itself does not depend on the CET, so icer_usd/icer_rank are
#' left untouched.
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param cet_usd_per_daly The CET to apply
#' @return league_table with net_dalys_realistic, net_dalys_full,
#'   diff_net_dalys, health_system_value_usd, and rank_nhp recomputed
recompute_net_benefit <- function(league_table, cet_usd_per_daly) {
  league_table %>%
    mutate(
      net_dalys_realistic     = total_dalys_realistic - total_cost_realistic_usd / cet_usd_per_daly,
      net_dalys_full          = total_dalys_full - total_cost_full_usd / cet_usd_per_daly,
      diff_net_dalys          = net_dalys_full - net_dalys_realistic,
      health_system_value_usd = diff_net_dalys * cet_usd_per_daly
    ) %>%
    arrange(desc(net_dalys_full)) %>%
    mutate(rank_nhp = row_number())
}

#' Efficiency frontier: interventions ordered by ICER, each drawn as a
#' bar whose width is its full-implementation cost and whose height is
#' DALYs averted per $1,000 spent, with a threshold line at the CET
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param cet_usd_per_daly Cost-effectiveness threshold
#' @return A ggplot object
build_efficiency_frontier_plot <- function(league_table, cet_usd_per_daly) {
  df <- league_table %>%
    filter(!is.na(icer_usd), total_cost_full_usd > 0) %>%
    arrange(icer_rank) %>%
    mutate(
      dalys_per_1000usd = 1000 / icer_usd,
      xmax = cumsum(total_cost_full_usd),
      xmin = xmax - total_cost_full_usd
    )

  threshold_efficiency <- 1000 / cet_usd_per_daly

  # DALYs averted per $1,000 spans several orders of magnitude across
  # this many interventions (a handful of very cheap, very effective
  # ones alongside costly, low-yield ones) - a log y-axis is needed
  # for the low-yield end of the frontier to stay visible at all.
  log_floor <- min(df$dalys_per_1000usd, na.rm = TRUE) / 2

  ggplot(df) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = log_floor, ymax = dalys_per_1000usd,
          fill = dalys_per_1000usd >= threshold_efficiency),
      color = "white", linewidth = 0.1
    ) +
    geom_hline(yintercept = threshold_efficiency, color = "#C0392B", linetype = "dashed", linewidth = 0.8) +
    scale_y_log10(labels = scales::label_comma()) +
    scale_fill_manual(values = c(`TRUE` = "#1F4E78", `FALSE` = "#B5533C"), guide = "none") +
    labs(
      title = "Efficiency frontier: DALYs averted per $1,000 spent, ordered by ICER",
      subtitle = paste0(
        "Dashed line: threshold efficiency at CET = $", cet_usd_per_daly,
        " per DALY averted. Log scale: values span several orders of magnitude"
      ),
      x = "Cumulative cost, full implementation ($)",
      y = "DALYs averted per $1,000 spent (log scale)"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank())
}

#' Cumulative net health benefit as interventions are added in
#' descending order of net health benefit, full vs realistic
#' implementation
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @return A ggplot object
build_net_benefit_curve_plot <- function(league_table) {
  df <- league_table %>%
    arrange(rank_nhp) %>%
    mutate(
      cumulative_cost_full           = cumsum(coalesce(total_cost_full_usd, 0)),
      cumulative_net_dalys_full      = cumsum(coalesce(net_dalys_full, 0)),
      cumulative_cost_realistic      = cumsum(coalesce(total_cost_realistic_usd, 0)),
      cumulative_net_dalys_realistic = cumsum(coalesce(net_dalys_realistic, 0))
    )

  long_df <- bind_rows(
    df %>% transmute(rank_nhp, cumulative_cost = cumulative_cost_full,
                      cumulative_net_dalys = cumulative_net_dalys_full,
                      scenario = "Full implementation"),
    df %>% transmute(rank_nhp, cumulative_cost = cumulative_cost_realistic,
                      cumulative_net_dalys = cumulative_net_dalys_realistic,
                      scenario = "Realistic implementation")
  )

  ggplot(long_df, aes(x = cumulative_cost, y = cumulative_net_dalys, color = scenario)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, color = "#C0392B", linetype = "dashed") +
    scale_color_manual(values = c(
      "Full implementation" = "#1F4E78", "Realistic implementation" = "#7FA6C9"
    )) +
    labs(
      title = "Cumulative net health benefit as interventions are added\n(in descending order of net health benefit)",
      x = "Cumulative cost ($)", y = "Cumulative net DALYs averted", color = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "top", panel.grid.minor = element_blank()
    )
}

#' Interventions affordable at a given CET (net health benefit >= 0
#' under full implementation), and the resulting budget and DALYs
#'
#' @param league_table Output of build_intervention_funnel()$league_table,
#'   or recompute_net_benefit() output for a non-reference CET
#' @param cet_usd_per_daly The CET this table applies to
#' @return A list with:
#'   table   - the qualifying interventions, in rank_nhp order
#'   summary - one row: cet_usd_per_daly, n_included, total_cost_full_usd,
#'             total_dalys_full, total_net_dalys_full
build_affordability_table <- function(league_table, cet_usd_per_daly) {
  qualifying <- league_table %>%
    filter(!is.na(net_dalys_full), net_dalys_full >= 0) %>%
    arrange(rank_nhp)

  summary <- data.frame(
    cet_usd_per_daly     = cet_usd_per_daly,
    n_included           = nrow(qualifying),
    total_cost_full_usd  = sum(qualifying$total_cost_full_usd, na.rm = TRUE),
    total_dalys_full     = sum(qualifying$total_dalys_full, na.rm = TRUE),
    total_net_dalys_full = sum(qualifying$net_dalys_full, na.rm = TRUE)
  )

  list(table = qualifying, summary = summary)
}

#' CET sensitivity table: affordability summary recomputed at each of
#' several CET scenarios, expressed as multipliers of the reference CET
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#'   (computed at the reference CET)
#' @param reference_cet config$cet_usd_per_daly
#' @param multipliers Named numeric vector of multipliers to apply to
#'   the reference CET (config$cet_sensitivity_multipliers)
#' @return One row per scenario: scenario, cet_usd_per_daly, n_included,
#'   total_cost_full_usd, total_dalys_full, total_net_dalys_full
build_cet_sensitivity_table <- function(league_table, reference_cet, multipliers) {
  lapply(names(multipliers), function(scenario_label) {
    cet_value <- reference_cet * multipliers[[scenario_label]]
    recomputed <- recompute_net_benefit(league_table, cet_value)
    affordability <- build_affordability_table(recomputed, cet_value)$summary
    cbind(data.frame(scenario = scenario_label), affordability)
  }) %>% bind_rows()
}
