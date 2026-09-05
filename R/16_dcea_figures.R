# ============================================================
# DCEA figures
#
# Reproduces, for Senegal, the figure types in Arnold, Nkhoma &
# Griffin (2020):
#   - Figure 2 (health equity impact plane): build_equity_plane_plot()
#   - Figure 3b/3d (direct benefit / opportunity cost / net benefit by
#     quintile): build_benefit_breakdown_plot()
#   - Figure 4b (baseline vs. post-package HALE by quintile):
#     build_hale_plot()
#
# Note: Arnold et al. show both a wealth-quintile and an urban/rural
# breakdown throughout. This project's DCEA prep workbook only builds
# a wealth-quintile distribution (tab 2/3/4 have no urban/rural split)
# - reproducing the residence dimension would need its own D/E/F rows
# by residence, which is a natural next extension but out of scope for
# this first working version.
# ============================================================

library(ggplot2)
library(dplyr)

quintile_labels <- c(
  q1 = "Poorest", q2 = "Poorer", q3 = "Middle", q4 = "Richer", q5 = "Richest"
)

#' Health equity impact plane: one point per intervention, x = its
#' impact on inequality (delta EDE - net health benefit), y = its net
#' health benefit - the Senegal equivalent of Arnold et al.'s Figure 2
#'
#' @param equity_metrics The `per_intervention` element of
#'   compute_equity_metrics()'s output (R/14)
#' @param highlight_col Optional logical column name in
#'   `equity_metrics` used to distinguish two point styles (e.g. an
#'   "included in a hypothetical package" flag); NULL draws all points
#'   the same way
build_equity_plane_plot <- function(equity_metrics, highlight_col = NULL) {
  df <- equity_metrics %>% filter(!is.na(inequality_impact), !is.na(total_net_benefit))

  p <- ggplot(df, aes(x = inequality_impact, y = total_net_benefit))

  if (!is.null(highlight_col) && highlight_col %in% names(df)) {
    p <- p + geom_point(aes(color = .data[[highlight_col]]), alpha = 0.75, size = 2.2) +
      scale_color_manual(values = c(`TRUE` = "#1F4E78", `FALSE` = "#B0B0B0"), name = NULL)
  } else {
    p <- p + geom_point(color = "#1F4E78", alpha = 0.75, size = 2.2)
  }

  p +
    geom_hline(yintercept = 0, color = "#7F7F7F", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "#7F7F7F", linewidth = 0.4) +
    labs(
      title = "Health equity impact plane",
      subtitle = "Each point is one intervention. Right of the vertical line: reduces health inequality.\nAbove the horizontal line: increases total population health.",
      x = "Inequality impact (DALYs averted-equivalent; ΔEDE − net health benefit)",
      y = "Net population health benefit (DALYs averted)"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank())
}

#' Direct benefit / opportunity cost / net benefit by quintile, summed
#' across a set of interventions - the Senegal equivalent of Arnold et
#' al.'s Figure 3b (current utilization) / 3d (full utilization)
#'
#' @param quintile_summary Output of aggregate_dcea_by_quintile() (R/12)
#' @param scenario "full" or "realistic" - which implementation
#'   scenario's columns to plot
#' @param unit_divisor Divide DALY totals by this before plotting
#'   (e.g. 1000 to show "thousands of DALYs"); default 1 (raw DALYs)
#' @param unit_label Axis label matching `unit_divisor`
build_benefit_breakdown_plot <- function(quintile_summary, scenario = c("full", "realistic"),
                                          unit_divisor = 1, unit_label = "DALYs averted") {
  scenario <- match.arg(scenario)
  suffix <- if (scenario == "full") "" else "_realistic"

  df <- quintile_summary %>%
    transmute(
      quintile = factor(quintile_labels[quintile], levels = unname(quintile_labels)),
      `Direct benefit` = .data[[paste0("direct_benefit", suffix)]] / unit_divisor,
      `Opportunity cost` = -.data[[paste0("opportunity_cost", suffix)]] / unit_divisor,
      `Net benefit` = .data[[paste0("net_benefit", suffix)]] / unit_divisor
    ) %>%
    tidyr::pivot_longer(-quintile, names_to = "component", values_to = "value") %>%
    mutate(component = factor(component, levels = c("Direct benefit", "Opportunity cost", "Net benefit")))

  scenario_title <- if (scenario == "full") "full implementation" else "realistic implementation"

  ggplot(df, aes(x = quintile, y = value, fill = component)) +
    geom_col(data = ~ filter(.x, component != "Net benefit"), position = "identity", alpha = 0.85) +
    geom_point(data = ~ filter(.x, component == "Net benefit"), color = "#1F1F1F", size = 2.5) +
    geom_hline(yintercept = 0, color = "#7F7F7F", linewidth = 0.4) +
    scale_fill_manual(values = c(
      "Direct benefit" = "#1F4E78", "Opportunity cost" = "#B5533C", "Net benefit" = "#1F1F1F"
    ), name = NULL) +
    labs(
      title = paste0("Direct benefit, opportunity cost and net benefit by quintile (", scenario_title, ")"),
      x = NULL, y = unit_label
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "top", panel.grid.minor = element_blank()
    )
}

#' Baseline vs. post-package HALE by quintile - the Senegal equivalent
#' of Arnold et al.'s Figure 4b
#'
#' @param baseline_hale Output of build_baseline_hale() (R/13)
#' @param package_equity Output of compute_package_equity() (R/14),
#'   for the same package/scenario/epsilon as `baseline_hale`
#' @param national_population config$dcea$national_population, used
#'   to convert the package's total net benefit back into a per-
#'   quintile per-capita HALE gain (must match what was used to build
#'   `package_equity`)
#' @param distribution Output of build_dcea_distribution() (R/12), used
#'   to recover the per-quintile net benefit for the same intervention
#'   set as `package_equity`
#' @param interventions Character vector of intervention names in the
#'   package (must match what was passed to compute_package_equity())
#' @param scenario "full" or "realistic"
build_hale_plot <- function(baseline_hale, distribution, interventions, national_population,
                             scenario = c("full", "realistic")) {
  scenario <- match.arg(scenario)
  net_col <- if (scenario == "full") "net_benefit" else "net_benefit_realistic"
  quintile_ids <- c("q1", "q2", "q3", "q4", "q5")
  quintile_population <- national_population / length(quintile_ids)

  by_quintile <- distribution %>%
    filter(intervention %in% interventions) %>%
    group_by(quintile) %>%
    summarise(net_benefit = sum(.data[[net_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(net_benefit_per_capita = net_benefit / quintile_population)

  df <- data.frame(
    quintile = factor(quintile_labels[quintile_ids], levels = unname(quintile_labels)),
    baseline = as.numeric(baseline_hale[1, quintile_ids]),
    gain = by_quintile$net_benefit_per_capita[match(quintile_ids, by_quintile$quintile)]
  )

  ggplot(df, aes(x = quintile)) +
    geom_col(aes(y = baseline), fill = "#B7C9DC", width = 0.6) +
    geom_col(aes(y = baseline + gain), fill = NA, color = "#1F4E78", linewidth = 0.9, width = 0.6) +
    geom_text(aes(y = baseline + gain, label = sprintf("+%.2f", gain)), vjust = -0.4, size = 3.2) +
    coord_cartesian(ylim = c(min(df$baseline) * 0.9, max(df$baseline + df$gain) * 1.05)) +
    labs(
      title = "Baseline vs. post-package healthy life expectancy (HALE), by quintile",
      subtitle = "Shaded bar: baseline HALE (simplified proxy, see R/13_dcea_baseline.R). Outline: baseline + package net benefit.",
      x = NULL, y = "HALE (years)"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank())
}
