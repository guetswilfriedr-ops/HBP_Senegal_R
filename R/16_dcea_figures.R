# ============================================================
# DCEA figures
#
# Reproduces, for Senegal, the figure types in Arnold, Nkhoma &
# Griffin (2020), for BOTH stratifiers (wealth quintile and
# residence):
#   - Figure 2 (health equity impact plane): build_equity_plane_plot()
#   - Figure 3 (direct benefit / opportunity cost / net benefit by
#     group): build_benefit_breakdown_plot()
#   - Figure 4 (baseline vs. post-package HALE by group):
#     build_hale_plot()
# Each function takes a `group_type` ("wealth" or "residence") and
# uses R/10_dcea_import.R's wealth_group_labels / residence_group_labels
# for axis text.
# ============================================================

library(ggplot2)
library(dplyr)

group_labels_for <- function(group_type) {
  if (group_type == "wealth") wealth_group_labels else residence_group_labels
}
group_ids_for <- function(group_type) {
  if (group_type == "wealth") wealth_group_ids else residence_group_ids
}

#' Health equity impact plane: one point per intervention, x = its
#' impact on inequality (delta EDE, population-scaled, minus net
#' health benefit), y = its net health benefit - the Senegal
#' equivalent of Arnold et al.'s Figure 2
#'
#' @param equity_metrics The `per_intervention` element of
#'   compute_equity_metrics()'s output (R/14), for ONE group_type
#' @param group_type "wealth" or "residence" - used only for the title
#' @param highlight_col Optional logical column name in
#'   `equity_metrics` used to distinguish two point styles (e.g. an
#'   "included in a hypothetical package" flag); NULL draws all points
#'   the same way
build_equity_plane_plot <- function(equity_metrics, group_type = c("wealth", "residence"), highlight_col = NULL) {
  group_type <- match.arg(group_type)
  df <- equity_metrics %>% filter(!is.na(inequality_impact), !is.na(total_net_benefit))
  dimension_label <- if (group_type == "wealth") "wealth quintile" else "residence (urban/rural)"

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
      title = paste0("Health equity impact plane (by ", dimension_label, ")"),
      subtitle = "Each point is one intervention. Right of the vertical line: reduces health inequality.\nAbove the horizontal line: increases total population health.",
      x = "Inequality impact (DALYs averted-equivalent; population-scaled ΔEDE − net health benefit)",
      y = "Net population health benefit (DALYs averted)"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank())
}

#' Direct benefit / opportunity cost / net benefit by group, summed
#' across a set of interventions - the Senegal equivalent of Arnold et
#' al.'s Figure 3
#'
#' @param group_summary Output of aggregate_dcea_by_group() (R/12)
#' @param group_type "wealth" or "residence"
#' @param scenario "full" or "realistic" - which implementation
#'   scenario's columns to plot
#' @param unit_divisor Divide DALY totals by this before plotting
#'   (e.g. 1000 to show "thousands of DALYs"); default 1 (raw DALYs)
#' @param unit_label Axis label matching `unit_divisor`
build_benefit_breakdown_plot <- function(group_summary, group_type = c("wealth", "residence"),
                                          scenario = c("full", "realistic"),
                                          unit_divisor = 1, unit_label = "DALYs averted") {
  group_type <- match.arg(group_type)
  scenario <- match.arg(scenario)
  suffix <- if (scenario == "full") "" else "_realistic"
  labels <- group_labels_for(group_type)

  df <- group_summary %>%
    transmute(
      group = factor(labels[group_id], levels = unname(labels)),
      `Direct benefit` = .data[[paste0("direct_benefit", suffix)]] / unit_divisor,
      `Opportunity cost` = -.data[[paste0("opportunity_cost", suffix)]] / unit_divisor,
      `Net benefit` = .data[[paste0("net_benefit", suffix)]] / unit_divisor
    ) %>%
    tidyr::pivot_longer(-group, names_to = "component", values_to = "value") %>%
    mutate(component = factor(component, levels = c("Direct benefit", "Opportunity cost", "Net benefit")))

  scenario_title <- if (scenario == "full") "full implementation" else "realistic implementation"
  dimension_label <- if (group_type == "wealth") "wealth quintile" else "residence"

  ggplot(df, aes(x = group, y = value, fill = component)) +
    geom_col(data = ~ filter(.x, component != "Net benefit"), position = "identity", alpha = 0.85) +
    geom_point(data = ~ filter(.x, component == "Net benefit"), color = "#1F1F1F", size = 2.5) +
    geom_hline(yintercept = 0, color = "#7F7F7F", linewidth = 0.4) +
    scale_fill_manual(values = c(
      "Direct benefit" = "#1F4E78", "Opportunity cost" = "#B5533C", "Net benefit" = "#1F1F1F"
    ), name = NULL) +
    labs(
      title = paste0("Direct benefit, opportunity cost and net benefit by ", dimension_label, " (", scenario_title, ")"),
      x = NULL, y = unit_label
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "top", panel.grid.minor = element_blank()
    )
}

#' Baseline vs. post-package HALE by group - the Senegal equivalent of
#' Arnold et al.'s Figure 4
#'
#' @param baseline_hale Output of build_baseline_hale() (R/13)
#' @param distribution Output of build_dcea_distribution() (R/12)
#' @param interventions Character vector of intervention names in the
#'   package
#' @param national_population config$dcea$national_population
#' @param group_type "wealth" or "residence"
#' @param scenario "full" or "realistic"
build_hale_plot <- function(baseline_hale, distribution, interventions, national_population,
                             group_type = c("wealth", "residence"), scenario = c("full", "realistic")) {
  group_type <- match.arg(group_type)
  scenario <- match.arg(scenario)
  net_col <- if (scenario == "full") "net_benefit" else "net_benefit_realistic"
  group_ids <- group_ids_for(group_type)
  labels <- group_labels_for(group_type)

  base <- get_baseline_vectors(baseline_hale, group_type, group_ids)
  group_population <- base$pop_weights * national_population

  by_group <- distribution %>%
    filter(.data$group_type == !!group_type, intervention %in% interventions) %>%
    group_by(group_id) %>%
    summarise(net_benefit = sum(.data[[net_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(net_benefit_per_capita = net_benefit / group_population[group_id])

  dimension_label <- if (group_type == "wealth") "wealth quintile" else "residence"

  df <- data.frame(
    group = factor(labels[group_ids], levels = unname(labels)),
    baseline = as.numeric(base$health[group_ids]),
    gain = by_group$net_benefit_per_capita[match(group_ids, by_group$group_id)]
  )

  ggplot(df, aes(x = group)) +
    geom_col(aes(y = baseline), fill = "#B7C9DC", width = 0.6) +
    geom_col(aes(y = baseline + gain), fill = NA, color = "#1F4E78", linewidth = 0.9, width = 0.6) +
    geom_text(aes(y = baseline + gain, label = sprintf("+%.2f", gain)), vjust = -0.4, size = 3.2) +
    coord_cartesian(ylim = c(min(df$baseline) * 0.9, max(df$baseline + df$gain) * 1.05)) +
    labs(
      title = paste0("Baseline vs. post-package healthy life expectancy (HALE), by ", dimension_label),
      subtitle = "Shaded bar: baseline HALE (simplified proxy, see R/13_dcea_baseline.R). Outline: baseline + package net benefit.",
      x = NULL, y = "HALE (years)"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank())
}
