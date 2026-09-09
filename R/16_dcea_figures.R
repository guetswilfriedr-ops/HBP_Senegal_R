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

#' Format a DALY total as millions for axis labels (e.g. 12,345,000 -> "12.3M")
millions_label <- function(x) paste0(format(round(x / 1e6, 1), nsmall = 1), "M")

#' Format a DALY total as thousands (e.g. 103,307 -> "103K") - used for
#' quantities too small to read once axes are scaled to millions
thousands_label <- function(x) paste0(format(round(x / 1e3, 0), nsmall = 0), "K")

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
      scale_color_manual(values = c(`TRUE` = "#000066", `FALSE` = "#D0CCD0"), name = NULL)
  } else {
    p <- p + geom_point(color = "#000066", alpha = 0.75, size = 2.2)
  }

  # Label up to 4 standout interventions - candidates are the top 3 by
  # net benefit plus the single most negative and single most positive
  # inequality impact, ranked by how far each sits from the origin (in
  # axis-normalized units) and added greedily, skipping any candidate
  # that would land too close to an already-labeled point. This yields
  # 2-4 labels depending on how clustered the data actually is, rather
  # than forcing 4 labels into a dense cluster where they'd overlap.
  x_range <- range(df$inequality_impact)
  y_range <- range(df$total_net_benefit)
  x_span <- diff(x_range)
  y_span <- diff(y_range)

  all_points <- df %>%
    dplyr::mutate(
      x_norm = (inequality_impact - x_range[1]) / x_span,
      y_norm = (total_net_benefit - y_range[1]) / y_span
    )

  candidates <- dplyr::bind_rows(
    df %>% dplyr::slice_max(total_net_benefit, n = 3),
    df %>% dplyr::slice_min(inequality_impact, n = 1),
    df %>% dplyr::slice_max(inequality_impact, n = 1)
  ) %>%
    dplyr::distinct(intervention, .keep_all = TRUE) %>%
    dplyr::mutate(
      x_norm = (inequality_impact - x_range[1]) / x_span,
      y_norm = (total_net_benefit - y_range[1]) / y_span,
      extremeness = pmax(abs(x_norm - 0.5), abs(y_norm - 0.5))
    ) %>%
    dplyr::arrange(dplyr::desc(extremeness))

  min_sep <- 0.12       # minimum normalized-plot distance between two labels
  min_sep_cluster <- 0.035  # minimum distance from ANY point, so a label
                           # is never dropped right on top of the dense
                           # unlabeled cluster it's meant to stand out from
  selected <- candidates[0, ]
  for (i in seq_len(nrow(candidates))) {
    cand <- candidates[i, ]
    d_cluster <- sqrt((all_points$x_norm - cand$x_norm)^2 + (all_points$y_norm - cand$y_norm)^2)
    n_close <- sum(d_cluster < min_sep_cluster) - 1  # exclude the point itself
    if (n_close > 0) next
    if (nrow(selected) == 0) {
      selected <- cand
    } else {
      d <- sqrt((selected$x_norm - cand$x_norm)^2 + (selected$y_norm - cand$y_norm)^2)
      if (all(d >= min_sep) && nrow(selected) < 4) selected <- dplyr::bind_rows(selected, cand)
    }
  }
  truncate_label <- function(x, max_chars = 32) {
    ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 1), "…"), x)
  }

  labeled_points <- selected %>%
    dplyr::mutate(
      label_short = truncate_label(intervention),
      label_hjust = ifelse(x_norm <= 0.5, 0, 1),
      label_nudge_x = x_span * 0.02 * ifelse(x_norm <= 0.5, 1, -1)
    )

  p +
    geom_hline(yintercept = 0, color = "#7C797C", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "#7C797C", linewidth = 0.4) +
    geom_text(
      data = labeled_points,
      aes(label = label_short, hjust = label_hjust),
      vjust = 0.5,
      nudge_x = labeled_points$label_nudge_x,
      size = 2.8, color = "#000066", fontface = "bold"
    ) +
    scale_x_continuous(labels = millions_label) +
    scale_y_continuous(labels = millions_label) +
    labs(
      x = paste0("Inequality impact, DALYs-equivalent (millions) - by ", dimension_label),
      y = "Net population health benefit, DALYs averted (millions)",
      caption = paste0(
        "Quadrants: ++ benefit and narrows inequality  |  +- benefit but widens inequality  |  ",
        "-+ loss but narrows inequality  |  -- loss and widens inequality"
      )
    ) +
    liser_chart_theme(base_size = 10)
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
    geom_point(data = ~ filter(.x, component == "Net benefit"), color = "#000000", size = 2.5) +
    geom_hline(yintercept = 0, color = "#7C797C", linewidth = 0.4) +
    scale_fill_manual(values = c(
      "Direct benefit" = "#000066", "Opportunity cost" = "#E30613", "Net benefit" = "#000000"
    ), name = NULL) +
    labs(x = NULL, y = unit_label) +
    liser_chart_theme(base_size = 10) +
    theme(legend.position = "top")
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
    geom_col(aes(y = baseline), fill = "#CCC6E0", width = 0.6) +
    geom_col(aes(y = baseline + gain), fill = NA, color = "#000066", linewidth = 0.9, width = 0.6) +
    geom_text(aes(y = baseline + gain, label = sprintf("+%.2f", gain)), vjust = -0.4, size = 3.2) +
    coord_cartesian(ylim = c(min(df$baseline) * 0.9, max(df$baseline + df$gain) * 1.05)) +
    labs(x = NULL, y = "HALE (years)") +
    liser_chart_theme(base_size = 10)
}

#' Direct benefit / opportunity cost / net benefit by group, full and
#' realistic implementation side by side in one figure (facets), so the
#' two scenarios can be compared directly instead of across two
#' separate images
#'
#' @param group_summary Output of aggregate_dcea_by_group() (R/12)
#' @param group_type "wealth" or "residence"
build_benefit_breakdown_combined_plot <- function(group_summary, group_type = c("wealth", "residence")) {
  group_type <- match.arg(group_type)
  labels <- group_labels_for(group_type)

  build_scenario_df <- function(scenario) {
    suffix <- if (scenario == "full") "" else "_realistic"
    scenario_label <- if (scenario == "full") "Full implementation" else "Realistic implementation"
    group_summary %>%
      transmute(
        group = factor(labels[group_id], levels = unname(labels)),
        `Direct benefit` = .data[[paste0("direct_benefit", suffix)]],
        `Opportunity cost` = -.data[[paste0("opportunity_cost", suffix)]],
        `Net benefit` = .data[[paste0("net_benefit", suffix)]]
      ) %>%
      tidyr::pivot_longer(-group, names_to = "component", values_to = "value") %>%
      mutate(scenario = factor(scenario_label, levels = c("Full implementation", "Realistic implementation")))
  }

  df <- dplyr::bind_rows(build_scenario_df("full"), build_scenario_df("realistic")) %>%
    mutate(component = factor(component, levels = c("Direct benefit", "Opportunity cost", "Net benefit")))

  # Opportunity cost is genuinely 1-2% of direct benefit for this
  # package (a sign of a cost-effective package, not a data error) -
  # its bar is real but reads as a sliver once the axis covers tens of
  # millions of DALYs. Rather than a "Net benefit" dot that lands
  # almost exactly on the direct-benefit bar's own top (indistinguishable
  # from it at this scale, and easy to mistake for clutter), both
  # quantities get an explicit text value instead: net benefit above
  # the bar, opportunity cost near the axis in thousands so it stays
  # legible even though its bar segment is not.
  bar_df <- df %>% dplyr::filter(component != "Net benefit")
  net_label_df <- df %>%
    dplyr::filter(component == "Direct benefit") %>%
    dplyr::inner_join(
      df %>% dplyr::filter(component == "Net benefit") %>%
        dplyr::select(group, scenario, net_value = value),
      by = c("group", "scenario")
    )
  opp_label_df <- df %>% dplyr::filter(component == "Opportunity cost")

  ggplot(bar_df, aes(x = group, y = value, fill = component)) +
    geom_col(position = "identity", alpha = 0.85) +
    geom_hline(yintercept = 0, color = "#7C797C", linewidth = 0.4) +
    geom_text(
      data = net_label_df, aes(y = value, label = paste0("Net: ", millions_label(net_value))),
      inherit.aes = TRUE, vjust = -0.4, size = 2.6, fontface = "bold", color = "#000066"
    ) +
    geom_text(
      data = opp_label_df,
      aes(y = pmin(value, 0), label = paste0("Opp. cost: ", thousands_label(-value))),
      inherit.aes = TRUE, vjust = 1.3, size = 2.2, color = "#E30613"
    ) +
    facet_wrap(~scenario, nrow = 1) +
    scale_fill_manual(values = c(
      "Direct benefit" = "#000066", "Opportunity cost" = "#E30613"
    ), name = NULL) +
    scale_y_continuous(labels = millions_label, expand = expansion(mult = c(0.12, 0.12))) +
    labs(x = NULL, y = "DALYs (millions)") +
    liser_chart_theme(base_size = 10) +
    theme(legend.position = "top", strip.background = element_rect(fill = "#CCC6E0", color = NA))
}

#' Baseline vs. post-package HALE, wealth quintile and residence side by
#' side in one figure (facets, free x scale since the two stratifiers
#' have a different number of groups)
#'
#' @param baseline_hale Output of build_baseline_hale() (R/13)
#' @param distribution Output of build_dcea_distribution() (R/12)
#' @param interventions Character vector of intervention names in the package
#' @param national_population config$dcea$national_population
#' @param scenario "full" or "realistic"
build_hale_combined_plot <- function(baseline_hale, distribution, interventions, national_population,
                                      scenario = c("full", "realistic")) {
  scenario <- match.arg(scenario)
  net_col <- if (scenario == "full") "net_benefit" else "net_benefit_realistic"

  build_group_df <- function(group_type) {
    group_ids <- group_ids_for(group_type)
    labels <- group_labels_for(group_type)
    base <- get_baseline_vectors(baseline_hale, group_type, group_ids)
    group_population <- base$pop_weights * national_population

    by_group <- distribution %>%
      filter(.data$group_type == !!group_type, intervention %in% interventions) %>%
      group_by(group_id) %>%
      summarise(net_benefit = sum(.data[[net_col]], na.rm = TRUE), .groups = "drop") %>%
      mutate(net_benefit_per_capita = net_benefit / group_population[group_id])

    data.frame(
      stratifier = if (group_type == "wealth") "Wealth quintile" else "Residence",
      group = factor(labels[group_ids], levels = unname(labels)),
      baseline = as.numeric(base$health[group_ids]),
      gain = by_group$net_benefit_per_capita[match(group_ids, by_group$group_id)]
    )
  }

  df <- dplyr::bind_rows(build_group_df("wealth"), build_group_df("residence")) %>%
    mutate(stratifier = factor(stratifier, levels = c("Wealth quintile", "Residence")))

  ggplot(df, aes(x = group)) +
    geom_col(aes(y = baseline), fill = "#CCC6E0", width = 0.6) +
    geom_col(aes(y = baseline + gain), fill = NA, color = "#000066", linewidth = 0.9, width = 0.6) +
    geom_text(aes(y = baseline + gain, label = sprintf("+%.2f", gain)), vjust = -0.4, size = 3.0) +
    facet_wrap(~stratifier, nrow = 1, scales = "free_x") +
    coord_cartesian(ylim = c(min(df$baseline) * 0.9, max(df$baseline + df$gain) * 1.05)) +
    labs(x = NULL, y = "HALE (years)") +
    liser_chart_theme(base_size = 10) +
    theme(strip.background = element_rect(fill = "#CCC6E0", color = NA))
}
