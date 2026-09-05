# ============================================================
# Efficiency, affordability, and cost-effectiveness-threshold
# (CET) sensitivity analysis
#
# Four views of the league table, each answering a different
# planning question, structured after Ochalek, Claxton, Revill,
# Sculpher & Rollinger (2016) CHE Research Paper 136:
#
#   - Efficiency frontier (their Figure 5): ordering every costed,
#     effective intervention by its ICER and stacking their cost
#     gives a declining curve of DALYs averted per $1,000 spent -
#     the rate at which additional spending buys additional
#     health as the budget is allocated to the best-value
#     interventions first. A threshold line at the CET marks
#     where that rate stops clearing the bar the health system is
#     willing to pay.
#   - Interventions ranked by net health benefit, full
#     implementation (their Figure 6): net DALYs averted per
#     intervention (which can be negative for one too costly to
#     be worth including) alongside the cumulative spend as more
#     interventions are added.
#   - Full vs realistic implementation (their Figure 7): the same
#     ranking, comparing net DALYs averted and cumulative spend at
#     100% implementation (solid lines) against the coverage
#     actually expected (dotted lines) - the gap between the two
#     is health left on the table by partial coverage.
#   - Budget reallocation (their Table 7): the underspend created
#     by partial implementation of the affordable core package can
#     fund additional interventions beyond it; this adds them, in
#     ascending ICER order, until the underspend runs out.
#   - CET sensitivity (their Table 8): affordability is a function
#     of the threshold itself, so this recomputes the affordable
#     package, its budget, and the reallocation above at several
#     CET scenarios. Senegal has no published set of alternative
#     CET estimates comparable to Ochalek et al.'s $38/$61/$120
#     range for Malawi, so scenarios here are expressed as a
#     proportion of the reference threshold (config$cet_usd_per_daly).
# ============================================================

library(dplyr)
library(ggplot2)

dollars_millions <- scales::label_dollar(scale = 1e-6, suffix = "M", accuracy = 1)

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

#' Figure 5 equivalent: for the interventions affordable at the CET
#' (ICER at or below the threshold) only, each drawn as a bar whose
#' width is its full-implementation cost and whose height is DALYs
#' averted per $1,000 spent, ordered by ICER, with a threshold line at
#' the CET and each bar labelled with its ICER rank (matching Table 4)
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param cet_usd_per_daly Cost-effectiveness threshold
#' @return A ggplot object
build_efficiency_frontier_plot <- function(league_table, cet_usd_per_daly) {
  df <- league_table %>%
    filter(!is.na(icer_usd), icer_usd <= cet_usd_per_daly, total_cost_full_usd > 0) %>%
    arrange(icer_rank) %>%
    mutate(
      xmax = cumsum(total_cost_full_usd),
      xmin = xmax - total_cost_full_usd,
      xmid = (xmin + xmax) / 2
    )

  threshold_efficiency <- 1000 / cet_usd_per_daly

  # DALYs averted per $1,000 spans several orders of magnitude across
  # this many interventions (a handful of very cheap, very effective
  # ones alongside costly, low-yield ones) - a log y-axis is needed
  # for the low-yield end of the frontier to stay visible at all.
  log_floor <- min(df$dalys_per_1000usd, na.rm = TRUE) / 2
  x_span <- max(df$xmax) - min(df$xmin)
  budget_usd <- max(df$xmax)

  best_value <- df %>% slice_head(n = 1)

  ggplot(df) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = log_floor, ymax = dalys_per_1000usd),
      fill = "#1F4E78", color = "white", linewidth = 0.1
    ) +
    geom_text(
      aes(x = xmid, y = dalys_per_1000usd, label = icer_rank),
      angle = 90, hjust = -0.2, size = 1.9, color = "#1F4E78"
    ) +
    geom_hline(yintercept = threshold_efficiency, color = "#C0392B", linetype = "dashed", linewidth = 0.8) +
    annotate(
      "text", x = min(df$xmin), y = threshold_efficiency, vjust = -0.6, hjust = 0,
      label = paste0("CET threshold: ", round(threshold_efficiency, 1), " DALYs / $1,000"),
      color = "#C0392B", size = 3, fontface = "italic"
    ) +
    annotate(
      "segment", x = best_value$xmax, xend = best_value$xmax + x_span * 0.12,
      y = best_value$dalys_per_1000usd, yend = best_value$dalys_per_1000usd * 0.35,
      arrow = arrow(length = unit(0.15, "cm")), color = "#1F4E78"
    ) +
    annotate(
      "text", x = best_value$xmax + x_span * 0.13, y = best_value$dalys_per_1000usd * 0.35,
      label = paste0("Best value (rank 1):\n", strwrap(best_value$intervention, width = 28) %>% paste(collapse = "\n")),
      hjust = 0, size = 2.8, color = "#1F4E78", lineheight = 0.9
    ) +
    annotate(
      "text", x = budget_usd, y = log_floor, vjust = 1.6, hjust = 1,
      label = paste0("Implied budget for the affordable package: ", dollars_millions(budget_usd)),
      size = 2.8, color = "#7A3324", fontface = "italic"
    ) +
    scale_x_continuous(labels = dollars_millions, expand = expansion(mult = c(0.01, 0.22))) +
    scale_y_log10(labels = scales::label_comma()) +
    labs(
      title = "DALYs averted per $1,000 for interventions affordable at the CET",
      subtitle = paste0(
        "Health opportunity cost: $", cet_usd_per_daly, "/DALY (", round(threshold_efficiency, 1), " DALYs per $1,000). ",
        "Bar labels: ICER rank from Table 4. Log scale"
      ),
      x = "Cumulative cost, full implementation",
      y = "DALYs averted per $1,000 spent (log scale)"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank())
}

#' Figure 6 equivalent: every intervention that reached the league
#' table, ranked by net health benefit (full implementation), as bars
#' (colour marks a positive vs. negative net benefit), with cumulative
#' spend overlaid as a line on a secondary axis
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @return A ggplot object
build_fig6_plot <- function(league_table) {
  df <- league_table %>% arrange(rank_nhp)
  scale_factor <- max(abs(df$net_dalys_full), na.rm = TRUE) / max(df$cumulative_cost_full_usd, na.rm = TRUE)

  ggplot(df, aes(x = rank_nhp)) +
    geom_col(aes(y = net_dalys_full, fill = net_dalys_full >= 0), width = 0.85) +
    geom_line(aes(y = cumulative_cost_full_usd * scale_factor), color = "#E8A33D", linewidth = 1) +
    geom_hline(yintercept = 0, color = "#C0392B", linetype = "dashed") +
    scale_fill_manual(values = c(`TRUE` = "#1F4E78", `FALSE` = "#B5533C"), guide = "none") +
    scale_y_continuous(
      name = "Net DALYs averted (full implementation)",
      labels = scales::label_comma(),
      sec.axis = sec_axis(~ . / scale_factor, name = "Cumulative spend, full implementation", labels = dollars_millions)
    ) +
    labs(
      title = "Interventions ranked by net health benefit, with cumulative spend",
      subtitle = "Full implementation (100%). Negative bars: cost exceeds the health opportunity cost of the CET",
      x = "Intervention rank (by net health benefit)"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.title.y.right = element_text(color = "#E8A33D"),
      panel.grid.minor = element_blank()
    )
}

#' Figure 7 equivalent: for the affordable core package only (ICER at
#' or below the CET), net health benefit and cumulative spend at full
#' implementation (solid lines) vs. realistic implementation (dotted
#' lines) - the gap between them is health and budget left unused by
#' partial coverage
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param cet_usd_per_daly Cost-effectiveness threshold
#' @return A ggplot object
build_fig7_plot <- function(league_table, cet_usd_per_daly) {
  core <- league_table %>%
    filter(!is.na(icer_usd), icer_usd <= cet_usd_per_daly) %>%
    arrange(rank_nhp) %>%
    mutate(
      x = row_number(),
      cumulative_cost_full_core      = cumsum(coalesce(total_cost_full_usd, 0)),
      cumulative_cost_realistic_core = cumsum(coalesce(total_cost_realistic_usd, 0))
    )

  scale_factor <- max(abs(core$net_dalys_full), na.rm = TRUE) / max(core$cumulative_cost_full_core, na.rm = TRUE)

  ggplot(core, aes(x = x)) +
    geom_line(aes(y = net_dalys_full, color = "Net DALYs averted"), linewidth = 1, linetype = "solid") +
    geom_line(aes(y = net_dalys_realistic, color = "Net DALYs averted"), linewidth = 1, linetype = "dotted") +
    geom_line(aes(y = cumulative_cost_full_core * scale_factor, color = "Cumulative spend"), linewidth = 1, linetype = "solid") +
    geom_line(aes(y = cumulative_cost_realistic_core * scale_factor, color = "Cumulative spend"), linewidth = 1, linetype = "dotted") +
    scale_color_manual(values = c("Net DALYs averted" = "#1F4E78", "Cumulative spend" = "#E8A33D")) +
    scale_y_continuous(
      name = "Net DALYs averted",
      labels = scales::label_comma(),
      sec.axis = sec_axis(~ . / scale_factor, name = "Cumulative spend", labels = dollars_millions)
    ) +
    labs(
      title = "Full vs realistic implementation: net health benefit and cumulative spend",
      subtitle = "Affordable core package only. Solid = full implementation (100%); dotted = realistic implementation",
      x = "Intervention rank within the affordable core package (by net health benefit)",
      color = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.title.y.right = element_text(color = "#E8A33D"),
      legend.position = "top", panel.grid.minor = element_blank()
    )
}

#' Table 4 equivalent: every intervention with a computable ICER,
#' ranked by ICER (ascending, i.e. best value for money first), with
#' cumulative spend running past the affordable-package cutoff so a
#' reader can see how much further spending would buy progressively
#' worse value. Not filtered by the CET - that filter is what
#' distinguishes Table 4 from Table 6/7 (the affordable core package)
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @return A data frame, in English column names, ready to write with
#'   write_xlsx_sheet() (no further relabelling needed)
build_table4_icer_ranking <- function(league_table) {
  league_table %>%
    filter(!is.na(icer_usd)) %>%
    arrange(icer_rank) %>%
    transmute(
      `Ranking based on ICER`                       = icer_rank,
      `Intervention`                                 = intervention,
      `ICER [$]`                                     = icer_usd,
      `DALYs averted per $1,000`                     = dalys_per_1000usd,
      `Cases per annum`                              = cases_full_2023,
      `Total cost (full implementation) [$]`         = total_cost_full_usd,
      `Cumulative cost [$]`                          = cumsum(coalesce(total_cost_full_usd, 0)),
      `Total DALYs averted (full implementation)`    = total_dalys_full
    )
}

#' Table 6 equivalent: interventions included in the affordable core
#' package (ICER at or below the CET), ranked by net health benefit,
#' with the impact on overall population health (net DALYs averted)
#' and cumulative spend at both full and realistic implementation
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param cet_usd_per_daly The CET defining the core package
#' @return A data frame, in English column names, ready to write with
#'   write_xlsx_sheet() (no further relabelling needed)
build_table6_net_benefit_summary <- function(league_table, cet_usd_per_daly) {
  league_table %>%
    filter(!is.na(icer_usd), icer_usd <= cet_usd_per_daly) %>%
    arrange(rank_nhp) %>%
    transmute(
      `#`                                                      = row_number(),
      `Intervention`                                            = intervention,
      `ICER [$]`                                                 = icer_usd,
      `DALYs averted per $1,000`                                  = dalys_per_1000usd,
      `Cases per annum`                                            = cases_full_2023,
      `Implementation level (%)`                                    = implementation_level_pct,
      `Total cost (full implementation) [$]`                         = total_cost_full_usd,
      `Cumulative cost (full implementation) [$]`                     = cumsum(coalesce(total_cost_full_usd, 0)),
      `Total cost (realistic implementation) [$]`                      = total_cost_realistic_usd,
      `Cumulative cost (realistic implementation) [$]`                  = cumsum(coalesce(total_cost_realistic_usd, 0)),
      `Total DALYs averted (full implementation)`                        = total_dalys_full,
      `Total DALYs averted (realistic implementation)`                    = total_dalys_realistic,
      `Net DALYs averted (full implementation)`                            = net_dalys_full,
      `Net DALYs averted (realistic implementation)`                        = net_dalys_realistic,
      `Difference in net DALYs averted`                                      = diff_net_dalys,
      `$ value to the health system of implementation`                        = health_system_value_usd
    )
}

#' Table 7 equivalent: the underspend created by partial (realistic)
#' implementation of the affordable core package, used to fund
#' additional interventions - beyond the core - in ascending ICER
#' order until the underspend is exhausted
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#' @param cet_usd_per_daly The CET defining the core package
#' @return A list with:
#'   core       - the core affordable package (icer <= CET), with the
#'                budget underspend it generates at realistic implementation
#'   additional - the additional interventions funded from that
#'                underspend, in English column names ready to write
#'   summary    - one row: cet_usd_per_daly, underspend_usd,
#'                n_additional_interventions, additional_cost_usd,
#'                additional_dalys_realistic
build_budget_reallocation_table <- function(league_table, cet_usd_per_daly) {
  core <- league_table %>%
    filter(!is.na(icer_usd), icer_usd <= cet_usd_per_daly) %>%
    arrange(icer_rank)

  underspend_usd <- sum(core$total_cost_full_usd, na.rm = TRUE) - sum(core$total_cost_realistic_usd, na.rm = TRUE)

  candidates <- league_table %>%
    filter(!is.na(icer_usd), icer_usd > cet_usd_per_daly) %>%
    arrange(icer_rank) %>%
    mutate(cumulative_additional_cost = cumsum(coalesce(total_cost_realistic_usd, 0))) %>%
    filter(cumulative_additional_cost <= underspend_usd)

  additional <- candidates %>%
    transmute(
      `Ranking based on ICER`                             = icer_rank,
      `Intervention`                                       = intervention,
      `ICER [$]`                                            = icer_usd,
      `DALYs averted per $1,000`                             = dalys_per_1000usd,
      `Cases per annum`                                       = cases_full_2023,
      `Implementation level (%)`                               = implementation_level_pct,
      `Total cost (realistic implementation) [$]`               = total_cost_realistic_usd,
      `Cumulative cost (additional interventions, realistic implementation) [$]` = cumulative_additional_cost,
      `Total DALYs averted (realistic implementation)`           = total_dalys_realistic
    )

  summary <- data.frame(
    cet_usd_per_daly              = cet_usd_per_daly,
    underspend_usd                = underspend_usd,
    n_additional_interventions    = nrow(additional),
    additional_cost_usd           = sum(candidates$total_cost_realistic_usd, na.rm = TRUE),
    additional_dalys_realistic    = sum(candidates$total_dalys_realistic, na.rm = TRUE)
  )

  list(core = core, additional = additional, summary = summary)
}

#' Table 8 equivalent: three (or more) alternative CET scenarios and
#' the resulting scale of the affordable package - budget and DALYs
#' averted at full and realistic implementation, the money left in
#' the budget by partial implementation, and the additional package
#' (Table 7) that underspend could fund
#'
#' @param league_table Output of build_intervention_funnel()$league_table
#'   (computed at the reference CET; icer_usd/icer_rank do not change
#'   with the CET so this is valid for every scenario)
#' @param reference_cet config$cet_usd_per_daly
#' @param multipliers Named numeric vector of multipliers to apply to
#'   the reference CET (config$cet_sensitivity_multipliers)
#' @return A data frame, in English column names, ready to write with
#'   write_xlsx_sheet() (no further relabelling needed)
build_table8_ehp_scale_sensitivity <- function(league_table, reference_cet, multipliers) {
  lapply(names(multipliers), function(scenario_label) {
    cet_value <- reference_cet * multipliers[[scenario_label]]
    realloc <- build_budget_reallocation_table(league_table, cet_value)
    core <- realloc$core

    data.frame(
      `CET scenario`                                          = scenario_label,
      `How much can Senegal afford to pay to avert a DALY? [$]` = cet_value,
      `Full implementation: total spend [$]`                    = sum(core$total_cost_full_usd, na.rm = TRUE),
      `Full implementation: total DALYs averted`                 = sum(core$total_dalys_full, na.rm = TRUE),
      `Realistic implementation: total spend [$]`                  = sum(core$total_cost_realistic_usd, na.rm = TRUE),
      `Realistic implementation: total DALYs averted`                = sum(core$total_dalys_realistic, na.rm = TRUE),
      `Money left in the budget [$]`                                   = realloc$summary$underspend_usd,
      `Max DALYs from moving realistic to full implementation`           = sum(core$total_dalys_full, na.rm = TRUE) - sum(core$total_dalys_realistic, na.rm = TRUE),
      `Extended package: budget [$]`                                      = sum(core$total_cost_full_usd, na.rm = TRUE),
      `Extended package: additional DALYs averted from the underspend`      = realloc$summary$additional_dalys_realistic,
      check.names = FALSE
    )
  }) %>% bind_rows()
}
