# ============================================================
# Shared LISER chart style
#
# One place for every ggplot2 chart in this project to draw its
# colors and theme from, so every figure across every workbook and
# report reads as one visual system (see the liser-style skill for
# the full charte graphique). Source this before building any chart.
# ============================================================

suppressMessages(library(ggplot2))

# LISER graphic-charter primaries and their tints actually used in
# this project's charts (see the liser-style skill's palette.md for
# the full 6-step ramps). Only on-palette hexes - no invented hues.
liser_bleu        <- "#000066"
liser_bleu_mid     <- "#7366A4"
liser_bleu_light   <- "#CCC6E0"
liser_rouge        <- "#E30613"
liser_rouge_mid    <- "#F07E5D"
liser_rouge_light  <- "#FBD5C3"
liser_cyan         <- "#0099FF"
liser_cyan_mid      <- "#56C4EF"
liser_cyan_light    <- "#D0ECFB"
liser_gris          <- "#000000"
liser_gris_mid       <- "#7C797C"
liser_gris_light     <- "#D0CCD0"
liser_rouge_fonce     <- "#990000"
liser_cyan_fonce       <- "#006699"
liser_cyan_fonce_mid    <- "#4BABD1"

# Categorical palette for charts with more than 2-3 series (e.g. one
# color per disease program): steps through the three primaries at
# full strength, then their mid tints, then the two darker
# supplementary colors and the mid grey - all on-palette, no invented
# hues, per the liser-style skill's "lean on tints" rule.
liser_categorical_palette <- c(
  liser_bleu, liser_cyan, liser_rouge,
  liser_bleu_mid, liser_cyan_mid, liser_rouge_mid,
  liser_cyan_fonce, liser_rouge_fonce, liser_gris_mid, liser_cyan_fonce_mid
)

#' Shared LISER chart theme: white canvas, no border box, light
#' horizontal guides only, bold LISER-blue titles - every chart in
#' this project should build on this theme so they read as one
#' family regardless of which script produced them.
liser_chart_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", color = liser_bleu, size = rel(1.15), margin = margin(b = 4)),
      plot.subtitle = element_text(color = "grey35", size = rel(0.85), margin = margin(b = 14)),
      plot.caption = element_text(color = "grey55", size = rel(0.68), hjust = 0, margin = margin(t = 12)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.background = element_rect(fill = liser_bleu_light, color = NA),
      strip.text = element_text(face = "bold", color = liser_bleu, size = rel(0.95)),
      axis.title = element_text(color = "grey25", size = rel(0.85)),
      axis.text = element_text(color = "grey25", size = rel(0.85)),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_text(color = liser_bleu, face = "bold", size = rel(0.85)),
      legend.text = element_text(size = rel(0.85)),
      plot.margin = margin(16, 20, 12, 16)
    )
}
