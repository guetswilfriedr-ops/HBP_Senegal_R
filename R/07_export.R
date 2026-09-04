# ============================================================
# Export functions
# ============================================================

library(ggplot2)

#' Write a data frame to a CSV file
#'
#' @param df Data frame to export
#' @param name File name without extension
#' @param dir Output directory
export_table <- function(df, name, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  write.csv(df, file.path(dir, paste0(name, ".csv")), row.names = FALSE)
}

#' Save a ggplot object as a PNG file
#'
#' @param plot A ggplot object
#' @param name File name without extension
#' @param dir Output directory
#' @param width Figure width in inches
#' @param height Figure height in inches
export_figure <- function(plot, name, dir, width = 8, height = 5) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  ggsave(file.path(dir, paste0(name, ".png")), plot = plot, width = width, height = height)
}
