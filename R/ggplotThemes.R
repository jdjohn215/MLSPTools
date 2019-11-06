#' theme for tables with scaling and font sizes optimized for Lubar Center slideshows
#'
#' @param PlotMargins A vector of margins for the graph in the form c(top, right, bottom, left). Units are lines.
#'
#' @export
#' @import ggplot2
#'

theme_LubarSlides <- function(PlotMargins = c(0.25, 0, 2, 1)) {
  ret <- theme(text = element_text(family = "serif"),
               legend.position = "none",
               panel.background = element_blank(),
               panel.grid.major.y = element_line(colour = "grey"),
               panel.grid.minor.y = element_line(colour = "grey"),
               axis.line.y = element_line(),
               axis.line.x = element_line(),
               title = element_text(face = "bold", size = 12),
               axis.text.y = element_text(face = "bold", size = 12),
               axis.text.x = element_text(face = "bold", size = 12),
               plot.margin = unit(PlotMargins, "lines"))

  ret
}
