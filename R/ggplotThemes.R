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
               panel.background = element_rect(fill = "linen"),
               axis.line.y = element_line(size = 1),
               axis.line.x = element_line(size = 1),
               axis.ticks = element_blank(),
               title = element_text(face = "bold", size = 12),
               axis.text.y = element_text(face = "bold", size = 12),
               axis.text.x = element_text(face = "bold", size = 12),
               plot.margin = unit(PlotMargins, "lines"))

  ret
}


#' theme for tables with default ggplot2 scaling and margins
#'
#' @export
#' @import ggplot2
#'

theme_MLSP <- function() {
  ret <- theme(text = element_text(family = "serif"),
               legend.position = "none",
               panel.background = element_rect(fill = "linen"),
               axis.line.y = element_line(size = 1),
               axis.line.x = element_line(size = 1),
               axis.ticks = element_line(size = 1))

  ret
}
