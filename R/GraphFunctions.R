#' Make a simple bar graph of a topline table
#'
#' \code{mlspToplineBar} creates a bar graph of a topline table
#'
#'  This function takes a topline table created with make.topline() and returns it as
#'  a bar chart.
#'
#' @param toplinetable The source table created by make.topline()
#' @param titlevar A quoted variable name. If supplied, the full text of this variable will be the title
#' @param title A character string. If supplied it will be the graph's title.
#' @param subtitle If supplied, it will be the graph's subtitle
#' @param theme Optional. One of either "LubarSlides" or "MLSP". This sets the theme to
#' theme_LubarSlides or theme_MLSP, respectively.
#' @param PlotMargins Only applicable when theme = "LubarSlides". It allows the user to manually change the plot.margin parameter.
#' @param wraptitle = the length at which to wrap the character title string. By default
#' it is 95 which matches the width of a LubarSlide.
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @import ggplot2
#'
#' @examples
#' make.topline(variable = g2, mulaw = orig, remove = c("refused"), format = "long")


mlspToplineBar <- function(toplinetable, titlevar = NULL, title = NULL, subtitle = NULL,
                           theme = NULL, LubarSlides = TRUE,
                           PlotMargins = c(0.25, 0, 2, 1),
                           wraptitle = 95){
  title.text <- "no title provided"
  if(!is.null(titlevar)){
    qs <- suppressMessages(readxl::read_excel("~/Dropbox/MuLawPoll1/IntegCurrentVariables.xlsx",
                                              sheet = 2)) %>%
      filter(!is.na(Q)) %>%
      select(variable = 1, question = 7)
    title.text <- qs$question[qs$variable == titlevar]
  } else if(!is.null(title)){
    title.text = title
  }

  p <- toplinetable %>%
    ggplot(aes(Response, `Valid Percent`, fill = Response)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(`Valid Percent`)), vjust = -0.5,
              size = 4, fontface = "bold", family = "serif") +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = NULL, limits = c(0,100),
                       labels = scales::percent_format(scale = 1, accuracy = 1),
                       expand = c(0,0.01)) +
    labs(title = str_wrap(title.text, width = wraptitle),
         subtitle = subtitle)

  if(!is.null(theme)){
    if(theme == "LubarSlides"){
      p <- p +
        theme_LubarSlides(PlotMargins = PlotMargins)
    } else if(theme == "MLSP"){
      p <- p +
        theme_MLSP()
    }
  }

  p
}

#' Make a grouped bar graph of a crosstab table
#'
#' \code{mlspCrosstabBar} creates a bar graph of a crosstab table
#'
#'  This function takes a crosstab table created with make.crosstab() and returns it as
#'  a bar chart.
#'
#' @param toplinetable The source table created by make.crosstab()
#' @param titlevar A quoted variable name. If supplied, the full text of this variable will be the title
#' @param title A character string. If supplied it will be the graph's title.
#' @param subtitle If supplied, it will be the graph's subtitle
#' @param theme Optional. One of either "LubarSlides" or "MLSP". This sets the theme to
#' theme_LubarSlides or theme_MLSP, respectively.
#' @param PlotMargins Only applicable when theme = "LubarSlides". It allows the user to manually change the plot.margin parameter.
#' @param wraptitle = the length at which to wrap the character title string. By default
#' it is 95 which matches the width of a LubarSlide.
#' @param legendPosition the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param legendJust anchor point for positioning legend inside plot ("center" or two-element numeric vector) or the justification according to the plot area when positioned outside the plot
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @import ggplot2
#'


mlspCrosstabBar <- function(crosstabtable, titlevar = NULL, title = NULL, subtitle = NULL,
                           theme = NULL, LubarSlides = TRUE,
                           PlotMargins = c(0.25, 0, 2, 1),
                           wraptitle = 95, legendPosition = "top",
                           legendJust = "right"){
  title.text <- "no title provided"
  if(!is.null(titlevar)){
    qs <- suppressMessages(readxl::read_excel("~/Dropbox/MuLawPoll1/IntegCurrentVariables.xlsx",
                                              sheet = 2)) %>%
      filter(!is.na(Q)) %>%
      select(variable = 1, question = 7)
    title.text <- qs$question[qs$variable == titlevar]
  } else if(!is.null(title)){
    title.text = title
  }

  p <- crosstabtable %>%
    rename(xgroup = 1) %>%
    pivot_longer(cols = -c(xgroup, n)) %>%
    ggplot(aes(name, value, fill = xgroup)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(value)), vjust = -0.5,
              position = position_dodge(width = 1),
              size = 4, fontface = "bold", family = "serif") +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = NULL, limits = c(0,100),
                       labels = scales::percent_format(scale = 1, accuracy = 1),
                       expand = c(0,0.01)) +
    labs(title = str_wrap(title.text, width = wraptitle),
         subtitle = subtitle)

  if(theme == "LubarSlides"){
    p <- p +
      theme_LubarSlides(PlotMargins = PlotMargins) +
      theme(legend.position = legendPosition,
            legend.justification = legendJust)
  } else if(theme == "MLSP"){
    p <- p +
      theme_MLSP() +
      theme(legend.position = legendPosition,
            legend.justification = legendJust)
  } else{
    p <- p +
      theme(legend.position = legendPosition,
            legend.justification = legendJust)
  }

  p
}
