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
#' it is 90 which matches the width of a LubarSlide.
#' @param xlabelAngle optional, the angle of the x-axis labels
#' @param xlabelWrap character length at which to wrap x-axis labels
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
                           wraptitle = 90, xlabelAngle = NULL,
                           xlabelWrap = 12){
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

  if(theme == "LubarSlides"){
    barlabelsize = 6
  } else {
    barlabelsize = 4
  }

  p <- toplinetable %>%
    ggplot(aes(Response, `Valid Percent`, fill = Response)) +
    geom_hline(yintercept = 50, color = "gray60") +
    geom_bar(stat = "identity") +
    geom_text(data = function(x) subset(x, `Valid Percent` < 93),
              aes(label = round(`Valid Percent`)), vjust = -0.5,
              size = barlabelsize, fontface = "bold", family = "serif") +
    geom_text(data = function(x) subset(x, `Valid Percent` > 92),
              aes(label = round(`Valid Percent`)), vjust = 1.2,
              size = barlabelsize, fontface = "bold", family = "serif") +
    scale_x_discrete(name = NULL, labels = function(x) str_wrap(x, width = xlabelWrap)) +
    scale_y_continuous(name = NULL, limits = c(0,100),
                       breaks = c(0,20,40,60,80,100),
                       labels = scales::percent_format(scale = 1, accuracy = 1),
                       expand = c(0,0.01)) +
    labs(title = str_wrap(title.text, width = wraptitle),
         subtitle = subtitle)

  if(!is.null(theme)){
    if(theme == "LubarSlides"){
      p <- p +
        theme_LubarSlides(PlotMargins = PlotMargins) +
        theme(axis.text.x = element_text(angle = xlabelAngle))
    } else if(theme == "MLSP"){
      p <- p +
        theme_MLSP() +
        theme(axis.text.x = element_text(angle = xlabelAngle))
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
#' it is 90 which matches the width of a LubarSlide.
#' @param legendPosition the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param legendJust anchor point for positioning legend inside plot ("center" or two-element numeric vector) or the justification according to the plot area when positioned outside the plot
#' @param xlabelAngle optional, the angle of the x-axis labels
#' @param xlabelWrap character length at which to wrap x-axis labels
#' @param facetncol optional, the number of facet columns
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#'


mlspCrosstabBar <- function(crosstabtable, titlevar = NULL, title = NULL, subtitle = NULL,
                           theme = "default", LubarSlides = TRUE,
                           PlotMargins = c(0.25, 0, 2, 1),
                           wraptitle = 90, legendPosition = "top",
                           legendJust = "right", xlabelAngle = NULL,
                           xlabelWrap = 12,
                           facetncol = NULL){
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

  if(theme == "LubarSlides"){
    barlabelsize = 6
  } else {
    barlabelsize = 4
  }

  # factor levels
  fact.levels <- names(crosstabtable[2:(ncol(crosstabtable)-1)])

  p <- crosstabtable %>%
    rename(xgroup = 1) %>%
    pivot_longer(cols = -c(xgroup, n)) %>%
    mutate(name = factor(name, levels = fact.levels)) %>%
    ggplot(aes(name, value, fill = name)) +
    geom_hline(yintercept = 50, color = "gray60") +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(value)), vjust = -0.5,
              position = position_dodge2(width = 1),
              size = barlabelsize, fontface = "bold", family = "serif") +
    scale_x_discrete(name = NULL, labels = function(x) str_wrap(x, width = xlabelWrap)) +
    scale_y_continuous(name = NULL, limits = c(0,100),
                       breaks = c(0,20,40,60,80,100),
                       labels = scales::percent_format(scale = 1, accuracy = 1),
                       expand = c(0,0.01)) +
    facet_wrap(facets = vars(xgroup), ncol = facetncol) +
    labs(title = str_wrap(title.text, width = wraptitle),
         subtitle = subtitle)

  if(theme == "LubarSlides"){
    p <- p +
      theme_LubarSlides(PlotMargins = PlotMargins) +
      theme(legend.position = legendPosition,
            legend.justification = legendJust,
            legend.title = element_blank(),
            legend.text = element_text(size = 13),
            axis.text.x = element_text(angle = xlabelAngle),
            strip.background = element_rect(fill = "linen"),
            strip.text = element_text(size = 13,
                                      face = "bold"))
  } else if(theme == "MLSP"){
    p <- p +
      theme_MLSP() +
      theme(legend.position = legendPosition,
            legend.justification = legendJust,
            legend.title = element_blank(),
            axis.text.x = element_text(angle = xlabelAngle),
            strip.background = element_rect(fill = "linen"),
            strip.text = element_text(size = 13,
                                      face = "bold"))
  } else{
    p <- p +
      theme(legend.position = legendPosition,
            legend.justification = legendJust,
            legend.title = element_blank(),
            axis.text.x = element_text(angle = xlabelAngle),
            strip.background = element_rect(fill = "linen"),
            strip.text = element_text(size = 13,
                                      face = "bold"))
  }

  p
}

################################################################################################
#' Make a time series scatterplot
#'
#' \code{mlspTimeSeriesScatter} creates a scatter plot with time on the x-axis
#'
#'  This function takes a topline table created with make.topline() and returns it as
#'  a bar chart.
#'
#' @param timeseriestable The source table created by make.ts()
#' @param titlevar A quoted variable name. If supplied, the full text of this variable will be the title
#' @param title A character string. If supplied it will be the graph's title.
#' @param subtitle If supplied, it will be the graph's subtitle
#' @param theme Optional. One of either "LubarSlides" or "MLSP". This sets the theme to
#' theme_LubarSlides or theme_MLSP, respectively.
#' @param PlotMargins Only applicable when theme = "LubarSlides". It allows the user to manually change the plot.margin parameter.
#' @param wraptitle = the length at which to wrap the character title string. By default
#' it is 90 which matches the width of a LubarSlide.
#' @param alpha the transparency of the dots, defaults to opaque
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @import ggplot2
#'
#' @examples
#' make.ts(g40, integ) %>% mlspTimeSeriesScatter()

mlspTimeSeriesScatter <- function(timeseriestable, titlevar = NULL, title = NULL, subtitle = NULL,
                            theme = "default", LubarSlides = TRUE,
                            PlotMargins = c(0.25, 0, 2, 1),
                            wraptitle = 90, legendPosition = "top",
                            legendJust = "right", alpha = 1){
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

  if(names(timeseriestable[3]) == "pct"){
    timeseriestable <- timeseriestable %>%
      rename(PollDate = 1, yvar = 2)
  } else if(names(timeseriestable[3]) != "pct"){
    timeseriestable <- timeseriestable %>%
      rename(PollDate = 1) %>%
      pivot_longer(cols = -PollDate, names_to = "yvar", values_to = "pct")
  }

  p <- timeseriestable %>%
    ggplot(aes(as.Date(PollDate), pct, color = yvar)) +
    geom_hline(yintercept = 50, color = "gray60") +
    geom_point(size = 3, alpha = alpha) +
    scale_x_date(name = NULL) +
    scale_y_continuous(name = NULL, limits = c(0,100),
                       breaks = c(0,20,40,60,80,100),
                       labels = scales::percent_format(scale = 1, accuracy = 1),
                       expand = c(0,0.01)) +
    labs(title = str_wrap(title.text, width = wraptitle),
         subtitle = subtitle)

  if(theme == "LubarSlides"){
    p <- p +
      theme_LubarSlides(PlotMargins = PlotMargins) +
      theme(legend.position = legendPosition,
            legend.justification = legendJust,
            legend.title = element_blank(),
            legend.text = element_text(size = 13))
  } else if(theme == "MLSP"){
    p <- p +
      theme_MLSP() +
      theme(legend.position = legendPosition,
            legend.justification = legendJust,
            legend.title = element_blank())
  } else{
    p <- p +
      theme(legend.position = legendPosition,
            legend.justification = legendJust,
            legend.title = element_blank())
  }

  p
}


##################################################################################
#' Make a dot and ribbon plot comparing symmetrical questions
#'
#' \code{compare.toplines} creates a scatter plot with time on the x-axis
#'
#'  This function takes a list of variables, makes toplines of them, and plots the toplines in a graph with
#'  one question per row and dots for the values of each question response.
#'
#' @param varlist a vector of quoted variable names
#' @param vartext a character vector of text which should be used to describe the variables
#' @param mulaw the relevant integ file
#' @param sortResponse The response category in the topline tables by which to sort the vartext
#' @param remove an optional vector of values to remove from the topline table, supplied to the remove
#' argument in make.topline()
#'
#' @return A ggplot object.
#' @export
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @importFrom forcats fct_reorder
#'
#' @examples
#' compare.toplines(varlist = c("d38", "d55", "d91", "d59", "d92"),
#' mulaw = demprimary,
#' vartext = c("Joe Biden", "Elizabeth Warren", "Kamala Harris", "Bernie Sanders", "Pete Buttigieg"),
#' sortResponse = "Favorable",
#' remove = c("don't know", "refused"))

compare.toplines <- function(varlist, vartext = NULL, mulaw, sortResponse, remove = NULL){
  if(is.null(vartext)){
    vartext <- varlist
  }

  toplinefunction <- function(variable){
    make.topline(variable = !!rlang::sym(variable),
                 mulaw = mulaw,
                 remove = remove) %>%
      mutate(Response = as.character(Response),
             variable = variable)
  }

  df <- map(varlist, toplinefunction) %>%
    bind_rows()

  vars.to.vartext <- tibble(varlist, vartext) %>%
    select(variable = 1, vartext = 2)
  df <- df %>%
    inner_join(vars.to.vartext) %>%
    select(-variable)

  sort.order <- df %>%
    filter(Response == sortResponse) %>%
    mutate(vartext = fct_reorder(vartext, `Valid Percent`))

  df <- df %>%
    mutate(vartext = factor(vartext, levels = levels(sort.order$vartext)))

  df.ribbon <- df %>%
    group_by(vartext) %>%
    summarise(min = min(`Valid Percent`),
              max = max(`Valid Percent`))

  ggplot() +
    geom_ribbon(data = df.ribbon,
                aes(x = vartext, ymin = min, ymax = max),
                color = "grey", size = 3) +
    geom_point(data = df, aes(vartext, `Valid Percent`, color = Response),
               size = 3) +
    geom_text(data = df, aes(vartext, `Valid Percent`, label = round(`Valid Percent`)),
              vjust = -1, family = "serif") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1),
                       name = NULL) +
    scale_x_discrete(name = NULL) +
    coord_flip() +
    ggthemes::theme_tufte() +
    theme(legend.title = element_blank(),
          legend.position = "top",
          legend.justification = "right",
          axis.line.x = element_line(),
          axis.line.y = element_line())
}
