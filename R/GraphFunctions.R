#' Make a simple bar graph of a topline table
#'
#' \code{mlspToplineBar} creates a bar graph of a topline table
#'
#'  This function takes a topline table created with make.topline() and returns it as
#'  a bar chart.
#'
#' @param tableinput The source table created by make.topline()
#' @param titlevar A quoted variable name. If supplied, the full text of this variable will be the title
#' @param title A character string. If supplied it will be the graph's title.
#' @param subtitle If supplied, it will be the graph's subtitle
#' @param fillPalette defaults to "guess", in which case a "best-guess" palette is assigned.
#' This argument also accepts the name of ColorBrewer palette or a character vector of color names.
#' @param theme Optional. One of either "LubarSlides" or "MLSP". This sets the theme to
#' theme_LubarSlides or theme_MLSP, respectively.
#' @param xlab an optional x-axis name
#' @param PlotMargins Only applicable when theme = "LubarSlides". It allows the user to manually change the plot.margin parameter.
#' @param wraptitle = the length at which to wrap the character title string. By default
#' it is 90 which matches the width of a LubarSlide.
#' @param xlabelAngle optional, the angle of the x-axis labels
#' @param xlabelWrap character length at which to wrap x-axis labels
#' @param xlabelSize specification of x-label size, defaults to 13
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @import ggplot2
#'
#' @examples
#' make.topline(variable = g2, mulaw = orig, remove = c("refused"), format = "long")


mlspToplineBar <- function(tableinput, titlevar = NULL, title = NULL, subtitle = NULL,
                           fillPalette = "guess",
                           theme = "default", xlab = NULL,
                           PlotMargins = c(0.25, 0, 3.5, 0.5),
                           wraptitle = 90, xlabelAngle = NULL,
                           xlabelWrap = 12, xlabelSize = 14){
  title.text <- "no title provided"
  if(!is.null(titlevar)){
    qs <- suppressMessages(readxl::read_excel("~/Dropbox/MuLawPoll1/IntegCurrentVariables.xlsx",
                                              sheet = 2)) %>%
      filter(!is.na(Q)) %>%
      select(variable = 1, question = 7) %>%
      mutate(question = str_replace_all(string = question, pattern = "[’]", replacement = "'"))
    title.text <- qs$question[qs$variable == titlevar]
  } else if(!is.null(title)){
    title.text = title
  }

  if(theme == "LubarSlides"){
    barlabelsize = 7
  } else {
    barlabelsize = 4
  }

  p <- tableinput %>%
    filter(Response != "(Missing)") %>%
    ggplot(aes(Response, `Valid Percent`, fill = Response)) +
    geom_hline(yintercept = 50, color = "gray80") +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = suppressMessages(guess.palette(table = tableinput,
                                                              fillPalette = fillPalette))) +
    geom_text(data = function(x) subset(x, `Valid Percent` < 93),
              aes(label = round(`Valid Percent`)), vjust = -0.5,
              size = barlabelsize, fontface = "bold", family = "serif") +
    geom_text(data = function(x) subset(x, `Valid Percent` > 92),
              aes(label = round(`Valid Percent`)), vjust = 1.2,
              size = barlabelsize, fontface = "bold", family = "serif") +
    scale_x_discrete(name = xlab, labels = function(x) str_wrap(x, width = xlabelWrap)) +
    scale_y_continuous(name = NULL, limits = c(0,100),
                       breaks = c(0,20,40,60,80,100),
                       labels = scales::percent_format(scale = 1, accuracy = 1),
                       expand = c(0,0.01)) +
    labs(title = str_wrap(title.text, width = wraptitle),
         subtitle = subtitle)

  if(!is.null(theme)){
    if(theme == "LubarSlides"){
      p <- p +
        theme_LubarSlides(PlotMargins = PlotMargins,
                          xlabelSize = xlabelSize) +
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
#' @param tableinput The source table created by make.crosstab()
#' @param titlevar A quoted variable name. If supplied, the full text of this variable will be the title
#' @param title A character string. If supplied it will be the graph's title.
#' @param subtitle If supplied, it will be the graph's subtitle
#' @param fillPalette defaults to "guess", in which case a "best-guess" palette is assigned.
#' This argument also accepts the name of ColorBrewer palette or a character vector of color names.
#' @param theme Optional. One of either "LubarSlides" or "MLSP". This sets the theme to
#' theme_LubarSlides or theme_MLSP, respectively.
#' @param xlab an optional x-axis name
#' @param PlotMargins Only applicable when theme = "LubarSlides". It allows the user to manually change the plot.margin parameter.
#' @param wraptitle = the length at which to wrap the character title string. By default
#' it is 90 which matches the width of a LubarSlide.
#' @param legendPosition the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param legendJust anchor point for positioning legend inside plot ("center" or two-element numeric vector) or the justification according to the plot area when positioned outside the plot
#' @param facetncol optional, the number of facet columns
#' @param ylimits optional y-axis limits parameter
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#'


mlspCrosstabBar <- function(tableinput, titlevar = NULL, title = NULL, subtitle = NULL,
                            fillPalette = "guess",
                            theme = "default", xlab = NULL,
                           PlotMargins = c(0.25, 0, 3.5, 0.5),
                           wraptitle = 90, legendPosition = "top",
                           legendJust = "right",
                           facetncol = NULL,
                           ylimits = NULL){
  title.text <- "no title provided"
  if(!is.null(titlevar)){
    qs <- suppressMessages(readxl::read_excel("~/Dropbox/MuLawPoll1/IntegCurrentVariables.xlsx",
                                              sheet = 2)) %>%
      filter(!is.na(Q)) %>%
      select(variable = 1, question = 7) %>%
      mutate(question = str_replace_all(string = question, pattern = "[’]", replacement = "'"))
    title.text <- qs$question[qs$variable == titlevar]
  } else if(!is.null(title)){
    title.text = title
  }

  if(theme == "LubarSlides"){
    barlabelsize = 7
  } else {
    barlabelsize = 4
  }

  # factor levels
  fact.levels <- names(tableinput[2:(ncol(tableinput)-1)])

  # pivot data
  table.longer <- tableinput %>%
    rename(xgroup = 1) %>%
    pivot_longer(cols = -c(xgroup, n)) %>%
    mutate(name = factor(name, levels = fact.levels))

  #pick ylimits
  if(!is.null(ylimits)){
    ylimits = ylimits
    } else if(max(table.longer$value) > 90){
    ylimits = c(0,109)
    } else {
    ylimits = c(0,100)
    }

  p <- table.longer %>%
    ggplot(aes(name, value, fill = name)) +
    geom_hline(yintercept = 50, color = "gray80") +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = suppressMessages(guess.palette(table = tableinput,
                                                              fillPalette = fillPalette))) +
    geom_text(aes(label = round(value)), vjust = -0.5,
              position = position_dodge2(width = 1),
              size = barlabelsize, fontface = "bold", family = "serif") +
    scale_x_discrete(name = xlab) +
    scale_y_continuous(name = NULL, limits = ylimits,
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
            axis.text.x = element_blank(),
            strip.background = element_rect(fill = "linen"),
            strip.text = element_text(size = 13,
                                      face = "bold"))
  } else if(theme == "MLSP"){
    p <- p +
      theme_MLSP() +
      theme(legend.position = legendPosition,
            legend.justification = legendJust,
            legend.title = element_blank(),
            strip.background = element_rect(fill = "linen"),
            strip.text = element_text(size = 13,
                                      face = "bold"))
  } else{
    p <- p +
      theme(legend.position = legendPosition,
            legend.justification = legendJust,
            legend.title = element_blank(),
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
#' @param tableinput The source table created by make.ts()
#' @param titlevar A quoted variable name. If supplied, the full text of this variable will be the title
#' @param title A character string. If supplied it will be the graph's title.
#' @param subtitle If supplied, it will be the graph's subtitle
#' @param fillPalette defaults to "guess", in which case a "best-guess" palette is assigned.
#' This argument also accepts the name of ColorBrewer palette or a character vector of color names.
#' @param theme Optional. One of either "LubarSlides" or "MLSP". This sets the theme to
#' theme_LubarSlides or theme_MLSP, respectively.
#' @param xlab an optional x-axis name
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

mlspTimeSeriesScatter <- function(tableinput, titlevar = NULL, title = NULL, subtitle = NULL,
                                  fillPalette = "guess",
                            theme = "default", xlab = NULL,
                            PlotMargins = c(0.25, 0, 3.5, 0.5),
                            wraptitle = 90, legendPosition = "top",
                            legendJust = "right", alpha = 1){
  title.text <- "no title provided"
  if(!is.null(titlevar)){
    qs <- suppressMessages(readxl::read_excel("~/Dropbox/MuLawPoll1/IntegCurrentVariables.xlsx",
                                              sheet = 2)) %>%
      filter(!is.na(Q)) %>%
      select(variable = 1, question = 7) %>%
      mutate(question = str_replace_all(string = question, pattern = "[’]", replacement = "'"))
    title.text <- qs$question[qs$variable == titlevar]
  } else if(!is.null(title)){
    title.text = title
  }


  # for graphing purposes
  orig.table <- tableinput

  if(names(tableinput[3]) == "pct"){
    tableinput <- tableinput %>%
      rename(PollDate = 1, yvar = 2)
  } else if(names(tableinput[3]) != "pct"){
    # factor levels
    fact.levels <- names(tableinput[2:(ncol(tableinput))])
    tableinput <- tableinput %>%
      rename(PollDate = 1) %>%
      pivot_longer(cols = -PollDate, names_to = "yvar", values_to = "pct") %>%
      mutate(yvar = factor(yvar, levels = fact.levels))
  }

  p <- tableinput %>%
    ggplot(aes(as.Date(PollDate), pct, color = yvar)) +
    geom_hline(yintercept = 50, color = "gray80") +
    geom_point(size = 3, alpha = alpha) +
    scale_color_manual(values = suppressMessages(guess.palette(table = orig.table,
                                                               fillPalette = fillPalette))) +
    scale_x_date(name = xlab) +
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
            legend.text = element_text(size = 14))
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
