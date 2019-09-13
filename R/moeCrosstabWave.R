#' Make a crosstab by wave of a variable
#'
#' \code{moe.crosstab.wave} returns a data.frame of a crosstab of two variables by wave along with margins of error
#'
#'  This function returns a crosstab  by waveof two supplied variables along with margins of error.
#'  The resulting data.frame defaults
#'  to long format, but can be in wide format if format = "wide" is specified.
#'
#' @param x The grouping variable
#' @param y the dependent variable
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param date the date variable. Defaults to zpollenddate
#' @param weights The weight variable, defaults to zwave_weight
#' @param format One of "wide" or "long", defaults to "long"
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom labelled to_factor
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' moe.crosstab.wave(zpid3, g40, integ, format = "wide")
#'
moe.crosstab.wave <- function(x, y, mulaw, date = zpollenddate, weights = zwave_weight, format = "long"){
  # Calculate the design effect for each wave individually, as that is
  # the level at which the statistic is being calculated
  stats.by.wave <- mulaw %>%
    filter(!is.na({{x}}),
           !is.na({{y}})) %>%
    mutate({{date}} := to_factor({{date}})) %>%
    group_by({{date}}) %>%
    summarise(deff = deff_calc({{weights}})) %>%
    ungroup()

  output <- mulaw %>%
    filter(!is.na({{x}}),
           !is.na({{y}})) %>%
    mutate({{date}} := to_factor({{date}}),
           {{x}} := to_factor({{x}}),
           {{y}} := to_factor({{y}})) %>%
    group_by({{date}}, {{x}}) %>%
    mutate(total = sum({{weights}}),
           n.row = length({{weights}})) %>%
    group_by({{date}}, {{x}}, {{y}}) %>%
    summarise(frequency = sum({{weights}}),
              n.row = first(n.row),
              pct = frequency/first(total)) %>%
    ungroup() %>%
    inner_join(stats.by.wave) %>%
    mutate(moe = moedeff_calc(pct = pct, deff = deff, n = n.row),
           pct = pct*100) %>%
    select(PollDate = {{date}}, {{x}}, {{y}}, frequency, pct, moe)


  # make wide if needed
  if(format == "wide"){
    output.wide <- output %>%
      select(-frequency) %>%
      pivot_wider(names_from = {{y}}, values_from = c("pct", "moe"))

    # fix names
    y.order <- output %>%
      select({{y}}) %>%
      pull() %>%
      levels()

    output.names <- names(output.wide)[3:length(names(output.wide))] %>%
      tibble() %>%
      rename(oldNames = 1) %>%
      mutate(order = (1:n())+2,
             name = str_sub(oldNames, 5, -1),
             suffix = str_sub(oldNames, 1, 3),
             name = factor(name, levels = y.order)) %>%
      arrange(name) %>%
      mutate(fixname = paste(name, suffix, sep = "_"))

    output.wide <- output.wide %>%
      select(1, 2, output.names$order)
    names(output.wide)[3:length(names(output.wide))] <- output.names$fixname

    output <- output.wide
  }

  output
}
