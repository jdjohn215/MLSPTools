#' Make a topline by wave of a variable
#'
#' \code{moe.topline.wave} returns a data.frame a crosstab of two variables along with margins of error
#'
#'  This function returns a crosstab of two supplied variables along with margins of error.
#'  The resulting data.frame defaults
#'  to long format, but can be in wide format if format = "wide" is specified.
#'
#' @param x The grouping variable
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param date the date variable, defaults to zpollenddate
#' @param weights The weight variable, defaults to zwave_weight
#' @param format One of "wide" or "long", defaults to "wide"
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom labelled to_factor
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' moe.crosstab(zpid3, g40, integ, format = "wide")
#'
moe.topline.wave <- function(x, mulaw, date = zpollenddate, weights = zwave_weight, format = "long"){

  # Calculate the design effect for each wave individually, as that is
  # the level at which the statistic is being calculated
  stats.by.wave <- mulaw %>%
    filter(!is.na({{x}})) %>%
    mutate({{date}} := to_factor({{date}})) %>%
    group_by({{date}}) %>%
    summarise(deff = deff_calc({{weights}}),
              n = length({{weights}})) %>%
    ungroup()

  output <- mulaw %>%
    filter(!is.na({{x}})) %>%
    mutate({{date}} := to_factor({{date}}),
           {{x}} := to_factor({{x}})) %>%
    group_by({{date}}) %>%
    mutate(total = sum({{weights}})) %>%
    group_by({{date}}, {{x}}) %>%
    summarise(frequency = sum({{weights}}),
              pct = frequency/first(total)) %>%
    ungroup() %>%
    inner_join(stats.by.wave) %>%
    mutate(moe = moedeff_calc(pct = pct, deff = deff, n = n),
           pct = pct*100) %>%
    select(PollDate = {{date}}, {{x}}, frequency, pct, moe)

  if(format == "wide"){
    output.wide <- output %>%
      select(-frequency) %>%
      pivot_wider(names_from = {{x}}, values_from = c("pct", "moe"))

    # fix names
    x.order <- output %>%
      select({{x}}) %>%
      pull() %>%
      levels()

    output.names <- names(output.wide)[2:length(names(output.wide))] %>%
      tibble() %>%
      rename(oldNames = 1) %>%
      mutate(order = (1:n())+1,
             name = str_sub(oldNames, 5, -1),
             suffix = str_sub(oldNames, 1, 3),
             name = factor(name, levels = x.order)) %>%
      arrange(name) %>%
      mutate(fixname = paste(name, suffix, sep = "_"))

    output.wide <- output.wide %>%
      select(1, output.names$order)
    names(output.wide)[2:length(names(output.wide))] <- output.names$fixname

    output <- output.wide
  }

  output
}
