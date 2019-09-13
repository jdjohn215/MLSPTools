#' Make a 3-way crosstab of a variable
#'
#' \code{moe.crosstab3way} returns a data.frame of a crosstab of two variables by a 3rd variable
#'  along with margins of error
#'
#'  This function returns a crosstab by a 3rd variable of two supplied variables along with margins of error.
#'  The resulting data.frame defaults
#'  to long format, but can be in wide format if format = "wide" is specified.
#'
#' @param x The grouping variable
#' @param y the dependent variable
#' @param z the 2nd control variable.
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
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
moe.crosstab3way <- function(x, y, z, mulaw, weights = zwave_weight, format = "long"){
  # calculate design effect for sample comprised of all waves question is present for
  valid.waves <- mulaw %>%
    filter(!is.na({{x}}),
           !is.na({{y}}),
           !is.na({{z}})) %>%
    mutate(zwave = remove_labels(zwave)) %>%
    group_by(zwave) %>%
    summarise() %>%
    pull()

  # this data.frame has the complete waves in which the variables was asked
  d.deff <- mulaw %>%
    filter(zwave %in% valid.waves) %>%
    rename(weights = {{weights}})

  deff <- deff_calc(d.deff$weights)

  output <- mulaw %>%
    filter(!is.na({{x}}),
           !is.na({{y}}),
           !is.na({{z}})) %>%
    mutate({{z}} := to_factor({{z}}),
           {{x}} := to_factor({{x}}),
           {{y}} := to_factor({{y}})) %>%
    group_by({{z}}, {{x}}) %>%
    mutate(total = sum({{weights}}),
           n.row = length({{weights}})) %>%
    group_by({{z}}, {{x}}, {{y}}) %>%
    summarise(frequency = sum({{weights}}),
              n.row = first(n.row),
              pct = frequency/first(total)) %>%
    ungroup() %>%
    mutate(moe = moedeff_calc(pct = pct, deff = deff, n = n.row),
           pct = pct*100) %>%
    select({{z}}, {{x}}, {{y}}, frequency, pct, moe)


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
