#' Make a crosstab of a variable
#'
#' \code{moe.crosstab} returns a data.frame a crosstab of two variables along with margins of error
#'
#'  This function returns a crosstab of two supplied variables along with margins of error.
#'  The resulting data.frame defaults
#'  to long format, but can be in wide format if format = "wide" is specified.
#'
#' @param x The grouping variable
#' @param y the dependent variable
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
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
moe.crosstab <- function(x, y, mulaw, weights = zwave_weight, format = "long"){
  # calculate design effect for sample comprised of all waves question is present for
  valid.waves <- mulaw %>%
    filter(!is.na({{x}}),
           !is.na({{y}})) %>%
    mutate(zwave = remove_labels(zwave)) %>%
    group_by(zwave) %>%
    summarise() %>%
    pull()

  # this data.frame has the complete waves in which the variables was asked
  d.deff <- mulaw %>%
    filter(zwave %in% valid.waves) %>%
    rename(weights = {{weights}})

  deff <- deff_calc(d.deff$weights)
  unweighted.n <- d.deff %>%
    filter(!is.na({{x}}),
           !is.na({{y}})) %>%
    nrow()

  output <- mulaw %>%
    filter(!is.na({{x}}),
           !is.na({{y}})) %>%
    mutate({{x}} := to_factor({{x}}),
           {{y}} := to_factor({{y}})) %>%
    group_by({{x}}) %>%
    mutate(total = sum({{weights}})) %>%
    group_by({{x}}, {{y}}) %>%
    summarise(observations = sum({{weights}}),
              pct = observations/first(total)) %>%
    ungroup() %>%
    mutate(moe = sqrt(deff)*1.96*sqrt((pct*(1-pct))/(unweighted.n - 1))*100) %>%
    mutate(pct = pct*100)

  if(format == "wide"){
    output.wide <- output %>%
      select(-observations) %>%
      pivot_wider(names_from = {{y}}, values_from = c("pct", "moe"))

    # fix names
    y.order <- output %>%
      select({{y}}) %>%
      pull() %>%
      levels()

    output.names <- names(output.wide)[2:length(names(output.wide))] %>%
      tibble() %>%
      rename(oldNames = 1) %>%
      mutate(order = (1:n())+1,
             name = str_sub(oldNames, 5, -1),
             suffix = str_sub(oldNames, 1, 3),
             name = factor(name, levels = y.order)) %>%
      arrange(name) %>%
      mutate(fixname = paste(name, suffix, sep = "_"))

    output.wide <- output.wide %>%
      select(1, output.names$order)
    names(output.wide)[2:length(names(output.wide))] <- output.names$fixname

    output <- output.wide
  }

  output
}

