#' Make a topline of a variable
#'
#' \code{moe.topline} returns a data.frame containing a topline table with the margin of error
#'
#'  This function returns the topline statistics of any given variable.
#'
#' @param x The variable, unquoted
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param weights The weight variable, defaults to zwave_weight
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom labelled to_factor
#' @importFrom tidyr spread
#'
#' @examples
#' make.ts(variable = g2, mulaw = orig, remove = c("refused"), format = "long")
#'
moe.topline <- function(x, mulaw, weights = zwave_weight){
  # calculate design effect for sample comprised of all waves question is present for
  valid.waves <- mulaw %>%
    filter(!is.na({{x}})) %>%
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
    filter(!is.na({{x}})) %>%
    nrow()

  output <- mulaw %>%
    filter(!is.na({{x}})) %>%
    mutate({{x}} := to_factor({{x}}),
           total = sum({{weights}})) %>%
    group_by({{x}}) %>%
    summarise(frequency = sum({{weights}}),
              pct = (frequency/first(total))) %>%
    ungroup() %>%
    mutate(moe = sqrt(deff)*1.96*sqrt((pct*(1-pct))/(unweighted.n - 1))*100) %>%
    mutate(pct = pct*100)

  output
}
