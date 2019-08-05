#' Make a time series of a variable
#'
#' \code{make.topline} returns a data.frame containing a time series of a variable
#'
#'  This function returns a topline table of any given variable as a data.frame.
#'
#' @param variable The variable, unquoted
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param weight The weight variable, defaults to zwave_weight
#' @param n Logical, determines if a row total is included or not
#' @param cumsum Logical, determines if a cumulative percent column is included or not
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom labelled to_factor
#' @importFrom forcats fct_explicit_na
#'
#' @examples
#' make.topline(variable = g2, mulaw = orig, remove = c("refused"), format = "long")
#'
make.topline <- function(variable, mulaw, weight = zwave_weight,
                         n = TRUE, cumsum = TRUE){

  # Some Nonstandard Evaluation witchcraft I don't understand
  variable <- enquo(variable)
  weight <- enquo(weight)

  # make list of valid waves
  valid.waves <- mulaw %>%
    filter(!is.na(!!variable)) %>%
    group_by(zwave) %>%
    summarise()
  valid.waves <- valid.waves$zwave

  # Make table
  d.output <- mulaw %>%
    # remove waves in which question is not asked
    filter(zwave %in% valid.waves) %>%
    # Convert to ordered factors
    mutate(!!variable := to_factor(!!variable),
           !!variable := forcats::fct_explicit_na(!!variable)) %>%
    # Calculate denominator
    mutate(total = sum(!!weight),
           valid.total = sum((!!weight)[!!variable != "(Missing)"])) %>%
    # Calculate proportions
    group_by(!!variable) %>%
    summarise(pct = (sum(!!weight)/first(total))*100,
              valid.pct = (sum(!!weight)/first(valid.total)*100),
              n = sum(!!weight)) %>%
    ungroup() %>%
    mutate(cum = cumsum(valid.pct),
           valid.pct = replace(valid.pct, !!variable == "(Missing)", NA),
           cum = replace(cum, !!variable == "(Missing)", NA)) %>%
    select(Response = !!variable, Frequency = n, Percent = pct,
           `Valid Percent` = valid.pct, `Cumulative Percent` = cum)

  if(n == FALSE){
    d.output <- select(d.output, -n)
  }

  if(cumsum == FALSE){
    d.output <- select(d.output, -`Cumulative Percent`)
  }

  d.output
}
