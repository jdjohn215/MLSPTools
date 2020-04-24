#' Make a topline of a variable
#'
#' \code{moe_topline} returns a data.frame containing a topline table with the margin of error
#'
#'  This is a wrapper around pollster::moe_topline()
#'
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param variable The variable, unquoted
#' @param weight The weight variable, defaults to zwave_weight
#' @param remove a character string of response values to remove from the output after performing all calculations
#' @param n Logical, determines if a row total is included or not
#' @param pct logical, determines if the pct column is included or not
#' @param valid_pct logical, determines if the valid percent column is included
#' @param cum_pct Logical, determines if a cumulative percent column is included or not
#' @param zscore defaults to 1.96, consistent with a 95\% confidence interval
#'
#' @return A tibble
#' @export
#'
#' @examples
#' moe_topline(integ, g40)
#'
moe_topline <- function(mulaw, variable, weight = zwave_weight,
                    remove = c(""), n = TRUE, pct = TRUE,
                    valid_pct = TRUE, cum_pct = TRUE, zscore = 1.96){
  pollster::moe_topline(df = mulaw, variable = {{variable}}, weight = {{weight}},
                    remove = remove, n = n, pct = pct, valid_pct = valid_pct,
                    cum_pct = cum_pct, zscore = zscore)
}
