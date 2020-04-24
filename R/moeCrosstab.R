#' Make a crosstab of a variable
#'
#' \code{moe_crosstab} returns a data.frame a crosstab of two variables along with margins of error
#'
#'  this is a wrapper around pollster::moe_crosstab
#'
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param x The independent variable
#' @param y The dependent variable
#' @param weight The weighting variable, defaults to zwave_weight
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n logical, if TRUE a column of row totals is included
#' @param pct_type Controls the kind of percentage values returned. One of "row" or "cell." Column percents are not supported.
#' @param format one of "long" or "wide"
#' @param zscore defaults to 1.96, consistent with a 95\% confidence interval
#'
#' @return A tibble
#' @export
#'
#' @examples
#' moe_crosstab(integ, zpid3, g40)

moe_crosstab <- function(mulaw, x, y, weight = zwave_weight,
                         remove = "", n = TRUE, pct_type = "row",
                         format = "wide", zscore = 1.96){
  pollster::moe_crosstab(df = mulaw, x = {{x}}, y = {{y}}, {{weight}}, remove = remove,
                         n = n, pct_type = pct_type, format = format, zscore =zscore)
}
