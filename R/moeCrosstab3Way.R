#' Make a 3-way crosstab of a variable
#'
#' \code{moe_crosstab_3way} returns a data.frame of a crosstab of two variables by a 3rd variable
#'  along with margins of error
#'
#'  This is a wrapper around pollster::moe_crosstab_3way
#'
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param x The independent variable
#' @param y The dependent variable
#' @param z The third variable
#' @param weight The weighting variable, defaults to zwave_weight
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n Logical. If TRUE add a row total column
#' @param pct_type one of "row" or "cell"
#' @param format one of "wide" or "long
#' @param zscore defaults to 1.96, consistent with a 95% confidence interval
#'
#' @return A tibble
#' @export
#'
#' @examples
#' moe_crosstab_3way(integ, zpid3, g40, ac7)
#'

moe_crosstab_3way <- function(mulaw, x, y, z, weight = zwave_weight,
                          remove = c(""), n = TRUE, pct_type = "row",
                          format = "long", zscore = 1.96){

  pollster::moe_crosstab_3way(df = mulaw, x = {{x}}, y = {{y}}, z = {{z}}, weight = {{weight}},
                          remove = remove, n = n, pct_type = pct_type,
                          format = format, zscore = zscore)
}
