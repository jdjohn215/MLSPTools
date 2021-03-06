#' Make a crosstab by wave of a variable
#'
#' \code{moe_crosstab_wave} returns a data.frame of a crosstab of two variables by wave along with margins of error
#'
#'  this is a wrapper around pollster::moe_crosstab_wave where z defaults to zpollenddate
#'
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param x The independent variable
#' @param y The dependent variable
#' @param z The third variable, defaults to zpollenddate
#' @param weight The weighting variable, defaults to zwave_weight
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n Logical. If TRUE add a row total column
#' @param pct_type one of "row" or "cell"
#' @param format one of "wide" or "long
#' @param zscore defaults to 1.96, consistent with a 95\% confidence interval
#' @param ... further arguments passed to pollster::moe_crosstab_3way, such as unwt_n
#'
#' @return A tibble
#' @export
#'
#' @examples
#' moe_crosstab_wave(integ, zpid3, g40)

moe_crosstab_wave <- function(mulaw, x, y, z = zpollenddate, weight = zwave_weight,
                          remove = c(""), n = TRUE, pct_type = "row",
                          format = "long", zscore = 1.96, ...){

  pollster::moe_crosstab_3way(df = mulaw, x = {{x}}, y = {{y}}, z = {{z}}, weight = {{weight}},
                          remove = remove, n = n, pct_type = pct_type,
                          format = format, zscore = zscore, ... = ...)

}
