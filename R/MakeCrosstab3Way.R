#' Make a weighted crosstab of row proportions, by wave
#'
#' \code{crosstab_3way} returns a data.frame containing a weighted crosstab of two variables
#' by wave.
#'
#'  This is a wrapper around pollster::crosstab_3way()
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
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' crosstab_3way(integ, zpid3, g40, ac7)
#'

crosstab_3way <- function(mulaw, x, y, z, weight = zwave_weight,
                               remove = c(""), n = TRUE, pct_type = "row",
                               format = "wide"){

  pollster::crosstab_3way(df = mulaw, x = {{x}}, y = {{y}}, z = {{z}}, weight = {{weight}},
                          remove = remove, n = n, pct_type = pct_type,
                          format = format)
}
