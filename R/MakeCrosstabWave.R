#' Make a weighted crosstab of row proportions, by wave
#'
#' \code{crosstab_wave} returns a data.frame containing a weighted crosstab of two variables
#' by wave.
#'
#'  This is a wrapper around pollster::crosstab_3way() where the default value of z is zpollenddate
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
#'
#' @return A tibble
#' @export
#'
#' @examples
#' crosstab_wave(integ, zpid3, g40)

crosstab_wave <- function(mulaw, x, y, z = zpollenddate, weight = zwave_weight,
                               remove = c(""), n = TRUE, pct_type = "row",
                               format = "wide"){

  pollster::crosstab_3way(df = mulaw, x = {{x}}, y = {{y}}, z = {{z}}, weight = {{weight}},
                          remove = remove, n = n, pct_type = pct_type,
                          format = format)

}
