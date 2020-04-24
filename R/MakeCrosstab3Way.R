#' Make a weighted crosstab of row proportions, by wave
#'
#' \code{crosstab_3way} returns a data.frame containing a weighted crosstab of two variables
#' by wave.
#'
#'  This function returns a crosstab by wave. The resulting data.frame is in long format, suitable
#'  for making a faceted plot. See the example below
#'
#' @param x The independent variable
#' @param y The dependent variable
#' @param z The third variable
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param weight The weighting variable, defaults to zwave_weight
#' @param n Logical. If TRUE add a row total column
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom labelled to_factor
#' @importFrom tidyr spread
#' @importFrom tidyr pivot_wider
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
