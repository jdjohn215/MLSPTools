#' Make a weighted crosstab of row proportions
#'
#' \code{crosstab} returns a data.frame containing a weighted crosstab of two variables
#'
#'  This is a wrapper around pollster::crosstab()
#'
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param x The independent variable
#' @param y The dependent variable
#' @param weight The weighting variable, defaults to zwave_weight
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n logical, if TRUE a column of row totals is included
#' @param pct_type Controls the kind of percentage values returned. One of "row," "cell," or "column."
#' @param format one of "long" or "wide"
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' crosstab(mulaw = df, x = zpid3, y = g40v2, remove = c("don't know", "refused"))

crosstab <- function(mulaw, x, y, weight = zwave_weight,
                          remove = "", n = TRUE, pct_type = "row",
                          format = "wide"){
  pollster::crosstab(df = mulaw, x = {{x}}, y = {{y}}, {{weight}}, remove = remove,
                     n = n, pct_type = pct_type, format = format)
}
