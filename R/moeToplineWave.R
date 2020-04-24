#' Make a topline by wave of a variable
#'
#' \code{moe.topline.wave} returns a data.frame a crosstab of two variables along with margins of error
#'
#'  This is a wrapper around pollster::moe_crosstab in which x is assumed to be zpollenddate.
#'  All you have to provide is the y-var to the `variable` argument
#'
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param variable The variable
#' @param date the date variable, defaults to zpollenddate
#' @param weight The weighting variable, defaults to zwave_weight
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n logical, if TRUE a column of row totals is included
#' @param pct_type Controls the kind of percentage values returned. One of "row," "cell," or "column."
#' @param format one of "long" or "wide"
#' @param zscore defaults to 1.96, consistent with a 95% confidence interval
#'
#' @return A tibble
#' @export
#'
#' @examples
#' moe_time_series(integ, g40)
moe_time_series <- function(mulaw, variable, date = zpollenddate, weight = zwave_weight,
                    remove = "", n = TRUE, pct_type = "row",
                    format = "long", zscore = 1.96){
  pollster::moe_crosstab(df = mulaw, x = {{date}}, y = {{variable}}, {{weight}}, remove = remove,
                     n = n, pct_type = pct_type, format = format, zscore = zscore)
}
