#' Calculate the design effect of a sample
#'
#' \code{deff_calc} returns a single number
#'
#'  This function returns the design effect of a given sample using the formula
#'  length(w)*sum(w^2)/(sum(w)^2).
#'  It is designed for use in the moe family of functions.
#'
#' @param w a vector of weights
#'
#' @return A number
#' @export
#'
#' @examples
#' deff_calc <- function(d$zwave_weight)
#'
deff_calc <- function(w){
  length(w)*sum(w^2)/(sum(w)^2)
}


#' Calculate the margin of error (including design effect) of a sample
#'
#' \code{moedeff_calc} returns a single number. It is designed for use in the moe family of functions.
#'
#'  This function returns the design effect of a given sample using the formula
#'  sqrt(deff)*1.96*sqrt((pct*(1-pct))/(n-1))*100
#'
#' @param pct a proportion
#' @param deff a design effect
#' @param n the sample size
#'
#' @return A percentage
#' @export
#'
#'
moedeff_calc <- function(pct, deff, n){
  sqrt(deff)*1.96*sqrt((pct*(1-pct))/(n-1))*100
}
