#' Forces rounded numbers to total their UNrounded sum, to any power of 10.
#'
#' \code{round_preserve_sum}
#'
#'  This functions returns a rounded vector of numbers, ensuring that they add
#'  to the same total as before rounding.
#'
#' @param x a vector of numeric values
#' @param digits what to round to, defaults to 0
#'
#' @return A character string
#' @export
#'
#' @examples
#' round_preserve_sum(c(2.2,4.5,8.7))
#' round_preserve_sum(c(2.2,4.5,8.7), digits = 1)

round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}
