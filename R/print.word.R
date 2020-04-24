#' Print a number as an english word
#'
#' \code{print.word} a character string of a number
#'
#'  This function is a wrapper for either the words() or Words() function from
#'  the english package. It defaults to upper case, but lower case can
#'  be set using case = lower. print.word() also adds a hyphen between words.
#'
#' @param number any numeric value
#' @param case either "upper" or "lower." Defaults to "upper."
#'
#' @return A character string
#' @export
#'
#' @examples
#' print.word(42)
#' print.word(42, case = "lower")

print.word <- function(number, case){
  # set default case to upper
  if(missing(case)){
    case = "upper"
  }

  if(stringr::str_to_lower(case) == "upper"){
    output <- stringr::str_replace(
      english::Words(number),
      " ", "-")
  }

  if(stringr::str_to_lower(case) == "lower"){
    output <- stringr::str_replace(
      english::words(number),
      " ", "-")
  }

  output
}
