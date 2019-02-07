#' Make a weighted crosstab of row proportions
#'
#' \code{make.crosstab} returns a data.frame containing a weighted crosstab of two variables
#'
#'  The x variable will appear as rows in the data.frame. The y variable will be columns.
#'
#' @param x The independent variable
#' @param y The dependent variable
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' This will not affect any calculations made. The vector is not case-sensitive.
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom labelled to_factor
#' @importFrom tidyr spread
#'
#' @examples
#' make.crosstab(x = zpid3, y = g40v2, mulaw = df, remove = c("don't know", "refused"))


make.crosstab <- function(x, y, mulaw, remove){
  # Some Nonstandard Evaluation witchcraft I don't understand
  x <- enquo(x)
  y <- enquo(y)

  # if remove is missing replace with empty string
  if(missing(remove)){
    remove = ""
  }

  # Make
  mulaw %>%
    # Remove missing cases
    filter(!is.na(!!x),
           !is.na(!!y)) %>%
    # Convert to ordered factors
    mutate(!!x := to_factor(!!x),
           !!y := to_factor(!!y)) %>%
    # Calculate denominator
    group_by(!!x) %>%
    mutate(total = sum(zwave_weight)) %>%
    # Calculate proportions
    group_by(!!x, !!y) %>%
    summarise(pct = sum(zwave_weight)/first(total)) %>%
    # Remove values included in "remove" string
    filter(!str_to_upper(!!x) %in% str_to_upper(remove),
           !str_to_upper(!!y) %in% str_to_upper(remove)) %>%
    # Spread so x is rows and y is columns
    spread(key = !!y, value = pct)
}
