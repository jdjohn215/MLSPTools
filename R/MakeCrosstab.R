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
#' @param weight The weighting variable, defaults to zwave_weight
#' @param n logical, if TRUE a column of row totals is included
#' @param cell logical, if TRUE cell percentages will be returned rather than row percentages (the default)
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @import tidyverse
#' @import labelled
#' @import haven
#' @importFrom labelled to_factor
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#'
#' @examples
#' make.crosstab(x = zpid3, y = g40v2, mulaw = df, remove = c("don't know", "refused"))


make.crosstab <- function(x, y, mulaw, remove,
                          weight = zwave_weight, n = TRUE,
                          cell = FALSE){
  # Some Nonstandard Evaluation witchcraft I don't understand
  x <- enquo(x)
  y <- enquo(y)
  weight <- enquo(weight)

  # if remove is missing replace with empty string
  if(missing(remove)){
    remove = ""
  }

  if(cell == FALSE){
    # Make
    d.output <- mulaw %>%
      # Remove missing cases
      filter(!is.na(!!x),
             !is.na(!!y)) %>%
      # Convert to ordered factors
      mutate(!!x := to_factor(!!x, sort_levels = "values"),
             !!y := to_factor(!!y, sort_levels = "values")) %>%
      # Calculate denominator
      group_by(!!x) %>%
      mutate(total = sum(!!weight)) %>%
      # Calculate proportions
      group_by(!!x, !!y) %>%
      summarise(pct = (sum(!!weight)/first(total))*100,
                n = first(total)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper(!!x) %in% str_to_upper(remove),
             !str_to_upper(!!y) %in% str_to_upper(remove)) %>%
      # Spread so x is rows and y is columns
      spread(key = !!y, value = pct, fill = 0) %>%
      rename(" " = !!x) %>%
      # move total row to end
      select(-one_of("n"), one_of("n")) %>%
      ungroup()

    if(n == FALSE){
      d.output <- d.output %>%
        select(-n)
    }
  } else if(cell == TRUE){
    # Make
    d.output <- mulaw %>%
      # Remove missing cases
      filter(!is.na(!!x),
             !is.na(!!y)) %>%
      # Convert to ordered factors
      mutate(!!x := to_factor(!!x, sort_levels = "values"),
             !!y := to_factor(!!y, sort_levels = "values")) %>%
      # Calculate denominator
      mutate(total = sum(!!weight)) %>%
      # Calculate proportions
      group_by(!!x, !!y) %>%
      summarise(pct = (sum(!!weight)/first(total))*100,
                n = first(total)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper(!!x) %in% str_to_upper(remove),
             !str_to_upper(!!y) %in% str_to_upper(remove)) %>%
      # Spread so x is rows and y is columns
      spread(key = !!y, value = pct, fill = 0) %>%
      rename(" " = !!x) %>%
      # move total row to end
      select(-one_of("n"), one_of("n")) %>%
      ungroup()

    if(n == FALSE){
      d.output <- d.output %>%
        select(-n)
    }
  }

  d.output
}
