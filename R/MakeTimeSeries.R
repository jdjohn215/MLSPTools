#' Make a time series of a variable
#'
#' \code{make.ts} returns a data.frame containing a time series of a variable
#'
#'  This function returns a time series of any given variable. The resulting data.frame defaults
#'  to wide format, but can be in long format if format = "long" is specified.
#'
#' @param variable The variable, unquoted
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' @param format One of "wide" or "long", defaults to "wide"
#' @param date The date variable, defaults to zpollenddate
#' @param weight The weight variable, defaults to zwave_weight
#' @param n Logical, determines if a row total is included or not
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom labelled to_factor
#' @importFrom tidyr spread
#'
#' @examples
#' make.ts(variable = g2, mulaw = orig, remove = c("refused"), format = "long")
#'
make.ts <- function(variable, mulaw, remove, format = "wide",
                    date = zpollenddate, weight = zwave_weight,
                    n = FALSE){

  # if remove is missing replace with empty string
  if(missing(remove)){
    remove = ""
  }

  # Make wide table
  if(format == "wide"){
    d.output <- mulaw %>%
      # Remove missing cases
      filter(!is.na({{variable}})) %>%
      # Convert to ordered factors
      mutate({{variable}} := to_factor({{variable}}, sort_levels = "values"),
             {{date}} := to_factor({{date}})) %>%
      # Calculate denominator
      group_by({{date}}) %>%
      mutate(total = sum({{weight}})) %>%
      # Calculate proportions
      group_by({{date}}, {{variable}}) %>%
      summarise(pct = (sum({{weight}})/first(total))*100,
                n = first(total)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{variable}}) %in% str_to_upper(remove)) %>%
      # Spread so x is rows and y is columns
      spread(key = {{variable}}, value = pct) %>%
      rename(PollDate = {{date}}) %>%
      # move total row to end
      select(-one_of("n"), one_of("n")) %>%
      ungroup()

    if(n == FALSE){
      d.output <- select(d.output, -n)
    }
  }

  # Make long table
  if(format == "long"){
    d.output <- mulaw %>%
      # Remove missing cases
      filter(!is.na({{variable}})) %>%
      # Convert to ordered factors
      mutate({{variable}} := to_factor({{variable}}, sort_levels = "values"),
             {{date}} := as.character({{date}})) %>%
      # Calculate denominator
      group_by(zpolldatestr) %>%
      mutate(total = sum({{weight}})) %>%
      # Calculate proportions
      group_by({{date}}, {{variable}}) %>%
      summarise(pct = (sum({{weight}})/first(total))*100,
                n = first(total)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{variable}}) %in% str_to_upper(remove)) %>%
      rename(PollDate = {{date}}) %>%
      ungroup()

    if(n == FALSE){
      d.output <- select(d.output, -n)
    }
  }

  d.output
}
