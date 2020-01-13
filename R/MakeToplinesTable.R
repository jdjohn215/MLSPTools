#' Make a weighted table of toplines for questions in a series
#'
#' \code{make.topline.table} returns a data.frame containing a table of weighted toplines for
#' variables with identical response options
#'
#'  This function returns a table with 1 row for each survey question and columns for the
#'  responses. It is designed for things like favorability questions.
#'
#' @param varnames a character vector of variable names
#' @param qtext a character vector of the descriptive text for each variable
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param weight The weighting variable, defaults to zwave_weight
#' @param n Logical. Controls whether a row total column is included
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom labelled to_factor
#' @importFrom labelled is.labelled
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#'
#' @examples
#' make.topline.table(varnames = c("f111", "f112", "f113", "f114", "f115", "f116", "f117", "f118"),
#' qtext = c("Joe Biden", "Bernie Sanders", "Kamala Harris", "Elizabeth Warren",
#' "Cory Booker", "Beto O'Rourke", "Amy Klobuchar", "Julian Castro"),
#' mulaw = df)
#'

make.topline.table <- function(varnames, qtext, remove, mulaw,
                               weight = zwave_weight, n = FALSE){


  # if remove is missing replace with empty string
  if(missing(remove)){
    remove = ""
  }

  # Make data.frame of names
  d.names <- data.frame(varnames, qtext)

  d <- mulaw %>%
    select({{weight}}, varnames) %>%
    mutate_if(is.labelled, to_factor, levels = "prefixed") %>%
    gather(key = "variable", value = "response", - {{weight}}) %>%
    filter(!is.na(response)) %>%
    inner_join(d.names, by = c("variable" = "varnames")) %>%
    group_by(qtext) %>%
    mutate(total = sum({{weight}})) %>%
    group_by(" " = qtext, response) %>%
    summarise(pct = (sum({{weight}})/first(total))*100,
              n = first(total)) %>%
    spread(key = response, value = pct, fill = 0) %>%
    # move total row to end
    select(-one_of("n"), one_of("n")) %>%
    ungroup()

  if(n == FALSE){
    d <- select(d, -n)
  }

  # Remove numeric prefixes from column names
  colnames(d)[str_sub(colnames(d), 1, 1) == "["] <- word(colnames(d)[str_sub(colnames(d), 1, 1) == "["], 2, -1)
  d
}
