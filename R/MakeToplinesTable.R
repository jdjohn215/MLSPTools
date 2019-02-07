#' Make a weighted table of toplines for questions in a series
#'
#' \code{make.topline.table} returns a data.frame containing a table of weighted toplines for
#' variables with identical response options
#'
#'  This function returns a table with 1 row for each survey question and columns for the
#'  responses. It is designed for things like favorability questions.
#'
#' @param vars a character vector of variable names
#' @param text a character vector of the descriptive text for each variable
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
#' @importFrom tidyr gather
#'
#' @examples
#' make.topline.table(vars = c("f111", "f112", "f113", "f114", "f115", "f116", "f117", "f118"),
#' text = c("Joe Biden", "Bernie Sanders", "Kamala Harris", "Elizabeth Warren",
#' "Cory Booker", "Beto O'Rourke", "Amy Klobuchar", "Julian Castro"),
#' mulaw = df)
#'

make.topline.table <- function(vars, text, remove, mulaw){
  # if remove is missing replace with empty string
  if(missing(remove)){
    remove = ""
  }

  # Make data.frame of names
  d.names <- data.frame(vars, text)

  mulaw %>%
    select(zwave_weight, vars) %>%
    mutate_if(is.labelled, to_factor) %>%
    gather(key = "variable", value = "response", - zwave_weight) %>%
    filter(!is.na(response)) %>%
    inner_join(d.names, by = c("variable" = "vars")) %>%
    group_by(text) %>%
    mutate(total = sum(zwave_weight)) %>%
    group_by(text, response) %>%
    summarise(pct = sum(zwave_weight)/first(total)) %>%
    spread(key = response, value = pct)
}
