#' Return a rounded marginal
#'
#' \code{mlsp} returns an integer value for a marginal. It's the percent of valid respondents
#' giving the specified response.
#'
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param variable The unquoted variable name
#' @param response The desired response. It can be either numeric or the value label.
#' @param weight The weighting variable, defaults to zwave_weight
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @examples
#' mlsp(mulaw = df, variable = g40, response = 1)
#' mlsp(mulaw = df, variable = g40, response = "approve)
#'

mlsp <- function(mulaw, variable, response,
                 weight = zwave_weight){
  variable <- enquo(variable)
  weight <- enquo(weight)

  # Do this if response is number
  if(suppressWarnings(!is.na(as.numeric(response)) == TRUE)){
    d.temp <- mulaw %>%
      filter(!is.na(!!variable)) %>%
      mutate(total = sum(!!weight)) %>%
      filter(!!variable == response) %>%
      summarise(pct = sum(!!weight)/first(total))
  }

  # Do this if response is value label
  if(suppressWarnings(is.na(as.numeric(response)) == TRUE)){

    # reformat response
    response = response %>%
      stringr::str_to_upper() %>%
      stringr::str_replace_all("[[:punct:]]", " ") %>%
      stringr::str_squish()

    d.temp <- mulaw %>%
      filter(!is.na(!!variable)) %>%
      mutate(total = sum(!!weight),
             !!variable := labelled::to_character(!!variable),
             !!variable := stringr::str_to_upper(!!variable),
             !!variable := stringr::str_replace_all(!!variable, "[[:punct:]]", " "),
             !!variable := stringr::str_squish(!!variable)) %>%
      filter(!!variable == response) %>%
      summarise(pct = sum(!!weight)/first(total))
  }

  round((d.temp$pct)*100, digits = 0)[1]
}
