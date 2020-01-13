#' Make a weighted crosstab of row proportions, by wave
#'
#' \code{make.crosstab.wave} returns a data.frame containing a weighted crosstab of two variables
#' by wave.
#'
#'  This function returns a crosstab by wave. The resulting data.frame is in long format, suitable
#'  for making a faceted plot. See the example below
#'
#' @param x The independent variable
#' @param y The dependent variable
#' @param mulaw The data.frame containing the version of the integrated file you wish to use
#' @param remove An optional character vector of values to remove from final table (e.g. DK/Ref).
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param date The date variable, defaults to zpollenddate
#' @param weight The weighting variable, defaults to zwave_weight
#' @param n Logical. If TRUE then a row totals column is included
#'
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom labelled to_factor
#' @importFrom tidyr spread
#'
#' @examples
#' make.crosstab.wave(x = zpid3, y = g40v2, mulaw = df, remove = c("don't know", "refused"))
#'
#' # Now make that output a faceted graph
#' make.crosstab.wave(x = zpid3, y = g40v2,
#'                    mulaw = df, remove = c("don't know", "refused")) %>%
#'      ggplot(aes(zpid3, ae2, col = ae2)) +
#'        geom_point() +
#'        geom_smoth() +
#'        facet_wrap(facets = vars(zpid3))

make.crosstab.wave <- function(x, y, mulaw, remove,
                               date = zpollenddate,
                               weight = zwave_weight,
                               n = TRUE){
  # Some Nonstandard Evaluation witchcraft I don't understand
  x <- enquo(x)
  y <- enquo(y)
  date <- enquo(date)
  weight <- enquo(weight)

  # if remove is missing replace with empty string
  if(missing(remove)){
    remove = ""
  }

  # Make
  d.output <- mulaw %>%
    # Remove missing cases
    filter(!is.na({{x}}),
           !is.na({{y}})) %>%
    # Convert to ordered factors
    mutate({{x}} := to_factor({{x}}, sort_levels = "values"),
           {{y}} := to_factor({{y}}, sort_levels = "values"),
           {{date}} := to_factor({{date}}, sort_levels = "values")) %>%
    # Calculate denominator
    group_by({{x}}, {{date}}) %>%
    mutate(total = sum({{weight}})) %>%
    # Calculate proportions
    group_by({{x}}, {{y}}, {{date}}) %>%
    summarise(pct = sum({{weight}})/first(total),
              n = first(total)) %>%
    # Remove values included in "remove" string
    filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
           !str_to_upper({{y}}) %in% str_to_upper(remove)) %>%
    ungroup()

  if(n == FALSE){
    d.output <- select(d.output, -n)
  }

  d.output
}
