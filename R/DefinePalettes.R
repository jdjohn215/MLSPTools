#' \code{pid3.palette} returns a vector of colors for 3-category party ID
#'
#' @param reverse if TRUE, reverses the order of the directionally relevant colors, but not the trailing colors
#'
#' @return A character vector
#' @export
pid3.palette <- function(reverse = FALSE){
  if(reverse == TRUE){
    c("#4292c6", "#807dba", "#ef3b2c", "#4daf4a", "#ff7f00", "#fb9a99")
  } else {
    c("#ef3b2c", "#807dba", "#4292c6", "#4daf4a", "#ff7f00", "#fb9a99")
  }
}

#' \code{pid5.palette} returns a vector of colors for 5-category party ID
#'
#' @param reverse if TRUE, reverses the order of the directionally relevant colors, but not the trailing colors
#'
#' @return A character vector
#' @export
pid5.palette <- function(reverse = FALSE){
  if(reverse == TRUE){
    c("#4292c6", "#9ecae1", "#807dba", "#fc9272", "#ef3b2c", "#4daf4a", "#ff7f00", "#fb9a99")
  } else {
    c("#ef3b2c", "#fc9272", "#807dba", "#9ecae1", "#4292c6", "#4daf4a", "#ff7f00", "#fb9a99")
  }
}

#' \code{fav2.palette} returns a vector of colors for 2-category generic positive/negative
#'
#' @param reverse if TRUE, reverses the order of the directionally relevant colors, but not the trailing colors
#'
#' @return A character vector
#' @export
fav2.palette <- function(reverse = FALSE){
  if(reverse == TRUE){
    c("#6a51a3", "#238b45", "#ff7f00", "#e41a1c", "#fb9a99")
  } else {
    c("#238b45", "#6a51a3", "#ff7f00", "#e41a1c", "#fb9a99")
  }
}

#' \code{fav4.palette} returns a vector of colors for 4-category generic positive/negative
#'
#' @param reverse if TRUE, reverses the order of the directionally relevant colors, but not the trailing colors
#'
#' @return A character vector
#' @export
fav4.palette <- function(reverse = FALSE){
  if(reverse == TRUE){
    c("#54278f", "#807dba", "#41ab5d", "#006d2c", "#ff7f00", "#e41a1c", "#fb9a99")
  } else {
    c("#006d2c", "#41ab5d", "#807dba", "#54278f", "#ff7f00", "#e41a1c", "#fb9a99")
  }
}

#' \code{vote.palette} returns a vector of colors for Democrat vs Republican
#'
#' @param reverse if TRUE, reverses the order of the directionally relevant colors, but not the trailing colors
#'
#' @return A character vector
#' @export
vote.palette <- function(reverse = FALSE){
  if(reverse == TRUE){
    c("#fbb4ae", "#b3cde3", "#7fc97f", "#beaed4", "#fdc086", "#ffff99")
  } else {
    c("#b3cde3", "#fbb4ae", "#7fc97f", "#beaed4", "#fdc086", "#ffff99")
  }
}

#' \code{sequence4.palette} returns a vector of colors for 4-category unidirectional sequence
#'
#' @param reverse if TRUE, reverses the order of the directionally relevant colors, but not the trailing colors
#'
#' @return A character vector
#' @export
sequence4.palette <- function(reverse = FALSE){
  if(reverse == TRUE){
    c("#a1d99b", "#74c476", "#31a354", "#006d2c", "#fdc086", "#beaed4", "#ffff99")
  } else {
    c("#006d2c", "#31a354", "#74c476", "#a1d99b", "#fdc086", "#beaed4", "#ffff99")
  }
}

#' \code{sequence3.palette} returns a vector of colors for 3-category unidirectional sequence
#'
#' @param reverse if TRUE, reverses the order of the directionally relevant colors, but not the trailing colors
#'
#' @return A character vector
#' @export
sequence3.palette <- function(reverse = FALSE){
  if(reverse == TRUE){
    c("#74c476", "#31a354", "#006d2c", "#fdc086", "#beaed4", "#ffff99")
  } else {
    c("#006d2c", "#31a354", "#74c476", "#fdc086", "#beaed4", "#ffff99")
  }
}
