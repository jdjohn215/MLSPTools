# These functions are called internally by MLSPTools.
# They cannot be accessed by a user who has simply loaded the package.

####################################################################################
# These functions are used to choose palettes in the mlspgraph functions
####################################################################################

# This function determines the type of input table
table.type <- function(table){
  if(names(table)[1] == "Response"){
    tabletype <- "topline"
  } else if(names(table)[1] == "PollDate"){
    tabletype <- "ts"
  } else {
    tabletype <- "crosstab"
  }
  tabletype
}

# based on the type of input table, this function extracts the values to which fill colors will be assigned
color.levels <- function(table){
  if(table.type(table) == "topline"){
    colorlevels <- table %>% mutate(Response = as.character(Response)) %>% pull(Response)
  } else if(table.type(table) == "ts"){
    colorlevels <- names(table)[2:ncol(table)]
  } else if(table.type(table) == "crosstab"){
    colorlevels <- names(table)[2:(ncol(table)-1)]
  }
  colorlevels
}

# this function uses the other two functions to assign a palette to the fill values
guess.palette <- function(table, fillPalette = "guess"){
  colorlevels <- color.levels(table) %>%
    str_to_lower()

  if(fillPalette[1] == "guess"){
    # Check if it's a party ID variable
    if(colorlevels[1] %in% c("republican", "rep")){
      # check if 3 category
      if(colorlevels[2] %in% c("independent", "ind")){
        mlspPalette <- pid3.palette
      } else {
        mlspPalette <- pid5.palette
      }
      #this covers 3-cat support/oppose question
    } else if(colorlevels[1] %in% c("approve", "support", "favor", "favorable")){
      mlspPalette <- fav2.palette()
    } else if(colorlevels[1] %in% c("strongly approve", "strongly support", "strongly favor", "strongly favorable")){
      mlspPalette <- fav4.palette()
      # this covers head-to-heads against Trump
    } else if(colorlevels[2] %in% c("donald trump", "trump", "walker", "scott walker")) {
      mlspPalette <- vote.palette()
    } else if(colorlevels[1] %in% c("donald trump", "trump", "walker", "scott walker")) {
      mlspPalette <- vote.palette(reverse = TRUE)
    }
    # This covers 4-cat sequential responses
    else if(word(colorlevels[1]) == "very" & word(colorlevels[4], 1, 3) == "not at all") {
      mlspPalette <- sequence4.palette()
    } else if(colorlevels[1] == "living comfortably") {
      mlspPalette <- sequence3.palette()
    }
    # assign fav2 if only 4 response categories
    else if(length(colorlevels) < 5) {
      mlspPalette <- fav2.palette()
    }
    # add default palette if none of the above conditions are satisfied
    else {
      mlspPalette <- RColorBrewer::brewer.pal(n = length(colorlevels), name = "Dark2")
    }
    # this next bit checks if the fillPalette is a supplied vector of colors
  } else if(length(fillPalette) > 1) {
    mlspPalette <- fillPalette
    # this checks if fillPalette is the name of an RColorBrewer palette
  } else if(fillPalette %in% rownames(RColorBrewer::brewer.pal.info)) {
    mlspPalette <- RColorBrewer::brewer.pal(n = length(colorlevels), name = fillPalette)
  }

  mlspPalette()
}
