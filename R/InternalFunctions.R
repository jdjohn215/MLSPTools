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

  # define the palettes
  pid3.palette <- c("#ef3b2c", "#807dba", "#4292c6", "#4daf4a", "#ff7f00", "#fb9a99")
  pid5.palette <- c("#ef3b2c", "#fc9272", "#807dba", "#9ecae1", "#4292c6", "#4daf4a", "#ff7f00", "#fb9a99")
  fav2.palette <- c("#238b45", "#6a51a3", "#ff7f00", "#e41a1c", "#fb9a99")
  fav4.palette <- c("#006d2c", "#41ab5d", "#807dba", "#54278f", "#ff7f00", "#e41a1c", "#fb9a99")

  # this palette is for races with a dem and republican in that order
  voteDR.palette <- c("#74add1", "#f46d43", "#7fc97f", "#beaed4", "#fdc086", "#ffff99")

  # this palette is for races with a rep and dem in that order
  voteRD.palette <- c("#f46d43", "#74add1", "#7fc97f", "#beaed4", "#fdc086", "#ffff99")

  # this palette is for 4-category sequences, where category 1 is darkest
  sequence4 <- c("#006d2c", "#31a354", "#74c476", "#a1d99b", "#fdc086", "#beaed4", "#ffff99")
  sequence3 <- c("#006d2c", "#31a354", "#74c476", "#fdc086", "#beaed4", "#ffff99")

  if(fillPalette == "guess"){
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
      mlspPalette <- fav2.palette
    } else if(colorlevels[1] %in% c("strongly approve", "strongly support", "strongly favor", "strongly favorable")){
      mlspPalette <- fav4.palette
      # this covers head-to-heads against Trump
    } else if(colorlevels[2] == "donald trump") {
      mlspPalette <- voteDR.palette
    } else if(colorlevels[1] == "donald trump") {
      mlspPalette <- voteRD.palette
    }
    # This covers 4-cat sequential responses
    else if(word(colorlevels[1]) == "very" & word(colorlevels[4], 1, 3) == "not at all") {
      mlspPalette <- "sequence4"
    } else if(colorlevels[1] == "living comfortably") {
      mlspPalette <- sequence3
    }
    # assign fav2 if only 4 response categories
    else if(length(colorlevels) < 5) {
      mlspPalette <- fav2.palette
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
    # this checks if fillPalette is the name of one of my custom palettes
  } else if(fillPalette %in% c("pid3", "pid5", "fav2", "fav4")) {
    if(fillPalette == "pid3") {
      mlspPalette <- pid3.palette
    } else if(fillPalette == "pid5") {
      mlspPalette <- pid5.palette
    } else if(fillPalette == "fav4") {
      mlspPalette <- fav4.palette
    } else if(fillPalette == "fav2") {
      mlspPalette <- fav2.palette
    } else if(fillPalette == "voteDR"){
      mlspPalette <- voteDR.palette
    } else if(fillPalette == "voteRD"){
      mlspPalette <- voteRD.palette
    }
  }

  mlspPalette
}
