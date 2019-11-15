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
  pid3.palette <- c("#cb181d", "#6a51a3", "#2171b5", "#4daf4a", "#ff7f00", "#fb9a99")
  pid5.palette <- c("#a50f15", "#ef3b2c", "#6a51a3", "#4292c6", "#08519c", "#4daf4a", "#ff7f00", "#fb9a99")
  fav2.palette <- c("#238b45", "#6a51a3", "#ff7f00", "#e41a1c", "#fb9a99")
  fav4.palette <- c("#006d2c", "#41ab5d", "#807dba", "#54278f", "#ff7f00", "#e41a1c", "#fb9a99")

  if(fillPalette == "guess"){
    # Check if it's a party ID variable
    if(colorlevels[1] == "republican"){
      # check if 3 category
      if(colorlevels[2] == "independent"){
        mlspPalette <- pid3.palette
      } else {
        mlspPalette <- pid5.palette
      }
      #this covers 3-cat support/oppose question
    } else if(colorlevels[1] %in% c("approve", "support", "favor")){
      mlspPalette <- fav2.palette
    } else if(colorlevels[1] %in% c("strongly approve", "strongly support", "strongly favor")){
      mlspPalette <- fav4.palette
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

  mlspPalette
}
