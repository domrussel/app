library(tidyverse)
library(sf)
library(leaflet)
require(spData)

# Read in the main data set
allchanges <- readRDS("all_changes.rds")

# Generate a map of all the states
map_dat <- spData::us_states %>% 
  left_join(filter(allchanges, yr==2017), by=c("NAME"="state")) %>% 
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84')

# A helper function to be used to create the percent labels on the bar charts
return_prty_pct <- function(x){
  returns <- numeric(length(x))
  for(i in 1:length(x)){
    val <- x[i]
    if(val > 0){
      new_item <- paste0("+", round(val, 1), "%")
    }
    else{
      new_item <- paste0(round(val, 1), "%")
    }
    returns[i] <- new_item
  }
  returns
}