library(tidyverse)
library(ggplot2)
library(sf)
library(leaflet)

setwd("C:/Users/Dominic/Documents/tuition_v_income/app")

allchanges <- readRDS("data/all_changes.rds")

map_dat <- spData::us_states %>% 
  left_join(filter(allchanges, yr==2017), by=c("NAME"="state")) %>% 
  st_as_sf()

map_dat <- st_transform(map_dat,'+proj=longlat +datum=WGS84')

pal <- colorNumeric("YlOrRd", domain = map_dat$change_tuition_adj)

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