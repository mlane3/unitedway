#leaflet map of UWcwbindex
library(leaflet)
library(dplyr)
library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)

#worldmap
m <- leaflet() %>%
  addTiles()
m

#map centered on the state of Georgia
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -84.386330, lat = 33.753746 , zoom = 9)
m
