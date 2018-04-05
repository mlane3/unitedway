#leaflet map of UWcwbindex
library(leaflet)
library(dplyr)
library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(readxl)
library(raster)

#worldmap
m <- leaflet() %>%
  addTiles()
m

#map centered on the state of Georgia
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -84.386330, lat = 33.753746 , zoom = 9)
m
#building a map without pipe operator by nesting functions together
m <- setView(addTiles(leaflet()),lng = -84.386330, lat = 33.753746, zoom = 9)
m
#building a map without pipe operator by creating intermediate objects
m_1 <- leaflet()
m_2 <- addTiles(m_1)
m <- setView(m_2, lng = -84.386330, lat = 33.753746, zoom = 9)
m
rm(m_1,m_2)

#read in zip file

install.packages("raster")
library(raster)
counties <- shapefile("data/United Way Tracts (TIGER 2010).shp")
# Customizing Look and Feel --------------------------------------

#Load counties shape C:\Users\Betty\Documents\GitHub\unitedway\datafile.
counties <- readOGR(dsn = "data/", layer ="United Way Tracks(TIGER2010))

 
