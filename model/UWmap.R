#leaflet map of UWcwbindex
library(leaflet)
library(dplyr)
library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(readxl)
library(shiny)


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

#intall needed package raster and run libaray

# install.packages("raster")
library(raster)
counties <- shapefile("data/United Way Tracts (TIGER 2010).shp")
# not sure if I am needing to "filter" the shape file for a less jummbled apperance


#add shapefile with the addPolygons function
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -84.386330, lat = 33.753746, zoom = 9) %>%
  addPolygons(data = counties)
m

#further customize our polygons appearance
#use a different basemap
m <- leaflet(counties) %>%
  addProviderTiles(providers$Thunderforest.Pioneer) %>%
  setView(lng = -84.386330, lat = 33.753746, zoom = 8)
m

m <- leaflet(counties) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng = -84.386330, lat = 33.753746, zoom = 8)
m

m <- leaflet(counties) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng = -84.386330, lat = 33.753746, zoom = 8)
m

m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng = -84.386330, lat = 33.753746, zoom = 8) %>%
  addPolygons(data = counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5)
m

#Let's create a chloropleth to show the child well being factors for each county

m <- leaflet() %>%
  setView(lng =-84.386330, lat = 33.753746,zoom = 8)  %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = counties,
              weight = 1, 
              smoothFactor = 0.5)
m

#read in excel file with truncated tract aka weave_ct2010
uwmapdata<-read_excel("Complete index table w trunct tract.xlsx")


#line up the states between our data and the shapefile
is.element(df0$TRACT, counties$TRACTCE10)


#now check that all shapefile states are in df0 data
for(i in 1:length(counties$TRACTCE10)){
  if(is.element(counties$TRACTCE10[i], df0$TRACT[i]) == FALSE)
    print(paste0(c(i,counties$TRACTCE10[i],df0$TRACT[i])))
  else{}
}
# df0 <- df0[order(match(df0$trunctract, counties$TRACTCE10)),]



#1) make the leaftlet interactive like to example(and if you have to make our own shiny do so)

variablename <- readline("What is the variable?")
# 2) select the column based on variable name: names(country) == variablename
# write an if or call statement that takes the input of a string


df0 <- df0[order(match(df0$TRACT, counties$TRACTCE10)),]
df0$TRACT<-uwmapdata$tract
mycolor <- as.numeric(df0$gradrate)

bins <- c(0, .10*max(mycolor), .20*max(mycolor), .30*max(mycolor), 
          .40*max(mycolor), .50*max(mycolor), .60*max(mycolor), .70*max(mycolor), Inf)
# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("RdYlBu", domain = mycolor, bins = bins)

#add our color pallet

m <- leaflet() %>%
  setView(lng = -84.386330, lat = 33.753746, zoom = 8) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = counties,
                          fillColor = pal(mycolor),
                          weight = 1, 
                          smoothFactor = 0.5,
                          color = "white",
                          fillOpacity = 0.8)
m

labels<-paste("<p>",uwmapdata$county,"<p>",
              "<p>", "CWB", round(uwmapdata$CWB_Index, digits = 5),"<p>",
              sep="")

m <- leaflet() %>%
  setView(lng = -84.386330, lat = 33.753746, zoom = 8) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = counties,
              fillColor = pal(mycolor),
              weight = 1, 
              smoothFactor = 0.5,
              color = "orange",
              fillOpacity =0.5,
              highlight= highlightOptions (weight = 5, color ="#666666", dashArray = "",
                                           fillOpacity = .7, bringToFront = TRUE ),
              label = lapply(labels, HTML))
m




