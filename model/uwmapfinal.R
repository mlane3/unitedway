#Final Map Draft
library(leaflet)
library(dplyr)
library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(readxl)
library(shiny)
library(raster)

#map centered on the state of Georgia
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -84.386330, lat = 33.753746 , zoom = 9)
m

counties <- shapefile("data/United Way Tracts (TIGER 2010).shp")

#add shapefile with the addPolygons function
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -84.386330, lat = 33.753746, zoom = 9) %>%
  addPolygons(data = counties)
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


df0 <- read_xlsx("2016 Original Data.xlsx")
names(df0) <- c('county','TRACT','gradrate','ccrpi',
                'grade3','grade8','lbw','childnohealth',
                'childpoverty','povertyrate','housingburden','momsnohs',
                'collegerate','adultsnoedu','adultnohealth','unemployment')

#line up the states between our data and the shapefile
is.element(df0$TRACT, counties$GEOID10)

#now check that all shapefile states are in df0 data
for(i in 1:length(counties$GEOID10)){
  if(is.element(counties$GEOID10[i], df0$TRACT[i]) == FALSE)
    print(paste0(c(i,counties$GEOID10[i],df0$TRACT[i])))
  else{}
}
dfUW <- df0[order(match(df0$TRACT, counties$GEOID10))]
mycolor <- as.numeric(df0$TRACT)
bins <- c(0, .10*max(mycolor), .20*max(mycolor), .30*max(mycolor), 
          .40*max(mycolor), .50*max(mycolor), .60*max(mycolor), .70*max(mycolor), Inf)
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

labels<-paste("<p>",df0$county,"<p>",
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
