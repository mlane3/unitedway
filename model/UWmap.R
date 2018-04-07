#leaflet map of UWcwbindex
library(leaflet)
library(dplyr)
library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(readxl)


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

install.packages("raster")
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

#Let's create a chloropleth to show the number of solved and unsolved murders
#for each state


m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              weight = 1, 
              smoothFactor = 0.5)
m

#We need to calculate our summary stats
df0 <- fbi_data %>%
  mutate(Solved = ifelse(Crime.Solved == "Yes", 1, 0)) %>%
  filter(Crime.Type == "Murder or Manslaughter") %>%
  group_by(State) %>%
  summarise(Num.Murders = n(),
            Num.Solved = sum(Solved)) %>%
  mutate(Num.Unsolved = Num.Murders - Num.Solved,
         Solve.Rate = Num.Solved/Num.Murders)

#line up the states between our data and the shapefile
is.element(df0$weave_ct2010, counties$TRACTCE10)

#should now be all true


#now check that all shapefile states are in df0 data
unique(is.element(counties$TRACTCE10, df0$weave_ct2010))


#we see that we're missing the following: 33, 34, 54, 55, 56
states <- subset(states, is.element(counties$TRACTCE10, df0$weave_ct2010))

#now order the crime stats so that its the same order as the shapefile states
df0 <- df0[order(match(df0$weave_ct2010, counties$TRACTCE10)),]

bins <- c(0.30, 0.40,0.50,0.60,0.70,0.80,0.90, 1.0)
pal <- colorBin("RdYlBu", domain = df0$Solve.Rate, bins = bins)

#add our color pallete to our map
m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              fillColor = pal(df0$Solve.Rate),
              weight = 1, 
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8)
m
