library(plotly)
library(leaflet)
library(plyr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(tidyverse)
library(sf)
library(rgeos)
library(httr)
library(jsonlite)
library(raster)
library(data.table)
library(leaflet.providers)
library(knitr)
library(kableExtra)


col_BoldRiverBlue = "#242456"
col_BoldSunYellow = "#FFD450"
col_BoldGrassGreen = "#2A553E"
col_BoldEarthGrey = "#7B7A66"
col_BoldBrickOrange = "#F26640"
col_LightRiverBlue = "#E3E6FF"
col_LightSunYellow = "#FFEDBA"
col_LightGrassGreen = "#C4F4D5"
col_LightEarthGrey = "#ECE2D6"
col_LightBrickOrange = "#FED3CF"
col_White = "#FFFFFF"
col_Black = "#000000"

# read Boundary

boundary = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/CRI-San_Jose-boundary.geojson",
                    quiet = TRUE)

boundary_centroid = st_centroid(boundary)
boundary_centroid_lat = boundary_centroid$geometry[[1]][2]
boundary_centroid_lon = boundary_centroid$geometry[[1]][1]

# birds observation

birds2020 = st_read("./data/biodiversity/data/biodiversity_map_layers_birds2020_sanjose.geojson",
                    quiet = TRUE)

birds2020 = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/biodiversity_map_layers_birds2020_sanjose.geojson",
                    quiet = TRUE)


label_birds2020 <- sprintf(
  "<strong>%s</strong><br/><strong>%s</strong><br/>",
  paste("Family", birds2020$family, sep=": "), 
  paste("Genus", birds2020$genus, sep =": ")
) %>% 
  lapply(htmltools::HTML)

leaflet(data = boundary, height = 500, width = "100%") %>% 
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  setView(lat = boundary_centroid_lat,
          lng = boundary_centroid_lon,
          zoom = 9) %>% 
  # Add boundaries
  addPolygons(data = boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = col_Black, weight = 3,dashArray = "3",
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = FALSE)) %>%
  # add birds layer
  addCircleMarkers(birds2020$lon, birds2020$lat, 
                   radius = 3,
                   fillColor = col_BoldBrickOrange,
                   color  = "black",
                   stroke = TRUE,
                   weight = 0.8,
                   fillOpacity = 0.6,
                   popup = ~as.character(birds2020$family),
                   label = label_birds2020) 

birds2020 %>% 
  as.data.frame() %>% 
  group_by(order) %>%
  summarise(nb_species = n()) %>% 
  arrange(desc(nb_species)) %>% 
  dplyr::select(`order name` = order,
         `Number of species`= nb_species)
