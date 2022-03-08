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
library(reactable)
library(readxl)
library(downloadthis)

library(vroom)

#########################
# get country gbif data
# gbif_data = vroom('./data/biodiversity/data/gbif/0171509-210914110416597.zip')
gbif_data = vroom('./data/biodiversity/data/gbif/gbif_db.zip')

#########################
# read all boundaries
urbanshift_boundaries = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/administrative_boundaries.geojson",
                                quiet = TRUE)

# change San jose boundary status
urbanshift_boundaries[urbanshift_boundaries$city_name == "CRI-San_Jose" & urbanshift_boundaries$boundary_data_source == "cities360", "boundary_use"] = TRUE
urbanshift_boundaries[urbanshift_boundaries$city_name == "CRI-San_Jose" & urbanshift_boundaries$boundary_data_source == "city_specific", "boundary_use"] = FALSE

# change IDN-Jakarta boundary status
urbanshift_boundaries[urbanshift_boundaries$city_name == "IDN-Jakarta" & urbanshift_boundaries$boundary_data_source == "cities360", "boundary_use"] = TRUE
urbanshift_boundaries[urbanshift_boundaries$city_name == "IDN-Jakarta" & urbanshift_boundaries$boundary_data_source == "city_specific", "boundary_use"] = FALSE

# filter right boundaries
urbanshift_boundaries = urbanshift_boundaries[urbanshift_boundaries$boundary_use == TRUE, ]

# add country iso code 2
urbanshift_boundaries = urbanshift_boundaries %>% 
  mutate(country_iso2 = str_sub(country_iso3, 1,2))

urbanshift_boundaries[urbanshift_boundaries$country_iso2 == "CH", "country_iso2"] = "CN"

#########################

for(i in 1:nrow(urbanshift_boundaries)){
  
  print(i)
  # get city name
  city_name = urbanshift_boundaries$city_name[i]
  print(city_name)
  
  # get city boundary
  city_boundary =  urbanshift_boundaries[urbanshift_boundaries$city_name == city_name, ]
  
  # get country iso code
  country_iso2 = city_boundary$country_iso2
  
  # filter country GBOF data
  gbif_data_country = gbif_data %>% 
    filter(countryCode == country_iso2) %>% 
    dplyr::select(class,order,family,genus,
                  countryCode,stateProvince,
                  decimalLatitude,decimalLongitude,
                  eventDate,year)
  
  # convert country gbif data to sf objects
  gbif_data_country_sf = st_as_sf(gbif_data_country, 
                                  coords = c("decimalLongitude", "decimalLatitude"),
                                  crs = st_crs(4326))
  
  # filter gbif within city boundary
  gbif_city_sf = st_join(gbif_data_country_sf, city_boundary, join = st_intersects)
  
  # drop points outside of boundaries
  gbif_city_sf = gbif_city_sf %>% 
    drop_na(id)
  
  # transform into data frame with lat,long coorindates
  gbif_city_df = gbif_city_sf %>% 
    mutate(lat = unlist(map(gbif_city_sf$geometry,1)),
           long = unlist(map(gbif_city_sf$geometry,2))) 
  
  nrow(gbif_city_df)
  
  #########################
  # plot 
  
  plot(st_geometry(city_boundary))
  plot(gbif_city_df$geometry,
       add = TRUE,
       col = "red")
  
  
  #########################
  # store output
  file_name = paste("GBIF",city_name, sep = "-")
  
  write.csv(gbif_city_df,
            paste("./data/biodiversity/data/gbif/",file_name,".csv", sep = ""),
            row.names = FALSE)
  
}




#########################
# explore output
#########################

# to exclude
# "ARG-Mar_del_Plata" 
# "IDN-Balikpapan"
# "CHN-Ningbo" 

city_name = "CRI-San_Jose"
city_gbif = read.csv(paste("./data/biodiversity/data/gbif/GBIF-",city_name,".csv", sep = ""))
city_boundary = urbanshift_boundaries[urbanshift_boundaries$city_name == city_name, ]

city_gbif_Aves = city_gbif %>% 
  filter(class == "Aves")

# plot map
leaflet(data = city_boundary, height = 500, width = "100%") %>% 
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>% 
  # Add boundaries
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = FALSE)) %>%
  # add cluster markers
  addMarkers(city_gbif_Aves$lat,
             city_gbif_Aves$long, 
             popup = ~as.character(city_gbif_Aves$family),
             # label = label_birds2020,
             clusterOptions = markerClusterOptions(),
             group = "Birds clusters") %>%
  # add birds layer
  addCircleMarkers(city_gbif_Aves$lat,
                   city_gbif_Aves$long, 
                   radius = 3,
                   fillColor = "green",
                   color  = "black",
                   stroke = TRUE,
                   weight = 0.8,
                   fillOpacity = 0.6,
                   popup = ~as.character(city_gbif_Aves$family),
                   # label = label_birds2020,
                   group = "Birds observations") %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Birds observations","Birds clusters"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

#########################
# convert to geojson format
#########################

city_name = "IDN-Balikpapan"
city_gbif = read.csv(paste("./data/biodiversity/data/gbif/GBIF-",city_name,".csv", sep = ""))

# convert country gbif data to sf objects
city_gbif_sf = st_as_sf(city_gbif, 
                        coords = c("lat", "long"),
                        crs = st_crs(4326))

# export geojson
st_write(city_gbif_sf, 
         paste("./data/biodiversity/data/gbif/GBIF-",city_name,".geojson", sep = ""),
         append=FALSE)


