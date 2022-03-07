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

##########################
# Test on ARG-Buenos Aires
#########################

gbif_ARG = vroom('./data/biodiversity/data/gbif/0171506-210914110416597.zip')

gbif_ARG_processed = gbif_ARG %>% 
  filter(countryCode == "AR", stateProvince == "Buenos Aires") %>% 
  dplyr::select(class,order,family,genus,
                countryCode,stateProvince,
                decimalLatitude,decimalLongitude,
                eventDate,year)

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

# specify the cityid
city_id = "ARG-Buenos_Aires" 
# extract city boundary
city_boundary = urbanshift_boundaries[urbanshift_boundaries$city_name == city_id, ]



#########################
gbif_ARG_processed_sf = st_as_sf(gbif_ARG_processed, 
                                 coords = c("decimalLongitude", "decimalLatitude"),
                                 crs = st_crs(4326))



# filter birds within city boundary

gbif_within_boundary = st_join(gbif_ARG_processed_sf, city_boundary, join = st_intersects)

gbif_within_boundary_coords <- gbif_within_boundary %>%
  drop_na(id) %>% 
  mutate(lat = unlist(map(gbif_within_boundary$geometry,1)),
         long = unlist(map(gbif_within_boundary$geometry,2)))

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
  # addMarkers(gbif_ARG_processed$decimalLongitude, 
  #            gbif_ARG_processed$decimalLatitude,
  #            popup = ~as.character(gbif_ARG_processed$family),
  #            # label = label_birds2020,
  #            clusterOptions = markerClusterOptions(),
  #            group = "Birds clusters") %>% 
  # add birds layer
  addCircleMarkers(separated_coord$lat,
                   separated_coord$long, 
                   radius = 3,
                   fillColor = "green",
                   color  = "black",
                   stroke = TRUE,
                   weight = 0.8,
                   fillOpacity = 0.6,
                   popup = ~as.character(separated_coord$family),
                   # label = label_birds2020,
                   group = "Birds observations") %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Birds observations"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

##########################
# Test on ARG-Buenos Aires
#########################

# add country iso code 2
urbanshift_boundaries = urbanshift_boundaries %>% 
  mutate(country_iso2 = str_sub(country_iso3, 1,2))

#
city_boundary_1 =  urbanshift_boundaries[urbanshift_boundaries$city_name == "IDN-Jakarta", ]

city_boundary_1_1 = city_boundary_1[1, ]
city_boundary_1_2 = city_boundary_1[2, ]

plot(st_geometry(city_boundary_1_1))

i = 23 #12, 13, 17, 19
city_name = urbanshift_boundaries$city_name[i]
city_name = "IDN-Jakarta"

# get city boundary
city_boundary =  urbanshift_boundaries[urbanshift_boundaries$city_name == city_name, ]
city_boundary = city_boundary[1, ]

country_iso2 = city_boundary$country_iso2

# get country gbif data
# gbif_data = vroom('./data/biodiversity/data/gbif/0171509-210914110416597.zip')

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

plot(st_geometry(city_boundary))
plot(gbif_city_df$geometry,
     add = TRUE,
     col = "red")

# store output
file_name = paste("GBIF",city_name, sep = "-")

write.csv(gbif_city_df,
          paste("./data/biodiversity/data/gbif/",file_name,".csv", sep = ""),
          row.names = FALSE)

##########################
# Test on ARG-Buenos Aires
#########################
gbif_data = vroom('./data/biodiversity/data/gbif/0171509-210914110416597.zip')
unique(gbif_data$countryCode)

# Filter AR
gbif_data_AR = gbif_data %>% 
  filter(countryCode == "AR") %>% 
  dplyr::select(class,order,family,genus,
                countryCode,stateProvince,
                decimalLatitude,decimalLongitude,
                eventDate,year)

write.csv(gbif_data_AR,
          './data/biodiversity/data/gbif/gbif_AR.csv',
          row.names = FALSE)

# Filter IN
gbif_data_IN = gbif_data %>% 
  filter(countryCode == "IN") %>% 
  dplyr::select(class,order,family,genus,
                countryCode,stateProvince,
                decimalLatitude,decimalLongitude,
                eventDate,year)

write.csv(gbif_data_IN,
          './data/biodiversity/data/gbif/gbif_IN.csv',
          row.names = FALSE)

# Filter CR
gbif_data_CR = gbif_data %>% 
  filter(countryCode == "CR") %>% 
  dplyr::select(class,order,family,genus,
                countryCode,stateProvince,
                decimalLatitude,decimalLongitude,
                eventDate,year)

write.csv(gbif_data_CR,
          './data/biodiversity/data/gbif/gbif_CR.csv',
          row.names = FALSE)

# Filter ID
gbif_data_ID = gbif_data %>% 
  filter(countryCode == "ID") %>% 
  dplyr::select(class,order,family,genus,
                countryCode,stateProvince,
                decimalLatitude,decimalLongitude,
                eventDate,year)

write.csv(gbif_data_ID,
          './data/biodiversity/data/gbif/gbif_ID.csv',
          row.names = FALSE)

# Filter ID
gbif_data_ID = gbif_data %>% 
  filter(countryCode == "ID") %>% 
  dplyr::select(class,order,family,genus,
                countryCode,stateProvince,
                decimalLatitude,decimalLongitude,
                eventDate,year)

write.csv(gbif_data_ID,
          './data/biodiversity/data/gbif/gbif_ID.csv',
          row.names = FALSE)


# china

gbif_data_CH = vroom('./data/biodiversity/data/gbif/0172656-210914110416597.zip')

# Filter CH
gbif_data_country = gbif_data_CH %>% 
  filter(countryCode == "CN") %>% 
  dplyr::select(class,order,family,genus,
                countryCode,stateProvince,
                decimalLatitude,decimalLongitude,
                eventDate,year)

city_name = "CHN-Ningbo"

# get city boundary
city_boundary =  urbanshift_boundaries[urbanshift_boundaries$city_name == city_name, ]


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