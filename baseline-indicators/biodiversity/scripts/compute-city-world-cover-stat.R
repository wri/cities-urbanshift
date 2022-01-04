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

# read all boundaries
urbanshift_boundaries = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/administrative_boundaries.geojson",
                                quiet = TRUE)

# select verified boundaries
urbanshift_boundaries = urbanshift_boundaries[urbanshift_boundaries$boundary_use == TRUE, ]

# cities list

urbanshift_cities = urbanshift_boundaries$city_name
urbanshift_cities = urbanshift_cities[! urbanshift_cities %in% c("ARG-Mar_del_Plata",
                                                                 "IDN-Balikpapan",
                                                                 "CHN-Ningbo")]

# compute statistics on world cover distribution at the city level

world.cover.city.stat = function(city_worldcover, year,city_id){
  city_worldcover_stat = as.data.frame(city_worldcover_raster) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    group_by(class) %>%
    tally() %>%
    mutate(area = n * res(city_worldcover_raster)[1] * res(city_worldcover_raster)[2]) %>% 
    dplyr::select(land_cover_classes = class,
                  nb_cells = n) %>% 
    mutate(land_cover_percent = round(nb_cells/sum(nb_cells) * 100,2))%>% 
    add_column(year = year,
               city_id = city_id) %>% 
    arrange(desc(land_cover_percent)) %>%
    mutate_at("land_cover_classes", as.character) %>%
    mutate(land_cover_classes = recode(land_cover_classes,
                                       "10" =  "Trees",
                                       "20" = "Shrubland" ,
                                       "30" = "Grassland",
                                       "40" = "Cropland",
                                       "50" =  "Built-up",
                                       "60" = "Barren / sparse vegetation",
                                       "70" = "Snow and ice",
                                       "80" = "Open water",
                                       "90" = "Herbaceous wetland",
                                       "95" = "Mangroves",
                                       "100" = "Moss and lichen")) %>% 
    dplyr::select(city_id = city_id,
                  year = year,
                  land_cover_class = land_cover_classes,
                  land_percent = land_cover_percent)
  
  return(city_worldcover_stat)
}


# initialize empty dataframe for storing indicators
cities_worldcover_stat = data.frame(city_id = as.character(),
                                    land_cover_class = as.character(),
                                    land_percent = as.numeric(),
                                    year = as.numeric())

for(i in 1:length(urbanshift_cities)){
  
  print(i)
  
  city_id = urbanshift_cities[i]
  print(city_id)
  
  # extract city boundary
  city_boundary = urbanshift_boundaries[urbanshift_boundaries$city_name == city_id, ]
  
  # read world cover by city
  
  worldcoverc_data_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/esa_world_cover/world_cover_",
                                city_id,
                                ".tif",
                                sep = "")
  city_worldcover_raster = raster(worldcoverc_data_path)
  city_worldcover  = raster::mask(city_worldcover_raster,
                                  city_boundary)
  
  city_worldcover_stat = city_worldcover_stat = world.cover.city.stat(city_worldcover = city_worldcover,
                                                                      year = 2020,
                                                                      city_id = city_id)
  
  
  cities_worldcover_stat = rbind(cities_worldcover_stat,
                                 city_worldcover_stat)
}




