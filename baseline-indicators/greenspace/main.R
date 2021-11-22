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

# Define path

path = paste(getwd(),"github/cities-urbanshift/baseline-indicators/greenspace",sep ="/")

# Define city

selected_city = "RWA-Kigali"

# Get city administrative boundaries ----

source(file = paste(path,"src/read_cities_boundaries.R", sep ="/"))

city_boundary = get.city.boundary(city_id = selected_city)

# Get Dynamic world data ----

city_landcover_dw_2020 = get.lulc.dw(year = 2020, city_id = selected_city, city_boundary = city_boundary)
city_landcover_dw_2019 = get.lulc.dw(year = 2019, city_id = selected_city, city_boundary = city_boundary)
city_landcover_dw_2018 = get.lulc.dw(year = 2018, city_id = selected_city, city_boundary = city_boundary)
city_landcover_dw_2017 = get.lulc.dw(year = 2017, city_id = selected_city, city_boundary = city_boundary)
city_landcover_dw_2016 = get.lulc.dw(year = 2016, city_id = selected_city, city_boundary = city_boundary)

# compute land cover statistics based on dynamic world data ----

source(file = paste(path,"src/compute_lulc_stat_dynamic_world.R", sep ="/"))

# compute land cover statistics for every year
city_landcover_dw_stat_2020 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2020, year = "2020")
city_landcover_dw_stat_2019 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2019, year = "2019")
city_landcover_dw_stat_2018 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2018, year = "2018")
city_landcover_dw_stat_2017 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2017, year = "2017")
city_landcover_dw_stat_2016 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2016, year = "2016")

# merge all years
city_landcover_dw_stat = bind_rows(list(city_landcover_dw_stat_2020,
                                     city_landcover_dw_stat_2019,
                                     city_landcover_dw_stat_2018,
                                     city_landcover_dw_stat_2017,
                                     city_landcover_dw_stat_2016))

# Save datasets to CSV files in tempdir()
write.csv(city_landcover_dw_stat, 
          file.path(tempdir(), "city_landcover_dw_stat.csv"))

# Upload files to S3 bucket
put_object(
  file = file.path(tempdir(), "city_landcover_dw_stat.csv"), 
  object = "city_landcover_dw_stat.csv", 
  bucket = "cities-urbanshift/baseline-indicators/greenspace"
)


