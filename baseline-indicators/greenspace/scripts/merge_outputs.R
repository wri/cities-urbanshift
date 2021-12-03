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

selected_city = "CRI-San_Jose" # "SLE-Freetown_core" #"RWA-Kigali"


# read outputs indicators

city_built_treecover_stat = read.csv(paste(path,"/outputs/", selected_city, "_built_treecover_stat.csv", sep =""))
city_built_vegetation_stat = read.csv(paste(path,"/outputs/", selected_city, "_built_vegetation_stat.csv", sep =""))
city_dw_landcover_stat = read.csv(paste(path,"/outputs/", selected_city, "_dw_landcover_stat.csv", sep =""))
city_tof_treecover_stat = read.csv(paste(path,"/outputs/", selected_city, "_tof_treecover_stat.csv", sep =""))
city_ulu_stat_treecover = read.csv(paste(path,"/outputs/", selected_city, "_ulu_stat_treecover.csv", sep =""))

# merge outputs

city_greenspace_indicators = city_built_treecover_stat %>% 
  bind_rows(city_built_vegetation_stat,
            city_dw_landcover_stat,
            city_tof_treecover_stat)

# export outputs

write.table(city_greenspace_indicators,
            paste(path,"/outputs/",selected_city,"_greenspace_indicators.csv", sep =""),
            sep = ",",
            row.names = FALSE)




