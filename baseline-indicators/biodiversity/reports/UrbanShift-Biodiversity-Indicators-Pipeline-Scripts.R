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

# set working directory to be alligned with notebooks
setwd("./github/cities-urbanshift/baseline-indicators/biodiversity/reports")

# city boundary ----

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

# cities_worldcover_stat - Metropolitan level ----

world.cover.city.stat = function(city_worldcover, year,city_id){
  city_worldcover_stat = as.data.frame(city_worldcover) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    group_by(class) %>%
    tally() %>%
    mutate(area = n * res(city_worldcover)[1] * res(city_worldcover)[2]) %>% 
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
  
  city_worldcover_stat = world.cover.city.stat(city_worldcover = city_worldcover,
                                                                      year = 2020,
                                                                      city_id = city_id)
  
  
  cities_worldcover_stat = rbind(cities_worldcover_stat,
                                 city_worldcover_stat)
}

# store outputs
write.csv(cities_worldcover_stat,
          "./data/cities_worldcover_stat.csv",
          row.names = FALSE)

# cities_worldcover_stat - municipality level ----

# read municipality boundaries

boundary_municipality = st_read("./data/boundaries/boundaries-CRI-San_Jose-ADM2.geojson",
                                quiet = TRUE)


# aggregate landcover by municipality

# initialize empty dataframe for storing indicators
municipalities_worldcover_stat = data.frame(city_id = as.character(),
                                    land_cover_class = as.character(),
                                    land_percent = as.numeric(),
                                    year = as.numeric())

for(i in 1:nrow(boundary_municipality)){
  
  print(i)
  
  # extract city boundary
  municipality = boundary_municipality[i, ]
  
  city_id = municipality$shapeName.1
  print(city_id)
  
  # read world cover by city
  municipality_worldcover  = raster::mask(city_worldcover_raster,
                                          municipality)
  
  municipality_worldcover_stat = world.cover.city.stat(city_worldcover = municipality_worldcover,
                                               year = 2020,
                                               city_id = city_id)
  
  print(municipality_worldcover_stat)
  municipalities_worldcover_stat = rbind(municipalities_worldcover_stat,
                                         municipality_worldcover_stat)
}


# store outputs
write.csv(municipalities_worldcover_stat,
          "./data/cities_worldcover_stat.csv",
          row.names = FALSE)

# compute I1 - metropolitan level----

# read cities_worldcover_stat
cities_worldcover_stat = read.csv("./data/cities_worldcover_stat.csv")

# initialize empty dataframe for storing indicators
biodiversity_indicators = data.frame(city_id = as.character(),
                                     year =  as.numeric(),
                                     indicator_name = as.character(),
                                     indicator_number = as.character(),
                                     value = as.numeric(),
                                     score = as.numeric())



# function to compute I1
compute.i1 = function(city_id,city_worldcover_stat,year){
  
  natural_areas_classes = c('Trees','Shrubland','Grassland','Snow/ice','Open water','Herbaceous wetland',
                            'Mangroves','Moss/lichen')
  
  biodiversity_indicators = city_worldcover_stat %>% 
    mutate(natural_areas_class =
             case_when(land_cover_class %in% natural_areas_classes ~ "Natural areas", 
                       !land_cover_class %in% natural_areas_classes ~ "Non Natural areas")
    ) %>% 
    group_by(natural_areas_class) %>% 
    summarise(natural_areas_percent = sum(land_percent)) %>% 
    filter(natural_areas_class == "Natural areas") %>% 
    add_column(city_id = city_id,
               year = year,
               indicator_name = "Proportion of natural areas",
               indicator_number = "I1") %>% 
    rename(value = natural_areas_percent) %>% 
    mutate(score =
             case_when(value <  1 ~ "0", 
                       value <=  6.9 ~ "1", 
                       value <=  13.9 ~ "2",
                       value <=  20 ~ "3",
                       value >  20 ~ "4")) %>% 
    dplyr::select(city_id,
                  year,
                  indicator_name,
                  indicator_number,
                  value,
                  score)
  
  return(biodiversity_indicators)
}


# compute I1 for urbanshift cities

for(i in 1:length(urbanshift_cities)){
  
  print(i)
  
  city_id = urbanshift_cities[i]
  print(city_id)
  
  city_worldcover_stat = cities_worldcover_stat[cities_worldcover_stat$city_id == city_id, ]
  
  biodiversity_inidcators_I1 = compute.i1(city_id = city_id,
                                          city_worldcover_stat = city_worldcover_stat,
                                          year = 2020)
  
  biodiversity_indicators = rbind(biodiversity_indicators,
                                  biodiversity_inidcators_I1)
  
}

# store outputs
write.csv(biodiversity_indicators,
          "./data/biodiversity_indicators.csv",
          row.names = FALSE)


# compute I1 - municiplity level----

natural_areas_classes = c('Trees','Shrubland','Grassland','Snow/ice','Open water','Herbaceous wetland',
                          'Mangroves','Moss/lichen')

municipalities_I1 = municipalities_worldcover_stat %>% 
  mutate(natural_areas_class =
           case_when(land_cover_class %in% natural_areas_classes ~ "Natural areas", 
                     !land_cover_class %in% natural_areas_classes ~ "Non Natural areas")
  ) %>% 
  group_by(natural_areas_class,city_id) %>% 
  summarise(natural_areas_percent = sum(land_percent)) %>% 
  filter(natural_areas_class == "Natural areas") %>% 
  add_column(year = year,
             indicator_name = "Proportion of natural areas",
             indicator_number = "I1") %>% 
  rename(value = natural_areas_percent) %>% 
  mutate(score =
           case_when(value <  1 ~ "0", 
                     value <=  6.9 ~ "1", 
                     value <=  13.9 ~ "2",
                     value <=  20 ~ "3",
                     value >  20 ~ "4")) %>% 
  dplyr::select(city_id,
                year,
                indicator_name,
                indicator_number,
                value,
                score)

municipalities_I1

# store outputs
write.csv(municipalities_I1,
          "./data/biodiversity_indicators_municipality.csv",
          row.names = FALSE)


# plot 

municipalities_I1 = read.csv("./data/biodiversity_indicators_municipality.csv")
boundary_municipality = st_read("./data/boundaries/boundaries-CRI-San_Jose-ADM2.geojson",
                                quiet = TRUE)

municipalities_I1_geo = left_join(boundary_municipality,
                                         municipalities_I1,
                                         by = c("shapeName.1" = "city_id"))


# define color palette for I1 levels
pal_value <- colorNumeric(palette = "Greens", 
                       domain = municipalities_I1_geo$value,
                       na.color = "transparent",
                       revers = FALSE)

# define color palette for S1 levels
pal_score <- colorFactor(palette = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"), 
                         levels = c("0","1","2","3","4"),
                         na.color = "transparent",
                         revers = TRUE)

# define labels

labels_I1 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                     municipalities_I1_geo$shapeName.1,
                     "Proportion of natural areas",
                     round(municipalities_I1_geo$value, 2), "",
                     "Score",municipalities_I1_geo$score) %>% 
  lapply(htmltools::HTML)



# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = municipalities_I1_geo,
              group = "Porportion of natural areas - value",
              fillColor = ~pal_value(value),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_I1,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_value,
            values = municipalities_I1_geo$value,
            opacity = 0.9,
            title = "% natural areas (2020)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # I1 score layer
  addPolygons(data = municipalities_I1_geo,
              group = "Porportion of natural areas - score",
              fillColor = ~pal_score(score),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 1,
              label = labels_I1,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  # I1 score legend
  addLegend(colors =  c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 20.0%)",
                       "3 (14.0% – 20.0%)",
                       "2 (7.0% – 13.9%)",
                       "1 (0% – 6.9%)",
                       "0 (< 1.0%)"),
            opacity = 1,
            title = "Natural areas scores (2020)",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("Porportion of natural areas - value","Porportion of natural areas - score"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

# plot chart
# prepare chart colors

value_vector = municipalities_I1_geo %>% 
  drop_na(score) %>% 
  arrange(score) %>% 
  pull(score) %>% 
  as.numeric()

color_vector = pal_score(value_vector)

# plot chart
municipalities_I1_geo %>% 
  arrange(desc(value)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(shapeName.1), 
            y = ~value, 
            marker = list(color = color_vector),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Percent of natural areas (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray =
                        ~value),
         yaxis = list(title = ''))
