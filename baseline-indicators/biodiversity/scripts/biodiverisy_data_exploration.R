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



# read Boundary ----

boundary = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/CRI-San_Jose-boundary.geojson",
                    quiet = TRUE)

boundary_centroid = st_centroid(boundary)
boundary_centroid_lat = boundary_centroid$geometry[[1]][2]
boundary_centroid_lon = boundary_centroid$geometry[[1]][1]

# Species count map at metropolitan level ----

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
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>% 
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
  addMarkers(birds2020$lon, 
             birds2020$lat,
             popup = ~as.character(birds2020$family),
             label = label_birds2020,
             clusterOptions = markerClusterOptions(),
             group = "Birds clusters") %>% 
  # add birds layer
  addCircleMarkers(birds2020$lon, birds2020$lat, 
                   radius = 3,
                   fillColor = col_BoldBrickOrange,
                   color  = "black",
                   stroke = TRUE,
                   weight = 0.8,
                   fillOpacity = 0.6,
                   popup = ~as.character(birds2020$family),
                   label = label_birds2020,
                   group = "Birds observations") %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Birds clusters","Birds observations"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
    hideGroup("Birds observations")



# statistics at metropolitan level ----

# orders ----

birds2020_stats_orders = birds2020 %>% 
  as.data.frame() %>% 
  group_by(order) %>%
  summarise(nb_species = n()) %>% 
  arrange(desc(nb_species)) %>% 
  dplyr::select(`order name` = order,
         `Number of species`= nb_species)

# plot chart
birds2020_stats_orders %>% 
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(`order name`), 
            y = ~`Number of species`, 
            marker = list(color = col_BoldRiverBlue),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Number of birds by species (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~`Number of species`),
         yaxis = list(title = 'Number of observations in 2020'))

# families ----

birds2020_stats_families = birds2020 %>% 
  as.data.frame() %>% 
  group_by(family) %>%
  summarise(nb_species = n()) %>% 
  arrange(desc(nb_species)) %>% 
  dplyr::select(`family name` = family,
                `Number of species`= nb_species)

# plot chart
birds2020_stats_families %>% 
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(`family name`), 
            y = ~`Number of species`, 
            marker = list(color = col_BoldRiverBlue),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Number of birds by species (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~`Number of species`),
         yaxis = list(title = 'Number of observations in 2020'))
        

# genus ----

birds2020_stats_genus = birds2020 %>% 
  as.data.frame() %>% 
  group_by(genus) %>%
  summarise(nb_species = n()) %>% 
  arrange(desc(nb_species)) %>% 
  dplyr::select(`genus name` = genus,
                `Number of species`= nb_species)

# plot table
birds2020_stats_genus %>% 
  arrange(desc(`Number of species`)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "400px")

# Species count map at municipality level ----

# read municipality boundaries ----

boundary_municipality = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/boundaries-CRI-San_Jose-municipality.geojson",
                                quiet = TRUE)

boundary_municipality = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/boundaries-CRI-San_Jose-ADM2.geojson",
                                quiet = TRUE)


leaflet(data = boundary, height = 500, width = "100%") %>% 
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>% 
  # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  setView(lat = boundary_centroid_lat,
          lng = boundary_centroid_lon,
          zoom = 9) %>% 
  # Add boundaries: metropolitan
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
  # Add boundaries: Municipality
  addPolygons(data = boundary_municipality,
              group = "Municipality boundaries",
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
                   label = label_birds2020,
                   group = "Birds observations") %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Birds observations","Municipality boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  ) 



# find birds observations within municipalities polygons
birds_in_geo_adm2 <- st_join(x = birds2020, 
                             y = boundary_municipality, 
                             join = st_within)

# agregate number of birds observations by municipality 
birds_in_geo_adm2_stat = birds_in_geo_adm2 %>% 
  as.data.frame() %>% 
  count(shapeName.1) %>% 
  dplyr::select(shapeName.1,
         nb_species = n)  
  
# join with geo
birds_adm2_stat_geo = boundary_municipality %>% 
  left_join(birds_in_geo_adm2_stat,
            by = c("shapeName.1"))

# prepare map

# define color palette for urban expansion levels
pal_nbspecieis_municipality <- colorNumeric(palette = "Reds", 
                                             domain = birds_adm2_stat_geo$nb_species,
                                             na.color = "transparent",
                                             revers = FALSE)

# define map labels
labels_adm2_nbspecies <- sprintf("<strong>%s</strong><br/>%s: %s %s",
                  birds_adm2_stat_geo$shapeName.1, 
                  "Observations: ",
                  round(birds_adm2_stat_geo$nb_species, 2), "") %>% 
  lapply(htmltools::HTML)


# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = birds_adm2_stat_geo,
              group = "Nb species",
              fillColor = ~pal_nbspecieis_municipality(nb_species),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_adm2_nbspecies,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_nbspecieis_municipality, 
            values = birds_adm2_stat_geo$nb_species, 
            opacity = 0.9, 
            title = "Number of species (2020)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # add birds layer
  addCircleMarkers(birds2020$lon, birds2020$lat,
                   radius = 3,
                   fillColor = col_BoldRiverBlue,
                   color  = "black",
                   stroke = TRUE,
                   weight = 0.8,
                   fillOpacity = 0.6,
                   label = label_birds2020,
                   group = "Birds observations") %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM"),
    overlayGroups = c("Nb species", "Birds observations"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Birds observations")


# plot chart
birds_in_geo_adm2_stat %>% 
  arrange(desc(nb_species)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(shapeName.1), 
            y = ~nb_species, 
            marker = list(color = col_BoldRiverBlue),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Number of birds by species (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~nb_species),
         yaxis = list(title = 'Number of observations in 2020'))


# protected areas map -----

protected_areas_sanjose = st_read("./data/biodiversity/data/biodiversity_map_layers_protected_areas_sanjose.geojson",
                                quiet = TRUE)

protected_areas_costarica = st_read("./data/biodiversity/data/biodiversity_map_layers_protected_areas_costarica.geojson",
                                  quiet = TRUE)

protected_areas_costarica = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/biodiversity_map_layers_protected_areas_sanjose.geojson",
                                    quiet = TRUE)

leaflet(data = boundary, height = 500, width = "100%") %>% 
  addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>% 
  # Add boundaries: metropolitan
  addPolygons(data = boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = col_Black, weight = 3,dashArray = "3",
              smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = FALSE)) %>%
  # Add boundaries: Municipality
  addPolygons(data = boundary_municipality,
              group = "Municipality boundaries",
              stroke = TRUE, color = "gray", weight = 3,dashArray = "3",
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = FALSE)) %>%
  # add protected areas layer
  addPolygons(data = protected_areas_sanjose,
              group = "Protected Areas",
              stroke = TRUE, color = col_BoldGrassGreen, weight = 3,dashArray = "3",
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 5,
                color = col_LightGrassGreen,
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = FALSE)) %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("Toner Lite","OSM","CartoDB"),
    overlayGroups = c("Municipality boundaries", "Protected Areas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Municipality boundaries")

# percent of protected areas by municipality ----

protectedarea_percent_by_municipality = read.csv("./data/biodiversity/data/protectedarea_percent_by_municipality.csv")

protectedarea_percent_by_municipality = read.csv("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/protectedarea_percent_by_municipality.csv")

# rename columns
protectedarea_percent_by_municipality = protectedarea_percent_by_municipality %>% 
  rename_with(.cols = 1, ~'municpality_name') %>% 
  rename_with(.cols = 2, ~'percent_protected_area')

# plot chart
protectedarea_percent_by_municipality %>% 
  arrange(desc(percent_protected_area)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(municpality_name), 
            y = ~percent_protected_area, 
            marker = list(color = col_BoldRiverBlue),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Percent of protected area by municipality (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~percent_protected_area),
         yaxis = list(title = 'Percent of protected area (%)'))
  


# world cover ----

worldcoverc_data_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/indicators_biodiversity_CRI-San_Jose-esa-worldcover-2020.tif"

city_worldcover_raster= raster(worldcoverc_data_path)

# mask raster based on administrative boundaries
city_worldcover  = raster::mask(city_worldcover_raster,boundary)

# plot map

# define colors for each class

Trees_10 = "#006400"
Shrubland_20 = "#ffbb22"
Grassland_30 = "#ffff4c" 
Cropland_40 = "#f096ff"
Built_up_50 = "#fa0000"
Barren_sparse_vegetation_60 = "#b4b4b4"
Snow_ice_70 = "#f0f0f0"
Open_Water_80 = "#0064c8"
Herbaceous_wetland_90 = "#0096a0"
Mangroves_95 = "#00cf75"
Moss_lichen_100 = "#fae6a0"

# define color vector
worldcover_col = c(Trees_10,
                   Shrubland_20,
                   Grassland_30,
                   Cropland_40,
                   Built_up_50,
                   Barren_sparse_vegetation_60,
                   Snow_ice_70,
                   Open_Water_80,
                   Herbaceous_wetland_90,
                   Mangroves_95,
                   Moss_lichen_100)

# define a color palette
pal_worldcover <- colorFactor(worldcover_col, 
                             values(city_worldcover),
                             na.color = "transparent")
# define labels
labels_worldcover = c('Trees','Shrubland','Grassland','Cropland','	Built-up',
                     'Barren / sparse vegetation','Snow and ice','Open water','Herbaceous wetland',
                     'Mangroves','Moss and lichen')


# create the map
map = leaflet(city_worldcover, height = 500, width = "100%")  %>%
  addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
  addPolygons(data = boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "gray", weight = 1,dashArray = "3",
              smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = boundary$name,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addRasterImage(city_worldcover, 
                 colors = pal_worldcover, 
                 opacity = 0.7,
                 maxBytes = 20 * 1024 * 1024,
                 group = "World Cover") %>%
  addLegend(pal = pal_worldcover,
            values = values(city_worldcover),
            title = "World Cover") %>% 
  # addLegend(colors = worldcover_col,
  #           labels = labels_worldcover,
  #           title = "World Cover") %>% 
  addLayersControl(
    baseGroups = c("Toner Lite", "OSM","CartoDB"),
    overlayGroups = c("World Cover","Administrative boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  )

map

# sub-city

boundary_municipality_SanJose = boundary_municipality[boundary_municipality$shapeName.1 == "Cantón San José", ]


municipality_SanJose_worldcover  = raster::crop(city_worldcover_raster,boundary_municipality_SanJose)
municipality_SanJose_worldcover  = raster::mask(municipality_SanJose_worldcover,boundary_municipality_SanJose)

unique(values(municipality_SanJose_worldcover))

# define colors for each class

Trees_10_green = "#006400"
Shrubland_20_orange = "#ffbb22"
Grassland_30_yellow = "#ffff4c" 
Cropland_40_mauve = "#f096ff"
Built_up_50_red = "#fa0000"
Barren_sparse_vegetation_60_gray = "#b4b4b4"
Snow_ice_70_white = "#f0f0f0"
Open_Water_80_blue = "#0064c8"
Herbaceous_wetland_90_blue2 = "#0096a0"
Mangroves_95_green2 = "#00cf75"
Moss_lichen_100_beige = "#fae6a0"

# define color vector
worldcover_col = c(Trees_10_green,
                   Shrubland_20_orange,
                   Grassland_30_yellow,
                   Cropland_40_mauve,
                   Built_up_50_red,
                   Barren_sparse_vegetation_60_gray,
                   Open_Water_80_blue)

# define a color palette
pal_worldcover <- colorFactor(worldcover_col, 
                              values(municipality_SanJose_worldcover),
                              na.color = "transparent")
# define labels
labels_worldcover = c('Trees','Shrubland','Grassland','Cropland','	Built-up',
                      'Barren / sparse vegetation','Open water')

# create the map
map = leaflet(municipality_SanJose_worldcover, height = 500, width = "100%")  %>%
  addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
  addPolygons(data = boundary_municipality_SanJose,
              group = "Administrative boundaries",
              stroke = TRUE, color = "gray", weight = 3,dashArray = "3",
              smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = boundary_municipality_SanJose$name,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addRasterImage(municipality_SanJose_worldcover, 
                 colors = pal_worldcover, 
                 opacity = 0.7,
                 maxBytes = 20 * 1024 * 1024,
                 group = "World Cover") %>%
  # addLegend(pal = pal_worldcover,
  #           values = values(municipality_SanJose_worldcover),
  #           title = "World Cover") %>% 
  addLegend(colors = worldcover_col,
            labels = labels_worldcover,
            title = "World Cover") %>%
  addLayersControl(
    baseGroups = c("Toner Lite", "OSM","CartoDB"),
    overlayGroups = c("World Cover","Administrative boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  )

map

year = 2020

city_worldcover_stat = as.data.frame(city_worldcover_raster) %>%
  rename_at(1, ~"class" ) %>% 
  drop_na(class) %>% 
  group_by(class) %>%
  tally() %>%
  mutate(area = n * res(city_worldcover_raster)[1] * res(city_worldcover_raster)[2]) %>% 
  dplyr::select(land_cover_classes = class,
                nb_cells = n) %>% 
  mutate(land_cover_percent = round(nb_cells/sum(nb_cells) * 100,2))%>% 
  add_column(year = year) %>% 
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
  dplyr::select('land cover class' = land_cover_classes,
                'land percent' = land_cover_percent,
                'year' = year)


# plot table
city_worldcover_stat %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")


###############################################################
# Baseline indicators
###############################################################
library(readxl)

biodiversity_baseline_indicators <- read_excel("./data/biodiversity/biodiversity baseline indicators.xlsx", sheet = "SCORES")

boundary_municipality$Municipality = str_remove(boundary_municipality$shapeName.1, "Cantón ")
# change encoding
boundary_municipality$Municipality = iconv(boundary_municipality$Municipality,from="UTF-8",to="ASCII//TRANSLIT")

# join with geo
biodiversity_baseline_indicators_geo = boundary_municipality %>% 
  left_join(biodiversity_baseline_indicators,
            by = c("Municipality")) %>% 
  rename(I1_percent_natural_areas_value = 'I-1 raw',
         I1_percent_natural_areas_score = 'I-1 score',
         I2_connectivity_value = 'I-2 raw',
         I2_connectivity_score = 'I-2 score',
         I3_percent_birds_built_area_value = 'I-3 raw',
         I3_percent_birds_built_area_score = 'I-3 score',
         I8_percent_protected_area_value = 'I-8 raw',
         I8_percent_protected_area_score = 'I-8 score',
         I10_water_value = 'I-10 raw',
         I10_water_score = 'I-10 score',
         I12_recreational_services_value = 'I-12 raw',
         I12_recreational_services_score = 'I-12 score',
         I13_proximity_parks_value = 'I-13 raw',
         I13_proximity_parkss_score = 'I-13 score') %>% 
  mutate_at("I1_percent_natural_areas_score", as.character) %>% 
  mutate_at("I2_connectivity_score", as.character) %>% 
  mutate_at("I3_percent_birds_built_area_score", as.character)%>% 
  mutate_at("I8_percent_protected_area_score", as.character) %>% 
  mutate_at("I10_water_score", as.character) %>% 
  mutate_at("I12_recreational_services_score", as.character) %>% 
  mutate_at("I13_proximity_parkss_score", as.character)

# I-1. Porportion of natural areas ----

# prepare map

# define color palette for I1 levels
pal_I1 <- colorNumeric(palette = "Greens", 
                       domain = biodiversity_baseline_indicators_geo$I1_percent_natural_areas_value,
                       na.color = "transparent",
                       revers = FALSE)

# define color palette for S1 levels
pal_score <- colorFactor(palette = c("green","yellowgreen","yellow","orange","red"), 
                         levels = c("0","1","2","3","4"),
                         na.color = "transparent",
                         revers = TRUE)


# define labels

labels_I1 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                     biodiversity_baseline_indicators_geo$shapeName.1,
                     "Proportion of natural areas",
                     round(biodiversity_baseline_indicators_geo$I1_percent_natural_areas_value, 2), "",
                     "Scores",biodiversity_baseline_indicators_geo$I1_percent_natural_areas_score) %>% 
  lapply(htmltools::HTML)



# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Porportion of natural areas",
              fillColor = ~pal_I1(I1_percent_natural_areas_value),
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
  addLegend(pal = pal_I1,
            values = biodiversity_baseline_indicators_geo$I1_percent_natural_areas_value,
            opacity = 0.9,
            title = "% natural areas (2020)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # I1 score layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Porportion of natural areas Score",
              fillColor = ~pal_S1(I1_percent_natural_areas_score),
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
  # I1 score legend
  addLegend(colors = c("green","yellow","orange","red"),
            labels = c("1","2","3","4"),
            opacity = 0.9,
            title = "Natural areas scores (2020)",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM"),
    overlayGroups = c("Porportion of natural areas","Porportion of natural areas Score"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Porportion of natural areas Score")

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(I1_percent_natural_areas_value)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(Municipality), 
            y = ~I1_percent_natural_areas_value, 
            marker = list(color = col_BoldGrassGreen),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Percent of natural areas (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~I1_percent_natural_areas_value),
         yaxis = list(title = 'Percent of natiral area (%)'))


# I-2. Connectivity -----

# define color palette for I1 levels
pal_I2 <- colorNumeric(palette = "Greens", 
                       domain = biodiversity_baseline_indicators_geo$I2_connectivity_value,
                       na.color = "transparent",
                       revers = FALSE)

# define labels

labels_I2 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                     biodiversity_baseline_indicators_geo$shapeName.1,
                     "Connectivity value",
                     round(biodiversity_baseline_indicators_geo$I2_connectivity_value, 2), "",
                     "Connectivity score",biodiversity_baseline_indicators_geo$I2_connectivity_score) %>% 
  lapply(htmltools::HTML)



# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Connectivity value",
              fillColor = ~pal_I2(I2_connectivity_value),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_I2,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_I2,
            values = biodiversity_baseline_indicators_geo$I2_connectivity_value,
            opacity = 0.9,
            title = "Connectivity value (2020)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # I2 score layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Connectivity Score",
              fillColor = ~pal_S1(I2_connectivity_score),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 1,
              label = labels_I2,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  # I1 score legend
  addLegend(colors = c("green","yellowgreen","yellow","orange","red"),
            labels = c("0","1","2","3","4"),
            opacity = 0.9,
            title = "Connectivity score (2020)",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM"),
    overlayGroups = c("Connectivity Value","Connectivity Score"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Connectivity Score")

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(I2_connectivity_value)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(Municipality), 
            y = ~I2_connectivity_value, 
            marker = list(color = col_BoldGrassGreen),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Connectivity measures (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~I2_connectivity_value),
         yaxis = list(title = 'Percent of natiral area (%)'))

### I-3 Native biodiversity in built-up areas (bird species) ----

# define color palette for I1 levels
pal_I3 <- colorNumeric(palette = "Greens", 
                       domain = biodiversity_baseline_indicators_geo$I3_percent_birds_built_area_value,
                       na.color = "transparent",
                       revers = FALSE)

# define labels

labels_I3 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                     biodiversity_baseline_indicators_geo$shapeName.1,
                     "Native biodiversity in built-up areas",
                     round(biodiversity_baseline_indicators_geo$I3_percent_birds_built_area_value, 2), "",
                     "Score",biodiversity_baseline_indicators_geo$I3_percent_birds_built_area_score) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Native biodiversity in buitl-up areas value",
              fillColor = ~pal_I3(I3_percent_birds_built_area_value),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_I3,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_I3,
            values = biodiversity_baseline_indicators_geo$I3_percent_birds_built_area_value,
            opacity = 0.9,
            title = "Native biodiversity in buitl-up areas value (2020)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # I3 score layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Native biodiversity in buitl-up areas score",
              fillColor = ~pal_score(I3_percent_birds_built_area_score),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 1,
              label = labels_I3,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  # I1 score legend
  addLegend(colors = c("green","yellowgreen","yellow","orange","red"),
            labels = c("0","1","2","3","4"),
            opacity = 0.9,
            title = "Native biodiversity in buitl-up areas score (2020)",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM"),
    overlayGroups = c("Native biodiversity in buitl-up areas value","Native biodiversity in buitl-up areas score"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Native biodiversity in buitl-up areas score")

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(I3_percent_birds_built_area_value)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(Municipality), 
            y = ~I3_percent_birds_built_area_value, 
            marker = list(color = col_BoldGrassGreen),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Native biodiversity in buitl-up areas (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~I3_percent_birds_built_area_value),
         yaxis = list(title = 'Percent of birds in built-up areas (%)'))

### I-8 Proportion of protected natural areas ----

# define color palette for I1 levels
pal_I8 <- colorNumeric(palette = "Greens", 
                       domain = biodiversity_baseline_indicators_geo$I8_percent_protected_area_value ,
                       na.color = "transparent",
                       revers = FALSE)

# define labels

labels_I8 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                     biodiversity_baseline_indicators_geo$shapeName.1,
                     "Protected areas",
                     round(biodiversity_baseline_indicators_geo$I8_percent_protected_area_value, 2), "%",
                     "Score",biodiversity_baseline_indicators_geo$I8_percent_protected_area_score) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Protected area value",
              fillColor = ~pal_I8(I8_percent_protected_area_value),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_I8,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_I8,
            values = biodiversity_baseline_indicators_geo$I8_percent_protected_area_value,
            opacity = 0.9,
            title = "% protected areas (2020)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # I8 score layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Protected area score",
              fillColor = ~pal_score(I8_percent_protected_area_score),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 1,
              label = labels_I8,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  # I1 score legend
  addLegend(colors = c("green","yellowgreen","yellow","orange","red"),
            labels = c("0","1","2","3","4"),
            opacity = 0.9,
            title = "Protected area score (2020)",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM"),
    overlayGroups = c("Protected area value","Protected area score"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Protected area score")

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(I8_percent_protected_area_value)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(Municipality), 
            y = ~I8_percent_protected_area_value, 
            marker = list(color = col_BoldGrassGreen),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Percent of protected areas (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~I8_percent_protected_area_value),
         yaxis = list(title = 'Percent of protected areas (%)'))




### I-10 Regulation of quantity of water ----

# define color palette for I1 levels
pal_I10 <- colorNumeric(palette = "Greens", 
                       domain = biodiversity_baseline_indicators_geo$I10_water_value ,
                       na.color = "transparent",
                       revers = FALSE)

# define labels

labels_I10 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                     biodiversity_baseline_indicators_geo$shapeName.1,
                     "Percent of permeable area",
                     round(biodiversity_baseline_indicators_geo$I10_water_value, 2), "%",
                     "Score",biodiversity_baseline_indicators_geo$I10_water_score) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Permeable area value",
              fillColor = ~pal_I10(I10_water_value),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_I10,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_I10,
            values = biodiversity_baseline_indicators_geo$I10_water_value,
            opacity = 0.9,
            title = "% permeable areas (2020)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # I8 score layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Permeable area score",
              fillColor = ~pal_score(I10_water_score),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 1,
              label = labels_I8,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  # I1 score legend
  addLegend(colors = c("green","yellowgreen","yellow","orange","red"),
            labels = c("0","1","2","3","4"),
            opacity = 0.9,
            title = "Permeable area score (2020)",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM"),
    overlayGroups = c("Permeable area value","Permeable area score"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Permeable area score")

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(I10_water_value)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(Municipality), 
            y = ~I10_water_value, 
            marker = list(color = col_BoldGrassGreen),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Regulation of quantity of water (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~I10_water_value),
         yaxis = list(title = 'Percent of permeable areas (%)'))


### I-12 Recreational services ----

# define color palette for I1 levels
pal_I12 <- colorNumeric(palette = "Greens", 
                        domain = biodiversity_baseline_indicators_geo$I12_recreational_services_value ,
                        na.color = "transparent",
                        revers = FALSE)

# define labels

labels_I12 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                      biodiversity_baseline_indicators_geo$shapeName.1,
                      "Recreational services",
                      round(biodiversity_baseline_indicators_geo$I12_recreational_services_value, 2), "%",
                      "Score",biodiversity_baseline_indicators_geo$I12_recreational_services_score) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Recreational services value",
              fillColor = ~pal_I12(I12_recreational_services_value),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_I12,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_I12,
            values = biodiversity_baseline_indicators_geo$I12_recreational_services_value,
            opacity = 0.9,
            title = "Recreational services (2020)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # I8 score layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Recreational services score",
              fillColor = ~pal_score(I12_recreational_services_score),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 1,
              label = labels_I12,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  # I1 score legend
  addLegend(colors = c("green","yellowgreen","yellow","orange","red"),
            labels = c("0","1","2","3","4"),
            opacity = 0.9,
            title = "Recreational services score (2020)",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM"),
    overlayGroups = c("Recreational services value","Recreational services score"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Recreational services score")

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(I12_recreational_services_value)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(Municipality), 
            y = ~I12_recreational_services_value, 
            marker = list(color = col_BoldGrassGreen),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Recreational services (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~I12_recreational_services_value),
         yaxis = list(title = 'Recreational services'))