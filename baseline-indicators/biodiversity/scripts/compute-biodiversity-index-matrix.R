
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
library(formattable)
library(reactablefmtr)


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
boundary = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/CRI-San_Jose-boundary.geojson", quiet = TRUE)

boundary_municipality = st_read("./data/biodiversity/boundaries-CRI-San_Jose-ADM2.geojson",
                                quiet = TRUE)


biodiversity_baseline_indicators <- read_excel("./data/biodiversity baseline indicators.xlsx", sheet = "SCORES")

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
         I13_proximity_parks_score = 'I-13 score') %>% 
  mutate_at("I1_percent_natural_areas_score", as.character) %>% 
  mutate_at("I2_connectivity_score", as.character) %>% 
  mutate_at("I3_percent_birds_built_area_score", as.character)%>% 
  mutate_at("I8_percent_protected_area_score", as.character) %>% 
  mutate_at("I10_water_score", as.character) %>% 
  mutate_at("I12_recreational_services_score", as.character) %>% 
  mutate_at("I13_proximity_parks_score", as.character)

# plot matrix



biodiversity_baseline_scores = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  drop_na(I1_percent_natural_areas_value) %>% 
  mutate(I1_percent_natural_areas_score = as.numeric(I1_percent_natural_areas_score),
         I2_connectivity_score = as.numeric(I2_connectivity_score),
         I3_percent_birds_built_area_score = as.numeric(I3_percent_birds_built_area_score),
         I8_percent_protected_area_score = as.numeric(I8_percent_protected_area_score),
         I10_water_score = as.numeric(I10_water_score),
         I12_recreational_services_score = as.numeric(I12_recreational_services_score),
         I13_proximity_parks_score = as.numeric(I13_proximity_parks_score)) %>% 
  dplyr::select(Municipality,
                "I1 - Percent of natural areas" = I1_percent_natural_areas_score,
                "I2 - Connectivity" = I2_connectivity_score,
                "I3 - Birds in built areas" = I3_percent_birds_built_area_score,
                "I8 - Protected areas" = I8_percent_protected_area_score,
                "I10 - Water" = I10_water_score,
                "I12 - Recreational services" = I12_recreational_services_score,
                "I13 - Proximity to parks" = I13_proximity_parks_score) %>% 
  mutate("Biodiversity Index" = rowSums(.[2:8])) %>% 
  mutate_at(vars("I1 - Percent of natural areas":"I13 - Proximity to parks"), ~ cell_spec(
    ., "html", 
    background = ifelse(. >= 4, "green", ifelse(. >= 3, "yellowgreen", ifelse(. >= 2, "yellow", ifelse(. >= 1, "orange", "red"))))
  )) %>% 
  mutate_at(vars("Biodiversity Index"), ~ cell_spec(
    ., "html", 
    font_size = "x-large",
    background = ifelse(. >= 28, "green", ifelse(. >= 21, "yellowgreen", ifelse(. >= 14, "yellow", ifelse(. >= 7, "orange", "red"))))
  ))

biodiversity_baseline_scores %>%
  kable(format = "html", escape = FALSE) %>%
  kable_styling("striped", full_width = FALSE) %>% 
  scroll_box(width = "100%", height = "700px")



# map

biodiversity_baseline_scores = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  drop_na(I1_percent_natural_areas_value) %>% 
  mutate(I1_percent_natural_areas_score = as.numeric(I1_percent_natural_areas_score),
         I2_connectivity_score = as.numeric(I2_connectivity_score),
         I3_percent_birds_built_area_score = as.numeric(I3_percent_birds_built_area_score),
         I8_percent_protected_area_score = as.numeric(I8_percent_protected_area_score),
         I10_water_score = as.numeric(I10_water_score),
         I12_recreational_services_score = as.numeric(I12_recreational_services_score),
         I13_proximity_parks_score = as.numeric(I13_proximity_parks_score)) %>% 
  dplyr::select(Municipality,
                "I1 - Percent of natural areas" = I1_percent_natural_areas_score,
                "I2 - Connectivity" = I2_connectivity_score,
                "I3 - Birds in built areas" = I3_percent_birds_built_area_score,
                "I8 - Protected areas" = I8_percent_protected_area_score,
                "I10 - Water" = I10_water_score,
                "I12 - Recreational services" = I12_recreational_services_score,
                "I13 - Proximity to parks" = I13_proximity_parks_score) %>% 
  mutate("Biodiversity Index" = rowSums(.[2:8]))

# join with geo
biodiversity_baseline_scores_geo = boundary_municipality %>% 
  left_join(biodiversity_baseline_scores,
            by = c("Municipality"))


# define color palette for I1 levels
pal_Index <- colorNumeric(palette = "RdYlGn", 
                        domain = biodiversity_baseline_scores_geo$`Biodiversity Index` ,
                        na.color = "transparent",
                        revers = FALSE)

# define labels

labels_Index <- sprintf("<strong>%s</strong><br/>%s: %s",
                      biodiversity_baseline_scores_geo$shapeName.1,
                      "Biodiversity index",
                      biodiversity_baseline_scores_geo$`Biodiversity Index`) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addPolygons(data = biodiversity_baseline_scores_geo,
              group = "Biodiversity index",
              fillColor = ~pal_Index(`Biodiversity Index`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_Index,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_Index,
            values = biodiversity_baseline_scores_geo$`Biodiversity Index`,
            opacity = 0.9,
            title = "Biodiversity index (2020)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Biodiversity index"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

# plot map -----

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
                     "Score",biodiversity_baseline_indicators_geo$I1_percent_natural_areas_score) %>% 
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
              fillColor = ~pal_score(I1_percent_natural_areas_score),
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
  addLegend(colors =  c("green","yellowgreen","yellow","orange","red"),
            labels = c("4 (> 20.0%)",
                       "3 (14.0% – 20.0%)",
                       "2 (7.0% – 13.9%)",
                       "1 (0% – 6.9%)",
                       "0 (< 1.0%)"),
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