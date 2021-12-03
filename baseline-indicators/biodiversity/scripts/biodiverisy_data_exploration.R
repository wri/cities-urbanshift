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

# Species count map at metropolitan level ----

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

# read municipality boundaries

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
  
