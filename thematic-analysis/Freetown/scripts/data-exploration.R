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

# administrative boundaries of freetown region -----

# define path
city_boundary_path = paste("https://storage.googleapis.com/urbanshift/freetown/boundaries/SLE-Freetown_region.geojson")

# read the data#
city_boundary <- st_read(city_boundary_path,
                         quiet = TRUE)

leaflet() %>%
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  # Add boundaries
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = col_BoldEarthGrey, weight = 1,dashArray = "3",
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = city_boundary$name,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Administrative boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  )


# classified maps ------

# Function to collect land classes by year

collect.land.class.by.year = function(year, city_boundary){
  
  # define path
  classified_map_path = paste("https://storage.googleapis.com/urbanshift/freetown/classified_maps/classified_map_",year,".tif",
                              sep = "")
  
  # collect raster data
  classified_map = raster(classified_map_path)
  
  # mask raster based on administrative boundaries
  classified_map_mask = raster::mask(classified_map,
                                     city_boundary)
  
  return(classified_map_mask)
  
}

# collect land classes by year

classified_map_2016 = collect.land.class.by.year(year ="2016", city_boundary = city_boundary)
classified_map_2017 = collect.land.class.by.year(year ="2017", city_boundary = city_boundary)
classified_map_2018 = collect.land.class.by.year(year ="2018", city_boundary = city_boundary)
classified_map_2019 = collect.land.class.by.year(year ="2019", city_boundary = city_boundary)
classified_map_2020 = collect.land.class.by.year(year ="2020", city_boundary = city_boundary)
classified_map_2021 = collect.land.class.by.year(year ="2021", city_boundary = city_boundary)

## color map
# define color vector
classified_map_col = c("green","blue")

# define a color palette
pal_classified_map <- colorFactor(classified_map_col, 
                                  values(classified_map_2016),
                                  na.color = "transparent")
# define labels
labels_classified_map = c('Non Urban','Urban')


# create the map
map_city_classification = leaflet(classified_map_2016, height = 500, width = "100%")  %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "gray", weight = 2,dashArray = "3",
              smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = city_boundary$name,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addRasterImage(classified_map_2016, 
                 colors = pal_classified_map, 
                 opacity = 0.7,
                 group = "Land classification 2016") %>%
  addRasterImage(classified_map_2021, 
                 colors = pal_classified_map, 
                 opacity = 0.7,
                 group = "Land classification 2021") %>%
  addLegend(colors = classified_map_col,
            labels = labels_classified_map,
            title = "Land Cover") %>% 
  addLayersControl(
    baseGroups = c( "OSM","CartoDB", "Toner Lite"),
    overlayGroups = c("Land classification 2016",
                      "Land classification 2021",
                      "Administrative boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  )

# plot the map
map_city_classification

# Urban expansion Temporal ----

# function for computing land cover statistics
CityLandClassStat = function(raster, year){
  city_landcover_stat_df = as.data.frame(raster) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    group_by(class) %>%
    tally() %>%
    mutate(area = n * res(raster)[1] * res(raster)[2]) %>% 
    dplyr::select(land_use_classes = class,
                  nb_cells = n) %>% 
    mutate(land_use_percent = round(nb_cells/sum(nb_cells) * 100,2))%>% 
    add_column(year = year) %>% 
    arrange(desc(land_use_percent)) %>%
    mutate_at("land_use_classes", as.character) %>%
    mutate(land_use_classes = recode(land_use_classes,
                                     "0" =  "Non Urban",
                                     "1" = "Urban" ))
  
  return(city_landcover_stat_df)
  
}

# compute land cover statistics for every year

city_landclass_stat_2016 = CityLandClassStat(raster = classified_map_2016, year = "2016")
# city_landclass_stat_2017 = CityLandClassStat(raster = classified_map_2017, year = "2017")
# city_landclass_stat_2018 = CityLandClassStat(raster = classified_map_2018, year = "2018")
# city_landclass_stat_2019 = CityLandClassStat(raster = classified_map_2019, year = "2019")
# city_landclass_stat_2020 = CityLandClassStat(raster = classified_map_2020, year = "2020")
city_landclass_stat_2021 = CityLandClassStat(raster = classified_map_2021, year = "2021")

city_landclass_stat = bind_rows(list(city_landclass_stat_2016,
                                     # city_landclass_stat_2017,
                                     # city_landclass_stat_2018,
                                     # city_landclass_stat_2019,
                                     # city_landclass_stat_2020,
                                     city_landclass_stat_2021))

# plot table
city_landclass_stat %>% 
  dplyr::select(year,land_use_classes,land_use_percent) %>% 
  pivot_wider(names_from = land_use_classes, 
              values_from = land_use_percent) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")

# plot chart
city_landclass_stat %>% 
  plot_ly() %>% 
  add_trace(x = ~year, 
            y = ~land_use_percent, 
            color = ~factor(land_use_classes),
            colors = c("green","blue"),
            type = "bar",
            text = ~paste("Land use: ", land_use_classes, '<br>Percent:', land_use_percent))  %>% 
  layout(yaxis = list(title = 'Percent of Urban/non urban area'), 
         xaxis = list(title = ''),
         barmode = 'stack',
         legend = list(orientation = 'h', x = 0.2, y = -0.5),
         height = 500, width = "100%")

# plot land cover evolution
city_landclass_stat %>% 
  dplyr::select(year,land_use_classes,land_use_percent) %>% 
  mutate_at("land_use_percent", as.numeric) %>%
  pivot_wider(names_from = land_use_classes, 
              values_from = land_use_percent) %>% 
  plot_ly(x= ~year, y=~Urban,name="Urban",type = 'scatter', 
          mode = 'none', stackgroup = 'one', groupnorm = 'percent', 
          fillcolor = "green") %>% 
  add_trace(y = ~`Non Urban`, name = 'Non Urban', fillcolor = "gray") %>%
  layout(title = 'Evolution percent of Urbanized area',
         xaxis = list(title = "",
                      showgrid = FALSE),
         yaxis = list(title = "Proportion from the Total Land Cover",
                      showgrid = FALSE,
                      ticksuffix = '%',
                      # range = list(0, 1),
                      fixedrange=FALSE),
         height = 500, width = "100%")

# Urban expansion Spatial ----

# compute raster difference
landcalss_diff = classified_map_2021 - classified_map_2016

# get urban expansion raster diff
landcalss_diff_urban = landcalss_diff
landcalss_diff_urban[landcalss_diff_urban < 0,] <- NA 


## color diff

# define color vector
class_diff_map_col = c("gray","red")

# define a color palette
pal_class_diff_map <- colorFactor(class_diff_map_col, 
                                  values(landcalss_diff_urban),
                                  na.color = "transparent")
# define labels
labels_class_diff_map = c('No Urban Expoansion','Urban Expansion')

# create the map
map_city_classification = leaflet(classified_map_2016, height = 500, width = "100%")  %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "gray", weight = 2,dashArray = "3",
              smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = city_boundary$name,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addRasterImage(classified_map_2016, 
                 colors = pal_classified_map, 
                 opacity = 0.7,
                 group = "Land classification 2016") %>%
  addRasterImage(classified_map_2021, 
                 colors = pal_classified_map, 
                 opacity = 0.7,
                 group = "Land classification 2021") %>%
  addRasterImage(landcalss_diff_urban,
                 colors = pal_class_diff_map,
                 opacity = 0.9,
                 group = "Urban Expansion 2016-2021") %>%
  addLegend(colors = classified_map_col,
            labels = labels_classified_map,
            title = "Land Cover") %>% 
  addLegend(colors = class_diff_map_col,
            labels = labels_class_diff_map,
            title = "Urban expansion",
            position = "bottomright") %>% 
  addLayersControl(
    baseGroups = c( "OSM","CartoDB", "Toner Lite"),
    overlayGroups = c("Land classification 2016",
                      "Land classification 2021",
                      "Urban Expansion 2016-2021",
                      "Administrative boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  )

# plot the map
map_city_classification


# Ward level ----

# define path
city_wards_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/thematic-analysis/freetown/data/freetown_wards.geojson")

# read the data
city_wards <- st_read(city_wards_path,
                      quiet = TRUE)

city_sub_boundary = city_wards %>% 
  rename(name = Ward_No)

# function for computing land cover statistics
CityLandClassStatSubBoundary = function(city_sub_boundary, classified_map, year){
  # get name
  name = city_sub_boundary$name
  # mask raster based on administrative boundaries
  classified_map_mask_i = raster::mask(classified_map,
                                       city_sub_boundary)
  city_landcover_stat_df = as.data.frame(classified_map_mask_i) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    group_by(class) %>%
    tally() %>%
    mutate(area = n * res(classified_map)[1] * res(classified_map)[2]) %>% 
    dplyr::select(land_use_classes = class,
                  nb_cells = n) %>% 
    mutate(land_use_percent = round(nb_cells/sum(nb_cells) * 100,2))%>% 
    add_column(year = year) %>% 
    add_column(name = name) %>%
    arrange(desc(land_use_percent)) %>%
    mutate_at("land_use_classes", as.character) %>%
    mutate(land_use_classes = recode(land_use_classes,
                                     "0" =  "Non Urban",
                                     "1" = "Urban" )) %>% 
    dplyr::select(name, year,land_use_classes,land_use_percent) %>% 
    pivot_wider(names_from = land_use_classes, 
                values_from = land_use_percent)
  
  return(city_landcover_stat_df)
  
}

# create empty dataframe
city_subregion_landclass_stat = data.frame(name = as.character(),
                                           year = as.character(),
                                           Urban = as.numeric(),
                                           'Non Urban' = as.numeric())

# compute urban area by ward

for(i in 1:nrow(city_sub_boundary)){
  print(i)
  
  city_sub_boundary_i = city_sub_boundary[i,]
  
  city_i_landclass_stat_2016 = CityLandClassStatSubBoundary(city_sub_boundary = city_sub_boundary_i,
                                                            classified_map = classified_map_2016, 
                                                            year = "2016")
  
  city_i_landclass_stat_2021 = CityLandClassStatSubBoundary(city_sub_boundary = city_sub_boundary_i,
                                                            classified_map = classified_map_2021, 
                                                            year = "2021")
  
  city_i_landclass_stat_2016_2021 = rbind(city_i_landclass_stat_2016,
                                          city_i_landclass_stat_2021)
  
  city_subregion_landclass_stat = rbind(city_subregion_landclass_stat,
                                        city_i_landclass_stat_2016_2021)
}

# compute urban expansion
city_subregion_urban_expansion = city_subregion_landclass_stat %>% group_by(name) %>% 
  mutate(
    urban_expansion = Urban - lag(Urban)
  ) %>% 
  drop_na(urban_expansion) %>% 
  dplyr::select(name, urban_expansion)

# merge with geo
city_subregion_urban_expansion_map = city_sub_boundary %>% 
  left_join(city_subregion_urban_expansion,
            by = "name")

# define color palette for urban expansion levels
pal_subRegion_urbanExpansion <- colorNumeric(palette = "Reds", 
                                             domain = city_subregion_urban_expansion_map$urban_expansion,
                                             na.color = "transparent",
                                             revers = FALSE)

# define map labels
labels <- sprintf("<strong>%s</strong><br/>%s: %s %s",
                  city_subregion_urban_expansion_map$name, 
                  "Urban expansion %",
                  round(city_subregion_urban_expansion_map$urban_expansion, 2), "") %>% 
  lapply(htmltools::HTML)


# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addPolygons(data = city_subregion_urban_expansion_map,
              group = "Urban expansion",
              fillColor = ~pal_subRegion_urbanExpansion(urban_expansion),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_subRegion_urbanExpansion, 
            values = city_subregion_urban_expansion_map$urban_expansion, 
            opacity = 0.9, 
            title = "Urban expansion (%)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Urban expansion"),
    options = layersControlOptions(collapsed = FALSE)
  )

# plot table
city_subregion_urban_expansion %>% 
  arrange(desc(urban_expansion)) %>% 
  dplyr::select('Ward number' = name, 
                'Urban expansion 2016-2020 (%)' = urban_expansion) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")

# plot chart
city_subregion_urban_expansion %>% 
  arrange(desc(urban_expansion)) %>% 
  filter(urban_expansion > 10) %>% 
  arrange(desc(urban_expansion)) %>%
  plot_ly() %>% 
  add_trace(x = ~factor(name), 
            y = ~urban_expansion, 
            marker = list(color = "red"),
            type = "bar",
            orientation = "v")  %>% 
  layout(title = "Top wards with highest urban expansion",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~urban_expansion),
         yaxis = list(title = 'Percent of urban expansion')
  )

# Planning area ----

# read raw data
planning_area = readOGR("./github/cities-urbanshift/thematic-analysis/Freetown/data/raw",
                        "Planning Area 12")

# write processed data: convert to geojson
writeOGR(planning_area,
         dsn = "./github/cities-urbanshift/thematic-analysis/Freetown/data/processed/planning_area.geojson",
         layer = "planning_area",
         driver = "GeoJSON",
         overwrite_layer=TRUE)

# read processed data
planning_area <- st_read("./github/cities-urbanshift/thematic-analysis/Freetown/data/processed/planning_area.geojson",
                         quiet = TRUE)


#rename field
city_sub_boundary = planning_area %>% 
  rename(name = P.Area)

# create empty dataframe
city_subregion_landclass_stat = data.frame(name = as.character(),
                                           year = as.character(),
                                           Urban = as.numeric(),
                                           'Non Urban' = as.numeric())

# compute urban area by planning area

for(i in 1:nrow(city_sub_boundary)){
  print(i)
  
  city_sub_boundary_i = city_sub_boundary[i,]
  
  city_i_landclass_stat_2016 = CityLandClassStatSubBoundary(city_sub_boundary = city_sub_boundary_i,
                                                            classified_map = classified_map_2016, 
                                                            year = "2016")
  
  city_i_landclass_stat_2021 = CityLandClassStatSubBoundary(city_sub_boundary = city_sub_boundary_i,
                                                            classified_map = classified_map_2021, 
                                                            year = "2021")
  
  city_i_landclass_stat_2016_2021 = rbind(city_i_landclass_stat_2016,
                                          city_i_landclass_stat_2021)
  
  city_subregion_landclass_stat = rbind(city_subregion_landclass_stat,
                                        city_i_landclass_stat_2016_2021)
}


# municipality boundaries ----


# define path
freetown_municipality_boundaries_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/thematic-analysis/freetown/data/SLE-Freetown_core-boundary.geojson")

# read the data
freetown_municipality_boundaries <- st_read(freetown_municipality_boundaries_path, quiet = TRUE)

# plot
leaflet() %>%
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  # Add boundaries
  addPolygons(data = freetown_municipality_boundaries,
              group = "Administrative boundaries",
              stroke = TRUE, color = "gray", weight = 1,dashArray = "3",
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.5) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Administrative boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  )


# process land use data ----

# define path
city_land_use_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/thematic-analysis/freetown/data/freetown_landuse_projected.geojson")

# read the data
city_land_use <- st_read(city_land_use_path,
                         quiet = TRUE)


color_landuse = c("gray20", #Residential, high density
                  "gray40", #Residential, medium density
                  "purple", #"Security - SE, Utility
                  "gray60", #Residential, low density
                 "yellow", # Civic and Culture
                 "red", #"Mix Commercial with Residential
                 "green", #Open Spaces, Recreational
                 "orange", #Commercia
                 "darkgreen", #Woodland, Forest
                 "blue", #"Waterbody, River, Creek
                 "turquoise", #"Coastal Wetland, Mangrove
                 "palegreen", #Urban Agriculture
                 "peachpuff4" #Industry 
                 )

color_landuse = c("yellow", # Civic and Culture
                  "turquoise", #"Coastal Wetland, Mangrove
                  "magenta", #Commercial
                  "red", #Industry
                  "pink", #"Mix Commercial with Residential
                  "green", #Open Spaces, Recreational
                  "gray20", #Residential, high density
                  "gray60", #Residential, low density
                  "gray40", #Residential, medium density
                  "purple", #"Security - SE, Utility
                  "palegreen", #Urban Agriculture
                  "blue", #"Waterbody, River, Creek
                  "darkgreen" #Woodland, Forest
                    )

# define a color palette
pal_land_use <- colorFactor(palette = color_landuse, 
                           domain = city_land_use$LegendLabe,
                           na.color = "transparent")

# define map labels
label_land_use <- sprintf(
  "<strong>%s</strong><br/><strong>%s</strong><br/>",
  paste("Land use class", city_land_use$LegendLabe, sep=": "), 
  paste("Land use Id", city_land_use$LU_ID, sep =": ")
) %>% 
  lapply(htmltools::HTML)

# plot the map
leaflet(city_land_use) %>%
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  # Add boundaries
  # addPolygons(data = city_land_use,
  #             group = "Land use",
  #             stroke = TRUE, color = "gray", weight = 1,dashArray = "3",
  #             smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.5) %>%
  addPolygons(data = city_land_use,
              group = "Land Use",
              fillColor = ~pal_land_use(LegendLabe),
              weight = .2,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.9,
              label = label_land_use,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal_land_use, 
            values  = city_land_use$LegendLabe, 
            opacity = 1, 
            title = "Land use classes",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Land Use"),
    options = layersControlOptions(collapsed = FALSE)
  )


