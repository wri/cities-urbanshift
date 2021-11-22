
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
library(sparkline)



# Read land use data ----

# define path
city_land_use_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/thematic-analysis/freetown/data/freetown_landuse_projected.geojson")

# read the data
city_land_use <- st_read(city_land_use_path,
                         quiet = TRUE)

# Plot land use map ----

# define colors bu land class
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

# Compute urban expansion by land use class ----

# get kand use classses
land_use_classes = unique(city_land_use$LegendLabe)

# add id for each polygon
city_land_use$name = paste("lu_area", c(1:nrow(city_land_use)), sep ="_")


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
  
  print(city_landcover_stat_df)
  
  # Add non urban statstic when urban percent equal to 100%
  
  # urban_percent = city_landcover_stat_df$Urban
  # if(urban_percent == 100){
  #   city_landcover_stat_df = city_landcover_stat_df %>% 
  #     add_column(`Non Urban` = 0)
  # }
  
  raster_values = unique(values(classified_map_mask_i))
  raster_values = raster_values[!is.na(raster_values)]
  nb_values = length(raster_values)
  
  if(nb_values == 1){
    urban_value = raster_values
    if(urban_value == 1){
      city_landcover_stat_df = city_landcover_stat_df %>% 
        add_column(`Non Urban` = 0)
    }else if(urban_value == 0){
      city_landcover_stat_df = city_landcover_stat_df %>% 
        add_column(Urban = 0)
    }
  }
  
  return(city_landcover_stat_df)
  
}

# create empty dataframe
city_subregion_landclass_stat = data.frame(name = as.character(),
                                           year = as.character(),
                                           Urban = as.numeric(),
                                           'Non Urban' = as.numeric())


# compute urban area by land use area

for(i in 1:nrow(city_land_use)){
  print(i)
  
  # i = 1
  city_sub_boundary_i = city_land_use[i,]
  
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
  
  print(city_i_landclass_stat_2016_2021)
}

# store output

write.csv(city_subregion_landclass_stat,
          "./github/cities-urbanshift/thematic-analysis/Freetown/output/land_use_urban_stat.csv")

# compute urban expansion ----
city_subregion_urban_expansion = city_subregion_landclass_stat %>% group_by(name) %>% 
  mutate(
    urban_expansion = Urban - lag(Urban)
  ) %>% 
  drop_na(urban_expansion) %>% 
  dplyr::select(name, urban_expansion)


# store output
write.csv(city_subregion_urban_expansion,
          "./github/cities-urbanshift/thematic-analysis/Freetown/output/land_use_urban_expansion.csv")


# Map Urban expansion ----

# read data
city_subregion_urban_expansion = read.csv("./github/cities-urbanshift/thematic-analysis/Freetown/output/land_use_urban_expansion.csv",
                                          row.names = 1)
# classify
city_subregion_urban_expansion = city_subregion_urban_expansion %>% 
  mutate(urban_expansion_class = case_when(urban_expansion <= 0 ~ "0",
                                           urban_expansion <= 5 ~ "1",
                                           urban_expansion <= 10 ~ "2",
                                           urban_expansion <= 20 ~ "3",
                                           urban_expansion <= 50 ~ "4",
                                           urban_expansion <= 100 ~ "5"))

# merge with geo
landuse_urban_expansion_map = city_land_use %>% 
  left_join(city_subregion_urban_expansion,
            by = "name")


# define colrs for map
pal_landuse_urban_expansion_colors = c("blue",
                                       RColorBrewer::brewer.pal(5, "Reds")[1:5]) #YlOrBr

# define color palette for urban expansion classes
pal_landuse_urban_expansion_class <- colorFactor(palette = pal_landuse_urban_expansion_colors, 
                                            domain = landuse_urban_expansion_map$urban_expansion_class,
                                            na.color = "transparent",
                                            revers = FALSE)

# define map labels
labels <- sprintf("<strong>%s</strong><br/>%s: %s %s",
                  landuse_urban_expansion_map$LegendLabe, 
                  "Urban expansion %",
                  round(landuse_urban_expansion_map$urban_expansion, 2), "") %>% 
  lapply(htmltools::HTML)


# plot map
leaflet(landuse_urban_expansion_map, height = 500, width = "100%") %>%
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addPolygons(data = landuse_urban_expansion_map,
              group = "Urban expansion",
              fillColor = ~pal_landuse_urban_expansion_class(urban_expansion_class),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 1,
              label = labels,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(colors = pal_landuse_urban_expansion_colors,
            labels = c("No urban expansion", "< 5 %","< 10 %","< 20 %","< 50 %","> 50 %"),
            # pal = pal_landuse_urban_expansion, 
            # values = landuse_urban_expansion_map$urban_expansion, 
            opacity = 0.9, 
            title = "Urban expansion (%)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM"),
    overlayGroups = c("Urban expansion"),
    options = layersControlOptions(collapsed = FALSE)
  ) 


# aggregate urban expansion by land use class ----


landuse_urban_expansion_stat = landuse_urban_expansion_map %>% 
  as.data.frame() %>% 
  dplyr::select(LegendLabe,urban_expansion) %>% 
  drop_na(urban_expansion) %>%
  group_by(LegendLabe) %>% 
  summarise(avg_urban_expansion = round(mean(urban_expansion),2))

# plot table
landuse_urban_expansion_stat %>% 
  arrange(desc(avg_urban_expansion)) %>% 
  dplyr::select('Land use class' = LegendLabe, 
                'Average Urban expansion 2016-2021 (%)' = avg_urban_expansion) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")
