
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

selected_city = "CRI-San_Jose"
selected_city = "RWA-Kigali"
selected_city = "SLE-Freetown_core"


######################################
# Get city administrative boundaries
######################################


aws.get.city.boundary = function(city_id){
  
  # define path
  city_boundary_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/",city_id,"-boundary.geojson",
                             sep = "")
  
  # read the data#
  city_boundary <- st_read(city_boundary_path,
                           quiet = TRUE)
  return(city_boundary)
  
}

city_boundary = aws.get.city.boundary(city_id = selected_city)


######################### Plot

# plot map
map = leaflet(city_boundary, height = 500, width = "100%") %>%
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

map

#                                                   Indicator 1: % land that is vegetation (non-crop) vegetation or water
#######################################################################################################################################


#################### Get Dynamic World Land Cover data

aws.get.dw.by.year = function(year, city_id, data_source){
  
  
  # get city boundary
  city_boundary = aws.get.city.boundary(city_id = city_id)
  
  # get local path
  # local_path = paste(getwd(),"/data/land_use/sentinel2_ndvi/",sep ="")
  local_path = "C:/Users/saifs/Documents/urbanshift/data/land_use/dynamic_world/"
  aws_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/dynamic_world/"
  
  if(data_source == "local"){
    path = local_path}
  
  else if(data_source == "aws"){
    path = aws_path
  }
  
  # define path to collect dw land cover
  # city_landcover_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/dynamic_world/", city_id, "-landcover", year,".tif",
  #                             sep = "")
  
  # define path
  city_landcover_path = paste(path, city_id,"-landcover",year,".tif",
                              sep = "")
  
  # collect raster data
  city_landcover_raster= raster(city_landcover_path)
  
  # mask raster based on administrative boundaries
  city_landcover_mask  = raster::mask(city_landcover_raster,city_boundary)
  
  return(city_landcover_mask)
}

city_landcover_dw_2020 = aws.get.dw.by.year(year = 2020, city_id = selected_city, data_source = "aws")
city_landcover_dw_2019 = aws.get.dw.by.year(year = 2019, city_id = selected_city, data_source = "aws")
city_landcover_dw_2018 = aws.get.dw.by.year(year = 2018, city_id = selected_city, data_source = "aws")
city_landcover_dw_2017 = aws.get.dw.by.year(year = 2017, city_id = selected_city, data_source = "aws")
city_landcover_dw_2016 = aws.get.dw.by.year(year = 2016, city_id = selected_city, data_source = "aws")


# map


plot.map = function(city_boundary, city_landcover_dw){
  # define colors for each class
  
  water_blue_0 = "#419BDF"
  trees_green_dark_1 = "#397D49"
  grass_green_light_2 = "#88B053" 
  flooded_veg_mauve_3 = "#7A87C6" 
  crops_orange_4 = "#E49635" 
  scrub_yellow_5 = "#DFC35A" 
  built_area_red_6 = "#C4281B" 
  bare_gray_7 = "#A59B8F" 
  snow_ice_mauve_light_8 = "#B39FE1" 
  
  # define color vector
  landcover_col = c(water_blue_0,
                    trees_green_dark_1,
                    grass_green_light_2,
                    flooded_veg_mauve_3,
                    crops_orange_4,
                    scrub_yellow_5,
                    built_area_red_6,
                    bare_gray_7,
                    snow_ice_mauve_light_8)
  
  
  # define a color palette
  pal_landcover <- colorFactor(landcover_col, 
                               values(city_landcover_dw),
                               na.color = "transparent")
  # define labels
  labels_landcover = c('Water','Trees','Grass','Flooded vegetation','Crops',
                       'Scrub/shrub','Built Area','Bare ground','Snow/Ice')
  
  # create the map
  map = leaflet(city_landcover_dw, height = 500, width = "100%")  %>%
    addTiles() %>%
    addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addPolygons(data = city_boundary,
                group = "Administrative boundaries",
                stroke = TRUE, color = "gray", weight = 1,dashArray = "3",
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
    addRasterImage(city_landcover_dw, 
                   colors = pal_landcover, 
                   opacity = 0.7,
                   group = "Land Cover") %>%
    addLegend(colors = landcover_col,
              labels = labels_landcover,
              title = "Land Cover") %>% 
    addLayersControl(
      baseGroups = c( "OSM","CartoDB", "Toner Lite"),
      overlayGroups = c("Land Cover","Administrative boundaries"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # plot the map
  return(map)
  
}


map = plot.map(city_landcover_dw = city_landcover_dw_2020,
               city_boundary = city_boundary)

map

#################### Compute land cover statistics based on Dynamic World 

# function for computing land cover statistics
compute.landcover.dw.stat = function(city_landcover_dw, year){
  city_landcover_dw_stat = as.data.frame(city_landcover_dw) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    group_by(class) %>%
    tally() %>%
    mutate(area = n * res(city_landcover_dw)[1] * res(city_landcover_dw)[2]) %>% 
    dplyr::select(land_use_classes = class,
                  nb_cells = n) %>% 
    mutate(land_use_percent = round(nb_cells/sum(nb_cells) * 100,2))%>% 
    add_column(year = year) %>% 
    arrange(desc(land_use_percent)) %>%
    mutate_at("land_use_classes", as.character) %>%
    mutate(land_use_classes = recode(land_use_classes,
                                     "0" =  "Water",
                                     "1" = "Trees" ,
                                     "2" = "Grass",
                                     "3" = "Flooded vegetation",
                                     "4" =  "Crops",
                                     "5" = "Scrub/shrub" ,
                                     "6"  = "Built Area" ,
                                     "7" = "Bare ground" ,
                                     "8"  = "Snow/Ice"))
  
  return(city_landcover_dw_stat)
  
}

# compute land cover statistics for every year
city_landcover_dw_stat_2020 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2020, year = "2020")
city_landcover_dw_stat_2019 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2019, year = "2019")
city_landcover_dw_stat_2018 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2018, year = "2018")
city_landcover_dw_stat_2017 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2017, year = "2017")
city_landcover_dw_stat_2016 = compute.landcover.dw.stat(city_landcover_dw = city_landcover_dw_2016, year = "2016")
# merge all years
city_landcover_stat = bind_rows(list(city_landcover_dw_stat_2020,
                                     city_landcover_dw_stat_2019,
                                     city_landcover_dw_stat_2018,
                                     city_landcover_dw_stat_2017,
                                     city_landcover_dw_stat_2016))

#################### Compute baseline indicator: 


# initialize empty dataframe for storing indicators
baseline_indicators = data.frame(city_id = as.character(),
                                 indicator_theme = as.character(),
                                 data_sources = as.character(),
                                 indicator_name = as.character(),
                                 year = as.character(),
                                 value = as.numeric())


# Compute baseline indicator: 

compute.baseline.indicators.dw.vegetation = function(city_landcover_stat, baseline_indicators, city_id){
  
  # define greenspace classes 
  land_cover_greenspace_classes = c("Trees","Grass","Scrub/shrub","Water","Flooded vegetation")
  
  
  # compute indicator
  baseline_indicators_dw_vegetation_land = city_landcover_stat %>% 
    mutate(greenspace_class =
             case_when(land_use_classes %in% land_cover_greenspace_classes ~ "Vegetation land", 
                       !land_use_classes %in% land_cover_greenspace_classes ~ "Non vegetation land")
    ) %>% 
    group_by(year,greenspace_class) %>% 
    summarise(greenspace_land_percent = sum(land_use_percent)) %>% 
    filter(greenspace_class == "Vegetation land") %>% 
    add_column(city_id = city_id,
               indicator_theme = "greenspace",
               data_sources = "Dynamic WOrld",
               indicator_name = "dynamic_world_vegetation_land_percent") %>% 
    rename(value = greenspace_land_percent) %>% 
    dplyr::select(city_id, 
                  indicator_theme,
                  data_sources,
                  indicator_name,
                  year,
                  value)
  
  baseline_indicators = bind_rows(list(baseline_indicators,
                                       baseline_indicators_dw_vegetation_land))
  
  return(baseline_indicators)
  
}


baseline_indicators = compute.baseline.indicators.dw.vegetation(city_landcover_stat = city_landcover_stat,
                                                                baseline_indicators = baseline_indicators,
                                                                city_id = selected_city)


# write table
write.table(baseline_indicators,
            paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_dw_landcover_stat.csv", sep =""),
            sep = ",",
            row.names = FALSE)
# read table
dw_landcover_stat = read.csv(paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_dw_landcover_stat.csv", sep =""))

# plot table
dw_landcover_stat %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")

#                                                   Indicator 2: % land that is vegetation (NDVI threshold, >0.4?)
#######################################################################################################################################

# Function to collect NDVI by year

aws.get.ndvi.by.year = function(year, city_id, ndvi_threshold, data_source){
  
  # get local path
  # local_path = paste(getwd(),"/data/land_use/sentinel2_ndvi/",sep ="")
  local_path = "C:/Users/saifs/Documents/urbanshift/data/land_use/sentinel2_ndvi/"
  aws_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/sentinel2_ndvi/"
  
  
  if(data_source == "local"){
    path = local_path}
  
  else if(data_source == "aws"){
    path = aws_path
  }
  
  
  # get city boundary
  city_boundary = aws.get.city.boundary(city_id = city_id)
  
  # define path
  city_ndvi_path = paste(path, city_id,"-maxNDVI",year,".tif",
                         sep = "")
  
  print(city_ndvi_path)
  # collect raster data
  city_ndvi = raster(city_ndvi_path)
  
  # mask raster based on administrative boundaries
  city_ndvi_mask = raster::mask(city_ndvi,
                                city_boundary)
  
  # classify with threshold
  city_ndvi_class = city_ndvi_mask
  city_ndvi_class[city_ndvi_class <= ndvi_threshold] = 0
  city_ndvi_class[city_ndvi_class > ndvi_threshold] = 1
  
  return(list(city_ndvi_mask, city_ndvi_class))
  
}

# collect NDVI by year

city_ndvi_2020 = aws.get.ndvi.by.year(year ="2020", city_id = selected_city, ndvi_threshold = 0.4, data_source = "aws")
city_ndvi_2019 = aws.get.ndvi.by.year(year ="2019", city_id = selected_city, ndvi_threshold = 0.4, data_source = "aws")
city_ndvi_2018 = aws.get.ndvi.by.year(year ="2018", city_id = selected_city, ndvi_threshold = 0.4, data_source = "aws")
city_ndvi_2017 = aws.get.ndvi.by.year(year ="2017", city_id = selected_city, ndvi_threshold = 0.4, data_source = "aws")
city_ndvi_2016 = aws.get.ndvi.by.year(year ="2016", city_id = selected_city, ndvi_threshold = 0.4, data_source = "aws")



################# plot

plot.map = function(city_ndvi){
  city_ndvi_mask = city_ndvi[[1]]
  
  # Aggregate for visualization performance
  city_ndvi_aggregate <- raster::aggregate(city_ndvi_mask, fact=0.0005/res(city_ndvi_mask))
  
  # classify with threshold
  city_ndvi_aggregate_class = city_ndvi_aggregate
  ndvi_threshold = 0.4
  city_ndvi_aggregate_class[city_ndvi_aggregate_class <= ndvi_threshold] = 0
  city_ndvi_aggregate_class[city_ndvi_aggregate_class > ndvi_threshold] = 1
  
  # define color palette for NDVI levels
  pal_ndvi <- colorNumeric(palette = "Greens",
                           domain = values(city_ndvi_aggregate),
                           na.color = "transparent")
  
  # define colors for each ndvi class
  ndvi_class_color = c("white","green")
  
  # define a color palette
  pal_ndvi_class <- colorFactor(ndvi_class_color,
                                values(city_ndvi_aggregate_class),
                                na.color = "transparent")
  
  # define labels for ndvi class
  ndvi_class_labels = c("Non-Vegetation","Vegetation")
  
  # Create the map
  map = leaflet(city_ndvi_aggregate_class, height = 500, width = "100%")  %>%
    addTiles() %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
    addPolygons(data = city_boundary,
                group = "Administrative boundaries",
                stroke = TRUE, color = "gray", weight = 4,dashArray = "3",
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
    addRasterImage(city_ndvi_aggregate,
                   colors = pal_ndvi,
                   opacity = 0.7,
                   group = "NDVI levels") %>%
    addLegend(pal = pal_ndvi,
              values = values(city_ndvi_aggregate),
              title = "NDVI level",
              position = "topright") %>%
    addRasterImage(city_ndvi_aggregate_class,
                   colors = pal_ndvi_class,
                   opacity = 0.7,
                   group = "NDVI class") %>%
    addLegend(colors = ndvi_class_color,
              labels = ndvi_class_labels,
              title = "NDVI class",
              position = "bottomright",
              opacity = 0.5) %>%
    addLayersControl(
      baseGroups = c( "Toner Lite","OSM","CartoDB"),
      overlayGroups = c("NDVI levels", "NDVI class","Administrative boundaries"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(map)
}


map_ndvi = plot.map(city_ndvi = city_ndvi_2020)

map_ndvi


###################################### Compute land cover statistics based on NDVI

# Function to Compute percent of ndvi classes

compute.ndvi.stat = function(raster, year){
  city_landcover_stat_df = as.data.frame(raster) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    group_by(class) %>% 
    tally() %>%
    mutate(area = n * res(raster)[1] * res(raster)[2]) %>% 
    dplyr::select(ndvi_classes = class,
                  nb_cells = n) %>% 
    add_column(year = year) %>% 
    mutate(ndvi_class_percent = round(nb_cells/sum(nb_cells) * 100,2))%>% 
    arrange(desc(ndvi_class_percent)) %>%
    mutate_at("ndvi_class_percent", as.character) %>%
    mutate(ndvi_classes = recode(ndvi_classes,
                                 "0" =  "Non Vegetation",
                                 "1" = "Vegetation" ))
  
  return(city_landcover_stat_df)
}

# compute ndvi class by year
city_ndvi_class_stat_2020 = compute.ndvi.stat(raster = city_ndvi_2020[[2]], year = "2020")
city_ndvi_class_stat_2019 = compute.ndvi.stat(raster = city_ndvi_2019[[2]], year = "2019")
city_ndvi_class_stat_2018 = compute.ndvi.stat(raster = city_ndvi_2018[[2]], year = "2018")
city_ndvi_class_stat_2017 = compute.ndvi.stat(raster = city_ndvi_2017[[2]], year = "2017")
city_ndvi_class_stat_2016 = compute.ndvi.stat(raster = city_ndvi_2016[[2]], year = "2016")

city_ndvi_class_stat = bind_rows(list(city_ndvi_class_stat_2020,
                                      city_ndvi_class_stat_2019,
                                      city_ndvi_class_stat_2018,
                                      city_ndvi_class_stat_2017,
                                      city_ndvi_class_stat_2016))


####################################### Compute baseline indicator: NDVI vegetation percent

# initialize empty dataframe for storing indicators
baseline_indicators = data.frame(city_id = as.character(),
                                 indicator_theme = as.character(),
                                 data_sources = as.character(),
                                 indicator_name = as.character(),
                                 year = as.character(),
                                 value = as.numeric())

compute.baseline.indicators.ndvi.vegetation = function(city_ndvi_class_stat, baseline_indicators, city_id){
  
  # compute indicator
  baseline_indicators_ndvi_vegetation_land = city_ndvi_class_stat %>% 
    filter(ndvi_classes == "Vegetation") %>% 
    add_column(city_id = city_id,
               indicator_theme = "greenspace",
               data_sources = "COPERNICUS/S2",
               indicator_name = "s2_ndvi_vegetation_land_percent") %>%
    rename(value = ndvi_class_percent) %>% 
    dplyr::select(city_id, 
                  indicator_theme,
                  data_sources,
                  indicator_name,
                  year,
                  value) %>% 
    mutate_at("value", as.numeric)
  
  baseline_indicators = bind_rows(list(baseline_indicators,
                                       baseline_indicators_ndvi_vegetation_land))
  
  return(baseline_indicators)
  
}

baseline_indicators = compute.baseline.indicators.ndvi.vegetation(city_ndvi_class_stat = city_ndvi_class_stat,
                                                                  city_id = selected_city,
                                                                  baseline_indicators = baseline_indicators)

# plot table
baseline_indicators %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")

# write table
write.table(baseline_indicators,
            paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_ndvi_vegetation_stat.csv", sep =""),
            sep = ",",
            row.names = FALSE)

# read table
ndvi_vegetation_stat = read.csv(paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_ndvi_vegetation_stat.csv", sep =""))

# plot table
ndvi_vegetation_stat %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")


#                                                   Indicator 3: % land that has tree cover
#######################################################################################################################################


# Function to collect Tree Outside Forest by year

aws.collect.tof.by.year = function(year, city_id, data_source, city_boundary){
  
  # get local path
  # local_path = paste(getwd(),"/data/land_use/trees_outside_forest/",sep ="")
  local_path = "C:/Users/saifs/Documents/urbanshift/data/land_use/trees_outside_forest/"
  aws_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/trees_outside_forest/"
  
  if(data_source == "local"){
    path = local_path}
  
  else if(data_source == "aws"){
    path = aws_path
  }
  
  # define path
  
  city_tof_path = paste(path, city_id,"-treecover",year,".tif",
                        sep = "")
  
  # collect raster data
  city_tof = raster(city_tof_path)
  
  # mask raster based on administrative boundaries
  city_tof_mask = raster::mask(city_tof,
                               city_boundary)
  
  return(city_tof_mask)
  
}

city_tof_2020 = aws.collect.tof.by.year(city_id = selected_city,  
                                        year = "2020", 
                                        data_source = "aws", 
                                        city_boundary = city_boundary)

################# plot

plot.map = function(city_tof, city_boundary){
  
  # Aggregate for visualization performance
  city_tof_aggregate <- raster::aggregate(city_tof, fact=0.0005/res(city_tof))
  
  # define color palette for NDVI levels
  pal_tof <- colorNumeric(palette = "Greens",
                          domain = values(city_tof),
                          na.color = "transparent")
  
  # Create the map
  map = leaflet(city_tof, height = 500, width = "100%")  %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
    addProviderTiles(providers$OpenStreetMap, group = "OSM") %>% 
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addPolygons(data = city_boundary,
                group = "Administrative boundaries",
                stroke = TRUE, color = "gray", weight = 4,dashArray = "3",
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
    addRasterImage(city_tof,
                   colors = pal_tof,
                   opacity = 0.7,
                   maxBytes = 20 * 1024 * 1024,
                   group = "TOF levels") %>%
    addLegend(pal = pal_tof,
              values = values(city_tof),
              title = "Tree Outside Forests percent",
              position = "topright") %>%
    addLayersControl(
      baseGroups = c("CartoDB", "OSM", "Toner Lite"),
      overlayGroups = c("TOF levels", "Administrative boundaries"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(map)
}


map_tof = plot.map(city_tof = city_tof_2020,
                   city_boundary = city_boundary)

map_tof


######################################  Compute baseline indicator: TOF average tree cover


compute.baseline.indicators.tof.treecover = function(city_id, year, city_tof){
  
  #compute average tree cover
  avg_tree_cover = round(cellStats(city_tof, 'mean'),2)
  
  # add to dataframe
  baseline_indicators_tof_tree_cover = data.frame(city_id = city_id,
                                                  indicator_theme = "greenspace",
                                                  data_sources = "Tree Outside Forests",
                                                  indicator_name = "tof_avg_tree_cover",
                                                  year = year,
                                                  value = avg_tree_cover)
  # add to baseline indicators
  baseline_indicators = bind_rows(list(baseline_indicators,baseline_indicators_tof_tree_cover))
  
  return(baseline_indicators)
}

# initialize empty dataframe for storing indicators
baseline_indicators = data.frame(city_id = as.character(),
                                 indicator_theme = as.character(),
                                 data_sources = as.character(),
                                 indicator_name = as.character(),
                                 year = as.character(),
                                 value = as.numeric())

baseline_indicators = compute.baseline.indicators.tof.treecover(city_id = selected_city, 
                                                                year = "2020", 
                                                                city_tof =  city_tof_2020)

# plot table
baseline_indicators %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "200px")

# write table
write.table(baseline_indicators,
            paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_tof_treecover_stat.csv", sep =""),
            sep = ",",
            row.names = FALSE)

# read table
tof_treecover_stat = read.csv(paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_tof_treecover_stat.csv", sep =""))

# plot table
tof_treecover_stat %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "100px")


#                                                   Indicator 4: % built land cover with vegetation
#######################################################################################################################################


########################### compute built area with vegetation stat

compute.built.land.with.vegetation = function(city_landcover_dw,city_ndvi, year){
  
  # filter built area from dw raster
  city_landcover_dw_built <- city_landcover_dw == 6
  
  # filter vegetation pixels from ndvi
  city_ndvi_vegetation = city_ndvi[[2]]
  
  # filter pixels with both built area land use and vegetation  
  city_built_vegetation_sum = city_landcover_dw_built + city_ndvi_vegetation
  city_built_vegetation = city_built_vegetation_sum == 2
  
  # compute statistics on percent of built area with vegetation
  city_built_vegetation_stat = as.data.frame(city_built_vegetation) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    group_by(class) %>%
    tally() %>%
    mutate(area = n * res(city_built_vegetation)[1] * res(city_built_vegetation)[2]) %>% 
    dplyr::select(land_use_classes = class,
                  nb_cells = n) %>% 
    mutate(land_use_percent = round(nb_cells/sum(nb_cells) * 100,2)) %>% 
    mutate_at("land_use_classes", as.character) %>% 
    mutate(land_use_classes = recode(land_use_classes,
                                     "TRUE" = "built area with vegetation",
                                     "FALSE" = "built area without vegetation")) %>% 
    mutate(land_use_classes = recode(land_use_classes,
                                     "1" = "built area with vegetation",
                                     "0" = "built area without vegetation")) %>%
    add_column(year = year) 
  
  return(city_built_vegetation_stat)
}

city_built_vegetation_stat_2020 = compute.built.land.with.vegetation(city_landcover_dw = city_landcover_dw_2020,
                                                                     city_ndvi = city_ndvi_2020,
                                                                     year = "2020")
city_built_vegetation_stat_2019 = compute.built.land.with.vegetation(city_landcover_dw = city_landcover_dw_2019,
                                                                     city_ndvi = city_ndvi_2019,
                                                                     year = "2019")
city_built_vegetation_stat_2018 = compute.built.land.with.vegetation(city_landcover_dw = city_landcover_dw_2018,
                                                                     city_ndvi = city_ndvi_2018,
                                                                     year = "2018")
city_built_vegetation_stat_2017 = compute.built.land.with.vegetation(city_landcover_dw = city_landcover_dw_2017,
                                                                     city_ndvi = city_ndvi_2017,
                                                                     year = "2017")
city_built_vegetation_stat_2016 = compute.built.land.with.vegetation(city_landcover_dw = city_landcover_dw_2016,
                                                                     city_ndvi = city_ndvi_2016,
                                                                     year = "2016")

city_built_vegetation_stat = bind_rows(list(city_built_vegetation_stat_2020,
                                            city_built_vegetation_stat_2019,
                                            city_built_vegetation_stat_2018,
                                            city_built_vegetation_stat_2017,
                                            city_built_vegetation_stat_2016))


# initialize empty dataframe for storing indicators
baseline_indicators = data.frame(city_id = as.character(),
                                 indicator_theme = as.character(),
                                 data_sources = as.character(),
                                 indicator_name = as.character(),
                                 year = as.character(),
                                 value = as.numeric())

########################### Compute Baseline indicator: % built land cover with vegetation


compute.baseline.indicators.built.vegetation = function(city_built_vegetation_stat, baseline_indicators, city_id){
  
  baseline_indicators_built_area_vegetation = city_built_vegetation_stat %>% 
    filter(land_use_classes == "built area with vegetation") %>% 
    add_column(city_id = city_id,
               indicator_theme = "greenspace",
               data_sources = "Dynamic World / Sentinel-2",
               indicator_name = "built_land_cover_with_vegetation_percent") %>% 
    rename(value = land_use_percent) %>% 
    mutate_at("year", as.character) %>% 
    dplyr::select(city_id, 
                  indicator_theme,
                  data_sources,
                  indicator_name,
                  year,
                  value)
  
  
  baseline_indicators = bind_rows(list(baseline_indicators,
                                       baseline_indicators_built_area_vegetation))
  
  return(baseline_indicators)
  
}

baseline_indicators = compute.baseline.indicators.built.vegetation(city_built_vegetation_stat = city_built_vegetation_stat,
                                                                   baseline_indicators = baseline_indicators,
                                                                   city_id = selected_city)

# plot table
baseline_indicators %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")


# write table
write.table(baseline_indicators,
            paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_built_vegetation_stat.csv", sep =""),
            sep = ",",
            row.names = FALSE)

# read table
built_vegetation_stat = read.csv(paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_built_vegetation_stat.csv", sep =""))

# plot table
built_vegetation_stat %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")

#                                                   Indicator 5: % built land cover with tree cover
#######################################################################################################################################


compute.built.treecover = function(city_landcover_dw,city_tof,city_id, year){
  
  # filter built area from dw raster
  city_landcover_dw_built <- city_landcover_dw == 6
  
  # To get percent of Tree cover in built area we convert non built land pixels to NA 
  city_landcover_dw_built_na = city_landcover_dw_built
  values(city_landcover_dw_built_na)[values(city_landcover_dw_built_na) == 0] = NA
  
  # and then multiply pixels values by resulted raster
  # Hence we have only tree cover values fro built area
  # then we compute the average tree cover of the obtained raster
  city_built_tree_cover = city_landcover_dw_built_na * city_tof
  
  #compute average tree cover
  avg_built_tree_cover = cellStats(city_built_tree_cover, 'mean')
  
  # add to dataframe
  city_built_treecover_stat = data.frame(city_id = city_id,
                                         indicator_theme = "greenspace",
                                         data_sources = "Dynamic World / Tree Outside Forests",
                                         indicator_name = "built_land_with_tree_cover_percent",
                                         year = year,
                                         value = avg_built_tree_cover)
  
  return(city_built_treecover_stat)
}



city_built_treecover_2020 = compute.built.treecover(city_landcover_dw = city_landcover_dw_2020,
                                                    city_tof = city_tof_2020,
                                                    city_id = selected_city,
                                                    year = "2020")

# write table
write.table(city_built_treecover_2020,
            paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_built_treecover_stat.csv", sep =""),
            sep = ",",
            row.names = FALSE)
# read table
built_treecover_stat = read.csv(paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_built_treecover_stat.csv", sep =""))

# plot table
built_treecover_stat %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "100px")


#                                                   Indicator 6: % ULU with tree cover
#######################################################################################################################################


###################################### Get Intra Urban Land Use data


aws.collect.ulu.by.year = function(year, city_id, data_source){
  
  # get local path
  # local_path = paste(getwd(),"/data/land_use/trees_outside_forest/",sep ="")
  local_path = "C:/Users/saifs/Documents/urbanshift/data/land_use/urban_land_use/"
  aws_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/urban_land_use/"
  
  if(data_source == "local"){
    path = local_path}
  
  else if(data_source == "aws"){
    path = aws_path
  }
  
  # define path
  # city_ulu_path = paste("https://storage.googleapis.com/urbanshift/indicators/greenspace/",city_id,"-urbanlanduse",year,".tif",
  #                       sep = "")
  city_ulu_path = paste(path, city_id,"-urbanlanduse",year,".tif",
                        sep = "")
  
  # collect raster data
  city_ulu = raster(city_ulu_path)
  
  # mask raster based on administrative boundaries
  city_ulu_mask = raster::mask(city_ulu,
                               city_boundary)
  
  return(city_ulu_mask)
  
}

city_ulu_2020 = aws.collect.ulu.by.year(city_id = selected_city,  year = "2020", data_source= "aws")


######################################  Compute Urban Land Use statistics

# function for computing land cover statistics
compute.ulu.stat = function(city_ulu, year){
  city_ulu_stat = as.data.frame(city_ulu) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    group_by(class) %>%
    tally() %>%
    mutate(area = n * res(city_ulu)[1] * res(city_ulu)[2]) %>% 
    dplyr::select(land_use_classes = class,
                  nb_cells = n) %>% 
    mutate(land_use_percent = round(nb_cells/sum(nb_cells) * 100,2))%>% 
    add_column(year = year) %>% 
    arrange(desc(land_use_percent)) %>%
    mutate_at("land_use_classes", as.character) %>%
    mutate(land_use_classes = recode(land_use_classes,
                                     "0" =  "Open space",
                                     "1" = "Non residential" ,
                                     "2" = "Atomistic",
                                     "3" = "Informal subdivision",
                                     "4" =  "Housing project",
                                     "5" = "Road" ))
  
  return(city_ulu_stat)
  
}

# compute land cover statistics for every year
city_ulu_stat_2020 = compute.ulu.stat(city_ulu = city_ulu_2020, year = "2020")

# write table
write.table(city_ulu_stat_2020,
            paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_ulu_stat.csv", sep =""),
            sep = ",",
            row.names = FALSE)
# read table
ulu_stat = read.csv(paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_ulu_stat.csv", sep =""))

# plot table
ulu_stat %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")


######################################  Compute Tree cover percent by ULU class

# function to compute percent of tree cover bu ULU class

compute.tree.cover.by.ulu.class = function(city_ulu,city_tof,ulu_class){
  # ulu_class = 0
  # print(city_ulu)
  # print(city_tof)
  
  # filter built area from dw raster
  city_ulu_class = city_ulu == ulu_class
  
  # To get percent of Tree cover in built area we convert non built land pixels to NA 
  values(city_ulu_class)[values(city_ulu_class) == 0] = NA
  
  # change resolution 
  res_fact = res(city_tof)[1]/res(city_ulu_class)[1]
  
  # Aggergate city_tof to have similar resolution
  city_tof_agg = disaggregate(city_tof, fact = res_fact)
  print(city_tof_agg)
  
  # and then multiply pixels values by resulted raster
  # Hence we have only tree cover values fro built area
  # then we compute the average tree cover of the obtained raster
  city_ulu_class_tree_cover = city_ulu_class * city_tof_agg
  
  #compute average tree cover
  avg_class_tree_cover = round(cellStats(city_ulu_class_tree_cover, 'mean'),2)
  
  return(avg_class_tree_cover)
}



tree_cover_ulu_stat = data.frame(ulu_class_code = c(0:5),
                                 ulu_class_label = c("open_space", 
                                                     "nonresidential",
                                                     "atomistic",
                                                     "informal_subdivision",
                                                     "formal_subdivision",
                                                     "housing_project"))

for(i in 1:nrow(tree_cover_ulu_stat)){
  ulu_class = tree_cover_ulu_stat[i, "ulu_class_code"]
  print(ulu_class)
  
  tree_cover_ulu_class = compute.tree.cover.by.ulu.class(city_ulu = city_ulu_2020,
                                                         city_tof = city_tof_2020,
                                                         ulu_class = ulu_class)
  
  tree_cover_ulu_stat[i, "tof_tree_cover_percent"] = tree_cover_ulu_class
  print(tree_cover_ulu_stat)
  
}

# Format table

tree_cover_ulu_stat$year = "2020"
tree_cover_ulu_stat$city_id = selected_city

tree_cover_ulu_stat %>% 
  rename('Year' = year,
         'City id' = city_id,
         'Urban Land Use Class code' = ulu_class_code,
         'Urban Land Use Class label' = ulu_class_label,
         'Tree cover percent' = tof_tree_cover_percent, ) %>% 
  dplyr::select('City id', 'Year','Urban Land Use Class label', 'Tree cover percent') %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "250px")

######################################  Compute vegetation percent by ULU class

compute.ndvi.vegetation.by.ulu.class = function(city_ulu,city_ndvi_veg_class,ulu_class){
  
  # ulu_class = 0
  
  # filter ulu raster by class
  city_ulu_class = city_ulu == ulu_class
  
  # To get percent of vegetation in each class we convert non selected claa land pixels to NA 
  values(city_ulu_class)[values(city_ulu_class) == 0] = NA
  
  # change resolution 
  res_fact = res(city_ndvi_veg_class)[1]/res(city_ulu_class)[1]
  
  # Aggergate city_tof to have similar resolution
  city_ndvi_veg_class_agg = disaggregate(city_ndvi_veg_class, fact = res_fact)
  
  # we substract ulu pixel by ndvi_vg pixels
  # if resulted value = 0 then the obtained pixel corresponds to vegetation area
  # if resulted value = 1 then the obtained pixel corresponds to non vegetation area
  city_ulu_class_ndvi_veg = city_ulu_class - city_ndvi_veg_class_agg
  
  print(city_ulu_class_ndvi_veg)
  
  #compute stat to get percent of vegetation land within selected ulu class
  city_ulu_class_ndvi_veg_stat = as.data.frame(city_ulu_class_ndvi_veg) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    group_by(class) %>%
    tally() %>%
    mutate(area = n * res(city_ulu_class_ndvi_veg)[1] * res(city_ulu_class_ndvi_veg)[2]) %>% 
    dplyr::select(land_use_classes = class,
                  nb_cells = n) %>% 
    mutate(land_use_percent = round(nb_cells/sum(nb_cells) * 100,2)) 
  
  city_ulu_class_ndvi_veg_stat = city_ulu_class_ndvi_veg_stat%>% 
    filter(land_use_classes == 0) 
  
  city_ulu_class_ndvi_veg_percent = city_ulu_class_ndvi_veg_stat$land_use_percent
  
  # print(city_ulu_class_ndvi_veg_percent)
  
}

for(i in 1:nrow(tree_cover_ulu_stat)){
  ulu_class = tree_cover_ulu_stat[i, "ulu_class_code"]
  # print(ulu_class)
  
  ndvi_vegetation_ulu_class_value = compute.ndvi.vegetation.by.ulu.class(city_ulu = city_ulu_2020,
                                                                         city_ndvi_veg_class = city_ndvi_2020[[2]],
                                                                         ulu_class = ulu_class)
  
  tree_cover_ulu_stat[i, "ndvi_vegetation_percent"] = ndvi_vegetation_ulu_class_value
  # print(tree_cover_ulu_stat)
  
}




######################################  fromat data frame

tree_cover_ulu_stat$year = "2020"
tree_cover_ulu_stat$city_id = selected_city


tree_cover_ulu_stat %>% 
  rename('Year' = year,
         'City id' = city_id,
         'Urban Land Use Class code' = ulu_class_code,
         'Urban Land Use Class label' = ulu_class_label,
         'Tree cover percent' = tof_tree_cover_percent, 
         'Vegetation percent' = ndvi_vegetation_percent) %>% 
  dplyr::select('City id', 'Year','Urban Land Use Class label', 'Tree cover percent','Vegetation percent') %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "250px")


# write table
write.table(tree_cover_ulu_stat,
            paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_ulu_stat_treecover.csv", sep =""),
            sep = ",",
            row.names = FALSE)

# read table
ulu_stat_treecover = read.csv(paste("C:/Users/saifs/Documents/urbanshift/data/indicators/",selected_city,"_ulu_stat_treecover.csv", sep =""))

# plot table
ulu_stat_treecover %>% 
  rename('Urban Land Use Class code' = ulu_class_code,
         'Urban Land Use Class label' = ulu_class_label,
         'Tree cover percent' = tof_tree_cover_percent, 
         'Vegetation percent' = ndvi_vegetation_percent,
         'Year' = year,
         'City id' = city_id) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "250px")

fig <- plot_ly(ulu_stat_treecover, 
               x = ~ulu_class_label, 
               y = ~tof_tree_cover_percent, 
               type = 'bar', 
               name = 'Tree cover percent')
fig <- fig %>% add_trace(y = ~ndvi_vegetation_percent, name = 'NDVI vegetation percent')
fig <- fig %>% layout(yaxis = list(title = 'Percent of area by urban land use class (%)'), 
                      xaxis = list(title = 'Urban land use classes'),
                      barmode = 'group',
                      height = 500, width = "100%")

fig