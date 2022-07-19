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
library(downloadthis)
library(leaflet.multiopacity)
library(leaflet.extras)
library(DT)

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

setwd("./github/cities-urbanshift/baseline-indicators/biodiversity/reports")

city_id = "CRI-San_Jose"

color_score_red_0 = "#145A32"
color_score_orange_1 = "#2ECC71"
color_score_yellow_2 = "#F4D03F"
color_score_green_3 = "#E67E22"
color_score_green_4 = "#C0392B"

# city-boundary ----

# read all boundaries
urbanshift_boundaries = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/administrative_boundaries.geojson",
                                quiet = TRUE)

# # select verified boundaries
# urbanshift_boundaries = urbanshift_boundaries[urbanshift_boundaries$boundary_use == TRUE, ]

# change San jose boundary status
urbanshift_boundaries[urbanshift_boundaries$city_name == "CRI-San_Jose" & urbanshift_boundaries$boundary_data_source == "cities360", "boundary_use"] = TRUE
urbanshift_boundaries[urbanshift_boundaries$city_name == "CRI-San_Jose" & urbanshift_boundaries$boundary_data_source == "city_specific", "boundary_use"] = FALSE

urbanshift_boundaries = urbanshift_boundaries[urbanshift_boundaries$boundary_use == TRUE, ]

# specify the cityid
city_id = "CRI-San_Jose"

# extract city boundary
city_boundary = urbanshift_boundaries[urbanshift_boundaries$city_name == city_id, ]

# set view param
city_boundary_centroid <- st_centroid(city_boundary)
city_boundary_centroid_coords = st_coordinates(city_boundary_centroid)
city_boundary_centroid_lng = city_boundary_centroid_coords[,"X"]
city_boundary_centroid_lat = city_boundary_centroid_coords[,"Y"]

# boundaries GAM

GAM_boundaries = st_read("./data/SanJose/boundaries_GAM/LimiteCantonal_GAM.shp",
                         quiet = TRUE)

st_write(GAM_boundaries,
         "./data/SanJose/boundaries_GAM/LimiteCantonal_GAM.geojson")

###################################################################################################
# biodiversity_indicators_outputs ----

path_indicators = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/tables/biodiversity_baseline_newmunis_main.csv"
biodiversity_baseline_indicators <- read.csv(path_indicators,
                                             check.names=FALSE)

biodiversity_baseline_indicators <- read_excel("./data/SanJose/biodiversity_baseline_newmunis.xlsx", 
                                               sheet = "MAIN")

biodiversity_baseline_indicators_11 <- read.csv("./data/SanJose/biodiversity_baseline_newmunis_SICB_11.csv")
biodiversity_baseline_indicators_11B <- read.csv("./data/SanJose/biodiversity_baseline_newmunis_SICB_11B.csv")

boundary_municipality = st_read("./data/SanJose/sj_newbounds.geojson",
                     quiet = TRUE)

# join with geo
biodiversity_baseline_indicators_geo = boundary_municipality %>% 
  left_join(biodiversity_baseline_indicators,
            by = c("shapeName" = "Municipality name"))

###################################################################################################
# biodiversity_indicators_outputs-metropolitan ----

biodiversity_baseline_indicators_metro <- read_excel("./data/SanJose/biodiversity_baseline_newmunis_metropolitan.xlsx", 
                                               sheet = "MAIN")

biodiversity_metro = biodiversity_baseline_indicators_metro %>% 
  slice(2L) %>% 
  dplyr::select(`SICB-1`,`SICB-2`,`SICB-3`,
                `SICB-7A`, `SICB-7B`, 
                `SICB-8`, `SICB-10`, `SICB-12`, `SICB-13`,
                `KBA(protected)`, `KBA(built-up)`)%>% 
  mutate(`SICB-1` = as.numeric(`SICB-1`)) %>% 
  mutate(`SICB-2` = as.numeric(`SICB-2`)) %>% 
  mutate(`SICB-3` = as.numeric(`SICB-3`)) %>% 
  mutate(`SICB-7A` = as.numeric(`SICB-7A`)) %>% 
  mutate(`SICB-7B` = as.numeric(`SICB-7B`)) %>% 
  mutate(`SICB-8` = as.numeric(`SICB-8`)) %>% 
  mutate(`SICB-10` = as.numeric(`SICB-10`)) %>% 
  mutate(`SICB-12` = as.numeric(`SICB-12`)) %>%
  mutate(`SICB-13` = as.numeric(`SICB-13`)) %>%
  mutate(`KBA-protected` = as.numeric(`KBA(protected)`)) %>%
  mutate(`KBA-built-up` = as.numeric(`KBA(built-up)`)) %>%
  # SICB-1
  mutate(`SICB-1-value` = round(`SICB-1`,4)*100) %>% 
  mutate(`SICB-1-score`=
           case_when(`SICB-1-value` < 1 ~ "0",
                     `SICB-1-value` <7 ~ "1",
                     `SICB-1-value` <14 ~ "2",
                     `SICB-1-value` <20 ~ "3",
                     `SICB-1-value` >= 20 ~ "4")) %>% 
  # SICB-2
  mutate(`SICB-2-value` = round(`SICB-2`,4)*100) %>% 
  mutate(`SICB-2-score`=
           case_when(`SICB-2-value` < 20 ~ "0",
                     `SICB-2-value` <39.9 ~ "1",
                     `SICB-2-value` <59.9 ~ "2",
                     `SICB-2-value` <79 ~ "3",
                     `SICB-2-value` >= 79 ~ "4")) %>% 
  # SICB-3
  mutate(`SICB-3-value` = round(`SICB-3`,4)*100) %>% 
  mutate(`SICB-3-score`=
           case_when(`SICB-3-value` < 6 ~ "0",
                     `SICB-3-value` <11 ~ "1",
                     `SICB-3-value` <16 ~ "2",
                     `SICB-3-value` <20 ~ "3",
                     `SICB-3-value` >= 20 ~ "4")) %>% 
  # SICB-7A
  mutate(`SICB-7A-value` = round(`SICB-7A`,4)*100) %>% 
  mutate(`SICB-7A-score`=
           case_when(`SICB-7A-value` < 20 ~ "0",
                     `SICB-7A-value` <  40 ~ "1",
                     `SICB-7A-value` < 60 ~ "2",
                     `SICB-7A-value` < 80 ~ "3",
                     `SICB-7A-value` >= 80 ~ "4")) %>% 
  # SICB-7B
  mutate(`SICB-7B-value` = round(`SICB-7B`,4)*100) %>% 
  mutate(`SICB-7B-score`=
           case_when(`SICB-7B-value` < 20 ~ "0",
                     `SICB-7B-value` <  40 ~ "1",
                     `SICB-7B-value` < 60 ~ "2",
                     `SICB-7B-value` < 80 ~ "3",
                     `SICB-7B-value` >= 80 ~ "4")) %>% 
  # SICB-8
  mutate(`SICB-8-value` = round(`SICB-8`,4)*100) %>% 
  mutate(`SICB-8-score`=
           case_when(`SICB-8-value` < 1 ~ "0",
                     `SICB-8-value` <  6 ~ "1",
                     `SICB-8-value` < 11 ~ "2",
                     `SICB-8-value` < 17 ~ "3",
                     `SICB-8-value` >= 17 ~ "4")) %>% 
  # SICB-10
  mutate(`SICB-10-value` = round(`SICB-10`,4)*100) %>% 
  mutate(`SICB-10-score`=
           case_when(`SICB-10-value` < 30 ~ "0",
                     `SICB-10-value` <  40 ~ "1",
                     `SICB-10-value` < 50 ~ "2",
                     `SICB-10-value` < 60 ~ "3",
                     `SICB-10-value` >= 60 ~ "4")) %>% 
  # SICB-12
  mutate(`SICB-12-value` = round(`SICB-12`,4)) %>%
  mutate(`SICB-12-score`=
           case_when(`SICB-12-value` < 0.1 ~ "0",
                     `SICB-12-value` <  0.3 ~ "1",
                     `SICB-12-value` < 0.6 ~ "2",
                     `SICB-12-value` < 0.9 ~ "3",
                     `SICB-12-value` >= 0.9 ~ "4")) %>% 
  # SICB-13
  mutate(`SICB-13-value` = round(`SICB-13`,4)*100) %>% 
  mutate(`SICB-13-score`=
           case_when(`SICB-13-value` < 30 ~ "0",
                     `SICB-13-value` <  49.9 ~ "1",
                     `SICB-13-value` < 69.9 ~ "2",
                     `SICB-13-value` < 89.9 ~ "3",
                     `SICB-13-value` >= 90 ~ "4")) %>% 
  # KBA 
  mutate(`KBA-protected-value` = round(`KBA-protected`,4)*100) %>% 
  mutate(`KBA-protected-built-up` = round(`KBA-built-up`,4)*100) %>% 
  mutate(`Region` = "San Jose metropolitan area")


        
View(biodiversity_metro)



###################################################################################################
# SCIB-1-table-Region ----


biodiversity_metro %>% 
  dplyr::select(`Region`,
                `SICB-1-value`,
                `SICB-1-score`) %>% 
  mutate_at(vars(`SICB-1-score`), ~ cell_spec(
    ., "html", 
    bold = TRUE,
    color = "white",
    background = ifelse(. == "4", "#145A32", ifelse(. ==  "3", "#2ECC71", ifelse(. == "2", "#F4D03F", ifelse(. == "2", "#E67E22", "#C0392B"))))
  )) %>% 
  kable(format = "html", escape = FALSE) %>%
  kable_styling("striped", full_width = FALSE) %>% 
  scroll_box(width = "100%", height = "100px")



###################################################################################################
# SCIB-1-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-1-value` = round(`SICB-1`,4)*100) %>% 
  mutate(`SICB-1-score`=
           case_when(`SICB-1-value` < 1 ~ "0",
                     `SICB-1-value` <7 ~ "1",
                     `SICB-1-value` <14 ~ "2",
                     `SICB-1-value` <20 ~ "3",
                     `SICB-1-value` >= 20 ~ "4"))

SCIB_1_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-1-value",
                "SICB-1-score")



###################################################################################################
# esa-data -----
worldcoverc_data_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/esa_world_cover/world_cover_10m_",
                              city_id,
                              ".tif",
                              sep = "")
city_worldcover_raster = raster(worldcoverc_data_path)
city_worldcover  = raster::mask(city_worldcover_raster,
                                city_boundary)

worldcoverc_data_path_v2 = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/CRI-San_Jose-ESA-landcover-2020.tif"
city_worldcover_raster = raster(worldcoverc_data_path_v2)
city_worldcover  = raster::mask(city_worldcover_raster,
                                city_boundary)




###################################################################################################
# natural-area-layer -----
city_worldcover_natural_areas_mask = city_worldcover

city_worldcover_natural_areas_mask[!city_worldcover_natural_areas_mask%in% c("10","20","30","90","95","100")] <- NA

city_worldcover_natural_areas = mask(x = city_worldcover,
                                     mask = city_worldcover_natural_areas_mask)


###################################################################################################
# SCIB-1-map ----

# prepare map

# define color palette for I1 levels
pal_SCIB_1 <- colorNumeric(palette = "Greens", 
                       domain = biodiversity_baseline_indicators_geo$`SICB-1-value`,
                       na.color = "transparent",
                       revers = FALSE)

# define color palette for scores
pal_score <- colorFactor(palette = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"), 
                         levels = c("0","1","2","3","4"),
                         na.color = "transparent",
                         revers = TRUE)

# define color palette for WOrld cover
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

worldcover_col = c(Trees_10_green,
                   Shrubland_20_orange,
                   Grassland_30_yellow,
                   Cropland_40_mauve,
                   Built_up_50_red,
                   Barren_sparse_vegetation_60_gray,
                   Snow_ice_70_white,
                   Open_Water_80_blue,
                   Herbaceous_wetland_90_blue2,
                   Mangroves_95_green2,
                   Moss_lichen_100_beige)
worldcover_labels = c('Trees','Shrubland','Grassland','Cropland','Built-up',
                      'Barren / sparse vegetation','Snow/ice','Open water','Herbaceous wetland',
                      'Mangroves','Moss/lichen')

# define a color palette
pal_worldcover <- colorFactor(palette = worldcover_col, 
                              # domain = values(city_worldcover),
                              levels = c("10","20","30","40","50","60",
                                         "70","80","90","95","100"),
                              na.color = "transparent")
# define labels
labels_SCIB_1 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                     biodiversity_baseline_indicators_geo$shapeName,
                     "Proportion of natural areas",
                     round(biodiversity_baseline_indicators_geo$`SICB-1-value`, 2), "",
                     "Score",biodiversity_baseline_indicators_geo$`SICB-1-score`) %>% 
  lapply(htmltools::HTML)



# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # GAM boundaries
  addPolygons(data = GAM_boundaries,
              group = "GAM  boundaries",
              stroke = TRUE, color = "red", weight = 2,dashArray = "1",
              smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 10,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = GAM_boundaries$provincia,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  # `SICB-1-value`
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Porportion of natural areas - value",
              fillColor = ~pal_SCIB_1(`SICB-1-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_1,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_1,
            values = biodiversity_baseline_indicators_geo$`SICB-1-value`,
            opacity = 0.9,
            title = "Percent of natural areas (%)",
            group = "Porportion of natural areas - value",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-1-score
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Porportion of natural areas - score",
              fillColor = ~pal_score(`SICB-1-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_1,
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
            title = "Natural areas scores (%)",
            group = "Porportion of natural areas - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # Raster of natural areas
  addRasterImage(city_worldcover_natural_areas,
                 colors = "#65B96B",
                 opacity = 1,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Natural land cover",
                 layerId = "Natural Areas") %>%
  # Raster of world cover
  addRasterImage(city_worldcover,
                 colors = pal_worldcover,
                 opacity = 1,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Land cover types",
                 layerId = "WorldCover") %>%
  addLegend(colors = worldcover_col,
            labels = worldcover_labels,
            title = "World Cover",
            group = "Land cover types",
            position = "bottomleft",
            opacity = 1) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Porportion of natural areas - value",
                      "Porportion of natural areas - score",
                      "Natural land cover",
                      "Land cover types",
                      "GAM  boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Porportion of natural areas - score",
              "Land cover types",
              "GAM  boundaries")) %>% 
  addFullscreenControl()

###################################################################################################
# SCIB-1-chart ----


# prepare chat colors
value_vector = biodiversity_baseline_indicators_geo %>% 
  drop_na(`SICB-1-value`) %>% 
  arrange(desc(`SICB-1-value`)) %>% 
  pull(`SICB-1-score`) %>% 
  as.numeric()

color_vector = pal_score(value_vector)

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(`SICB-1-value`)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(shapeName), 
            y = ~`SICB-1-value`, 
            marker = list(color = color_vector),
            type = "bar",
            text = ~`SICB-1-value`, textposition = 'auto', insidetextfont = list(size=20, color = 'white'),
            orientation = "v")  %>% 
  layout(title = "Proportion of natural areas (2020)",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~`SICB-1-value`),
         yaxis = list(title = ''))

###################################################################################################
# SCIB-2-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-2-value` = round(`SICB-2`,2)*100) %>% 
  mutate(`SICB-2-score`=
           case_when(`SICB-2-value` < 20 ~ "0",
                     `SICB-2-value` <39.9 ~ "1",
                     `SICB-2-value` <59.9 ~ "2",
                     `SICB-2-value` <79 ~ "3",
                     `SICB-2-value` >= 79 ~ "4"))

SCIB_2_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-2-value",
                "SICB-2-score")


##################################################################################
# SCIB-2-map ----

# define color palette for I1 levels
pal_SCIB_2 <- colorNumeric(palette = "Greens", 
                           domain = biodiversity_baseline_indicators_geo$`SICB-2-value`,
                           na.color = "transparent",
                           revers = FALSE)

# define labels
labels_SCIB_2 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                         biodiversity_baseline_indicators_geo$shapeName,
                         "Connectivity value",
                         round(biodiversity_baseline_indicators_geo$`SICB-2-value`, 2), "",
                         "Score",biodiversity_baseline_indicators_geo$`SICB-2-score`) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Connectivity - value",
              fillColor = ~pal_SCIB_2(`SICB-2-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_2,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_2,
            values = biodiversity_baseline_indicators_geo$`SICB-2-value`,
            opacity = 0.9,
            title = "Connectivity level",
            group = "Connectivity - value",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # I1 score layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Connectivity - score",
              fillColor = ~pal_score(`SICB-2-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_2,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  # I1 score legend
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 79 %)",
                       "3 (60% – 79%)",
                       "2 (40% – 59.9%)",
                       "1 (20% – 39.9%)",
                       "0 (< 20%)"),
            opacity = 0.9,
            title = "Connectivity score",
            group = "Connectivity - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # Raster of natural areas
  addRasterImage(city_worldcover_natural_areas, 
                 colors = "#65B96B",
                 opacity = 1,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Natural land cover") %>%
  # # Raster of world cover
  # addRasterImage(city_worldcover, 
  #                colors = pal_worldcover,
  #                opacity = 1,
  #                maxBytes = 20 * 1024 * 1024,
  #                project=FALSE,
  #                group = "World Cover",
  #                layerId = "WorldCover") %>%
  # addLegend(colors = worldcover_col,
  #           labels = worldcover_labels,
  #           title = "World Cover",
  #           group = "World Cover",
  #           position = "bottomleft",
  #           opacity = 1) %>%
  # Layers control
  addLayersControl(
    # baseGroups = c("Porportion of natural areas - value","Porportion of natural areas - score"),
    overlayGroups = c("Natural land cover",
                      "Connectivity - score",
                      "Connectivity - value"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Connectivity - score")) %>% 
  addFullscreenControl()

###################################################################################################
# SCIB-3-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-3-value` = round(`SICB-3 (using 5-yr data)`,4)*100) %>%
  mutate(`SICB-3-value` = if_else(is.na(`SICB-3-value`), 0, `SICB-3-value`)) %>% 
  mutate(`SICB-3-score`=
           case_when(`SICB-3-value` < 6 ~ "0",
                     `SICB-3-value` <11 ~ "1",
                     `SICB-3-value` <16 ~ "2",
                     `SICB-3-value` <20 ~ "3",
                     `SICB-3-value` >= 20 ~ "4"))

SCIB_3_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-3-value",
                "SICB-3-score")

###################################################################################################
# built-up-layer -----
city_worldcover_built_mask = city_worldcover

city_worldcover_built_mask[!city_worldcover_built_mask%in% c("50")] <- NA

city_worldcover_built = mask(x = city_worldcover,
                                     mask = city_worldcover_built_mask)

###################################################################################################
# birds-layer -----

birds2020 = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/biodiversity_map_layers_birds2020_sanjose.geojson",
                    quiet = TRUE)


##################################################################################
# SCIB-3-map ----

# define color palette for I1 levels
pal_SCIB_3 <- colorNumeric(palette = "Greens", 
                           domain = biodiversity_baseline_indicators_geo$`SICB-3-value`,
                           na.color = "transparent",
                           revers = FALSE)

# define labels
labels_SCIB_3 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                         biodiversity_baseline_indicators_geo$shapeName,
                         "Native birds in built-up areas",
                         round(biodiversity_baseline_indicators_geo$`SICB-3-value`, 2), "",
                         "Score",biodiversity_baseline_indicators_geo$`SICB-3-score`) %>% 
  lapply(htmltools::HTML)

# labels birds
label_birds2020 <- sprintf(
  "<strong>%s</strong><br/><strong>%s</strong><br/>",
  paste("Family", birds2020$family, sep=": "), 
  paste("Genus", birds2020$genus, sep =": ")
) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # `SICB-3-value`
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Native birds in built-up areas - value",
              fillColor = ~pal_SCIB_3(`SICB-3-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_3,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_3,
            values = biodiversity_baseline_indicators_geo$`SICB-3-value`,
            opacity = 0.9,
            title = "Percent of native birds <br/> in built-up areas ",
            group = "Native birds in built-up areas - value",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # `SICB-3-score`
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Native birds in built-up areas - score",
              fillColor = ~pal_score(`SICB-3-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_3,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  # I1 score legend
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 20 %)",
                       "3 (16% – 20%)",
                       "2 (11% – 15.9%)",
                       "1 (6% – 10.9%)",
                       "0 (< 6%)"),
            opacity = 0.9,
            title = "Native birds in built-up areas (score)",
            group = "Native birds in built-up areas - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # # add birds layer clusters
  # addMarkers(birds2020$lon,
  #            birds2020$lat,
  #            # popup = ~as.character(birds2020$family),
  #            label = label_birds2020,
  #            clusterOptions = markerClusterOptions(),
  #            group = "Birds clusters") %>%
  # add birds layer points
  addCircleMarkers(birds2020$lon, birds2020$lat,
                   radius = 3,
                   fillColor = col_BoldBrickOrange,
                   color  = "black",
                   stroke = TRUE,
                   weight = 0.8,
                   fillOpacity = 0.6,
                   # popup = ~as.character(birds2020$family),
                   label = label_birds2020,
                   group = "Bird observations") %>%
  # # Raster of built-up areas
  # addRasterImage(city_worldcover_built,
  #                colors = "#fa0000",
  #                opacity = 0.8,
  #                maxBytes = 20 * 1024 * 1024,
  #                project=FALSE,
  #                group = "Built-up Areas",
  #                layerId = "Built-up Areas") %>%
  # Raster of world cover
  # addRasterImage(city_worldcover,
  #                colors = pal_worldcover,
  #                opacity = 1,
  #                maxBytes = 20 * 1024 * 1024,
  #                project=FALSE,
  #                group = "World Cover",
  #                layerId = "WorldCover") %>%
  # addLegend(colors = worldcover_col,
  #           labels = worldcover_labels,
  #           title = "World Cover",
  #           group = "World Cover",
  #           position = "bottomleft",
  #           opacity = 1) %>%
  # Layers control
  addLayersControl(
    # baseGroups = c("Porportion of natural areas - value","Porportion of natural areas - score"),
    overlayGroups = c("Native birds in built-up areas - value",
                      "Native birds in built-up areas - score",
                      "Bird observations"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Native birds in built-up areas - score")) %>% 
  addFullscreenControl()


###################################################################################################
# SCIB-3-chart ----


# prepare chat colors
value_vector = biodiversity_baseline_indicators_geo %>% 
  drop_na(`SICB-3-value`) %>% 
  arrange(desc(`SICB-3-value`)) %>% 
  pull(`SICB-3-score`) %>% 
  as.numeric()

color_vector = pal_score(value_vector)

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(`SICB-3-value`)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(shapeName), 
            y = ~`SICB-3-value`, 
            marker = list(color = color_vector),
            type = "bar",
            text = ~`SICB-3-value`, textposition = 'auto', insidetextfont = list(size=20, color = 'white'),
            orientation = "v")  %>% 
  layout(title = "",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~`SICB-1-value`),
         yaxis = list(title = ''))
###################################################################################################
# SCIB-4 ----

# load data
SCIB_4 <- read_excel("./data/SanJose/IUCN RedList counts.xlsx", 
                     sheet = "Tracheophyta")

# identify municipalities with threatened species
SCIB_4_table = SCIB_4 %>% 
  mutate(SCIB_4_red=
           case_when(`Near Threatened` > 0 ~ 1,
                     Vulnerable > 0 ~ 1,
                     Endangered > 0 ~ 1,
                     `Critically Endagered`> 0 ~ 1,
                     `Extinct in the Wild`> 0 ~ 1)) %>% 
  mutate(SCIB_4_red = if_else(is.na(SCIB_4_red), 0, SCIB_4_red)) %>% 
  rename("Municipality name" = muni_name)
  
# plot table

DT::datatable(SCIB_4_table,
              options=list(columnDefs = list(list(visible=FALSE, targets=c(8))))) %>% formatStyle(
  'SCIB_4_red',
  target = 'row',
  backgroundColor = styleEqual(c(0, 1), c('white', 'red'))
)

write.csv(SCIB_4_table,
          "./data/SanJose/tables/SICB_4_table.csv")


SCIB_4_table %>% 
  mutate(`Region` = "San Jose metropolitan area",
         `Near Threatened` = sum(`Near Threatened`),
         `Vulnerable` = sum(SCIB_4_table$`Vulnerable`),
         `Endangered` = sum(SCIB_4_table$`Endangered`),
         `Critically Endagered` = sum(SCIB_4_table$`Critically Endagered`),
         `Extinct in the Wild` = sum(SCIB_4_table$`Extinct in the Wild`),
         `Total` = sum(SCIB_4_table$`Total`)) %>% 
  dplyr::select(`Region`,
                `Near Threatened`,
                `Vulnerable`,
                `Endangered`,
                `Critically Endagered`,
                `Extinct in the Wild`,
                `Total`) %>%
  slice(1) %>%
  kable(format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) 

###################################################################################################
# SCIB-5 ----

# load data
SCIB_5 <- read_excel("./data/SanJose/IUCN RedList counts.xlsx", 
                     sheet = "Aves")

# identify municipalities with threatened species
SCIB_5_table = SCIB_5 %>% 
  mutate(SCIB_5_red=
           case_when(`Near Threatened` > 0 ~ 1,
                     Vulnerable > 0 ~ 1,
                     Endangered > 0 ~ 1,
                     `Critically Endagered`> 0 ~ 1,
                     `Extinct in the Wild`> 0 ~ 1)) %>% 
  mutate(SCIB_5_red = if_else(is.na(SCIB_5_red), 0, SCIB_5_red)) %>% 
  rename("Municipality name" = muni_name)

# plot table region
SCIB_5_table %>% 
  mutate(`Region` = "San Jose metropolitan area",
         `Near Threatened` = sum(`Near Threatened`),
         `Vulnerable` = sum(`Vulnerable`),
         `Endangered` = sum(`Endangered`),
         `Critically Endagered` = sum(`Critically Endagered`),
         `Extinct in the Wild` = sum(`Extinct in the Wild`),
         `Total` = sum(`Total`)) %>% 
  dplyr::select(`Region`,
                `Near Threatened`,
                `Vulnerable`,
                `Endangered`,
                `Critically Endagered`,
                `Extinct in the Wild`,
                `Total`) %>%
  slice(1) %>%
  kable(format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) 

# plot table

DT::datatable(SCIB_5_table,
              options=list(
                columnDefs = list(list(visible=FALSE, targets=c(8))),
                pageLength = 10)) %>% 
  formatStyle('SCIB_5_red',
              target = 'row',
              backgroundColor = styleEqual(c(0, 1), c('white', 'red')))

###################################################################################################
# SCIB-6 ----

# load data
SCIB_6 <- read_excel("./data/SanJose/IUCN RedList counts.xlsx", 
                     sheet = "Arthropoda")

# identify municipalities with threatened species
SCIB_6_table = SCIB_6 %>% 
  mutate(SCIB_6_red=
           case_when(`Near Threatened` > 0 ~ 1,
                     Vulnerable > 0 ~ 1,
                     Endangered > 0 ~ 1,
                     `Critically Endagered`> 0 ~ 1,
                     `Extinct in the Wild`> 0 ~ 1)) %>% 
  mutate(SCIB_6_red = if_else(is.na(SCIB_6_red), 0, SCIB_6_red)) %>% 
  rename("Municipality name" = muni_name)

# plot table region
SCIB_6_table %>% 
  mutate(`Region` = "San Jose metropolitan area",
         `Near Threatened` = sum(`Near Threatened`),
         `Vulnerable` = sum(`Vulnerable`),
         `Endangered` = sum(`Endangered`),
         `Critically Endagered` = sum(`Critically Endagered`),
         `Extinct in the Wild` = sum(`Extinct in the Wild`),
         `Total` = sum(`Total`)) %>% 
  dplyr::select(`Region`,
                `Near Threatened`,
                `Vulnerable`,
                `Endangered`,
                `Critically Endagered`,
                `Extinct in the Wild`,
                `Total`) %>%
  slice(1) %>%
  kable(format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) 


# plot table

DT::datatable(SCIB_6_table,
              options=list(
                columnDefs = list(list(visible=FALSE, targets=c(8))),
                pageLength = 10)) %>% 
  formatStyle('SCIB_6_red',
              target = 'row',
              backgroundColor = styleEqual(c(0, 1), c('white', 'red')))

###################################################################################################
# SCIB-4-5-6 regional ----

SCIB_4_5_6_region = data.frame("Species group" = c("Vascular plants", "Birds", "Arthropods"),
                               "Number of threatened species" = c(sum(SCIB_4$Total),
                                                                  sum(SCIB_5$Total),
                                                                  sum(SCIB_6$Total)))

###################################################################################################
# SCIB-7A-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  # as.data.frame() %>% 
  mutate(`SICB-7A-value` = round(`SICB-7A`*100),2) %>% 
  mutate(`SICB-7A-score`=
           case_when(`SICB-7A-value` < 20 ~ "0",
                     `SICB-7A-value` <  40 ~ "1",
                     `SICB-7A-value` < 60 ~ "2",
                     `SICB-7A-value` < 80 ~ "3",
                     `SICB-7A-value` >= 80 ~ "4"))

SCIB_7A_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-7A-value",
                "SICB-7A-score")



##################################################################################
# GLAD Habitat gain-loss ----

glad_habitat_change = raster("./data/SanJose/gladhabitatchange_epsg4326.tif")

city_glad_habitat_change  = raster::mask(glad_habitat_change,
                                         city_boundary)

glad_habitat_change_v2 = raster("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/CRI-San_Jose-GLAD_habitat_changes-2000_2020.tif")

city_glad_habitat_change  = raster::mask(glad_habitat_change_v2,
                                         city_boundary)


# # mask gain or loss
# glad_habitat_loss_mask = city_glad_habitat_change
# 
# glad_habitat_loss_mask[glad_habitat_loss_mask==0] <- NA
# 
# city_habitat_gain = mask(x = city_glad_habitat_change,
#                              mask = glad_habitat_loss_mask)
##################################################################################
# SCIB-7A-map ----

# define color palette for I1 levels
pal_SCIB_7A <- colorNumeric(palette = "Greens", 
                           domain = biodiversity_baseline_indicators_geo$`SICB-7A-value`,
                           na.color = "transparent",
                           revers = FALSE)

# define labels
labels_SCIB_7A <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                         biodiversity_baseline_indicators_geo$shapeName,
                         "Proportion of habitat restored",
                         round(biodiversity_baseline_indicators_geo$`SICB-7A-value`, 2), "",
                         "Score",biodiversity_baseline_indicators_geo$`SICB-7A-score`) %>% 
  lapply(htmltools::HTML)


# GLAD Habitat changes define a color palette
pal_glad_habitat_change <- colorFactor(palette = c("red","blue"), 
                                       levels = c("0","1"),
                                       na.color = "transparent")
# GLAD Habitat changes labels
glad_habitat_change_labels = c('Habitat loss','Habitat gain')


# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  # addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # Raster of world cover
  addRasterImage(city_worldcover,
                 colors = pal_worldcover,
                 opacity = 0.7,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Land cover types",
                 layerId = "WorldCover") %>%
  addLegend(colors = worldcover_col,
            labels = worldcover_labels,
            title = "World Cover",
            group = "Land cover types",
            position = "bottomleft",
            opacity = 0.7) %>%
  # Raster of habitat changes
  addRasterImage(city_glad_habitat_change,
                 colors = pal_glad_habitat_change,
                 opacity = 0.7,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Habitat changes") %>%
  addLegend(colors = c("red","blue"),
            labels = glad_habitat_change_labels,
            title = "Habitat changes",
            group = "Habitat changes",
            position = "bottomleft",
            opacity = 0.5) %>%
  # `SICB-7A-value`
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Proportion of habitat restored - value",
              fillColor = ~pal_SCIB_7A(`SICB-7A-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_7A,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_7A,
            values = biodiversity_baseline_indicators_geo$`SICB-7A-value`,
            opacity = 0.9,
            title = "Proportion of habitat restored (%)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-7A-score
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Proportion of habitat restored - score",
              fillColor = ~pal_score(`SICB-7A-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_7A,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 80 %)",
                       "3 (60% – 79.9%)",
                       "2 (40% – 59.9%)",
                       "1 (20% – 39.9%)",
                       "0 (< 20%)"),
            opacity = 0.9,
            title = "Proportion of habitat restored (score)",
            group = "Proportion of habitat restored - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Proportion of habitat restored - value",
                      "Proportion of habitat restored - score",
                      "Land cover types",
                      "Habitat changes"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Proportion of habitat restored - score",
              "Land cover types")) %>% 
  addFullscreenControl()


###################################################################################################
# SCIB-7B-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  # as.data.frame() %>% 
  mutate(`SICB-7B-value` = round(`SICB-7B`*100),2) %>% 
  mutate(`SICB-7B-score`=
           case_when(`SICB-7B-value` < 20 ~ "0",
                     `SICB-7B-value` <  40 ~ "1",
                     `SICB-7B-value` < 60 ~ "2",
                     `SICB-7B-value` < 80 ~ "3",
                     `SICB-7B-value` >= 80 ~ "4"))

SCIB_7B_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-7B-value",
                "SICB-7B-score")


##################################################################################
# SCIB-7B-map ----

# define color palette for I1 levels
pal_SCIB_7B <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`SICB-7B-value`,
                            na.color = "transparent",
                            revers = FALSE)

# define labels
labels_SCIB_7B <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                          biodiversity_baseline_indicators_geo$shapeName,
                          "Type of habitat restored",
                          round(biodiversity_baseline_indicators_geo$`SICB-7B-value`, 2), "",
                          "Score",biodiversity_baseline_indicators_geo$`SICB-7B-score`) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  # addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # Raster of world cover
  addRasterImage(city_worldcover,
                 colors = pal_worldcover,
                 opacity = 0.7,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Land cover types",
                 layerId = "WorldCover") %>%
  addLegend(colors = worldcover_col,
            labels = worldcover_labels,
            title = "World Cover",
            group = "Land cover types",
            position = "bottomleft",
            opacity = 0.7) %>%
  # Raster of habitat changes
  addRasterImage(city_glad_habitat_change,
                 colors = pal_glad_habitat_change,
                 opacity = 0.7,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Habitat changes") %>%
  addLegend(colors = c("red","blue"),
            labels = glad_habitat_change_labels,
            title = "Habitat changes",
            group = "Habitat changes",
            position = "bottomleft",
            opacity = 0.5) %>%
  # SICB-7B-value`
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Type of habitat restored - value",
              fillColor = ~pal_SCIB_7B(`SICB-7B-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_7B,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_7B,
            values = biodiversity_baseline_indicators_geo$`SICB-7B-value`,
            opacity = 0.9,
            title = "Type of habitat restored (value)",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-7B-score
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Type of habitat restored - score",
              fillColor = ~pal_score(`SICB-7B-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_7B,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 80 %)",
                       "3 (60% – 79.9%)",
                       "2 (40% – 59.9%)",
                       "1 (20% – 39.9%)",
                       "0 (< 20%)"),
            opacity = 0.9,
            title = "Type of habitat restored (score)",
            group = "Type of habitat restored - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Type of habitat restored - value",
                      "Type of habitat restored - score",
                      "Land cover types",
                      "Habitat changes"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Type of habitat restored - score",
              "Land cover types")) %>% 
  addFullscreenControl()

###################################################################################################
# SCIB-8-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  # as.data.frame() %>% 
  mutate(`SICB-8-value` = round(`SICB-8`*100),2) %>% 
  mutate(`SICB-8-score`=
           case_when(`SICB-8-value` < 1 ~ "0",
                     `SICB-8-value` <  6 ~ "1",
                     `SICB-8-value` < 11 ~ "2",
                     `SICB-8-value` < 17 ~ "3",
                     `SICB-8-value` >= 17 ~ "4"))

SCIB_8_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-8-value",
                "SICB-8-score")


##################################################################################
# WDPA-data-map ----

wdpa_city = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/biodiversity_map_layers_protected_areas_sanjose.geojson",
                                  quiet = TRUE)

##################################################################################
# SCIB-8-map ----

# define color palette for I1 levels
pal_SCIB_8 <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`SICB-8-value`,
                            na.color = "transparent",
                            revers = FALSE)

# define labels
labels_SCIB_8 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                          biodiversity_baseline_indicators_geo$shapeName,
                          "Proportion of protected natural areas",
                          round(biodiversity_baseline_indicators_geo$`SICB-8-value`, 2), "",
                          "Score",biodiversity_baseline_indicators_geo$`SICB-8-score`) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  # addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # add protected areas layer
  addPolygons(data = wdpa_city,
              group = "Protected Areas",
              stroke = TRUE, color = col_BoldGrassGreen, weight = 3,dashArray = "3",
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 5,
                color = col_LightGrassGreen,
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = FALSE)) %>% 
  # SICB-8-score
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Proportion of protected natural areas - score",
              fillColor = ~pal_score(`SICB-8-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_8,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 17 %)",
                       "3 (11.1% – 17%)",
                       "2 (6.1% – 11%)",
                       "1 (1% – 6%)",
                       "0 (< 1%)"),
            opacity = 0.9,
            title = "Proportion of protected </br> natural areas (score)",
            group = "Proportion of protected natural areas - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-8-value
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Proportion of protected natural areas - value",
              fillColor = ~pal_SCIB_8(`SICB-8-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_8,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_8,
            values = biodiversity_baseline_indicators_geo$`SICB-8-value`,
            opacity = 0.9,
            title = "Proportion of protected </br> natural areas (%)",
            group = "Proportion of protected natural areas - value",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Proportion of protected natural areas - value",
                      "Proportion of protected natural areas - score",
                      "Protected Areas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Proportion of protected natural areas - value")) %>% 
  addFullscreenControl()

###################################################################################################
# SCIB-8-chart ----


# prepare chat colors
value_vector = biodiversity_baseline_indicators_geo %>% 
  drop_na(`SICB-8-value`) %>% 
  arrange(desc(`SICB-8-value`)) %>% 
  pull(`SICB-8-score`) %>% 
  as.numeric()

color_vector = pal_score(value_vector)

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(`SICB-8-value`)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(shapeName), 
            y = ~`SICB-8-value`, 
            marker = list(color = color_vector),
            type = "bar",
            text = ~`SICB-8-value`, textposition = 'auto', insidetextfont = list(size=20, color = 'white'),
            orientation = "v")  %>% 
  layout(title = "",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~`SICB-1-value`),
         yaxis = list(title = ''))

###################################################################################################
# SCIB-10-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-10-value` = round(`SICB-10`*100),2) %>%
  # mutate(`SICB-10-value` = 100 - round(`SICB-10`*100),2) %>%
  mutate(`SICB-10-score`=
           case_when(`SICB-10-value` < 30 ~ "0",
                     `SICB-10-value` <  40 ~ "1",
                     `SICB-10-value` < 50 ~ "2",
                     `SICB-10-value` < 60 ~ "3",
                     `SICB-10-value` >= 60 ~ "4"))

SCIB_10_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-10-value",
                "SICB-10-score")


##################################################################################
# impervious-areas ----

impervious_areas = raster("./data/SanJose/SJ data_impervious.tif")

impervious_areas_mask = impervious_areas
impervious_areas_mask[impervious_areas_mask==0] <- NA

city_impervious_areas = mask(x = impervious_areas,
                             mask = impervious_areas_mask)

impervious_areas = raster("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/CRI-San_Jose-GAIA_impervious_surfaces-2018.tif")

impervious_areas_mask = impervious_areas
impervious_areas_mask[impervious_areas_mask==0] <- NA

city_impervious_areas = mask(x = impervious_areas,
                             mask = impervious_areas_mask)

impervious_urban_areas = raster("./data/SanJose/city_glad_built_impervious.tif")

###################################################################################################
# SCIB-10B-table ----

biodiversity_baseline_indicators_SICB_10B <- read.csv("./data/SanJose/biodiversity_baseline_newmunis_SICB_10B.csv")

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  left_join(biodiversity_baseline_indicators_SICB_10B, by = "shapeName")

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-10B-value` = SICB_10B_value) 


##################################################################################
# SCIB-10-map ----

# define color palette for I1 levels
pal_SCIB_10 <- colorNumeric(palette = "Greens", 
                           domain = biodiversity_baseline_indicators_geo$`SICB-10-value`,
                           na.color = "transparent",
                           revers = FALSE)

# define labels
labels_SCIB_10 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                         biodiversity_baseline_indicators_geo$shapeName,
                         "Percent of permeable areas",
                         round(biodiversity_baseline_indicators_geo$`SICB-10-value`, 2), "",
                         "Score",biodiversity_baseline_indicators_geo$`SICB-10-score`) %>% 
  lapply(htmltools::HTML)

# define color palette for pal_SCIB_10B
pal_SCIB_10B <- colorNumeric(palette = "Blues", 
                             domain = biodiversity_baseline_indicators_geo$`SICB-10B-value`,
                             na.color = "gray",
                             revers = FALSE)
# define labels SCIB_10B
labels_SCIB_10B <- sprintf("<strong>%s</strong><br/>%s: %s %s",
                           biodiversity_baseline_indicators_geo$shapeName,
                           "Percent of impermeable surfaces",
                           round(biodiversity_baseline_indicators_geo$`SICB-10B-value`, 2), "") %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # SICB-10B-value
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Impermeable surfaces in urban areas",
              fillColor = ~pal_SCIB_10B(`SICB-10B-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_10B,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_10B,
            values = biodiversity_baseline_indicators_geo$`SICB-10B-value`,
            opacity = 0.9,
            title = "Impermeable surfaces <br/> in urban areas (%)",
            group = "Impermeable surfaces in urban areas",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # Raster of impervious areas
  addRasterImage(city_impervious_areas, 
                 colors = col_BoldRiverBlue,
                 opacity = 0.8,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Impermeable surfaces") %>%
  # SICB-10-value
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Percent of permeable areas - value",
              fillColor = ~pal_SCIB_10(`SICB-10-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_10,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_10,
            values = biodiversity_baseline_indicators_geo$`SICB-10-value`,
            opacity = 0.9,
            title = "Percent of permeable areas (%)",
            group = "Percent of permeable areas - value",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-10-score
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Percent of permeable areas - score",
              fillColor = ~pal_score(`SICB-10-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_10,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 60 %)",
                       "3 (50% – 59.9%)",
                       "2 (40% – 49.9)",
                       "1 (30% – 39.9%)",
                       "0 (< 30%)"),
            opacity = 0.9,
            title = "Percent of permeable areas (score)",
            group = "Percent of permeable areas - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Percent of permeable areas - value",
                      "Percent of permeable areas - score",
                      "Impermeable surfaces in urban areas",
                      "Impermeable surfaces"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Percent of permeable areas - score",
              "Impermeable surfaces in urban areas")) %>% 
  addFullscreenControl()

###################################################################################################
# SCIB-10-chart ----


# prepare chat colors
value_vector = biodiversity_baseline_indicators_geo %>% 
  drop_na(`SICB-10-value`) %>% 
  arrange(desc(`SICB-10-value`)) %>% 
  pull(`SICB-10-score`) %>% 
  as.numeric()

color_vector = pal_score(value_vector)

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(`SICB-10-value`)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(shapeName), 
            y = ~`SICB-10-value`, 
            marker = list(color = color_vector),
            type = "bar",
            text = ~`SICB-10-value`, textposition = 'auto', insidetextfont = list(size=20, color = 'white'),
            orientation = "v")  %>% 
  layout(title = "",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~`SICB-10-value`),
         yaxis = list(title = ''))


##################################################################################
# tml-data ----

# tml = raster("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/trees_outside_forest/CRI-San_Jose-treecover2020.tif")

# tml = raster("./data/SanJose/CRI-San_Jose-tml-2020-10p.tif")

tml = raster("./data/SanJose/CRI-San_Jose-tml-2020.tif")

tml = raster("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/CRI-San_Jose-TML-trees-2020.tif")
city_tml = raster::mask(tml,
                        city_boundary)
city_tml[city_tml<11] = NA

city_tml_aggregate <- raster::aggregate(city_tml, fact=0.0005/res(city_tml))


##################################################################################
# LST  ----

lst = raster("./data/SanJose/LSTmeanRegion.tif")

lst = raster("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/CRI-San_Jose-LSTmean-20210305.tif")

city_lst  = raster::mask(lst,
                         city_boundary)

# define color for for lst subset

city_lst_subset = city_lst
city_lst_subset[city_lst_subset<20] = NA

pal_lst_subset<- colorNumeric("RdYlBu", 
                              values(city_lst_subset),
                              na.color = "transparent",
                              reverse = TRUE)

city_lst_subset_values = values(city_lst_subset)[!is.na(values(city_lst_subset))]
pal_lst_subset_legend <- colorNumeric("RdYlBu", 
                                      city_lst_subset_values,
                                      na.color = "transparent",
                                      reverse = TRUE)
##################################################################################
# compute-SICB-11 ----

biodiversity_baseline_indicators_SICB_11 = data.frame("shapeName" = biodiversity_baseline_indicators$`Municipality name`,
                                                      "SICB_11_value" = 0)

for(i in 1:nrow(boundary_municipality)){
  print(i)
  # get municipality boundary
  boundary_municipality_i = boundary_municipality[i, ]
  # get municipality name
  municipality_name = boundary_municipality_i$shapeName
  # crop raster
  municipality_tml = raster::mask(tml,
                                  boundary_municipality_i)
  #compute average tree cover
  municipality_tml_mean = cellStats(municipality_tml, 'mean')
  # fill data
  biodiversity_baseline_indicators_SICB_11[biodiversity_baseline_indicators_SICB_11$shapeName == municipality_name, "SICB_11_value"] = municipality_tml_mean
  
}


write.csv(biodiversity_baseline_indicators_SICB_11,
          "./data/SanJose/biodiversity_baseline_newmunis_SICB_11.csv")

biodiversity_baseline_indicators_11 <- read.csv("./data/SanJose/biodiversity_baseline_newmunis_SICB_11.csv")


##################################################################################
# SCIB-11-table----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  left_join(biodiversity_baseline_indicators_11, by = "shapeName")

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-11-value` = SICB_11_value) %>% 
  mutate(`SICB-11-score`=
           case_when(`SICB-11-value` <10 ~ "0",
                     `SICB-11-value` <  25 ~ "1",
                     `SICB-11-value` < 40 ~ "2",
                     `SICB-11-value` < 55 ~ "3",
                     `SICB-11-value` >= 55 ~ "4"))


SCIB_11_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-11-value",
                "SICB-11-score")


##################################################################################
# SCIB-11B-table----

biodiversity_baseline_indicators_SICB_11B <- read.csv("./data/SanJose/biodiversity_baseline_newmunis_SICB_11B.csv")

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  left_join(biodiversity_baseline_indicators_SICB_11B, by = "shapeName")

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-11B-value` = SICB_11B_value) 
##################################################################################
# SCIB-11-map ----

# define color for tree cover
pal_tml <- colorNumeric(palette = "Greens",
                        domain = values(city_tml), #values(city_tml_aggregate)
                        na.color = "transparent")


# define color palette for pal_SCIB_11
pal_SCIB_11 <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`SICB-11-value`,
                            na.color = "gray",
                            revers = FALSE)

# define labels
labels_SCIB_11 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                          biodiversity_baseline_indicators_geo$shapeName,
                          "Percent of tree cover",
                          round(biodiversity_baseline_indicators_geo$`SICB-11-value`, 2), "",
                          "Score",biodiversity_baseline_indicators_geo$`SICB-11-score`) %>% 
  lapply(htmltools::HTML)

# define color palette for pal_SCIB_11
pal_SCIB_11B <- colorNumeric(palette = "Greens", 
                             domain = biodiversity_baseline_indicators_geo$`SICB-11B-value`,
                             na.color = "gray",
                             revers = FALSE)
# define labels SCIB_11
labels_SCIB_11B <- sprintf("<strong>%s</strong><br/>%s: %s %s",
                           biodiversity_baseline_indicators_geo$shapeName,
                           "Percent of tree cover",
                           round(biodiversity_baseline_indicators_geo$`SICB-11B-value`, 2), "") %>% 
  lapply(htmltools::HTML)


# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # LST raster subset
  addRasterImage(city_lst_subset,
                 colors = pal_lst_subset,
                 opacity = 0.5,
                 project=FALSE,
                 group = "Land Surface Temperature",
                 maxBytes = 8 * 1024 * 1024) %>%
  # Legend for LST raster
  addLegend(pal = pal_lst_subset_legend,
            values = city_lst_subset_values,
            opacity = 0.6,
            title = "Land Surface Temperature (°C)",
            group = "Land Surface Temperature",
            position = "bottomleft") %>%
  # Raster of tree cover
  addRasterImage(city_tml, #city_tml_aggregate
                 colors = pal_tml,
                 opacity = 0.9,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Tree cover") %>%
  addLegend(pal = pal_tml,
            values = values(city_tml), #values(city_tml_aggregate),
            title = "Tree cover percent",
            group = "Tree cover",
            position = "bottomleft") %>%
  # SICB-11B-value
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Tree cover in urban areas",
              fillColor = ~pal_SCIB_11B(`SICB-11B-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.8,
              label = labels_SCIB_11B,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_11B,
            values = biodiversity_baseline_indicators_geo$`SICB-11B-value`,
            opacity = 0.9,
            title = "Tree cover in urban areas (%)",
            group = "Tree cover in urban areas",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # `SICB-11-value`
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Percent of tree cover - value",
              fillColor = ~pal_SCIB_11(`SICB-11-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.8,
              label = labels_SCIB_11,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_11,
            values = biodiversity_baseline_indicators_geo$`SICB-11-value`,
            opacity = 0.9,
            title = "Percent of tree cover",
            group = "Percent of tree cover - value",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-11-score
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Percent of tree cover - score",
              fillColor = ~pal_score(`SICB-11-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.8,
              label = labels_SCIB_11,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 55 %)",
                       "3 (40% – 54.9%)",
                       "2 (25% – 39.9)",
                       "1 (10.1% – 24.9%)",
                       "0 (< 10%)"),
            opacity = 0.9,
            title = "Percent of tree cover (score)",
            group = "Percent of tree cover - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Percent of tree cover - value",
                      "Percent of tree cover - score",
                      "Tree cover in urban areas",
                      "Land Surface Temperature",
                      "Tree cover"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Percent of tree cover - score",
              "Tree cover in urban areas",
              "Land Surface Temperature")) %>% 
  addFullscreenControl()


###################################################################################################
# SCIB-10-chart ----


# prepare chat colors
value_vector = biodiversity_baseline_indicators_geo %>% 
  drop_na(`SICB-11-value`) %>% 
  arrange(desc(`SICB-11-value`)) %>% 
  pull(`SICB-11-score`) %>% 
  as.numeric()

color_vector = pal_score(value_vector)

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(`SICB-11-value`)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(shapeName), 
            y = ~`SICB-11-value`, 
            marker = list(color = color_vector),
            type = "bar",
            text = ~round(`SICB-11-value`,2), textposition = 'auto', insidetextfont = list(size=20, color = 'white'),
            orientation = "v")  %>% 
  layout(title = "",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~`SICB-10-value`),
         yaxis = list(title = ''))

###################################################################################################
# SCIB-12-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-12-value` = round(`SICB-12`,4)) %>%
  mutate(`SICB-12-score`=
           case_when(`SICB-12-value` < 0.1 ~ "0",
                     `SICB-12-value` <  0.3 ~ "1",
                     `SICB-12-value` < 0.6 ~ "2",
                     `SICB-12-value` < 0.9 ~ "3",
                     `SICB-12-value` >= 0.9 ~ "4"))

SCIB_12_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-12-value",
                "SICB-12-score")

##################################################################################
# osm-data ----

osm_parks_polygons = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/amenity/recreational_servcies/CRI-San_Jose_parks_polygons.geojson",
                    quiet = TRUE)

osm_parks_points = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/amenity/recreational_servcies/CRI-San_Jose_parks_points.geojson",
                             quiet = TRUE)
osm_parks_points_df <- osm_parks_points %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

##################################################################################
# pop-raster-data ----

# pop = raster("./data/SanJose/CRI-San_Jose_pop.tif")
pop = raster("./data/SanJose/CRI-San_Jose-pop2020.tif")
pop = raster("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/CRI-San_Jose-WorldPop-population-2020.tif")

city_pop = raster::mask(pop,
                        city_boundary)

city_pop[is.na(city_pop)] <- 0

pop_values = values(city_pop)[!is.na(values(city_pop))]

pal_pop <- colorNumeric("RdYlBu", 
                        pop_values,
                        na.color = "transparent",
                        reverse = TRUE)

##################################################################################
# pop-data ----


pop = raster("./data/SanJose/CRI-San_Jose_pop.tif")
pop[is.na(pop)] <- 0

city_pop = raster::mask(pop,
                        city_boundary)

pop_values = values(city_pop)[!is.na(values(city_pop))]

pal_pop <- colorNumeric("RdYlBu",#"RdYlBu", 
                        pop_values,
                        na.color = "transparent",
                        reverse = FALSE)

##################################################################################
# SCIB-12-map ----

# define color palette for I1 levels
pal_SCIB_12 <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`SICB-12-value`,
                            na.color = "transparent",
                            revers = FALSE)

# define labels
labels_SCIB_12 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                          biodiversity_baseline_indicators_geo$shapeName,
                          "Recreational services",
                          round(biodiversity_baseline_indicators_geo$`SICB-12-value`, 4), "",
                          "Score",biodiversity_baseline_indicators_geo$`SICB-12-score`) %>% 
  lapply(htmltools::HTML)

# # define labels
# labels_amenity <- sprintf("<strong>%s</strong> %s <br/><strong>%s:</strong> %s",
#                           "Name",osm_parks_points_df$name, 
#                           "Amenity type", osm_parks_points_df$leisure) %>% 
#   lapply(htmltools::HTML)

# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # layer population
  addRasterImage(city_pop,
                 colors = pal_pop ,
                 opacity = 0.9,
                 group = "Population",
                 project=FALSE,
                 maxBytes = 8 * 1024 * 1024,
                 layerId = "Population") %>% 
  # Legend for population 
  addLegend(pal = pal_pop ,
            values = pop_values,
            opacity = 0.9,
            title = "Population count </br> (persons per 100m)",
            group = "Population",
            position = "bottomleft") %>%
  # Plot parks polygons
  addPolygons(data = osm_parks_polygons,
              group = "Park/recreation areas",
              stroke = TRUE, color = "green", weight = 3,dashArray = "1",
              smoothFactor = 0.5, 
              fill = TRUE, 
              fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE)) %>% 
  # SICB-12-value
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Recreational services - value",
              fillColor = ~pal_SCIB_12(`SICB-12-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_12,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_12,
            values = biodiversity_baseline_indicators_geo$`SICB-12-value`,
            opacity = 0.9,
            title = "Recreational services </br> (ha/1000 persons)",
            group = "Recreational services - value",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-12-score
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Recreational services - score",
              fillColor = ~pal_score(`SICB-12-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_12,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 0.9 ha/1000 persons)",
                       "3 (0.7 – 0.9 ha/1000 persons)",
                       "2 (0.4 – 0.6 ha/1000 persons)",
                       "1 (0.1 – 0.3 ha/1000 persons)",
                       "0 (< 0.1 ha/1000 persons)"),
            opacity = 0.9,
            title = "Recreational services (score)",
            group = "Recreational services - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # # Plot parks points
  # addCircleMarkers(osm_parks_points_df$lon, osm_parks_points_df$lat,
  #                  radius = 4,
  #                  color = "black",
  #                  fillColor = col_BoldGrassGreen,
  #                  stroke = TRUE,
  #                  fillOpacity = 0.7,
  #                  weight = 1,
  #                  # popup = ~as.character(name),
  #                  label = labels_amenity,
  #                  group = "Recreational services locations (points)") %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Recreational services - value",
                      "Recreational services - score",
                      "Park/recreation areas",
                      "Population"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Recreational services - value",
              "Park/recreation areas")) %>% 
  addFullscreenControl()

###################################################################################################
# SCIB-12-chart ----


# prepare chat colors
value_vector = biodiversity_baseline_indicators_geo %>% 
  drop_na(`SICB-12-value`) %>% 
  arrange(desc(`SICB-12-value`)) %>% 
  pull(`SICB-12-score`) %>% 
  as.numeric()

color_vector = pal_score(value_vector)

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(`SICB-12-value`)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(shapeName), 
            y = ~`SICB-12-value`, 
            marker = list(color = color_vector),
            type = "bar",
            text = ~round(`SICB-12-value`,2), textposition = 'auto', insidetextfont = list(size=20, color = 'white'),
            orientation = "v")  %>% 
  layout(title = "",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~`SICB-10-value`),
         yaxis = list(title = ''))


###################################################################################################
# SCIB-13-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-13-value` = round(`SICB-13`*100),2) %>%
  mutate(`SICB-13-score`=
           case_when(`SICB-13-value` < 30 ~ "0",
                     `SICB-13-value` <  49.9 ~ "1",
                     `SICB-13-value` < 69.9 ~ "2",
                     `SICB-13-value` < 89.9 ~ "3",
                     `SICB-13-value` >= 90 ~ "4"))

SCIB_13_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "SICB-13-value",
                "SICB-13-score")

##################################################################################
# SCIB-13-map ----

# define color palette for I1 levels
pal_SCIB_13 <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`SICB-13-value`,
                            na.color = "transparent",
                            revers = FALSE)

# define labels
labels_SCIB_13 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                          biodiversity_baseline_indicators_geo$shapeName,
                          "Proximity to parks",
                          round(biodiversity_baseline_indicators_geo$`SICB-13-value`, 2), "",
                          "Score",biodiversity_baseline_indicators_geo$`SICB-13-score`) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # layer population
  addRasterImage(city_pop,
                 colors = pal_pop ,
                 opacity = 0.9,
                 group = "Population",
                 project=FALSE,
                 maxBytes = 8 * 1024 * 1024,
                 layerId = "Population") %>% 
  # Legend for population 
  addLegend(pal = pal_pop ,
            values = pop_values,
            opacity = 0.9,
            title = "Population count </br> (persons per 100m)",
            group = "Population",
            position = "bottomleft") %>%
  # Plot parks polygons
  addPolygons(data = osm_parks_polygons,
              group = "Park/recreation areas",
              stroke = TRUE, color = "green", weight = 3,dashArray = "1",
              smoothFactor = 0.5, 
              fill = TRUE, 
              fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE)) %>% 
  # SICB-13-value
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Proximity to parks - value",
              fillColor = ~pal_SCIB_13(`SICB-13-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_13,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_13,
            values = biodiversity_baseline_indicators_geo$`SICB-13-value`,
            opacity = 0.9,
            title = "Percent of population living </br> within 400m from a park (%)",
            group = "Proximity to parks - value",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-13-score
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Proximity to parks - score",
              fillColor = ~pal_score(`SICB-13-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_13,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 90%)",
                       "3 (70% – 89.9%)",
                       "2 (50% – 69.9%)",
                       "1 (30% – 49.9%)",
                       "0 (< 30%)"),
            opacity = 0.9,
            title = "Proximity to parks (score)",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Proximity to parks - value",
                      "Proximity to parks - score",
                      "Park/recreation areas",
                      "Population"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Proximity to parks - value",
              "Park/recreation areas")) %>% 
  addFullscreenControl()





###################################################################################################
# SCIB-13-chart ----


# prepare chat colors
value_vector = biodiversity_baseline_indicators_geo %>% 
  drop_na(`SICB-13-value`) %>% 
  arrange(desc(`SICB-13-value`)) %>% 
  pull(`SICB-13-score`) %>% 
  as.numeric()

color_vector = pal_score(value_vector)

# plot chart
biodiversity_baseline_indicators_geo %>% 
  arrange(desc(`SICB-13-value`)) %>%
  plot_ly(height = 500, width = 900) %>% 
  add_trace(x = ~factor(shapeName), 
            y = ~`SICB-13-value`, 
            marker = list(color = color_vector),
            type = "bar",
            text = ~round(`SICB-13-value`,2), textposition = 'auto', insidetextfont = list(size=20, color = 'white'),
            orientation = "v")  %>% 
  layout(title = "",
         xaxis = list(title = '', categoryorder = "array",categoryarray = ~`SICB-10-value`),
         yaxis = list(title = ''))

##################################################################################
# kba-table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`KBA-protected-value` = as.numeric(as.character(`KBA(protected)`))) %>% 
  mutate(`KBA-protected-value` = round(`KBA-protected-value`,4)*100) %>%
  mutate(`KBA-built-up-value` = as.numeric(as.character(`KBA(built-up)`))) %>% 
  mutate(`KBA-built-up-value` = round(`KBA-built-up-value`,4)*100)

KBA_table = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate("city_id" = city_id) %>% 
  dplyr::select("city_id",
                "Municipality"= "shapeName",
                "KBA-protected-value",
                "KBA-built-up-value")

###################################################################################################
# SCIB-kba-table-Region ----


biodiversity_metro %>% 
  dplyr::select(`Region`,
                `KBA in protected areas` = `KBA-protected-value`,
                `KBA in built-up areas` =`KBA-protected-built-up`) %>% 
  kable(format = "html", escape = FALSE) %>%
  kable_styling("striped", full_width = FALSE) %>% 
  scroll_box(width = "100%", height = "100px")


##################################################################################
# kba-data-map ----

kba = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/KBA/CRI-San_Jose_kba.geojson",
              quiet = TRUE)

target_crs <- st_crs(4326)
kba_crs <- st_transform(kba, crs = target_crs)

##################################################################################
# kba-map ----

# define color palette for KBA-protected-value
pal_KBA_protected <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`KBA-protected-value`,
                            na.color = "gray",
                            revers = FALSE)
# define color palette for KBA-protected-value
pal_KBA_built <- colorNumeric(palette = "Greens", 
                                  domain = biodiversity_baseline_indicators_geo$`KBA-built-up-value`,
                                  na.color = "gray",
                                  revers = FALSE)

labels_KBA<- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                          biodiversity_baseline_indicators_geo$shapeName,
                          "KBA protected areas",
                          biodiversity_baseline_indicators_geo$`KBA-protected-value`, "",
                          "KBA built-up areas",biodiversity_baseline_indicators_geo$`KBA-built-up-value`) %>% 
  lapply(htmltools::HTML)

# define labels for kba
labels_kba<- sprintf("<strong>%s: %s",
                     "Name",kba_crs$IntName) %>% 
  lapply(htmltools::HTML)

leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # Key Biodiversity Areas
  addPolygons(data = kba_crs,
              group = "Key Biodiversity Areas",
              stroke = TRUE, color = col_BoldGrassGreen, weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = TRUE, fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = labels_kba,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  # `KBA-protected-value`
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "KBA Protected areas",
              fillColor = ~pal_KBA_protected(`KBA-protected-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_KBA,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_KBA_protected,
            values = biodiversity_baseline_indicators_geo$`KBA-protected-value`,
            opacity = 0.9,
            title = "Percent of KBA </br> (protected areas)",
            position = "topright",
            group = "KBA Protected areas",
            labFormat = labelFormat(suffix = "")) %>%
  # `KBA-built-up-value`
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "KBA built-up areas",
              fillColor = ~pal_KBA_built(`KBA-built-up-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_KBA,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_KBA_built,
            values = biodiversity_baseline_indicators_geo$`KBA-built-up-value`,
            opacity = 0.9,
            title = "Percent of KBA </br> (builit-up areas)",
            position = "topright",
            group = "KBA built-up areas",
            labFormat = labelFormat(suffix = "")) %>%
  # # Raster of world cover
  # addRasterImage(city_worldcover, 
  #                colors = pal_worldcover,
  #                opacity = 1,
  #                maxBytes = 20 * 1024 * 1024,
  #                project=FALSE,
  #                group = "World Cover",
  #                layerId = "WorldCover") %>%
  # addLegend(colors = worldcover_col,
  #           labels = worldcover_labels,
  #           title = "World Cover",
  #           group = "World Cover",
  #           position = "bottomleft",
  #           opacity = 1) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("KBA Protected areas",
                      "KBA built-up areas",
                      "Key Biodiversity Areas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("KBA built-up areas")) %>%
  addFullscreenControl()


########################################################################################
# SICB-index ----

color_score_red_0 = "#145A32"
color_score_orange_1 = "#2ECC71"
color_score_yellow_2 = "#F4D03F"
color_score_green_3 = "#E67E22"
color_score_green_4 = "#C0392B"

biodiversity_baseline_scores = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate(`SICB-1-score` = as.numeric(`SICB-1-score`),
         `SICB-2-score` = as.numeric(`SICB-2-score`),
         `SICB-3-score` = as.numeric(`SICB-3-score`),
         `SICB-7A-score` = as.numeric(`SICB-7A-score`),
         `SICB-7B-score` = as.numeric(`SICB-7B-score`),
         `SICB-8-score` = as.numeric(`SICB-8-score`),
         `SICB-10-score` = as.numeric(`SICB-10-score`),
         `SICB-12-score` = as.numeric(`SICB-12-score`),
         `SICB-13-score` = as.numeric(`SICB-13-score`)) %>% 
  dplyr::select("Municipality name" = shapeName,
                "SICB-1 - Percent of natural areas" = `SICB-1-score`,
                "SICB-2 - Connectivity" = `SICB-2-score`,
                "SICB-3 - Birds in built areas" = `SICB-3-score`,
                "SICB-7A - Proportion of habitat restored" = `SICB-7A-score`,
                "SICB-7B - Types of habitat restored" = `SICB-7B-score`,
                "SICB-8 - Protected areas" = `SICB-8-score`,
                "SICB-10 - Permeale areas" = `SICB-10-score`,
                "SICB-12 - Recreational services" = `SICB-12-score`,
                "SICB-13 - Proximity to parks" = `SICB-13-score`) %>% 
  mutate("Biodiversity Index" = rowSums(.[2:10]))

biodiversity_baseline_scores = biodiversity_baseline_indicators_geo %>% 
  as.data.frame() %>% 
  mutate(`SICB-1-score` = as.numeric(`SICB-1-score`),
         `SICB-2-score` = as.numeric(`SICB-2-score`),
         `SICB-3-score` = as.numeric(`SICB-3-score`),
         `SICB-7A-score` = as.numeric(`SICB-7A-score`),
         `SICB-7B-score` = as.numeric(`SICB-7B-score`),
         `SICB-8-score` = as.numeric(`SICB-8-score`),
         `SICB-10-score` = as.numeric(`SICB-10-score`),
         `SICB-12-score` = as.numeric(`SICB-12-score`),
         `SICB-13-score` = as.numeric(`SICB-13-score`)) %>% 
  dplyr::select("Municipality name" = shapeName,
                "SICB-1 - Percent of natural areas" = `SICB-1-score`,
                "SICB-2 - Connectivity" = `SICB-2-score`,
                "SICB-3 - Birds in built areas" = `SICB-3-score`,
                "SICB-7A - Proportion of habitat restored" = `SICB-7A-score`,
                "SICB-7B - Types of habitat restored" = `SICB-7B-score`,
                "SICB-8 - Protected areas" = `SICB-8-score`,
                "SICB-10 - Permeale areas" = `SICB-10-score`,
                "SICB-12 - Recreational services" = `SICB-12-score`,
                "SICB-13 - Proximity to parks" = `SICB-13-score`) %>% 
  mutate("Biodiversity Index" = rowSums(.[2:10])) %>% 
  dplyr::select("Municipality name" ,
                "Biodiversity Index",
                "SICB-1 - Percent of natural areas" ,
                "SICB-2 - Connectivity",
                "SICB-3 - Birds in built areas" ,
                "SICB-7A - Proportion of habitat restored", 
                "SICB-7B - Types of habitat restored" ,
                "SICB-8 - Protected areas" ,
                "SICB-10 - Permeale areas" ,
                "SICB-12 - Recreational services",
                "SICB-13 - Proximity to parks")

# biodiversity_baseline_scores = biodiversity_baseline_scores %>% 
#   mutate_at(vars("SICB-1 - Percent of natural areas":"SICB-13 - Proximity to parks"), ~ cell_spec(
#     ., "html", 
#     bold = TRUE,
#     color = "white",
#     background = ifelse(. >= 4, color_score_green_4, 
#                         ifelse(. >= 3, color_score_green_3, 
#                                ifelse(. >= 2,color_score_yellow_2, 
#                                       ifelse(. >= 1, color_score_orange_1, 
#                                              color_score_red_0))))
#   )) %>% 
#   mutate_at(vars("Biodiversity Index"), ~ cell_spec(
#     ., "html", 
#     font_size = "x-large",
#     bold = TRUE,
#     color = "black",
#     background = ifelse(. >= 9*4, color_score_green_4, 
#                         ifelse(. >= 9*3, color_score_green_3, 
#                                ifelse(. >= 9*2,color_score_yellow_2, 
#                                       ifelse(. >= 9*1, color_score_orange_1, 
#                                              color_score_red_0))))
#   ))

# biodiversity_baseline_scores %>%
#   kable(format = "html", escape = FALSE) %>%
#   kable_styling("striped", full_width = FALSE) %>% 
#   scroll_box(width = "100%", height = "700px")


# plot table


SICB_var = c("SICB-1 - Percent of natural areas","SICB-2 - Connectivity","SICB-3 - Birds in built areas",
             "SICB-7A - Proportion of habitat restored","SICB-7B - Types of habitat restored",
             "SICB-8 - Protected areas","SICB-10 - Permeale areas","SICB-12 - Recreational services",
             "SICB-13 - Proximity to parks")

rownames(biodiversity_baseline_scores) = biodiversity_baseline_scores$`Municipality name`
DT::datatable(biodiversity_baseline_scores,
              extensions = c('Scroller','FixedColumns'), 
              options = list(
                columnDefs = list(list(visible=FALSE, targets=c(1))),
                order = list(list(2, 'desc')),
                pageLength = 34,
                dom = 't',
                scrollX = TRUE,
                deferRender = TRUE,
                scrollY = 800,
                fixedColumns = TRUE)) %>% formatStyle(
                  SICB_var,
                  fontWeight = "bold",
                  color = "white",
                  textAlign = "center",
                  backgroundColor = styleInterval(c(0, 1,2,3), 
                                                  c(color_score_green_4,color_score_green_3,color_score_yellow_2,color_score_orange_1,color_score_red_0))
                ) %>% formatStyle(
                  "Biodiversity Index" ,
                  fontWeight = "bold",
                  color = "white",
                  fontSize = "20px",
                  textAlign = "center",
                  backgroundColor = styleInterval(c(0, 9*1,9*2,9*3), 
                                                  c(color_score_green_4,color_score_green_3,color_score_yellow_2,color_score_orange_1,color_score_red_0))
                )

##################################################################################
# esa-map ----

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

worldcover_col = c(Trees_10_green,
                   Shrubland_20_orange,
                   Grassland_30_yellow,
                   Cropland_40_mauve,
                   Built_up_50_red,
                   Barren_sparse_vegetation_60_gray,
                   Snow_ice_70_white,
                   Open_Water_80_blue,
                   Herbaceous_wetland_90_blue2,
                   Mangroves_95_green2,
                   Moss_lichen_100_beige)

worldcover_labels = c('Trees','Shrubland','Grassland','Cropland','Built-up',
                      'Barren / sparse vegetation','Snow/ice','Open water','Herbaceous wetland',
                      'Mangroves','Moss/lichen')


# define a color palette
pal_worldcover <- colorFactor(palette = worldcover_col, 
                              # domain = values(city_worldcover),
                              levels = c("10","20","30","40","50","60",
                                         "70","80","90","95","100"),
                              na.color = "transparent")

# create the map
map = leaflet(city_worldcover, height = 500, width = "100%")  %>%
  addTiles() %>%
  # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  # addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "gray", weight = 3,dashArray = "3",
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
  addRasterImage(city_worldcover, 
                 colors = pal_worldcover,
                 opacity = 1,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "World Cover",
                 layerId = "WorldCover") %>%
  addLegend(colors = worldcover_col,
            labels = worldcover_labels,
            title = "World Cover",
            opacity = 1) %>%
  # addRasterImage(city_worldcover_natural_areas, 
  #                colors = "#65B96B",
  #                opacity = 1,
  #                maxBytes = 20 * 1024 * 1024,
  #                project=FALSE,
  #                group = "Natural Areas",
  #                layerId = "Natural Areas") %>%
  addLayersControl(
    # baseGroups = c("Toner Lite", "OSM","CartoDB"),
    overlayGroups = c("World Cover","Administrative boundaries", "Natural Areas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addOpacityControls(group = c("Natural Areas"), #c("World Cover","Natural Areas")
                     collapsed = FALSE, position = "topright",
                     title = "Opacity control")  %>% 
  addFullscreenControl()



map



##################################################################################
# osm-map ----

park_icon <- makeIcon(
  iconUrl = "./data/SanJose/park_icon.png",
  iconWidth = 15, iconHeight = 15,
)

# plot map
leaflet(height = 700, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  # Plot parks polygons
  addPolygons(data = osm_parks_polygons,
              group = "Recreational services (areas)",
              stroke = TRUE, color = "green", weight = 3,dashArray = "1",
              smoothFactor = 0.5, fill = TRUE, fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE)) %>% 
  # Plot parks points
  addCircleMarkers(osm_parks_points_df$lon, osm_parks_points_df$lat,
                   radius = 6,
                   color = "black",
                   fillColor = col_BoldGrassGreen,
                   stroke = TRUE,
                   fillOpacity = 0.7,
                   weight = 2,
                   # popup = ~as.character(name),
                   label = labels_amenity,
                   group = "Recreational services locations (points)") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 5,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # addMarkers(osm_parks_points_df$lon,
  #            osm_parks_points_df$lat,
  #            icon = park_icon,
  #            group= "Parks location") %>% 
  # Layers control
  addLayersControl(
    # baseGroups = c("Porportion of natural areas - value","Porportion of natural areas - score"),
    overlayGroups = c("Administrative boundaries",
                      "Recreational services (areas)",
                      "Recreational services locations (points)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  # hideGroup(c("Recreational services - score",
  #             "Recreational services (areas)",
  #             "Recreational services locations (points)")) %>% 
  addFullscreenControl()


##################################################################################
# habitat ----
habitat = raster("./data/SanJose/new_habitat.tif")

projected_habitat <- projectRaster(habitat, crs = "+proj=longlat +datum=WGS84 +no_defs")

city_habitat = raster::mask(habitat,
                            city_boundary)
# Write the RasterLayer to disk (See datatype documentation for other formats)
writeRaster(projected_raster, filename="C:/temp/binary_utm15.tif", datatype='INT1U', overwrite=TRUE)


##################################################################################
# GLAD built-up ----

glad_built = raster("./data/SanJose/gladbuiltup2020_epsg4326.tif")
city_glad_built  = raster::mask(glad_built,
                                city_boundary)


city_glad_built_mask = city_glad_built

city_glad_built_mask[!city_glad_built_mask>0] <- NA

city_glad_built_layer = mask(x = city_glad_built,
                             mask = city_glad_built_mask)

##################################################################################
# Fig 2 ----

# define color palette for scores
pal_score <- colorFactor(palette = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"), 
                         levels = c("0","1","2","3","4"),
                         na.color = "transparent",
                         revers = TRUE)

# SICB-2 color palettes
pal_SCIB_2 <- colorNumeric(palette = "Greens", 
                           domain = biodiversity_baseline_indicators_geo$`SICB-2-value`,
                           na.color = "transparent",
                           revers = FALSE)

# SICB-2 labels
labels_SCIB_2 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                         biodiversity_baseline_indicators_geo$shapeName,
                         "Connectivity value",
                         round(biodiversity_baseline_indicators_geo$`SICB-2-value`, 2), "",
                         "Score",biodiversity_baseline_indicators_geo$`SICB-2-score`) %>% 
  lapply(htmltools::HTML)


# SICB-7A color palettes
pal_SCIB_7A <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`SICB-7A-value`,
                            na.color = "transparent",
                            revers = FALSE)

# SICB-7A labels
labels_SCIB_7A <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                          biodiversity_baseline_indicators_geo$shapeName,
                          "Proportion of habitat restored",
                          round(biodiversity_baseline_indicators_geo$`SICB-7A-value`, 2), "",
                          "Score",biodiversity_baseline_indicators_geo$`SICB-7A-score`) %>% 
  lapply(htmltools::HTML)

# GLAD Habitat changes define a color palette
pal_glad_habitat_change <- colorFactor(palette = c("red","blue"), 
                                       levels = c("0","1"),
                                       na.color = "transparent")
# GLAD Habitat changes labels
glad_habitat_change_labels = c('Habitat loss','Habitat gain')


# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addMapPane("Land Cover Natural areas", zIndex = 410) %>% 
  addMapPane("Habitat changes", zIndex = 420) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  # addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # Raster of world cover
  addRasterImage(city_worldcover_natural_areas,
                 colors = "#65B96B",
                 opacity = 0.4,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 # options = pathOptions(pane = "Land Cover Natural areas",
                 group = "Land Cover Natural areas") %>%
  # Raster of habitat changes
  addRasterImage(city_glad_habitat_change,
                 colors = pal_glad_habitat_change,
                 opacity = 0.7,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 # options = pathOptions(pane = "Habitat changes"),
                 group = "Habitat changes") %>%
  addLegend(colors = c("red","blue"),
            labels = glad_habitat_change_labels,
            title = "Habitat changes",
            group = "Habitat changes",
            position = "bottomleft",
            opacity = 0.5) %>%
  # SICB-2 score layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Connectivity - score",
              fillColor = ~pal_score(`SICB-2-score`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.5,
              label = labels_SCIB_2,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  # SICB-2 score legend
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 79 %)",
                       "3 (60% – 79%)",
                       "2 (40% – 59.9%)",
                       "1 (20% – 39.9%)",
                       "0 (< 20%)"),
            opacity = 0.5,
            title = "Connectivity score",
            group = "Connectivity - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-7A value layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Proportion of habitat restored",
              fillColor = ~pal_SCIB_7A(`SICB-7A-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_7A,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_7A,
            values = biodiversity_baseline_indicators_geo$`SICB-7A-value`,
            opacity = 0.9,
            title = "Proportion of habitat restored (%)",
            group = "Proportion of habitat restored",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Proportion of habitat restored",
                      "Connectivity - score",
                      "Habitat changes",
                      "Land Cover Natural areas"),
    options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
  ) %>% 
  hideGroup(c("Connectivity - score","Land Cover Natural areas")) %>% 
  addFullscreenControl()



##################################################################################
# SICB-11-B ----

# city_tml_30m = raster("./data/SanJose/CRI-San_Jose-tml-2020-30m.tif")
# 
# city_tml_30m  = raster::mask(city_tml_30m,
#                                 city_boundary)
# 
# fct_aggregate = res(city_glad_built_layer)/res(city_tml)
# city_tml_aggregate = aggregate(city_tml, fact=fct_aggregate)
# 

# prepare rasters
city_glad_built_layer[city_glad_built_layer == 250] = 1
city_glad_built_tml_10m <- resample(city_glad_built_layer, city_tml)
city_glad_built_tml_10m_percent = city_glad_built_tml_10m * city_tml

writeRaster(city_glad_built_tml_10m_percent,
            "./data/SanJose/city_glad_built_tml_10m_percent.tif")


# compute indicator

biodiversity_baseline_indicators_SICB_11B = data.frame("shapeName" = biodiversity_baseline_indicators$`Municipality name`,
                                                      "SICB_11B_value" = 0)


for(i in 1:nrow(boundary_municipality)){
  print(i)
  # get municipality boundary
  boundary_municipality_i = boundary_municipality[i, ]
  # get municipality name
  municipality_name = boundary_municipality_i$shapeName
  print(municipality_name)
  # crop raster
  municipality_built_tml = raster::mask(city_glad_built_tml_10m_percent,
                                        boundary_municipality_i)
  #compute average tree cover
  municipality_built_tml_mean = cellStats(municipality_built_tml, 'mean')
  print(municipality_built_tml_mean)
  # fill data
  biodiversity_baseline_indicators_SICB_11B[biodiversity_baseline_indicators_SICB_11B$shapeName == municipality_name, "SICB_11B_value"] = municipality_built_tml_mean
  
}

write.csv(biodiversity_baseline_indicators_SICB_11B,
          "./data/SanJose/biodiversity_baseline_newmunis_SICB_11B.csv")


##################################################################################
# SCIB-11B-table----

biodiversity_baseline_indicators_SICB_11B <- read.csv("./data/SanJose/biodiversity_baseline_newmunis_SICB_11B.csv")

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  left_join(biodiversity_baseline_indicators_SICB_11B, by = "shapeName")

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-11B-value` = SICB_11B_value) 


##################################################################################
# SICB-10-B ----

city_impervious  = raster::mask(impervious_areas,
                                city_boundary)

# prepare rasters
city_glad_built_layer[city_glad_built_layer == 250] = 1
city_glad_built_resampled <- resample(city_glad_built_layer, city_impervious)
city_glad_built_impervious = city_glad_built_resampled * city_impervious


writeRaster(city_glad_built_impervious,
            "./data/SanJose/city_glad_built_impervious.tif")

# compute indicator

biodiversity_baseline_indicators_SICB_10B = data.frame("shapeName" = biodiversity_baseline_indicators$`Municipality name`,
                                                       "SICB_10B_value" = 0)


for(i in 1:nrow(boundary_municipality)){
  print(i)
  # get municipality boundary
  boundary_municipality_i = boundary_municipality[i, ]
  # get municipality name
  municipality_name = boundary_municipality_i$shapeName
  # crop raster
  municipality_built_impervious = raster::mask(city_glad_built_impervious,
                                               boundary_municipality_i)
  # compute percent of previous in built areas
  municipality_built_impervious_stat = as.data.frame(municipality_built_impervious) %>%
    rename_at(1, ~"class" ) %>% 
    drop_na(class) %>% 
    mutate(class = as.factor(class))%>% 
    group_by(class) %>%
    tally() %>%
    mutate(area = n * res(municipality_built_impervious)[1] * res(municipality_built_impervious)[2]) %>% 
    dplyr::select(land_cover_classes = class,
                  nb_cells = n) %>% 
    mutate(land_cover_percent = round(nb_cells/sum(nb_cells) * 100,2)) %>% 
    dplyr::filter(land_cover_classes == "1") %>% 
    pull(land_cover_percent)
  # fill data
  biodiversity_baseline_indicators_SICB_10B[biodiversity_baseline_indicators_SICB_10B$shapeName == municipality_name, "SICB_10B_value"] = municipality_built_impervious_stat
  
}

biodiversity_baseline_indicators_SICB_10B = biodiversity_baseline_indicators_SICB_11B %>% 
  dplyr::select(shapeName,SICB_10B_value)

write.csv(biodiversity_baseline_indicators_SICB_10B,
          "./data/SanJose/biodiversity_baseline_newmunis_SICB_10B.csv")

biodiversity_baseline_indicators_SICB_10B <- read.csv("./data/SanJose/biodiversity_baseline_newmunis_SICB_10B.csv")


##################################################################################
# SICB-10-B table ----

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  left_join(biodiversity_baseline_indicators_SICB_10B, by = "shapeName")

# biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>%
#   dplyr::select(-c("X","SICB_10B_value.y","SICB_10B_value.x"))

biodiversity_baseline_indicators_geo = biodiversity_baseline_indicators_geo %>% 
  mutate(`SICB-10B-value` = SICB_10B_value) 

##################################################################################
# Fig 3 ----

# define color for for lst
pal_lst<- colorNumeric("RdYlBu", 
                       values(city_lst),
                         na.color = "transparent",
                         reverse = TRUE)

city_lst_values = values(city_lst)[!is.na(values(city_lst))]
pal_lst_legend <- colorNumeric("RdYlBu", 
                               city_lst_values,
                               na.color = "transparent",
                               reverse = TRUE)

# define color for for lst subset

city_lst_subset = city_lst
city_lst_subset[city_lst_subset<20] = NA

pal_lst_subset<- colorNumeric("RdYlBu", 
                       values(city_lst_subset),
                       na.color = "transparent",
                       reverse = TRUE)

city_lst_subset_values = values(city_lst_subset)[!is.na(values(city_lst_subset))]
pal_lst_subset_legend <- colorNumeric("RdYlBu", 
                                      city_lst_subset_values,
                               na.color = "transparent",
                               reverse = TRUE)

# define color for tree cover
pal_tml <- colorNumeric(palette = "Greens",
                        # domain = values(city_tml_aggregate),
                        domain = values(city_tml),
                        na.color = "transparent")

# define color palette for pal_SCIB_11
pal_SCIB_11B <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`SICB-11B-value`,
                            na.color = "gray",
                            revers = FALSE)
# define labels SCIB_11
labels_SCIB_11B <- sprintf("<strong>%s</strong><br/>%s: %s %s",
                           biodiversity_baseline_indicators_geo$shapeName,
                           "Percent of tree cover",
                           round(biodiversity_baseline_indicators_geo$`SICB-11B-value`, 2), "") %>% 
  lapply(htmltools::HTML)

# define color palette for pal_SCIB_10B
pal_SCIB_10B <- colorNumeric(palette = "Blues", 
                             domain = biodiversity_baseline_indicators_geo$`SICB-10B-value`,
                             na.color = "gray",
                             revers = FALSE)
# define labels SCIB_10B
labels_SCIB_10B <- sprintf("<strong>%s</strong><br/>%s: %s %s",
                           biodiversity_baseline_indicators_geo$shapeName,
                           "Percent of impermeable surfaces",
                           round(biodiversity_baseline_indicators_geo$`SICB-10B-value`, 2), "") %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # # LST raster
  # addRasterImage(city_lst,
  #                colors = pal_lst,
  #                opacity = 0.6,
  #                project=FALSE,
  #                group = "Land Surface Temperature",
  #                maxBytes = 8 * 1024 * 1024) %>%
  # # Legend for LST raster
  # addLegend(pal = pal_lst_legend,
  #           values = city_lst_values,
  #           opacity = 0.6,
  #           title = "Land Surface Temperature (°C)",
  #           group = "Land Surface Temperature",
  #           position = "bottomleft") %>%
  # LST raster subset
  addRasterImage(city_lst_subset,
                 colors = pal_lst_subset,
                 opacity = 0.5,
                 project=FALSE,
                 group = "Land Surface Temperature",
                 maxBytes = 8 * 1024 * 1024) %>%
  # Legend for LST raster
  addLegend(pal = pal_lst_subset_legend,
            values = city_lst_subset_values,
            opacity = 0.6,
            title = "Land Surface Temperature (°C)",
            group = "Land Surface Temperature",
            position = "bottomleft") %>%
  # Raster of impervious areas
  addRasterImage(city_impervious_areas, 
                 colors = col_BoldRiverBlue,
                 opacity = 0.8,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Impermeable surfaces") %>%
  # Raster of tree cover
  addRasterImage(city_tml,
                 colors = pal_tml,
                 opacity = 0.6,
                 maxBytes = 20 * 1024 * 1024,
                 project=FALSE,
                 group = "Tree cover") %>%
  addLegend(pal = pal_tml,
            values = values(city_tml), #values(city_tml_aggregate),
            title = "Tree cover percent",
            group = "Tree cover",
            position = "bottomleft") %>%
  # SICB-10B-value
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Percent of impermeable surfaces in urban areas",
              fillColor = ~pal_SCIB_10B(`SICB-10B-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_10B,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_10B,
            values = biodiversity_baseline_indicators_geo$`SICB-10B-value`,
            opacity = 0.9,
            title = "Percent of urban areas </br> with impermeable surfaces (%)",
            group = "Percent of impermeable surfaces in urban areas",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-11B-value
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Percent of tree cover in urban areas",
              fillColor = ~pal_SCIB_11B(`SICB-11B-value`),
              weight = 1,
              opacity = 1,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_11B,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_SCIB_11B,
            values = biodiversity_baseline_indicators_geo$`SICB-11B-value`,
            opacity = 0.9,
            title = "Percent of urban areas </br> with tree cover (%)",
            group = "Percent of tree cover in urban areas",
            position = "topright",
            labFormat = labelFormat(suffix = "")) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Percent of tree cover in urban areas",
                      "Percent of impermeable surfaces in urban areas",
                      "Land Surface Temperature",
                      "Impermeable surfaces",
                      "Tree cover"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Land Surface Temperature",
              "Percent of tree cover in urban areas",
              "Percent of impermeable surfaces in urban areas"
              )) %>%
  addFullscreenControl()

##################################################################################
# Fig 4 ----

# SCIB_12 define color palette for I1 levels
pal_SCIB_12 <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`SICB-12-value`,
                            na.color = "transparent",
                            revers = FALSE)

# SCIB_12 define labels
labels_SCIB_12 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                          biodiversity_baseline_indicators_geo$shapeName,
                          "Recreational services",
                          round(biodiversity_baseline_indicators_geo$`SICB-12-value`, 4), "",
                          "Score",biodiversity_baseline_indicators_geo$`SICB-12-score`) %>% 
  lapply(htmltools::HTML)

# SCIB_13 define color palette for SCIB_13
pal_SCIB_13 <- colorNumeric(palette = "Greens", 
                            domain = biodiversity_baseline_indicators_geo$`SICB-13-value`,
                            na.color = "transparent",
                            revers = FALSE)

# SCIB_13 define labels
labels_SCIB_13 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                          biodiversity_baseline_indicators_geo$shapeName,
                          "Proximity to parks",
                          round(biodiversity_baseline_indicators_geo$`SICB-13-value`, 2), "",
                          "Score",biodiversity_baseline_indicators_geo$`SICB-13-score`) %>% 
  lapply(htmltools::HTML)

# plot map
leaflet(height = 500, width = "100%") %>%
  addTiles() %>%
  setView(lng = city_boundary_centroid_lng,
          lat = city_boundary_centroid_lat,
          zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "black", weight = 3,dashArray = "3",
              smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
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
  # SICB-12-score
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Recreational services - score",
              fillColor = ~pal_score(`SICB-12-score`),
              weight = 1,
              opacity = 0.8,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_12,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  # SICB-12-score score legend
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 0.9 ha/1000 persons)",
                       "3 (0.7 – 0.9 ha/1000 persons)",
                       "2 (0.4 – 0.6 ha/1000 persons)",
                       "1 (0.1 – 0.3 ha/1000 persons)",
                       "0 (< 0.1 ha/1000 persons)"),
            opacity = 0.9,
            title = "Recreational services (score)",
            group = "Recreational services - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # SICB-13- score layer
  addPolygons(data = biodiversity_baseline_indicators_geo,
              group = "Proximity to parks - score",
              fillColor = ~pal_score(`SICB-13-score`),
              weight = 1,
              opacity = 0.8,
              color = "grey",
              fillOpacity = 0.7,
              label = labels_SCIB_13,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 6px"),
                textsize = "15px",
                direction = "auto")) %>%
  # SICB-13- score legend
  addLegend(colors = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
            labels = c("4 (> 90%)",
                       "3 (70% – 89.9%)",
                       "2 (50% – 69.9%)",
                       "1 (30% – 49.9%)",
                       "0 (< 30%)"),
            opacity = 0.9,
            title = "Proximity to parks (% of population </br> living within 400m from parks)",
            group = "Proximity to parks - score",
            position = "bottomright",
            labFormat = labelFormat(suffix = "")) %>%
  # Plot parks polygons
  addPolygons(data = osm_parks_polygons,
              group = "Recreational services",
              stroke = TRUE, color = "green", weight = 3,dashArray = "1",
              smoothFactor = 0.5, fill = TRUE, fillOpacity = 0.4,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE)) %>% 
  # layer population
  addRasterImage(city_pop,
                 colors = pal_pop ,
                 opacity = 0.9,
                 group = "Population count",
                 project=FALSE,
                 maxBytes = 8 * 1024 * 1024,
                 layerId = "Population") %>% 
  # Legend for population 
  addLegend(pal = pal_pop ,
            values = pop_values,
            opacity = 0.9,
            title = "Population count </br> (persons per 100m)",
            group = "Population count",
            position = "bottomleft") %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Proximity to parks - score",
                      "Recreational services",
                      "Recreational services - score",
                      "Population count"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Recreational services - score",
              "Proximity to parks - score")) %>% 
  addFullscreenControl()
