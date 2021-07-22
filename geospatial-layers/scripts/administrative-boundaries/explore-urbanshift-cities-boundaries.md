UrbanShift - Administrative boundaries
================
Saif Shabou
22 July 2021

``` r
library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)
library(httr)
library(jsonlite)
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(patchwork)
library(maps)
```

# Application to UrbanShift cities

``` r
# load data Urbanshift cities geojson


# UrbanShift_cities <- st_read("cities-urbanshift/geospatial-layers/data/raw/administrative-boundaries/urbanshift-cities/UrbanShift_cities.geojson",
#                              quiet = TRUE)
# 
# UrbanShift_cities %>% 
#   kable() %>%
#   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13) %>% 
#   scroll_box(width = "100%", height = "400px")
# 
# # load data Urbanshift secondary cities geojson
# 
# UrbanShift_cities <- st_read("cities-urbanshift/geospatial-layers/data/raw/administrative-boundaries/urbanshift-cities/UrbanShift_secondarycities.geojson",
#                              quiet = TRUE)
# 
# UrbanShift_secondarycities %>% 
#   kable() %>%
#   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13) %>% 
#   scroll_box(width = "100%", height = "400px")
# 
# 
# 
# UrbanShift_cities = UrbanShift_cities %>% 
#   # add geoType column
#   mutate(geomType = st_geometry_type(UrbanShift_cities, by_geometry = TRUE)) %>% 
#   # filter on geomType
#   filter(geomType %in% c("POLYGON", "MULTIPOLYGON")) %>% 
#   # Compute centroid
#   mutate(geomCentroid = st_centroid(geometry)) %>% 
#   # add x,y column
#   mutate(geomCentroidToSplit = as.character(geomCentroid)) %>% 
#   mutate(geomCentroidToSplit = str_remove(geomCentroidToSplit, "c\\(")) %>% 
#   mutate(geomCentroidToSplit = str_remove(geomCentroidToSplit, "\\)")) %>% 
#   separate(geomCentroidToSplit, c("long", "lat"), sep = ", ") %>% 
#   mutate_at(c("long","lat"), as.numeric)

  
# plot 1

# ggplot(data = world) +
#   geom_sf() +
#   geom_sf(data = UrbanShift_cities$geomCentroid, 
#           fill = col_BoldBrickOrange, color = col_BoldRiverBlue,pch = 21, size = 3) +
#   geom_sf_label(data = UrbanShift_cities, aes(label = name),size=3,color = col_BoldGrassGreen,fill = col_White,
#                hjust=1, vjust=2,
#                nudge_x = 5,nudge_y = 5,
#                check_overlap = TRUE,
#                label.size = 1) +
#   theme_minimal()

# plot 2

# ggplot(data = world) +
#   geom_sf() +
#   geom_sf(data = UrbanShift_cities$geomCentroid, 
#           fill = col_BoldBrickOrange, color = col_BoldRiverBlue,pch = 21, size = 3) +
#   # geom_sf_text(data = UrbanShift_cities, aes(label = name),size=2,color = col_BoldGrassGreen,
#   #               hjust=1, vjust=1,
#   #               nudge_x = 5,nudge_y = 5) +
#   theme_minimal()

# plot 3

# ggplot(UrbanShift_cities, aes(x=long, y= lat)) +   
#   borders("world", colour=NA, fill=col_LightEarthGrey)  +
#   geom_point(color=col_BoldBrickOrange, alpha = .8, size = 2) +
#   # theme(panel.background = element_rect(fill = col_LightRiverBlue, colour = col_LightRiverBlue)) +
#   geom_text(aes(x=long, y= lat, label=name),
#             color = col_BoldRiverBlue, check_overlap = F, size = 2,hjust=1, vjust=1.5) +
#   theme_minimal()
```