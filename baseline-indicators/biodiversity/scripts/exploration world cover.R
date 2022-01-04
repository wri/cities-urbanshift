worldcoverc_data_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/indicators_biodiversity_CRI-San_Jose-esa-worldcover-2020.tif"

city_worldcover_raster= raster(worldcoverc_data_path)

# mask raster based on administrative boundaries
city_worldcover  = raster::mask(city_worldcover_raster,boundary)

worldcoverc_data_path_test = "./github/cities-urbanshift/baseline-indicators/biodiversity/scripts/test.tif"
city_worldcover_raster_test= raster(worldcoverc_data_path_test)
city_worldcover_test  = raster::mask(city_worldcover_raster_test,boundary)

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
                   
                   Grassland_30,
                   Cropland_40,
                   Built_up_50,
                   Barren_sparse_vegetation_60,
                   Snow_ice_70,
                   Open_Water_80,
                   Herbaceous_wetland_90,
                   Mangroves_95,
                   Moss_lichen_100)

worldcover_col = c(Trees_10,
                   Grassland_30,
                   Cropland_40,
                   Open_Water_80,
                   Built_up_50,
                   Barren_sparse_vegetation_60,
                   Shrubland_20)

# define a color palette
pal_worldcover <- colorFactor(worldcover_col, 
                              values(city_worldcover_test),
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
  addRasterImage(city_worldcover_test, 
                 colors = pal_worldcover, 
                 opacity = 0.7,
                 maxBytes = 20 * 1024 * 1024,
                 group = "World Cover") %>%
  addLegend(pal = pal_worldcover,
            values = values(city_worldcover_test),
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





# read all boundaries
urbanshift_boundaries = st_read("./github/cities-urbanshift/geospatial-layers/data/boundaries/administrative_boundaries.geojson",
                                quiet = TRUE)

# get list of cities
urbanshift_cities = unique(urbanshift_boundaries$city_name)

city_id = urbanshift_cities[1]

# extract city boundary

city_boundary = urbanshift_boundaries[urbanshift_boundaries$city_name == city_id, ]

# read world cover by city

worldcoverc_data_path = paste("./github/cities-urbanshift/baseline-indicators/biodiversity/scripts/data/world_cover_esa/world_cover_",
                              city_id,
                              ".tif",
                              sep = "")
city_worldcover_raster = raster(worldcoverc_data_path)
city_worldcover  = raster::mask(city_worldcover_raster,
                                city_boundary)

# plot city world cover

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

worldcover_col = c(Grassland_30,
                   Barren_sparse_vegetation_60,
                   Shrubland_20,
                   Trees_10,
                   Snow_ice_70,
                   Open_Water_80,
                   Built_up_50,
                   Cropland_40,
                   Moss_lichen_100,
                   Herbaceous_wetland_90)

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
  addPolygons(data = city_boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = "gray", weight = 1,dashArray = "3",
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


