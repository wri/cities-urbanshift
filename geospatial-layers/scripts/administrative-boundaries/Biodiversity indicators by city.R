
# read all boundaries
urbanshift_boundaries = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/administrative_boundaries.geojson",
                                quiet = TRUE)

urbanshift_boundaries = urbanshift_boundaries[urbanshift_boundaries$boundary_use == TRUE, ]


# get list of cities
urbanshift_cities = unique(urbanshift_boundaries$city_name)
urbanshift_cities = urbanshift_cities[! urbanshift_cities %in% c("ARG-Mar_del_Plata",
                                                                 "IDN-Balikpapan",
                                                                 "CHN-Ningbo")]

# specify the cityid
city_id = "CRI-San_Jose"

# extract city boundary

city_boundary = urbanshift_boundaries[urbanshift_boundaries$city_name == city_id, ]
city_boundary

# read world cover by city

worldcoverc_data_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/esa_world_cover/world_cover_",
                              city_id,
                              ".tif",
                              sep = "")

# worldcoverc_data_path = paste("./github/cities-urbanshift/baseline-indicators/biodiversity/scripts/data/world_cover_esa/world_cover_",
#                               city_id,
#                               ".tif",
#                               sep = "")
city_worldcover_raster = raster(worldcoverc_data_path)
city_worldcover  = raster::mask(city_worldcover_raster,
                                     city_boundary)


# plot city world cover

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

worldcover_col = c(Trees_10_green,
                   Shrubland_20_orange,
                   Grassland_30_yellow,
                   Cropland_40_mauve,
                   Built_up_50_red,
                   Barren_sparse_vegetation_60_gray,
                   Snow_ice_70,
                   Open_Water_80_blue,
                   Herbaceous_wetland_90,
                   Mangroves_95,
                   Moss_lichen_100)

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
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
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
                 opacity = 0.9,
                 maxBytes = 20 * 1024 * 1024,
                 group = "World Cover") %>%
  addLegend(colors = worldcover_col,
            labels = worldcover_labels,
            title = "World Cover") %>%
  addLayersControl(
    baseGroups = c("Toner Lite", "OSM","CartoDB"),
    overlayGroups = c("World Cover","Administrative boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  )

map


# compute world cover stats

world.cover.city.stat = function(city_worldcover, year){
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
  
  return(city_worldcover_stat)
}

city_worldcover_stat = world.cover.city.stat(city_worldcover = city_worldcover,
                                             year = 2020)

# Baseline indicator ----

# initialize empty dataframe for storing indicators
biodiversity_indicators = data.frame(city_id = as.character(),
                                     indicator_name = as.character(),
                                     indicator_number = as.character(),
                                     value = as.numeric(),
                                     score = as.numeric())

# I1- proportion of natural areas ----

natural_areas_classes = c('Trees','Shrubland','Grassland','Snow/ice','Open water','Herbaceous wetland',
                      'Mangroves','Moss/lichen')

# compute indicator
city_worldcover_stat %>% 
  mutate(natural_areas_class =
           case_when(`land cover class` %in% natural_areas_classes ~ "Natural areas", 
                     !`land cover class` %in% natural_areas_classes ~ "Non Natural areas")
  ) %>% 
  group_by(natural_areas_class) %>% 
  summarise(natural_areas_percent = sum(`land percent`)) %>% 
  filter(natural_areas_class == "Natural areas") %>% 
  add_column(city_id = city_id,
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
                indicator_name,
                indicator_number,
                value,
                score)
         
compute.i1 = function(city_id,city_worldcover_stat){
  
  natural_areas_classes = c('Trees','Shrubland','Grassland','Snow/ice','Open water','Herbaceous wetland',
                            'Mangroves','Moss/lichen')
  
  biodiversity_indicators = city_worldcover_stat %>% 
    mutate(natural_areas_class =
             case_when(`land cover class` %in% natural_areas_classes ~ "Natural areas", 
                       !`land cover class` %in% natural_areas_classes ~ "Non Natural areas")
    ) %>% 
    group_by(natural_areas_class) %>% 
    summarise(natural_areas_percent = sum(`land percent`)) %>% 
    filter(natural_areas_class == "Natural areas") %>% 
    add_column(city_id = city_id,
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
                  indicator_name,
                  indicator_number,
                  value,
                  score)
  
  return(biodiversity_indicators)
}


biodiversity_indicators = compute.i1(city_id = city_id,
                                     city_worldcover_stat = city_worldcover_stat)

# plot table
city_worldcover_stat %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "100px")
  
