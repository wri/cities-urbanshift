###########################################################################################################
# convert shapefile to geojson ----

# read raw data
planning_area = readOGR("./github/cities-urbanshift/thematic-analysis/Freetown/data/raw",
                        "Planning Area 12")

# write processed data: convert to geojson
writeOGR(planning_area,
         dsn = "./github/cities-urbanshift/thematic-analysis/Freetown/data/processed/planning_area.geojson",
         layer = "planning_area",
         driver = "GeoJSON",
         overwrite_layer=TRUE)



###########################################################################################################
# compute urban expansion level by planning area boundary ----

# load aggregation function

src("./github/cities-urbanshift/thematic-analysis/Freetown/scripts/src/land-class-stat-by-polygon.R")

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

# write outputs
write.csv(city_subregion_landclass_stat, 
          "./github/cities-urbanshift/thematic-analysis/Freetown/output/planning_area_urban_stat.csv")

# compute urban expansion
city_subregion_urban_expansion = city_subregion_landclass_stat %>% group_by(name) %>% 
  mutate(
    urban_expansion = Urban - lag(Urban)
  ) %>% 
  drop_na(urban_expansion) %>% 
  dplyr::select(name, urban_expansion)

# write outputs
write.csv(city_subregion_urban_expansion, 
          "./github/cities-urbanshift/thematic-analysis/Freetown/output/planning_area_urban_expansion.csv")

###########################################################################################################
# Plot the map  -----

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
  dplyr::select('Planning area number' = name, 
                'Urban expansion 2016-2021 (%)' = urban_expansion) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 13)%>% 
  scroll_box(width = "100%", height = "300px")

