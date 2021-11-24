# function for computing land cover statistics by polygons

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