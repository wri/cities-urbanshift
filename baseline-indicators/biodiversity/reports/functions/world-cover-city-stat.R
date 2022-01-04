# compute statistics on world cover distribution at the city level

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

# test

# city_worldcover_stat = world.cover.city.stat(city_worldcover = city_worldcover,
#                                              year = 2020)
