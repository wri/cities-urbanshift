
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