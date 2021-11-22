#################### Get Dynamic World Land Cover data

get.lulc.dw = function(year, city_id, city_boundary){
  
  
  # define path
  path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/dynamic_world/"
  
  # define path
  city_landcover_path = paste(path, city_id,"-landcover",year,".tif",
                              sep = "")
  
  # collect raster data
  city_landcover_raster= raster(city_landcover_path)
  
  # mask raster based on administrative boundaries
  city_landcover_mask  = raster::mask(city_landcover_raster,city_boundary)
  
  return(city_landcover_mask)
}


# Test

# year = 2020
# selected_city = "RWA-Kigali"
# city_boundary = st_read(paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/",selected_city,"-boundary.geojson",sep =""))
# 
# 
# city_landcover_dw_2020 = get.lulc.dw(year = 2020, city_id = selected_city, city_boundary = city_boundary)
# city_landcover_dw_2019 = aws.get.dw.by.year(year = 2019, city_id = selected_city, data_source = "aws")
# city_landcover_dw_2018 = aws.get.dw.by.year(year = 2018, city_id = selected_city, data_source = "aws")
# city_landcover_dw_2017 = aws.get.dw.by.year(year = 2017, city_id = selected_city, data_source = "aws")
# city_landcover_dw_2016 = aws.get.dw.by.year(year = 2016, city_id = selected_city, data_source = "aws")