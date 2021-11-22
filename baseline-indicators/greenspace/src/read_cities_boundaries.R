

######################################
# Get city administrative boundaries
######################################


get.city.boundary = function(city_id){
  
  # define path
  city_boundary_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/",city_id,"-boundary.geojson",
                             sep = "")
  
  # read the data#
  city_boundary <- st_read(city_boundary_path,
                           quiet = TRUE)
  return(city_boundary)
  
}

# city_boundary = get.city.boundary(city_id = selected_city)