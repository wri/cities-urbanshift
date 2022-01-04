
# Split the administrative boundaries by cities


# read all boundaries
urbanshift_boundaries = st_read("./github/cities-urbanshift/geospatial-layers/data/boundaries/administrative_boundaries.geojson",
                   quiet = TRUE)

# get list of cities
urbanshift_cities = unique(urbanshift_boundaries$city_name)

# split the geijson ibto distinct files
for(i in 1:length(urbanshift_cities)){
  
  # i = 1
  print(i)
  
  city_name = urbanshift_cities[i]
  print(city_name)
  
  urbanshift_boundaries_city = urbanshift_boundaries[urbanshift_boundaries$city_name == city_name, ]
  
  export_file_name = paste("./github/cities-urbanshift/geospatial-layers/data/boundaries/admin_boundaries_",
                           city_name,
                           ".geojson",
                           sep = "")
  
  st_write(urbanshift_boundaries_city,
           export_file_name,
           delete_dsn=TRUE)
}

