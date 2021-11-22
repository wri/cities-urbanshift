# initialize baseline indicators table

geo_indicators = data.frame("geocomponent_id" = as.character(),
                            "geocomponent_name" = as.character(),
                            "indicator_id" = as.character(),
                            "indicator_name" = as.character(),
                            "project_name" = as.character(),
                            "value" = as.numeric(),
                            "year" = as.character())

# Save datasets to CSV files in tempdir()
write.csv(geo_indicators, 
          file.path(tempdir(), "geo_indicators.csv"))

# Upload files to S3 bucket
put_object(
  file = file.path(tempdir(), "geo_indicators.csv"), 
  object = "geo_indicators.csv", 
  bucket = "cities-urbanshift/db"
)