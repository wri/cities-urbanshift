library(jsonlite)


city_name = "RWA-Kigali"
city = "Kigali"
country = "Rwanda"
city_id = city_name

setwd("C:/Users/Saif.Shabou/OneDrive - World Resources Institute/Documents/UrbanShift/github/cities-urbanshift")

#########################
# Boundaries
#########################

data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/",
                 city_name,
                 "-boundary.geojson",
                 sep = "")

dataset_name = paste("Administrative boundaries extract for", city_name, sep = " ")
description = "The geoBoundaries global database is an online, open license resource of boundaries for every country in the world. We currently track approximately 300,000 boundaries across 199 entities, including all 195 UN member states, Greenland, Taiwan, Niue, and Kosovo. All boundaries are available to view or download in common file formats, including shapefiles. It is produced and maintained by the William & Mary geoLab since 2017."
tags = c(city_name, 
         "Boundaries",
         "Geography:Africa",
         "Geography:Africa:Rwanda",
         "Geography:Africa:Rwanda:Kigali",
         "Time:2017")
year = 2017
format = "geojson"
data_source = "geoBoundaries"
source_url = "https://www.geoboundaries.org/"
provider = "geoBoundaries"
license = "ODbL"
snippet = paste("Administrative boundaries data for ",city,", ",country," (", year,")", sep = "")
crs = "ESPG:4326"

metadata_boundary = list(title = dataset_name,
                         city = city,
                         country = country,
                         city_id = city_name,
                         description = description,
                         snippet = snippet,
                         tags = tags,
                         year = year,
                         format = format,
                         data_source = data_source,
                         source_url = source_url,
                         provider = provider,
                         url = data_url,
                         license = license,
                         crs = crs)

metadata_boundary_json =jsonlite::toJSON(metadata_boundary,
                                         pretty=TRUE,
                                         auto_unbox=TRUE)

#########################
# Store output
#########################

metadata = list(metadata_boundary)

metadata_json =jsonlite::toJSON(metadata,
                                pretty=TRUE,
                                auto_unbox=TRUE)


metadata_file_path = paste("./geospatial-layers/",
                           city_name,
                           "_urbanshif_geolayers_metadata.json",
                           sep = "")

write(metadata_json, metadata_file_path)
