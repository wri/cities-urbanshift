library(jsonlite)



urbanshift_metadata <-list(list(filename = "dataset 1",
                                description="This a description",
                                title = "This is a title",
                                tags=c("tag_1","tag_2","tag_3")),
                           list(filename = "dataset 2",
                                description="This a description",
                                title = "This is a title",
                                tags=c("tag_1","tag_2"))
)



urbanshift_metadata_json =jsonlite::toJSON(urbanshift_metadata,pretty=TRUE,auto_unbox=TRUE)

write(urbanshift_metadata_json, "./data/urbanshift_metadata_json")

#########################
# ESA world cover
#########################

city_name = "CRI-San_Jose"

data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/esa_world_cover/world_cover_",
                 city_name,
                 ".tif",
                 sep = "")

data = raster(data_url)

dataset_name = paste("ESA World cover land cover extract for", city_name, sep = " ")
description = "The European Space Agency (ESA) WorldCover 10 m 2020 product provides a global land cover map for 2020 at 10 m resolution based on Sentinel-1 and Sentinel-2 data. The WorldCover product comes with 11 land cover classes, aligned with UN-FAO’s Land Cover Classification System, and has been generated in the framework of the ESA WorldCover project. The World Cover product comes with 11 land cover classes: Tree cover, Shrubland, Grassland, Cropland, Built-up, Bare / sparse vegetation, Snow and ice, Open water, Herbaceous wetland, Mangroves, Moss and lichen."
tags = c(city_name, 
         "Biodiversity",
         "Land cover",
         "Geography:America:Costa_Rica")
year = 2020
spatial_resolution = "10m"
temporal_resolution = "yearly"
spatial_extent = "Global"
temporal_extent = "2020"
extent = list(list(extent(data)[1], extent(data)[3]),list(extent(data)[2], extent(data)[4]))
format = "raster"
source = "https://esa-worldcover.org/en"
provider = "European Space Agency (ESA)"


metadata_esa_world_cover = list(title = dataset_name,
                                description = description,
                                tags = tags,
                                year = year,
                                spatial_resolution = spatial_resolution,
                                temporal_resolution = temporal_resolution,
                                spatial_extent = spatial_extent,
                                temporal_extent = temporal_extent,
                                extent = extent,
                                format = format,
                                source = source,
                                provider = provider,
                                url = data_url)

metadata_esa_world_cover_json =jsonlite::toJSON(metadata_esa_world_cover,
                                                pretty=TRUE,
                                                auto_unbox=TRUE)


#########################
# Global Biodiversity Information Facility
#########################

data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/GBIF-",
                 city_name,
                 ".geojson",
                 sep = "")

dataset_name = paste("Global Biodiversity Information Facility for", city_name, sep = " ")
description = "The Global Biodiversity Information Facility (GBIF) is an international network and data infrastructure funded by the world’s governments and aimed at providing anyone, anywhere, open access to data about all types of life on Earth.."
tags = c(city_name, 
         "Biodiversity",
         "Geography:America:Costa_Rica")
year = 2020
format = "geojson"
source = "https://www.gbif.org/en/"
provider = "Global Biodiversity Information Facility"

metadata_gbif = list(title = dataset_name,
                                description = description,
                                tags = tags,
                                year = year,
                                format = format,
                                source = source,
                     provider = provider,
                     url = data_url)

metadata_gbif_json =jsonlite::toJSON(metadata_gbif,
                                     pretty=TRUE,
                                     auto_unbox=TRUE)


#########################
# Amenity
#########################

data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/amenity_",
                 city_name,
                 ".geojson",
                 sep = "")

dataset_name = paste("Urban amenities extract for", city_name, sep = " ")
description = "OpenStreetMap (OSM) provides a free access to the different geographical features mapped by OSM contributors. Amenity data represents physical features on the ground with their corresponding geographic attributes."
tags = c(city_name, 
         "Amenity",
         "Geography:America:Costa_Rica")
year = 2022
format = "geojson"
source = "https://wiki.openstreetmap.org/wiki/Elements"
provider = "OpenStreetMap"

metadata_amenity = list(title = dataset_name,
                     description = description,
                     tags = tags,
                     year = year,
                     format = format,
                     source = source,
                     provider = provider,
                     url = data_url)

metadata_amenity_json =jsonlite::toJSON(metadata_amenity,
                                     pretty=TRUE,
                                     auto_unbox=TRUE)

#########################
# Boundaries
#########################

data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/",
                 city_name,
                 "-boundary.geojson",
                 sep = "")

dataset_name = paste("Administrative boundaries extract for", city_name, sep = " ")
description = "OpenStreetMap (OSM) provides a free access to the different geographical features mapped by OSM contributors. Amenity data represents physical features on the ground with their corresponding geographic attributes."
tags = c(city_name, 
         "Amenity",
         "Geography:America:Costa_Rica")
year = 2022
format = "geojson"
source = "https://wiki.openstreetmap.org/wiki/Elements"
provider = "OpenStreetMap"

metadata_amenity = list(title = dataset_name,
                        description = description,
                        tags = tags,
                        year = year,
                        format = format,
                        source = source,
                        provider = provider,
                        url = data_url)

metadata_amenity_json =jsonlite::toJSON(metadata_amenity,
                                        pretty=TRUE,
                                        auto_unbox=TRUE)

#########################
# Urban Land Use
#########################


data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/urban_land_use/",
                 city_name,
                 "-urbanlanduse2020.tif",
                 sep = "")

data = raster(data_url)

dataset_name = paste("Urban Land Use extract for", city_name, sep = " ")
description = "The ULU data provides land use and land cover information of urban areas based on the application of supervised classification model trained on high resolution Sentinel-2 stallite imagery data. Urban land classes include: open space,non residential area,residential atomistic,residential informal,residential forma,housing project, and roads."
tags = c(city_name, 
         "Land Use",
         "Geography:America:Costa_Rica")
year = 2020
extent = list(list(extent(data)[1], extent(data)[3]),list(extent(data)[2], extent(data)[4]))
format = "raster"
source = "https://www.wri.org/research/spatial-characterization-urban-land-use-through-machine-learning"
provider = "World Resources Institute (WRI)"


metadata_ulu = list(title = dataset_name,
                                description = description,
                                tags = tags,
                                year = year,
                                extent = extent,
                                format = format,
                                source = source,
                                provider = provider,
                                url = data_url)

metadata_ulu_json =jsonlite::toJSON(metadata_ulu,
                                                pretty=TRUE,
                                                auto_unbox=TRUE)


#########################
# Store output
#########################

metadata = list(metadata_esa_world_cover,
                metadata_gbif,
                metadata_amenity,
                metadata_ulu)

metadata_json =jsonlite::toJSON(metadata,
                                     pretty=TRUE,
                                     auto_unbox=TRUE)

write(metadata_json, "./data/metadata.json")
