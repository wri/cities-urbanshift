library(jsonlite)
library(sf)

city_georef = read.csv("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/city_georef.csv",
                       fileEncoding="UTF-8-BOM")

city_name = "CRI-San_Jose"
city = "San Jose"
country = "Costa Rica"
city_id = city_name

setwd("C:/Users/Saif.Shabou/OneDrive - World Resources Institute/Documents/UrbanShift")

##################################################
# ESA world cover --------------------



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
         "Geography:America",
         "Geography:America:Costa_Rica",
         "Geography:America:Costa_Rica:San_Jose",
         "Time:2020")
year = 2020
spatial_resolution = "10m"
temporal_resolution = "yearly"
spatial_extent = "Global"
temporal_extent = "2020"
extent = list(list(extent(data)[1], extent(data)[3]),list(extent(data)[2], extent(data)[4]))
format = "raster"
data_source = "ESA World cover"
source_url = "https://esa-worldcover.org/en"
provider = "European Space Agency (ESA)"
license = "CC BY 4.0"
snippet = paste("Land cover data for ",city,", ",country," (", year,")", sep = "")
crs = "ESPG:4326"

legend = list(list("value"=10,"color"="006400","class"="Trees"),
              list("value"=20,"color"="ffbb22","class"="Shrubland"),
              list("value"=30,"color"="ffff4c","class"="Grassland"),
              list("value"=40,"color"="f096ff","class"="Cropland"),
              list("value"=50,"color"="fa0000","class"="Built-up"),
              list("value"=60,"color"="b4b4b4","class"="Barren / sparse vegetation"),
              list("value"=70,"color"="f0f0f0","class"="Snow and ice"),
              list("value"=80,"color"="0064c8","class"="Open water"),
              list("value"=90,"color"="0096a0","class"="Herbaceous wetland"),
              list("value"=95,"color"="00cf75","class"="Mangroves"),
              list("value"=100,"color"="fae6a0","class"="Moss and lichen"))

metadata_esa_world_cover = list(title = dataset_name,
                                city = city,
                                country = country,
                                city_id = city_name,
                                description = description,
                                snippet = snippet,
                                tags = tags,
                                year = year,
                                spatial_resolution = spatial_resolution,
                                temporal_resolution = temporal_resolution,
                                spatial_extent = spatial_extent,
                                temporal_extent = temporal_extent,
                                extent = extent,
                                format = format,
                                data_source = data_source,
                                source_url = source_url,
                                provider = provider,
                                url = data_url,
                                license = license,
                                snippet = snippet,
                                crs = crs,
                                legend = legend)

metadata_esa_world_cover_json =jsonlite::toJSON(metadata_esa_world_cover,
                                                pretty=TRUE,
                                                auto_unbox=TRUE)


##################################################
# Global Biodiversity Information Facility ----------------


data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/",
                 city_georef[1, "city_id"],
                 "-GBIF-species_observation-2020.geojson",
                 sep = "")

title = paste("Global Biodiversity Information Facility for", city_georef[1, "city_name"], sep = " ")
description = "The Global Biodiversity Information Facility (GBIF) is an international network and data infrastructure funded by the world’s governments and aimed at providing anyone, anywhere, open access to data about all types of life on Earth.."
year = 2020
tags = c(city_georef[1, "city_name"], 
         "Biodiversity",
         paste("Geography",city_georef[1, "continent"], sep = ":"),
         paste("Geography",city_georef[1, "continent"],city_georef[1, "country_name"], sep = ":"),
         paste("Geography",city_georef[1, "continent"],city_georef[1, "country_name"],city_georef[1, "city_name"],  sep = ":"),
         paste("Time",year,sep=":"))
format = "geojson"
data_source = "Global Biodiversity Information Facility"
source_url = "https://www.gbif.org/en/"
provider = "Global Biodiversity Information Facility"
license = "CC BY 4.0"
snippet = paste("Global Biodiversity Information Facility data for ",city_georef[1, "city_name"],", ",city_georef[1, "country_name"]," (", year,")", sep = "")
crs = "ESPG:4326"

metadata_gbif = list(title = title,
                     city = city_georef[1, "city_name"],
                     country = city_georef[1, "country_name"],
                     city_id = city_georef[1, "city_id"],
                     description = description,
                     snippet = snippet,
                     tags = tags,
                     year = year,
                     format = format,
                     data_source = data_source,
                     source_url = source_url,
                     provider = provider,
                     status = "published",
                     url = data_url,
                     license = license,
                     crs = crs)

metadata_gbif_json =jsonlite::toJSON(metadata_gbif,
                                     pretty=TRUE,
                                     auto_unbox=TRUE)

write(metadata_gbif_json, 
      "./data/s3/datahub/metadata_GBIF.json")


##################################################
# Amenity ----------------------


data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/amenity_",
                 city_name,
                 ".geojson",
                 sep = "")

dataset_name = paste("Urban amenities extract for", city_name, sep = " ")
description = "OpenStreetMap (OSM) provides a free access to the different geographical features mapped by OSM contributors. Amenity data represents physical features on the ground with their corresponding geographic attributes."
tags = c(city_name, 
         "Amenity",
         "Geography:America",
         "Geography:America:Costa_Rica",
         "Geography:America:Costa_Rica:San_Jose",
         "Time:2022")
year = 2022
format = "geojson"
data_source = "OpenStreetMap"
source_url = "https://wiki.openstreetmap.org/wiki/Elements"
provider = "OpenStreetMap"
license = "ODbL"
snippet = paste("Amenity data for ",city,", ",country," (", year,")", sep = "")
crs = "ESPG:4326"

metadata_amenity = list(title = dataset_name,
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

metadata_amenity_json =jsonlite::toJSON(metadata_amenity,
                                     pretty=TRUE,
                                     auto_unbox=TRUE)

##################################################
# OSM - parks ----------------------


data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/",
                 city_georef[1, "city_id"],
                 "-OSM-recreational_services-2022.geojson",
                 sep = "")

title = paste("Recreational services extract for", city_georef[1, "city_name"], sep = " ")
description = "OpenStreetMap (OSM) provides a free access to the different geographical features mapped by OSM contributors. Amenity data represents physical features on the ground with their corresponding geographic attributes."
year = 2022
tags = c(city_georef[1, "city_name"], 
         "Biodiversity",
         paste("Geography",city_georef[1, "continent"], sep = ":"),
         paste("Geography",city_georef[1, "continent"],city_georef[1, "country_name"], sep = ":"),
         paste("Geography",city_georef[1, "continent"],city_georef[1, "country_name"],city_georef[1, "city_name"],  sep = ":"),
         paste("Time",year,sep=":"))
format = "geojson"
data_source = "OpenStreetMap"
source_url = "https://wiki.openstreetmap.org/wiki/Elements"
provider = "OpenStreetMap"
license = "ODbL"
snippet = paste("Recreational services for ",city_georef[1, "city_name"],", ",city_georef[1, "country_name"]," (", year,")", sep = "")
crs = "ESPG:4326"
status = "published"

metadata_parks = list(title = title,
                        city = city_georef[1, "city_name"],
                        country = city_georef[1, "country_name"],
                        city_id = city_name,
                        description = description,
                        snippet = snippet,
                        tags = tags,
                        year = year,
                        format = format,
                        data_source = data_source,
                        source_url = source_url,
                        provider = provider,
                        status = status,
                        url = data_url,
                        license = license,
                        crs = crs)

metadata_parks_json =jsonlite::toJSON(metadata_parks,
                                        pretty=TRUE,
                                        auto_unbox=TRUE)


write(metadata_parks_json, 
      "./data/s3/datahub/metadata_OSM_parks.json")

##################################################
# IUCN redlist ----------------------


data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/",
                 city_georef[1, "city_id"],
                 "-IUCN-RedList_counts.xlsx",
                 sep = "")

title = paste("Threatened species extract for", city_georef[1, "city_name"], sep = " ")
description = "The International Union for Conservation of Nature (IUCN) Red List of Threatened Species  is the world's most comprehensive inventory of the global conservation status of biological species."
year = 2022
tags = c(city_georef[1, "city_name"], 
         "Biodiversity",
         paste("Geography",city_georef[1, "continent"], sep = ":"),
         paste("Geography",city_georef[1, "continent"],city_georef[1, "country_name"], sep = ":"),
         paste("Geography",city_georef[1, "continent"],city_georef[1, "country_name"],city_georef[1, "city_name"],  sep = ":"),
         paste("Time",year,sep=":"))
format = "xlsx"
data_source = "The IUCN Red List of Threatened Species"
source_url = "https://www.iucnredlist.org/"
provider = "The International Union for Conservation of Nature"
license = "ODbL"
snippet = paste("Threatened species for ",city_georef[1, "city_name"],", ",city_georef[1, "country_name"]," (", year,")", sep = "")
crs = "ESPG:4326"
status = "integrated"

metadata_redlist = list(title = title,
                      city = city_georef[1, "city_name"],
                      country = city_georef[1, "country_name"],
                      city_id = city_name,
                      description = description,
                      snippet = snippet,
                      tags = tags,
                      year = year,
                      format = format,
                      data_source = data_source,
                      source_url = source_url,
                      provider = provider,
                      status = status,
                      url = data_url,
                      license = license,
                      crs = crs)

metadata_redlist_json =jsonlite::toJSON(metadata_redlist,
                                      pretty=TRUE,
                                      auto_unbox=TRUE)


write(metadata_redlist_json, 
      "./data/s3/datahub/metadata_IUCN_redlist.json")

##################################################
# Boundaries San Jose --------------------


data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/",
                 city_name,
                 "-boundary.geojson",
                 sep = "")

dataset_name = paste("Administrative boundaries extract for", city_name, sep = " ")
description = "The geoBoundaries global database is an online, open license resource of boundaries for every country in the world. We currently track approximately 300,000 boundaries across 199 entities, including all 195 UN member states, Greenland, Taiwan, Niue, and Kosovo. All boundaries are available to view or download in common file formats, including shapefiles. It is produced and maintained by the William & Mary geoLab since 2017."
tags = c(city_name, 
         "Boundaries",
         "Geography:America",
         "Geography:America:Costa_Rica",
         "Geography:America:Costa_Rica:San_Jose",
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

##################################################
# Boundaries All cities regional level --------------------




metadata_boundary = list()

for(i in 1:nrow(city_georef)){
  # i = 2
  print(i)
  
  city_id= city_georef$city_id[i]
  print(city_id)
  
  data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/ADM1/boundary-",
                   city_id,
                   "-ADM1.geojson",
                   sep = "")
  
  city_boundary = st_read(data_url,
                          quiet = TRUE)
  
  dataset_name = paste("Administrative boundaries extract for", 
                       city_georef[i, "city_name"], 
                       "(regional level)",
                       sep = " ")
  description = "The geoBoundaries global database is an online, open license resource of boundaries for every country in the world. We currently track approximately 300,000 boundaries across 199 entities, including all 195 UN member states, Greenland, Taiwan, Niue, and Kosovo. All boundaries are available to view or download in common file formats, including shapefiles. It is produced and maintained by the William & Mary geoLab since 2017."
  year = 2017
  tags = c(city_georef[i, "city_name"], 
           "Boundaries",
           paste("Geography",city_georef[i, "continent"], sep = ":"),
           paste("Geography",city_georef[i, "continent"],city_georef[i, "country_name"], sep = ":"),
           paste("Geography",city_georef[i, "continent"],city_georef[i, "country_name"],city_georef[i, "city_name"],  sep = ":"),
           paste("Time",year,sep=":"))
  format = "geojson"
  status = "published"
  data_source = "geoBoundaries"
  source_url = "https://www.geoboundaries.org/"
  provider = "geoBoundaries"
  license = "ODbL"
  snippet = paste("Administrative boundaries data for ",city_georef[i, "city_name"],", ",city_georef[i, "country_name"]," (", year,")", sep = "")
  crs = "ESPG:4326"
  
  metadata_boundary_city = list(title = dataset_name,
                                city = city_georef[i, "city_name"],
                                country = city_georef[i, "country_name"],
                                city_id = city_georef[i, "city_id"],
                                description = description,
                                snippet = snippet,
                                tags = tags,
                                year = year,
                                format = format,
                                data_source = data_source,
                                source_url = source_url,
                                provider = provider,
                                url = data_url,
                                status = status,
                                license = license,
                                crs = crs)
  
  
  metadata_boundary[[i]] = metadata_boundary_city
}



metadata_json =jsonlite::toJSON(metadata_boundary,
                                pretty=TRUE,
                                auto_unbox=TRUE)

write(metadata_json, 
      "./data/s3/datahub/metadata_boundaries_ADM1.json")

##################################################
# Boundaries Sub-regional level --------------------

i = 1
city_id= city_georef$city_id[i]
print(city_id)

data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/ADM2/boundary-",
                 city_id,
                 "-ADM2.geojson",
                 sep = "")

city_boundary = st_read(data_url,
                        quiet = TRUE)

dataset_name = paste("Administrative boundaries extract for", 
                     city_georef[i, "city_name"], 
                     "(sub-regional level)",
                     sep = " ")
description = "The geoBoundaries global database is an online, open license resource of boundaries for every country in the world. We currently track approximately 300,000 boundaries across 199 entities, including all 195 UN member states, Greenland, Taiwan, Niue, and Kosovo. All boundaries are available to view or download in common file formats, including shapefiles. It is produced and maintained by the William & Mary geoLab since 2017."
year = 2017
tags = c(city_georef[i, "city_name"], 
         "Boundaries",
         paste("Geography",city_georef[i, "continent"], sep = ":"),
         paste("Geography",city_georef[i, "continent"],city_georef[i, "country_name"], sep = ":"),
         paste("Geography",city_georef[i, "continent"],city_georef[i, "country_name"],city_georef[i, "city_name"],  sep = ":"),
         paste("Time",year,sep=":"))
format = "geojson"
status = "published"
data_source = "geoBoundaries"
source_url = "https://www.geoboundaries.org/"
provider = "geoBoundaries"
license = "ODbL"
snippet = paste("Administrative boundaries data for ",city_georef[i, "city_name"],", ",city_georef[i, "country_name"]," (", year,")", sep = "")
crs = "ESPG:4326"

metadata_boundary_city = list(title = dataset_name,
                              city = city_georef[i, "city_name"],
                              country = city_georef[i, "country_name"],
                              city_id = city_georef[i, "city_id"],
                              description = description,
                              snippet = snippet,
                              tags = tags,
                              year = year,
                              format = format,
                              data_source = data_source,
                              source_url = source_url,
                              provider = provider,
                              url = data_url,
                              status = status,
                              license = license,
                              crs = crs)

metadata_boundary_city_json = jsonlite::toJSON(metadata_boundary_city,
                                               pretty=TRUE,
                                               auto_unbox=TRUE)

write(metadata_boundary_city_json, 
      "./data/s3/datahub/metadata_boundaries_ADM2.json")


# append

metadata_boundary_all_levels = list()

metadata_boundary_all_levels = list(metadata_boundary,
                                    metadata_boundary_city)


metadata_json =jsonlite::toJSON(metadata_boundary_all_levels,
                                pretty=TRUE,
                                auto_unbox=TRUE)

write(metadata_boundary_all_levels, 
      "./data/s3/datahub/metadata_boundaries.json")



##################################################
# Protected areas ---------------------


data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/data/CRI-San_Jose/",
                 city_georef[1, "city_id"],
                 "-WDPA-protected_areas_2022.geojson",
                 sep = "")

dataset_name = paste("Protected Areas extract for", city_georef[1, "city_name"], sep = " ")
description = "The World Database on Protected Areas (WDPA) is the most comprehensive global database of marine and terrestrial protected areas. It is a joint project between UN Environment Programme and the International Union for Conservation of Nature (IUCN), and is managed by UN Environment Programme World Conservation Monitoring Centre (UNEP-WCMC), in collaboration with governments, non-governmental organisations, academia and industry."
year = 2020
tags = c(city_georef[1, "city_name"], 
         "Biodiversity",
         paste("Geography",city_georef[1, "continent"], sep = ":"),
         paste("Geography",city_georef[1, "continent"],city_georef[1, "country_name"], sep = ":"),
         paste("Geography",city_georef[1, "continent"],city_georef[1, "country_name"],city_georef[1, "city_name"],  sep = ":"),
         paste("Time",year,sep=":"))
format = "geojson"
data_source = "World Database on Protected Areas"
source_url = "https://www.protectedplanet.net/en"
provider = "Protected Planet"
license = ""
snippet = paste("Protected Areas data for ",city,", ",country," (", year,")", sep = "")
crs = "ESPG:4326"
status = "integrated"

metadata_wdpa = list(title = dataset_name,
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
                     status = status,
                         url = data_url,
                     license = license,
                     crs = crs)

metadata_wdpa_json =jsonlite::toJSON(metadata_wdpa,
                                         pretty=TRUE,
                                         auto_unbox=TRUE)

write(metadata_wdpa_json, 
      "./data/s3/datahub/metadata_WDPA.json")

##################################################
# Urban Land Use ---------------------------------



data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/urban_land_use/",
                 city_name,
                 "-urbanlanduse2020.tif",
                 sep = "")

data = raster(data_url)

dataset_name = paste("Urban Land Use extract for", city_name, sep = " ")
description = "The ULU data provides land use and land cover information of urban areas based on the application of supervised classification model trained on high resolution Sentinel-2 stallite imagery data. Urban land classes include: open space,non residential area,residential atomistic,residential informal,residential forma,housing project, and roads."
tags = c(city_name, 
         "Land Use",
         "Geography:America",
         "Geography:America:Costa_Rica",
         "Geography:America:Costa_Rica:San_Jose",
         "Time:2020")
year = 2020
extent = list(list(extent(data)[1], extent(data)[3]),list(extent(data)[2], extent(data)[4]))
format = "raster"
data_source = "Urban Land Use"
source_url = "https://www.wri.org/research/spatial-characterization-urban-land-use-through-machine-learning"
provider = "World Resources Institute (WRI)"
license = "CC BY 4.0"
snippet = paste("Urban Land Use data for ",city,", ",country," (", year,")", sep = "")
crs = "ESPG:4326"

metadata_ulu = list(title = dataset_name,
                    city = city,
                    country = country,
                    city_id = city_name,
                                description = description,
                    snippet = snippet,
                                tags = tags,
                                year = year,
                                extent = extent,
                                format = format,
                    data_source = data_source,
                    source_url = source_url,
                                provider = provider,
                                url = data_url,
                    license = license,
                    crs = crs)

metadata_ulu_json =jsonlite::toJSON(metadata_ulu,
                                                pretty=TRUE,
                                                auto_unbox=TRUE)

##################################################
# Tree Outside of Forests -----



data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/trees_outside_forest/",
                 city_name,
                 "-treecover2020.tif",
                 sep = "")

data = raster(data_url)

dataset_name = paste("Trees Outside of Forests extract for", city_name, sep = " ")
description = "The TOF project provides tree extent data at 10m scale based on trained Convolutional Neural Network using satellite imagery (Sentinel-1 and Sentinel-2). It enables accurate reporting of tree cover outside of dense, closed-canopy forests and urban areas."
tags = c(city_name, 
         "Land Cover",
         "Greenspace",
         "Geography:America",
         "Geography:America:Costa_Rica",
         "Geography:America:Costa_Rica:San_Jose",
         "Time:2020")
spatial_resolution = "10m"
temporal_resolution = "yearly"
spatial_extent = "Global"
temporal_extent = "2020"
year = 2020
extent = list(list(extent(data)[1], extent(data)[3]),list(extent(data)[2], extent(data)[4]))
format = "raster"
data_source = "Trees Outside of Forests"
source_url = "https://arxiv.org/abs/2005.08702"
provider = "World Resources Institute (WRI)"
license = " "
snippet = paste("Tree cover data for ",city,", ",country," (", year,")", sep = "")
crs = "ESPG:4326"

metadata_tof = list(title = dataset_name,
                    city = city,
                    country = country,
                    city_id = city_name,
                    description = description,
                    snippet = snippet,
                    tags = tags,
                    year = year,
                    extent = extent,
                    spatial_resolution = spatial_resolution,
                    temporal_resolution = temporal_resolution,
                    format = format,
                    data_source = data_source,
                    source_url = source_url,
                    provider = provider,
                    url = data_url,
                    license = license,
                    crs = crs)

metadata_tof_json =jsonlite::toJSON(metadata_tof,
                                    pretty=TRUE,
                                    auto_unbox=TRUE)

#########################
# Dynamic WOrld -----


years = seq(2016,2020)

metadata_dw = list()

for(i in 1:length(years)){ 
  
  print(i)
  
  year = years[i]
  
  data_url = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/dynamic_world/",
                   city_name,
                   "-landcover",
                   year,
                   ".tif",
                   sep = "")
  
  print(data_url)
  
  data = raster(data_url)
  
  dataset_name = paste("Dynamic World Land Cover extract for", city_name, year, sep = " ")
  description = "The Dynamic World Land Cover product displays a global map of land use/land cover (LULC) provided from ESA Sentinel-2 imagery at 10m resolution. It is composed of 10 land use classes: water, trees,grass,flooded vegetation,crops,scrub/shrub, built area,bare ground, and snow/ice. "
  tags = c(city_name, 
           "Land Cover",
           "Geography:America",
           "Geography:America:Costa_Rica",
           "Geography:America:Costa_Rica:San_Jose",
           paste("Time",year,sep = ":")
           )
  spatial_resolution = "30m"
  temporal_resolution = "yearly"
  spatial_extent = "Global"
  temporal_extent = "2000-2020"
  year = year
  extent = list(list(extent(data)[1], extent(data)[3]),list(extent(data)[2], extent(data)[4]))
  format = "raster"
  data_source = "Dynamic World Land Cover"
  source_url = "https://www.landcarbonlab.org/data/#global-land-cover-change"
  provider = "World Resources Institute (WRI) / University of Maryland"
  license = " "
  snippet = paste("Dynamic World Land cover data for ",city,", ",country," (", year,")", sep = "")
  crs = "ESPG:4326"
  
  metadata_dw_year = list(title = dataset_name,
                          city = city,
                          country = country,
                          city_id = city_name,
                          description = description,
                          snippet = snippet,
                          tags = tags,
                          year = year,
                          extent = extent,
                          spatial_resolution = spatial_resolution,
                          temporal_resolution = temporal_resolution,
                          format = format,
                          data_source = data_source,
                          source_url = source_url,
                          provider = provider,
                          url = data_url,
                          license = license,
                          crs = crs)
  
  metadata_dw[[i]] = metadata_dw_year
}

metadata_dw_json =jsonlite::toJSON(metadata_dw,
                                    pretty=TRUE,
                                    auto_unbox=TRUE)


#########################
# Store output
#########################

metadata = list(metadata_boundary,
                metadata_esa_world_cover,
                metadata_gbif,
                metadata_amenity,
                metadata_ulu,
                metadata_tof,
                metadata_wdpa,
                metadata_dw[[1]],
                metadata_dw[[2]],
                metadata_dw[[3]],
                metadata_dw[[4]],
                metadata_dw[[5]])

metadata_json =jsonlite::toJSON(metadata,
                                     pretty=TRUE,
                                     auto_unbox=TRUE)

write(metadata_json, "./github/cities-urbanshift/geospatial-layers/urbanshif_geolayers_metadata.json")
