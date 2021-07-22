######################################################
# reference administrative boundaries in data catalog
######################################################


################### Fill datasets description

# get function
source("https://raw.githubusercontent.com/wri/cities-urbanshift/main/geospatial-layers/data-catalog/fill_datacatalog_datasets_desc.R")

# read tables
datacatalog_datasets_desc = read.csv("https://raw.githubusercontent.com/wri/cities-urbanshift/main/geospatial-layers/data-catalog/datacatalog_datasets_desc.csv")


# reference UrbanShift_cities

datacatalog_datasets_desc = datacatalog.fill.datasetsDesc(datacatalog_datasets_desc = datacatalog_datasets_desc,
                                                          datasetName = "Administrative Boundaries",
                                                          dataSources = "geoBoundaries",
                                                          objectType = "administrative-boundaries",
                                                          projectName = "UrbanShift",
                                                          storageArea = "Github",
                                                          storageStage = "raw",
                                                          storagePath = "https://raw.githubusercontent.com/wri/cities-urbanshift/main/geospatial-layers/data/raw/administrative-boundaries/urbanshift-cities/UrbanShift_cities.geojson",
                                                          datasetFormat = "geojson",
                                                          datasetDesc = "This datasets contains the geographical boundaries of UrbanShift cities at diffrent administrative levels",
                                                          contacts = "")

# writes results
write.csv(datacatalog_datasets_desc, "cities-urbanshift/geospatial-layers/data-catalog/datacatalog_datasets_desc.csv",
          row.names = FALSE)


################### Fill dataset fileds description

# read tables
datacatalog_datasets_desc = read.csv("https://raw.githubusercontent.com/wri/cities-urbanshift/main/geospatial-layers/data-catalog/datacatalog_datasets_desc.csv")

# reference administrative boundaries fields
datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "id",
                                                                               attributeType = "string",
                                                                               attributeDesc = "Identifier of the city")

datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "name",
                                                                               attributeType = "string",
                                                                               attributeDesc = "Name of the city")

datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "shapeGroup",
                                                                               attributeType = "string",
                                                                               attributeDesc = "")

datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "shapeISO",
                                                                               attributeType = "string",
                                                                               attributeDesc = "")

datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "shapeType",
                                                                               attributeType = "string",
                                                                               attributeDesc = "")

datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "ADM0_shape",
                                                                               attributeType = "string",
                                                                               attributeDesc = "")

datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "ADM1_sha_1",
                                                                               attributeType = "string",
                                                                               attributeDesc = "")

datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "ADM1_shape",
                                                                               attributeType = "string",
                                                                               attributeDesc = "")

datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "ADMHIERARC",
                                                                               attributeType = "string",
                                                                               attributeDesc = "")

datacatalog_datasets_attributes_desc = datacatalog.fill.datasetsAttributesDesc(datacatalog_datasets_attributes_desc = datacatalog_datasets_attributes_desc,
                                                                               datasetId = "administrative-boundaries-UrbanShift",
                                                                               datasetName = "Administrative Boundaries",
                                                                               attributeName = "geometry",
                                                                               attributeType = "geometry",
                                                                               attributeDesc = "")
