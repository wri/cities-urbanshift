
#########################
# initialize data catalog 
#########################


# initialize datacatalog_datasets_desc table
datacatalog_datasets_desc = data.frame("datasetName" = as.character(),
                                       "datasetId" = as.character(),
                                       "dataSources" = as.character(),
                                       "objectType" = as.character(),
                                       "projectName" = as.character(),
                                       "tags" = as.character(),
                                       "creationDate" = as.character(),
                                       "storagePath" = as.character(),
                                       "storageArea" = as.character(),
                                       "storageStage" = as.character(),
                                       "datasetFormat" = as.character(),
                                       "datasetDesc" = as.character(),
                                       "contacts" = as.character())

# write table
write.csv(datacatalog_datasets_desc, "cities-urbanshift/geospatial-layers/data-catalog/datacatalog_datasets_desc.csv",
          row.names = FALSE)

# initialize datacatalog_datasets_attributes_desc table
datacatalog_datasets_attributes_desc = data.frame("datasetId" = as.character(),
                                                  "datasetName" = as.character(),
                                                  "attributeName" = as.character(),
                                                  "attributeId" = as.character(),
                                                  "attributeType" = as.character(),
                                                  "attributeDesc" = as.character())

# write table
write.csv(datacatalog_datasets_attributes_desc, "cities-urbanshift/geospatial-layers/data-catalog/datacatalog_datasets_attributes_desc.csv",
          row.names = FALSE)