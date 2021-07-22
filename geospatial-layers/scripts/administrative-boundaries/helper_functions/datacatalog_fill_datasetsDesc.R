

# Function to fill data catalog with datasets description

datacatalog.fill.datasetsDesc = function(datacatalog_datasets_desc,
                                         datasetName,
                                         dataSources,
                                         objectType,
                                         projectName,
                                         storagePath,
                                         storageArea,
                                         storageStage,
                                         datasetFormat,
                                         datasetDesc,
                                         contacts){
  
  k = nrow(datacatalog_datasets_desc) + 1
  
  datacatalog_datasets_desc[k, "datasetName"] = datasetName
  datacatalog_datasets_desc[k, "dataSources"] = dataSources
  datacatalog_datasets_desc[k, "objectType"] = objectType
  datacatalog_datasets_desc[k, "projectName"] = projectName
  datacatalog_datasets_desc[k, "storagePath"] = storagePath
  datacatalog_datasets_desc[k, "storageArea"] = storageArea
  datacatalog_datasets_desc[k, "storageStage"] = storageStage
  datacatalog_datasets_desc[k, "datasetFormat"] = datasetFormat
  datacatalog_datasets_desc[k, "datasetDesc"] = datasetDesc
  datacatalog_datasets_desc[k, "contacts"] = contacts
  
  datacatalog_datasets_desc[k, "datasetId"] = paste(objectType,projectName,sep = "-")
  
  return(datacatalog_datasets_desc)
  
}