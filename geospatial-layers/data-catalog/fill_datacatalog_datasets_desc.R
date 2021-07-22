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
  
  # verify if the dataset exist
  k_dataset = datacatalog_datasets_desc %>% 
    filter(objectType == objectType & projectName == projectName) %>% 
    count() %>% 
    as.numeric
  
  
  if(k_dataset == 0){
    # if dataset doesn't exist we add a new dataset in the table 
    k = nrow(datacatalog_datasets_desc) + 1
  } else 
    # if the dataset exists, we overwrite
  {
    k = which(datacatalog_datasets_desc$objectType == objectType & datacatalog_datasets_desc$projectName == projectName)
  }
  
  # fill the properties
  
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