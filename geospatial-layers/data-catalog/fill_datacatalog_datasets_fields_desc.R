# Function to fill data catalog with datasets fields description


datacatalog.fill.datasetsAttributesDesc = function(datacatalog_datasets_attributes_desc,
                                                   datasetId,
                                                   datasetName,
                                                   attributeName,
                                                   attributeType,
                                                   attributeDesc){
  
  # verify if the field exist
  k_field = datacatalog_datasets_attributes_desc %>% 
    filter(datasetName == datasetName & attributeName == attributeName) %>% 
    count() %>% 
    as.numeric
  
  
  if(k_field == 0){
    # if dataset doesn't exist we add a new dataset in the table 
    k = nrow(datacatalog_datasets_attributes_desc) + 1
  } else 
    # if the dataset exists, we overwrite
  {
    k = which(datacatalog_datasets_attributes_desc$datasetName == datasetName & datacatalog_datasets_attributes_desc$attributeName == attributeName)
  }
  
  
  datacatalog_datasets_attributes_desc[k, "datasetName"] = datasetName
  datacatalog_datasets_attributes_desc[k, "datasetId"] = datasetName
  datacatalog_datasets_attributes_desc[k, "attributeName"] = attributeName
  datacatalog_datasets_attributes_desc[k, "attributeType"] = attributeType
  datacatalog_datasets_attributes_desc[k, "attributeDesc"] = attributeDesc
  
  
  datacatalog_datasets_attributes_desc[k, "attributeId"] = paste(datasetId,attributeName,sep = "-")
  
  return(datacatalog_datasets_attributes_desc)
  
}