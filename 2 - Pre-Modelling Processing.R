# Load relevant data
############################
  raw_modelling_data <- readRDS(paste0(project_wd,"/data/raw_modelling_data.rds"))

# Pre-Modelling Data Processing
############################
  
# Split into train and validate
############################
  modelling_data_train <- raw_modelling_data %>%
    dplyr::filter(modelling_bucket_6 > 0)
  
  modelling_data_validate <- raw_modelling_data %>%
    dplyr::filter(modelling_bucket_6 == 0)
  
# Save relevant objects
############################      
  saveRDS(
    object = modelling_data_train
    ,file = paste0(
      project_wd
      ,"/data/"
      ,"modelling_data_train"
      ,".rds"
    )
  )   
  saveRDS(
    object = modelling_data_validate
    ,file = paste0(
      project_wd
      ,"/data/"
      ,"modelling_data_validate"
      ,".rds"
    )
  )       
  
  