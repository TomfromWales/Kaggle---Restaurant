# Load relevant data
############################
  modelling_data_train <- readRDS(paste0(project_wd,"/data/modelling_data_train.rds"))
  modelling_data_validate <- readRDS(paste0(project_wd,"/data/modelling_data_validate.rds"))

# Quick model to predict visitors when no reservations made/available
#################
  
  