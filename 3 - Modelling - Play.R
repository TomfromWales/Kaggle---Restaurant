# Load relevant data
############################
  modelling_data_train <- readRDS(paste0(project_wd,"/data/modelling_data_train.rds"))
  modelling_data_validate <- readRDS(paste0(project_wd,"/data/modelling_data_validate.rds"))
  sample_submission <- readRDS(paste0(project_wd,"/data/sample_submission.rds"))

# Quick model to predict visitors when no reservations made/available
############################

  h2o.init()
  
  as.h2o(modelling_data_train,destination_frame = "modelling_data_train_h2o")
  as.h2o(modelling_data_validate,destination_frame = "modelling_data_validate_h2o")
  as.h2o(raw_modelling_data,destination_frame = "raw_modelling_data.h2o")
  
  h2o.gbm(
    training_frame = "modelling_data_train_h2o"
    ,y = "visitors"
    ,x = c("visit_date_month","visit_date_day_of_week","genre","num_local_competitors","num_local_competitors_same_genre")
    ,ntrees = 250
    ,max_depth = 3
    ,learn_rate = 0.1
    ,distribution = "poisson"
    ,sample_rate = 0.7
    ,model_id = "test_h2o_gbm"
  )
  
  validate_predictions <- h2o.predict(
    h2o.getModel("test_h2o_gbm")
    ,h2o.getFrame("modelling_data_validate_h2o")
  )
  
  summary(validate_predictions)
  
  for_plots_validate <- cbind.data.frame(
    modelling_data_validate
    ,preds = as.matrix(validate_predictions)[,1]
    ,weight = rep(1,nrow(modelling_data_validate))
  )
  
  full_predictions <- h2o.predict(
    h2o.getModel("test_h2o_gbm")
    ,h2o.getFrame("raw_modelling_data.h2o")
  ) 
  
  for_submission <- cbind.data.frame(
    raw_modelling_data
    ,preds = as.matrix(full_predictions)[,1]
  )  
  
  h2o.shutdown()
  
  
# Hurried plots
#=========================
  plotly_actual_vs_predicted(
    data = for_plots_validate
    ,actual = "visitors"
    ,prediction = "preds"
    ,weight = "weight"
    ,feature = "visit_date_month"
  )
  
# Weak submission
#=========================
  submission_prep <- for_submission %>%
    mutate(
      id = paste(air_store_id,visit_date,sep="_")
      ,visitors = preds
    )%>%
    dplyr::select(id,visitors)
  
  submission <- sample_submission %>%
    dplyr::select(id) %>%
    dplyr::left_join(submission_prep,by="id")
  
  write.csv(submission,"weak_gbm_no_reserve_data.csv",row.names = FALSE)
   
  
  
  
