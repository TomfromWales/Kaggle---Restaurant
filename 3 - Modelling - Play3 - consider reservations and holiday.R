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
    ,x = c(
      "visit_date_month"
      ,"visit_date_day_of_week"
      ,"genre"
      ,"num_local_competitors"
      ,"num_local_competitors_same_genre"
      ,"reserve_visitors"
      ,"reserve_visitors_day_lag_0"
      ,"reserve_visitors_day_lag_2"
      ,"reserve_visitors_day_lag_3"
      ,"reserve_visitors_day_lag_4"
      ,"reserve_visitors_day_lag_5"
      ,"reserve_visitors_day_lag_6"
      ,"reserve_visitors_day_lag_7to13"
      ,"reserve_visitors_day_lag_14plus"
      ,"reserve_visitors_booking_early_morning"
      # ,"reserve_visitors_booking_evening"
      ,"reserve_visitors_booking_work"
      ,"reserve_visitors_booking_unsociable"
      ,"reserve_visitors_booking_late_night"
      ,"visit_date_day_of_month"
      ,"holiday_flg"
      ,"holiday_flg_lag1"
      ,"holiday_flg_lagminus1"
      ,"holiday_flg_lag2"
      ,"holiday_flg_lagminus2"
      ,"holiday_flg_lag3"
      ,"holiday_flg_lagminus3"
      ,"MaxV_alltime"
      ,"MeanV_alltime"
      ,"MaxV_todate"
      ,"MinV_todate"
      ,"MeanV_todate"
    )
    ,ntrees = 1000
    ,max_depth = 10
    ,learn_rate = 0.1
    ,distribution = "poisson"
    ,sample_rate = 0.6
    ,min_rows = 3
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
  as.data.frame(h2o.varimp(h2o.getModel("test_h2o_gbm")))
  
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
  
  write.csv(submission,"moderate_gbm_2018_01_14__21_23.csv",row.names = FALSE)
   
  
  
  
