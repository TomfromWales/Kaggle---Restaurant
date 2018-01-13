h2o_gbm_CV_fun <- function(
  data
  ,response
  ,features
  ,k_fold_bucket_variable
  ,hyper_parameters
){
  
  library(h2o)
  h2o.init()
  
  # Check numbers of buckets
  #====================
    num_folds = max(data[,k_fold_bucket_variable])
    
  # Set up data splits and send to h2o
  #===================
    for(k in 1:num_folds){
      # Isolate the folds and send to h2o
      #----------------
        train <- data %>%
          dplyr::filter_(interp(~v!=k, v=as.name(k_fold_bucket_variable)))
        as.h2o(train,destination_frame = paste0("train_",k))
        
        validate <- data %>%
          dplyr::filter_(interp(~v==k, v=as.name(k_fold_bucket_variable)))
        as.h2o(validate,destination_frame = paste0("validate_",k))
        assign(paste0("validate_",k),validate)
    }
  
  # Set up list with parameter sets to search over
  #===================
    search_grid = expand.grid(hyper_parameters)
    
  # Build and evaluate models
  #====================
    for(i in 1:nrow(search_grid)){
      
      Preds_for_validation <- head(validate_1,0) #initialise CV predictions data
      
      for(k in 1:num_folds){
        # Build model
        #----------------
          h2o.gbm(
            training_frame = h2o.getFrame(paste0("train_",k))
            ,y = response
            ,x = features
            ,ntrees = search_grid[i,"ntrees"]
            ,max_depth = search_grid[i,"max_depth"]
            ,learn_rate = search_grid[i,"learn_rate"]
            ,distribution = "poisson"
            ,sample_rate = search_grid[i,"sample_rate"]
            ,min_rows = search_grid[i,"min_rows"]
            ,model_id = "gbm"
          )        
        
        # Make Predictions
        #----------------    
          predictions <- as.matrix(
            h2o.predict(
              h2o.getModel("gbm")
              ,h2o.getFrame(paste0("validate_",k))
            )
          )
          predictions_df <- cbind.data.frame(
            get(paste0("validate_",k))
            ,predictions = predictions
          )
            
          Preds_for_validation <- bind_rows(
            Preds_for_validation
            ,predictions_df
          )
        
      }
      
      # Evaluate Performance
      #-----------------
        search_grid[i,"rmsle"] = rmsle(
          data = Preds_for_validation
          ,prediction = "predict"
          ,actual = response
        )
      
      
    }
  
  # Output
  #================
    return(search_grid)
  
}

# test data ---------------------------------------------------------

# data <- cbind.data.frame(
#   y = c(10,20,23,34,53,53,23,37)
#   ,x = c(1,2,3,2,4,5,4,2)
#   ,k_bucket = c(1,2,2,1,1,1,2,2) 
# )
# 
# hyper_parameters = list(
#   ntrees = c(100,1000,5000)
#   ,max_depth = c(3,6,9)
#   ,learn_rate = c(0.1)
#   ,min_rows = c(1)
#   ,sample_rate = c(0.8)
# )
# 
# h2o_gbm_CV_fun(
#   data = data
#   ,response = "y"
#   ,features = c("x")
#   ,k_fold_bucket_variable = "k_bucket"
#   ,hyper_parameters = hyper_parameters
# )
  
# modelling ---------------------------------------------------------

  modelling_data_train <- readRDS(paste0(project_wd,"/data/modelling_data_train.rds"))
  modelling_data_validate <- readRDS(paste0(project_wd,"/data/modelling_data_validate.rds"))
  sample_submission <- readRDS(paste0(project_wd,"/data/sample_submission.rds"))
  
  modelling_data_train_with_k <- modelling_data_train
  modelling_data_train_with_k$k_bucket_2 = ceiling(runif(nrow(modelling_data_train_with_k))*2)
  modelling_data_train_with_k$k_bucket_3 = ceiling(runif(nrow(modelling_data_train_with_k))*3)
  modelling_data_train_with_k$k_bucket_4 = ceiling(runif(nrow(modelling_data_train_with_k))*4)
  modelling_data_train_with_k$k_bucket_5 = ceiling(runif(nrow(modelling_data_train_with_k))*5)
  
  hyper_parameters = list(
    ntrees = c(1000)
    ,max_depth = c(10)
    ,learn_rate = c(0.1)
    ,min_rows = c(50,100,200)
    ,sample_rate = c(0.8)
  )
  
  results <- h2o_gbm_CV_fun(
    data = modelling_data_train_with_k
    ,response = "visitors"
    ,features = c(
      "visit_date_month"
      ,"visit_date_day_of_week"
      ,"genre"
      ,"num_local_competitors"
      ,"num_local_competitors_same_genre"
      ,"reserve_visitors"
      ,"reserve_visitors_day_lag_0"
      ,"visit_date_day_of_month"
      ,"holiday_flg"
      # ,"holiday_flg_lag1"
      # ,"holiday_flg_lagminus1"
      # ,"holiday_flg_lag2"
      # ,"holiday_flg_lagminus2"
      # ,"holiday_flg_lag3"
      ,"holiday_flg_lagminus3"
      ,"rest_max_ever_visitors"
      ,"rest_mean_visitors_all_time"
    )
    ,k_fold_bucket_variable = "k_bucket_2"
    ,hyper_parameters = hyper_parameters
  )











  