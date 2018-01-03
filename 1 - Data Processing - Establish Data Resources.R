# Read in data
############################
  # Source data from Kaggle
  #=========================
    # Read in files
    #-----------------------
      source_data_location <- paste0(project_wd,"/data/source_data")
      
      for(filename in c("air_reserve","air_store_info","air_visit_data","date_info","hpg_reserve","hpg_store_info","sample_submission","store_id_relation")){
        
        assign(
          filename,
          fread(paste0(source_data_location,"/",filename,".csv"),data.table = FALSE)
        )
    
        print(filename)
        print(head(get(filename)))
      }

    # Clean & reformat data
    #-----------------------
      air_visit_data <- air_visit_data %>%
        mutate(visit_date = ymd(visit_date))
      
      date_info <- date_info %>%
        mutate(calendar_date = ymd(calendar_date))      
      
      air_reserve <- air_reserve %>%
        mutate(
          visit_datetime = ymd_hms(visit_datetime)
          ,reserve_datetime = ymd_hms(reserve_datetime)
        )
      
      hpg_reserve <- hpg_reserve %>%
        mutate(
          visit_datetime = ymd_hms(visit_datetime)
          ,reserve_datetime = ymd_hms(reserve_datetime)
        )
      
    # Store data in project folders
    #-----------------------      
      for(filename in c("air_reserve","air_store_info","air_visit_data","date_info","hpg_reserve","hpg_store_info","sample_submission","store_id_relation")){
       
        saveRDS(
          object = get(filename)
          ,file = paste0(
            project_wd
            ,"/data/"
            ,filename
            ,".rds"
          )
        ) 
        
      }  
      
  # External data
  #=========================    
    # Weather data from , this is all that's allowed (from "http://www.data.jma.go.jp/gmd/risk/obsdl/index.php#")
    #-----------------------      
      
    # Store data in project folders
    #-----------------------  
# Other bits
############################
  # List of all air_store_ids so we can join all of our features onto a central key
  #=========================
    air_store_id_list = air_store_info %>%
      select(air_store_id)
      
  # Add modelling splits to air_visits_data
  #=========================
    set.seed(777)
    
    air_visit_data_with_splits <- cbind.data.frame(
      air_visit_data
      ,modelling_bucket_6 = floor(runif(nrow(air_visit_data))*6)
    )
    
    full_rest_visit_combo <- expand.grid(
      air_store_id = unique(c(air_visit_data_with_splits$air_store_id,air_reserve$air_store_id))
      ,visit_date = seq(from=as.Date('2016-01-01'),to=as.Date('2017-05-31'),by=1)
      ,stringsAsFactors = FALSE
    )
      
    air_visit_data_with_splits <- full_rest_visit_combo %>%
      dplyr::left_join(air_visit_data_with_splits,by=c("air_store_id","visit_date"))    
    
  # Store data in project folders
  #=========================
    for(filename in c("air_visit_data_with_splits")){
     
      saveRDS(
        object = get(filename)
        ,file = paste0(
          project_wd
          ,"/data/"
          ,filename
          ,".rds"
        )
      ) 
      
    }        
      
      
      
      
      
      
      
      
      