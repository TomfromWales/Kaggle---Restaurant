# Load relevant data
############################
  air_visit_data_with_splits <- readRDS(paste0(project_wd,"/data/air_visit_data_with_splits.rds"))
  
# Create restaurant specific features
############################
  restaurant_capacity_and_popularity_feats_to_date <- air_visit_data_with_splits %>%
    select(air_store_id,visit_date,visitors)%>%
    arrange(air_store_id,visit_date) %>%
    group_by(air_store_id)%>%
    mutate(
      MeanV=cummean_custom(visitors)
      ,MaxV=cummax_custom(visitors)
      ,MinV=cummin_custom(visitors)
      ,MeanV_todate = lag(MeanV)
      ,MaxV_todate = lag(MaxV)
      ,MinV_todate = lag(MinV)
    )%>%
    select(-MeanV,-MaxV,-MinV,-visitors)
  
  restaurant_capacity_and_popularity_feats_all_time <- air_visit_data_with_splits %>%
    select(air_store_id,visitors)%>%
    group_by(air_store_id)%>%
    summarise(
      MaxV_alltime = max(visitors,na.rm = TRUE)
      ,MeanV_alltime = mean(visitors,na.rm = TRUE)
    )

# Save relevant objects
############################      
  saveRDS(
    object = restaurant_capacity_and_popularity_feats_to_date
    ,file = paste0(
      project_wd
      ,"/data/"
      ,"restaurant_capacity_and_popularity_feats_to_date"
      ,".rds"
    )
  )   
  saveRDS(
    object = restaurant_capacity_and_popularity_feats_all_time
    ,file = paste0(
      project_wd
      ,"/data/"
      ,"restaurant_capacity_and_popularity_feats_all_time"
      ,".rds"
    )
  )     
  
  