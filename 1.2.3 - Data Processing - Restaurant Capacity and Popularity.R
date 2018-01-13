# Load relevant data
############################
  air_visit_data_with_splits <- readRDS(paste0(project_wd,"/data/air_visit_data_with_splits.rds"))
  
# Create new features
############################
  restaurant_capacity_and_popularity_feats <- air_visit_data_with_splits %>%
    select(air_store_id,visitors)%>%
    group_by(air_store_id)%>%
    summarise(
      rest_max_ever_visitors = max(visitors,na.rm = TRUE),
      rest_mean_visitors_all_time = mean(visitors,na.rm = TRUE)
    )
  
# Save relevant objects
############################      
  saveRDS(
    object = restaurant_capacity_and_popularity_feats
    ,file = paste0(
      project_wd
      ,"/data/"
      ,"restaurant_capacity_and_popularity_feats"
      ,".rds"
    )
  )   
  
  
  