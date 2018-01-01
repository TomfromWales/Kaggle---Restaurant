# Load relevant data
############################
  air_visit_data_with_splits <- readRDS(paste0(project_wd,"/data/air_visit_data_with_splits.rds"))
  local_competition <- readRDS(paste0(project_wd,"/data/local_competition.rds"))
  reservation_data <- readRDS(paste0(project_wd,"/data/reservation_data.rds"))

# Join everything together
############################
  raw_modelling_data <- air_visit_data_with_splits %>%
    dplyr::left_join(local_competition,by = "air_store_id") %>%
    dplyr::left_join(reservation_data, by = c("air_store_id","visit_date"))%>%
    mutate(
      visit_date_day_of_week = wday(visit_date)
      ,visit_date_month = month(visit_date)
    )
  
# Save relevant objects
############################      
  saveRDS(
    object = raw_modelling_data
    ,file = paste0(
      project_wd
      ,"/data/"
      ,"raw_modelling_data"
      ,".rds"
    )
  )   
  
  