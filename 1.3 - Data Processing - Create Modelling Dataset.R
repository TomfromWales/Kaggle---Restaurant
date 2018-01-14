# Load relevant data
############################
  air_visit_data_with_splits <- readRDS(paste0(project_wd,"/data/air_visit_data_with_splits.rds"))
  restaurant_capacity_and_popularity_feats_to_date <- readRDS(paste0(project_wd,"/data/restaurant_capacity_and_popularity_feats_to_date.rds"))
  restaurant_capacity_and_popularity_feats_all_time <- readRDS(paste0(project_wd,"/data/restaurant_capacity_and_popularity_feats_all_time.rds"))
  local_competition <- readRDS(paste0(project_wd,"/data/local_competition.rds"))
  reservation_data <- readRDS(paste0(project_wd,"/data/reservation_data.rds"))
  date_info <- readRDS(paste0(project_wd,"/data/date_info.rds"))

# Join everything together
############################
  raw_modelling_data <- air_visit_data_with_splits %>%
    dplyr::left_join(local_competition,by = "air_store_id") %>%
    dplyr::left_join(reservation_data, by = c("air_store_id","visit_date"))%>%
    # For the following merge to work Establish Data Resource needs to convert calendar_date to date format
    dplyr::left_join(date_info%>%select(-day_of_week),by = c("visit_date" = "calendar_date"))%>%
    dplyr::left_join(restaurant_capacity_and_popularity_feats_to_date,by = c("air_store_id","visit_date"))%>%
    dplyr::left_join(restaurant_capacity_and_popularity_feats_all_time,by = "air_store_id")%>%
    mutate(
      visit_date_day_of_week = lubridate::wday(visit_date)
      ,visit_date_day_of_month =lubridate::mday(visit_date)
      ,visit_date_month = lubridate::month(visit_date)
      ,holiday_flg_lag1 = dplyr::lag(holiday_flg)
      ,holiday_flg_lagminus1 = dplyr::lead(holiday_flg)
      ,holiday_flg_lag2 = dplyr::lag(holiday_flg,n=2)
      ,holiday_flg_lagminus2 = dplyr::lead(holiday_flg,n=2)
      ,holiday_flg_lag3 = dplyr::lag(holiday_flg,n=3)
      ,holiday_flg_lagminus3 = dplyr::lead(holiday_flg,n=3)  
      
      ,holiday_flg_lag1 = ifelse(is.na(holiday_flg_lag1),0,holiday_flg_lag1)
      ,holiday_flg_lagminus1 = ifelse(is.na(holiday_flg_lagminus1),0,holiday_flg_lagminus1)
      ,holiday_flg_lag2 = ifelse(is.na(holiday_flg_lag2),0,holiday_flg_lag2)
      ,holiday_flg_lagminus2 = ifelse(is.na(holiday_flg_lagminus2),0,holiday_flg_lagminus2)
      ,holiday_flg_lag3 = ifelse(is.na(holiday_flg_lag3),0,holiday_flg_lag3)
      ,holiday_flg_lagminus3 = ifelse(is.na(holiday_flg_lagminus3),0,holiday_flg_lagminus3)       
      
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
  
  