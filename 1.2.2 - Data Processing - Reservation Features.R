# Load relevant data
############################
  air_store_id_list <- readRDS(paste0(project_wd,"/data/air_store_id_list.rds"))
  air_reserve <- readRDS(paste0(project_wd,"/data/air_reserve.rds"))
  hpg_reserve <- readRDS(paste0(project_wd,"/data/hpg_reserve.rds"))
  store_id_relation <- readRDS(paste0(project_wd,"/data/store_id_relation.rds"))

# Gather details based on reservations
############################
  air_reserve_feats <- air_reserve %>%
    mutate(
      reserve_date = date(reserve_datetime),
      reserve_hour = hour(reserve_datetime),
      reserve_wday = wday(reserve_datetime, label = TRUE),
      visit_date = date(visit_datetime),
      visit_hour = hour(visit_datetime),
      visit_wday = wday(visit_datetime, label = TRUE),
      reservation_lag_hours = time_length(visit_datetime - reserve_datetime, unit = "hour"),
      reservation_lag_days = time_length(visit_datetime - reserve_datetime, unit = "day")
    )
    
  hpg_reserve_feats <- hpg_reserve %>%
    mutate(
      reserve_date = date(reserve_datetime),
      reserve_hour = hour(reserve_datetime),
      reserve_wday = wday(reserve_datetime, label = TRUE),
      visit_date = date(visit_datetime),
      visit_hour = hour(visit_datetime),
      visit_wday = wday(visit_datetime, label = TRUE),
      reservation_lag_hours = time_length(visit_datetime - reserve_datetime, unit = "hour"),
      reservation_lag_days = time_length(visit_datetime - reserve_datetime, unit = "day")
    )%>%
    dplyr::left_join(
      store_id_relation,by = "hpg_store_id"
    )%>%
    dplyr::filter(
      !is.na(air_store_id)
    )%>%
    select(-hpg_store_id)
  
  comb_reserve_feats <- rbind.data.frame(
    air_reserve_feats
    ,hpg_reserve_feats
  )%>%
    select(
      air_store_id
      ,visit_date
      ,reserve_visitors
      ,reserve_hour
      ,reserve_wday
      ,visit_hour
      ,visit_wday
      ,reservation_lag_hours
      ,reservation_lag_days
    )%>%
    mutate(
      #==reservation lags==#
        reserve_visitors_day_lag_0 = ifelse(floor(reservation_lag_days)==0,reserve_visitors,0)
        ,reserve_visitors_day_lag_1 = ifelse(floor(reservation_lag_days)==1,reserve_visitors,0)
        ,reserve_visitors_day_lag_2 = ifelse(floor(reservation_lag_days)==2,reserve_visitors,0)
        ,reserve_visitors_day_lag_3 = ifelse(floor(reservation_lag_days)==3,reserve_visitors,0)
        ,reserve_visitors_day_lag_4 = ifelse(floor(reservation_lag_days)==4,reserve_visitors,0)
        ,reserve_visitors_day_lag_5 = ifelse(floor(reservation_lag_days)==5,reserve_visitors,0)
        ,reserve_visitors_day_lag_6 = ifelse(floor(reservation_lag_days)==6,reserve_visitors,0)
        ,reserve_visitors_day_lag_7to13 = ifelse(floor(reservation_lag_days)>=7 & floor(reservation_lag_days)<=13,reserve_visitors,0)
        ,reserve_visitors_day_lag_14plus = ifelse(floor(reservation_lag_days)>=14,reserve_visitors,0)
      #==reservation hours==#
        ,reserve_visitors_booking_late_night = ifelse(
          reserve_hour>=22 | reserve_hour<=1
          ,reserve_visitors
          ,0
        )
        ,reserve_visitors_booking_unsociable = ifelse(
          reserve_hour>=2 | reserve_hour<=5
          ,reserve_visitors
          ,0
        )
        ,reserve_visitors_booking_early_morning = ifelse(
          reserve_hour>=6 | reserve_hour<=8
          ,reserve_visitors
          ,0
        )  
        ,reserve_visitors_booking_work = ifelse(
          reserve_hour>=9 | reserve_hour<=5
          ,reserve_visitors
          ,0
        )    
        ,reserve_visitors_booking_evening = ifelse(
          reserve_hour>=6 | reserve_hour<=9
          ,reserve_visitors
          ,0
        )         
    )
    
  comb_reserve_feats2 <- comb_reserve_feats %>%
    group_by(
      air_store_id,visit_date
    )%>%
    summarise(
      #==reservation - total visitors==#
        reserve_visitors = sum(reserve_visitors,na.rm=TRUE)
      #==reservation lags==#
        ,reserve_visitors_day_lag_0 = sum(reserve_visitors_day_lag_0,na.rm=TRUE)
        ,reserve_visitors_day_lag_1 = sum(reserve_visitors_day_lag_1,na.rm=TRUE)
        ,reserve_visitors_day_lag_2 = sum(reserve_visitors_day_lag_2,na.rm=TRUE)
        ,reserve_visitors_day_lag_3 = sum(reserve_visitors_day_lag_3,na.rm=TRUE)
        ,reserve_visitors_day_lag_4 = sum(reserve_visitors_day_lag_4,na.rm=TRUE)
        ,reserve_visitors_day_lag_5 = sum(reserve_visitors_day_lag_5,na.rm=TRUE)
        ,reserve_visitors_day_lag_6 = sum(reserve_visitors_day_lag_6,na.rm=TRUE)
        ,reserve_visitors_day_lag_7to13 = sum(reserve_visitors_day_lag_7to13,na.rm=TRUE)
        ,reserve_visitors_day_lag_14plus = sum(reserve_visitors_day_lag_14plus,na.rm=TRUE)
      #==reservation hours==#
        ,reserve_visitors_booking_late_night = sum(reserve_visitors_booking_late_night,na.rm=TRUE)
        ,reserve_visitors_booking_unsociable = sum(reserve_visitors_booking_unsociable,na.rm=TRUE)
        ,reserve_visitors_booking_early_morning = sum(reserve_visitors_booking_early_morning,na.rm=TRUE)
        ,reserve_visitors_booking_work = sum(reserve_visitors_booking_work,na.rm=TRUE)
        ,reserve_visitors_booking_evening = sum(reserve_visitors_booking_evening,na.rm=TRUE)
    )%>%
    as.data.frame()
  
  # Join onto air_store_id_list
  #============================
    reservation_data <- air_store_id_list %>%
      dplyr::left_join(comb_reserve_feats2,by = "air_store_id")
  
# Save relevant objects
############################      
  saveRDS(
    object = reservation_data
    ,file = paste0(
      project_wd
      ,"/data/"
      ,"reservation_data"
      ,".rds"
    )
  )   
  
  
  