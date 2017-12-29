library(tidyverse)
library(lubridate)
library(data.table)

# Read in data
############################
  # Source data from Kaggle
  #=========================
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
  #=========================
    air_visit_data <- air_visit_data %>%
      mutate(visit_date = ymd(visit_date))
    
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
    
# Feature Prep
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
  
  # Get features on number of competition (total, and similar class) "nearby"
  #==========================
    # Get a deduped list of restaurants
    #===================================
      hpg_store_info2 <- hpg_store_info %>%
        dplyr::left_join(store_id_relation,by = "hpg_store_id")
      hpg_rests_in_air <- hpg_store_info2 %>%
        dplyr::filter(!is.na(air_store_id))%>%
        select(-latitude,-longitude)
      hpg_rests_NOT_in_air <- hpg_store_info2 %>%
        dplyr::filter(is.na(air_store_id))
      
      all_restaurants_temp <- air_store_info %>%
        dplyr::left_join(hpg_rests_in_air,by="air_store_id")
      
      all_restaurants <- bind_rows(all_restaurants_temp,hpg_rests_NOT_in_air)
            
    # EDA - what cross over in categories do we get between air and hpg
    #====================================
      all_restaurants_both <- all_restaurants%>%dplyr::filter(!is.na(air_store_id)&!is.na(hpg_store_id))
      
      all_restaurants_both %>% select(air_genre_name,hpg_genre_name)
      
      write.csv(
        table(all_restaurants_both$air_genre_name,all_restaurants_both$hpg_genre_name)
        ,"air_genre_X_hpg_genre.csv"
      )
      write.csv(
        table(all_restaurants_both$hpg_genre_name,all_restaurants_both$air_genre_name)
        ,"hpg_genre_X_air_genre.csv"
      )
      
    # Map hpg restaurant genres to the air categories
    #=================================================
      genre_mapping <- cbind.data.frame(
        hpg_genre = c(
          "Amusement bar",
          "Cafe",
          "Creation",
          "Creative Japanese food",
          "Grilled meat",
          "International cuisine",
          "Italian",
          "Japanese cuisine/Kaiseki",
          "Japanese food in general",
          "Japanese style",
          "Karaoke",
          "Okonomiyaki/Monja/Teppanyaki",
          "Party",
          "Seafood",
          "Spain Bar/Italian Bar",
          "Steak/Hamburger/Curry"
        )
        ,air_genre = c(
          "Bar/Cocktail",
          "Bar/Cocktail",
          "Izakaya",
          "Izakaya",
          "Yakiniku/Korean food",
          "Dining bar",
          "Italian/French",
          "Japanese food",
          "Japanese food",
          "Izakaya",
          "Bar/Cocktail",
          "Okonomiyaki/Monja/Teppanyaki",
          "Karaoke/Party",
          "Izakaya",
          "Italian/French",
          "Western food"
        )
      )

      
      
      
      
      
      
      
      
      
  # Get everything together
  #==========================
    full_data <- air_visit_data %>%
      dplyr::left_join(comb_reserve_feats2,by = c("air_store_id","visit_date"))  

    
# Read in encodings
############################    
       
# Summarise reservations by "reservation lag"

    
    
    

