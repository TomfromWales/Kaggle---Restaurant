raw_modelling_data <- readRDS(paste0(project_wd,"/data/raw_modelling_data.rds"))
modelling_data_train <- readRDS(paste0(project_wd,"/data/modelling_data_train.rds"))

# In temp the global quantiles and means are assessed
# These would be the end points to which reconcile the cumulative over previous dates

# Checking temp on train and whole raw modelling data
# I would like to create the features in temp only
temp <- modelling_data_train %>%
          select(air_store_id,visit_date,visitors) %>%
#          group_by(air_store_id,visit_date) %>%
         group_by(air_store_id) %>%
         summarise(CountRws=n(),
                   CountNotNA=sum(!is.na(visitors)),
                   MeanV=mean(visitors,na.rm=T),
                   # Quantiles From Max to Min
                   q100V=quantile(visitors,probs=c(1),na.rm=T),
                   q095V=quantile(visitors,probs=c(0.05),na.rm=T),
                   q075V=quantile(visitors,probs=c(0.05),na.rm=T),
                   q050V=quantile(visitors,probs=c(0.05),na.rm=T),
                   q025V=quantile(visitors,probs=c(0.05),na.rm=T),
                   q005V=quantile(visitors,probs=c(0.05),na.rm=T),
                   q000V=quantile(visitors,probs=c(0),na.rm=T)
                   )

# Each row is identified by a store and a day
# The time window of the study is 517 days
# Not missing observations are distributed 
# from at least 20 dates up to 477 dates for each store
# In the train sample this goes from 17 up to 415 dates

  modelling_data_train %>%
  select(air_store_id,visit_date,visitors) %>%
  arrange(air_store_id,visit_date) %>%
  group_by(air_store_id) %>%
  mutate(MeanV=cummean(visitors),MaxV=cummax(visitors),MinV=cummin(visitors)) %>%
  # Filter on line one of temp to see if we match  
  filter(air_store_id==temp$air_store_id[1], row_number() == temp$CountRws[1])

  # Matches output of slice 1 on temp  
  temp %>%
    slice(1)
  
  #It does match.
  # Hence
  
CumFeats <-  modelling_data_train %>%
    select(air_store_id,visit_date,visitors) %>%
    arrange(air_store_id,visit_date) %>%
    group_by(air_store_id) %>%
    mutate(MeanV=cummean(visitors),MaxV=cummax(visitors),MinV=cummin(visitors))
    
raw_modelling_data <- raw_modelling_data %>%
                      left_join(y=CumFeats,by=c("air_store_id","visit_date")
                      )


