
submission_prep <- raw_modelling_data %>%
  mutate(
    id = paste(air_store_id,visit_date,sep="_")
    ,visitors = reserve_visitors
  )%>%
  dplyr::select(id,visitors)

submission <- sample_submission %>%
  dplyr::select(id) %>%
  dplyr::left_join(submission_prep,by="id")

mean_visitors <- round(mean(raw_modelling_data$visitors,na.rm = TRUE),0)

submission <- submission %>%
  mutate(
    visitors = ifelse(is.na(visitors),mean_visitors,visitors)
  )

write.csv(submission,"benchmark 2.csv",row.names = FALSE)
  