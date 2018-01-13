rmsle <- function(
  data
  ,prediction
  ,actual
){
  
  # Error Handling
  #==================
    data$prediction <- data[,prediction]

    if(nrow(data%>%dplyr::filter(is.na(prediction)))>0){
      stop("NAs in the predictions")
    }
    
  # Calculate RMLSE
  #==================    
    value <- Metrics::rmsle(
      actual = data[,actual]
      ,predicted = data[,prediction]
    )
    
  # Output
  #================== 
    return(value)
}






