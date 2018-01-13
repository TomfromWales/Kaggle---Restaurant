rmlse <- function(
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
    value <- sqrt(
      (1/nrow(data))*
      sum(
        log(data[,"prediction"]+1)
        - log(data[,"actual"]+1)
      )^2
    )
    
  # Output
  #================== 
    return(value)
}

