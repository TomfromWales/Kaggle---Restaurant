
cummax_custom <- function(x){
  x2 <- ifelse(is.na(x),0,x)
  return(cummax(x2))
}
cummean_custom <- function(x){
  #==apply corrected cummean
    x2 <- ifelse(is.na(x),0,x)
    weights <- ifelse(is.na(x),0,1)
    cum_weights <- cumsum(weights)
    cum_x <- cumsum(x2)
    
    x3 <- cum_x/cum_weights
  #==output
    return(x3)
}
cummin_custom <- function(x){
  #if na then returns global min, else returns the cummin
    x2 <- ifelse(is.na(x),max(x,na.rm=TRUE),x)
    x3 <- cummin(x2)
    x3[is.na(x)] <- min(x,na.rm=TRUE)
    return(x3)
}
# rolling_avg <- function(x){
#   n=2
#   as.numeric(stats::filter(x,rep(1/n,n), sides=1))
# }

# test data --------------------------------------------
  
  # df <- cbind.data.frame(
  #   air_store_id = c(rep(1,10),2,2)
  #   ,visit_date = c(seq(1,10),5,6)
  #   ,visitors = c(seq(2,6),NA,seq(1,4),5,6)
  # )
  # 
  #  df %>%
  #   arrange(air_store_id,visit_date) %>%
  #   group_by(air_store_id) %>%
  #   mutate(MeanV=cummean_custom(visitors),MaxV=cummax_custom(visitors),MinV=cummin_custom(visitors),ra=rolling_avg(visitors))

   
   