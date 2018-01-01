#' @title Plot a graph of observed vs predicted values across a feature.
#' @description This function takes a dataset with associated predictions.
#' For a specified feature, a plot of the observed vs the predicted values is produced.
#'
#' @param data name of the data frame to evaluate
#' @param prediction prediction per unit weight
#' @param actual name of the column containing the weights
#' @param weight name of the column containing the weights
#' @param feature name of the column containing the feature of interest
#' @return a (plotly) plot is returned
#' @details Some details here
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plotly_actual_vs_predicted
#' @export
#'
#=============================================================================
# Change History :
#   [2017/12/06 [Tom Snowdon] [Created]
#=============================================================================

plotly_actual_vs_predicted <- function(data,actual,prediction,weight,feature){

  library("plotly")

  # Error Handling
  #===============================================
    # Force data into a data frame
    #------------------------------------
      data <- as.data.frame(data)

  # Data Prep
  #===============================================
    #---Copy each featureiable to save complicated dplyr references--# ###Should be removed in future.###
      data[,"weight"] <- data[,weight]
      data[,"prediction"] <- data[,prediction]
      data[,"actual"] <- data[,actual]
      data[,"feature"] <- data[,feature]

      data[,"weighted_prediction"] <- data[,"weight"]*data[,"prediction"]
      TotalWeight <- sum(data[,weight])

    #--Summaries--# ###Is this nececcary, or can we just use sufficient stats when calling ggplot?
      data_summarised <- data %>%
        group_by(feature) %>%
          summarise(
            weight_sum = sum(weight),
            actuals_mean = sum(actual)/weight_sum,
            preds_mean = sum(weighted_prediction)/weight_sum,
            weight_perc = sum(weight)/TotalWeight
          )
      data_summarised <- as.data.frame(data_summarised) ###Convert to dataframe to work with other functions below###
      data_summarised$feature <- as.factor(data_summarised$feature)

  # Create plot
  #===============================================
   # Cals to help with axes on the plot
    #------------------------------------
      MinPreds <- min(subset(data_summarised[,"preds_mean"],!is.na(data_summarised[,"preds_mean"])))
      MinActuals <- min(subset(data_summarised[,"actuals_mean"],!is.na(data_summarised[,"actuals_mean"])))
      MinWeight <- min(subset(data_summarised[,"weight_perc"],!is.na(data_summarised[,"weight_perc"])))

      MaxPreds <- max(subset(data_summarised[,"preds_mean"],!is.na(data_summarised[,"preds_mean"])))
      MaxActuals <- max(subset(data_summarised[,"actuals_mean"],!is.na(data_summarised[,"actuals_mean"])))
      MaxWeight <- max(subset(data_summarised[,"weight_perc"],!is.na(data_summarised[,"weight_perc"])))

      MeanPreds <- mean(subset(data_summarised[,"preds_mean"],!is.na(data_summarised[,"preds_mean"])))
      MeanActuals <- mean(subset(data_summarised[,"actuals_mean"],!is.na(data_summarised[,"actuals_mean"])))
      MeanWeight <- mean(subset(data_summarised[,"weight_perc"],!is.na(data_summarised[,"weight_perc"])))

      #==Calc tick marks==#
        y_plot_max = max(MaxPreds,MaxActuals)*1.1
        y_plot_min = min(MinPreds,MinActuals)*0.8
        y_tick_space_FUN <- function(range,max_ticks){
          minimum_tick_space = range/max_ticks
          magnitude = 10^floor(log10(minimum_tick_space))
          residual = minimum_tick_space/magnitude

          if (residual>5){
            y_tick = 10*magnitude
          }else if(residual > 2){
            y_tick = 5*magnitude
          }else if(residual > 1){
            y_tick = 2*magnitude
          }else{
            y_tick = magnitude
          }
          return(y_tick)
        }
      y_tick_space = y_tick_space_FUN(y_plot_max - y_plot_min,10)

    # Plot
    #------------------------------------
      p <- plot_ly(
        data = data_summarised
        ,x = ~feature #~get(feature)
        ,y = ~weight_perc
        ,name = weight
        ,type = "bar"
        ,marker = list(color="khaki")
      )%>%
        add_trace(
          x = ~feature
          ,y = ~actuals_mean
          ,name = actual
          ,type = "scatter"
          ,marker = list(color="magenta")
          ,mode = "lines+markers"
          ,line = list(color="magenta")
          ,yaxis = 'y2'
        )%>%
        add_trace(
          x = ~feature
          ,y = ~preds_mean
          ,name = prediction
          ,type = "scatter"
          ,marker = list(color="green")
          ,mode = "lines+markers"
          ,line = list(color="green")
          ,yaxis = "y2"
        )%>%
        layout(
          title = paste0("Actual vs Predicted : ",feature)
          ,legend = list(
            orientation = "h"
          )
          ,xaxis = list(
            title = feature
            ,showgrid = TRUE
          )
          ,yaxis = list(
            title = "Weight",
            showgrid = FALSE
            ,range = c(0,MaxWeight*3)
            ,side = "left"
            ,linecolor = "black"
            ,fixedrange = TRUE
          )
          ,yaxis2 = list(
            title = "Response",
            showgrid = TRUE
            ,range = c(y_plot_min,y_plot_max)
            ,side = "right"
            ,overlaying = "y"
            ,autosize = "T"
            ,linecolor = "black"
            ,dtick = y_tick_space
          )
          ,bargap = 0.25
          ,margin = list(t=100,l=50,r=50,b=50)
          ,plot_bgcolor = "rgb(220,250,250)"
          ,paper_bgcolor = "rgb(220,250,250)"
        )

  return(p)

}


#
# test <- cbind.data.frame(
#   Obs = c(0,3,2,2)
#   ,Pred = c(0.5,1.5,1.8,2.2)
#   ,Weight = c(0.5,0.5,1,2)
#   ,Feat = c(1,1,2,3)
# )
#
# plotly_actual_vs_predicted(
#   data = test
#   ,actual = "Obs"
#   ,prediction = "Pred"
#   ,weight = "Weight"
#   ,feature = "Feat"
# )





