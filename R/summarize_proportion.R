#' @title Calculation the proportion of minP choosing model 1
#' @param data_sim the simulated data
#' @return Proportion that the minP test statistic chooses
#' @export
#'

summarize_proportion<-function(data_sim){
  sum(data_sim$chosen==1)/sum(data_sim$chosen)
}

