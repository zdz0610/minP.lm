#' @title Calculation the correlation between the prespecified test statistics
#' @param data_sim the simulated data
#' @return Pearson correlation coefficient between test statistics
#' @importFrom stats cor
#' @export
#'
summarize_corr<-function(data_sim){
  cor(data_sim$T_1,data_sim$T_2)
}
