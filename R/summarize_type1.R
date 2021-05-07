#' @title Calculation of Type 1 error and critical values of tests
#' @param data_sim the simulated data
#' @param signi_level significance level (default alpha=0.05)
#' @param permuted whether the simulated data includes the minP via permutation (default is FALSE)
#' @return list of type 1 errors and critical values
#' @importFrom stats quantile
#' @export
#'
summarize_type1<-function(data_sim,signi_level=0.05,permuted=FALSE){
  type1_error<-apply(
    X=data_sim,2,FUN=function(a){
      sum(a<signi_level)/length(data_sim[,1])
    }
  )
  crit_val<-apply(
    X=data_sim,2,FUN=function(a){
      quantile(a,signi_level)
    }
  )
  if(permuted){
    type1_error=type1_error[1:4]
    crit_val= crit_val[1:4]
  }else{
    type1_error=type1_error[1:3]
    crit_val= crit_val[1:3]
  }
  return(list(type1_error=type1_error,crit_val=crit_val))

}
