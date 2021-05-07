
#' @title Calculation of Power for tests
#' @param data_sim the simulated data
#' @param crit_val identified critical value of minP from 'summarize_type1' function
#' @param signi_level significance level (default alpha=0.05)
#' @param permuted whether the simulated data includes the minP via permutation (default is FALSE)
#' @return value of power for minP test, and prespecified models
#' @export


summarize_power<-function(data_sim,crit_val,signi_level=0.05,permuted=FALSE){

  if(permuted){
    power=rep(0,4)
    power[1]=sum(data_sim$observed_minP<crit_val)/length(data_sim[,1])
    power[2]=sum(data_sim$permuted_minP<signi_level)/length(data_sim[,1])
    power[3]=sum(data_sim$p_1<signi_level)/length(data_sim[,1])
    power[4]=sum(data_sim$p_2<signi_level)/length(data_sim[,1])

  }else{
    power=rep(0,3)
    power[1]=sum(data_sim$observed_minP<crit_val)/length(data_sim[,1])
    power[2]=sum(data_sim$p_1<signi_level)/length(data_sim[,1])
    power[3]=sum(data_sim$p_2<signi_level)/length(data_sim[,1])
  }

  return(power)

}
