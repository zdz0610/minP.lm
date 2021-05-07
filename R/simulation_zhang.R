#' @title Simulation for the prespecified iterations
#'
#' @param n_sim number of simulations
#' @param parallel_state whether to use the parallel function (Default is False)
#' @param random_seed specify a random seed to reproduce the results (Default seed is 123)
#' @param beta_0 coefficient of the treatment
#' @param beta_1 coefficient of the covariate 1
#' @param beta_2 cofficient of the covariate 2
#' @param mu the overall mean response
#' @param sigma standard error of the error term
#' @param na number of patients in the control
#' @param nb number of patients in the treatment
#' @param permuted whether use the permutation algorithm (Default is FALSE)
#' @return data_sim
#' @import parallel
#' @importFrom stats lm rbinom rnorm runif
#' @export
#'

simulation_zhang<-function(n_sim,parallel_state=FALSE,random_seed=123,beta_0,beta_1,beta_2,mu,sigma,na,nb,permuted=FALSE){
  data_sim<-NULL
  if(parallel_state){
    RNGkind("L'Ecuyer-CMRG")
    set.seed (random_seed) # Set seed
    if (Sys.info()[1] == "Windows"){
      cl = parallel::makeCluster(detectCores()-2)
      parallel::clusterSetRNGStream(cl, iseed=random_seed)
      data_sim= parallel::parLapply(cl, 1:n_sim,one_time_simulation,beta_0=beta_0,beta_1=beta_1,beta_2=beta_2,mu=mu,sigma=sigma,na=na,nb=nb,permuted=permuted)
      parallel::stopCluster(cl)
    } else {
      data_sim= parallel::mclapply (1:n_sim,one_time_simulation,beta_0=beta_0,beta_1=beta_1,beta_2=beta_2,mu=mu,sigma=sigma,na=na,nb=nb,permuted=permuted,mc.cores=detectCores())
    }

    data_sim<-data.frame(matrix(unlist(data_sim), nrow=length(data_sim), byrow=TRUE))
    if(permuted){
      colnames(data_sim) <- c("permuted_minP","observed_minP","p_1","p_2","chosen","T_1","T_2")
    }else{
      colnames(data_sim) <- c("observed_minP","p_1","p_2","chosen","T_1","T_2")
    }

    return(data_sim)
    #sapply (op, function (x) x$draw)
    #sapply (op, function (x) x$arg)

  }else{
    set.seed(random_seed)
    for(sim_times in 1:n_sim){
      temp<-one_time_simulation(beta_0=beta_0,beta_1=beta_1,beta_2=beta_2,mu=mu,sigma=sigma,na=na,nb=nb,permuted=permuted)
      data_sim<-rbind(data_sim,temp)

    }
    data_sim<-as.data.frame(data_sim)
    if(permuted){
      colnames(data_sim) <- c("permuted_minP","observed_minP","p_1","p_2","chosen","T_1","T_2")
    }else{
      colnames(data_sim) <- c("observed_minP","p_1","p_2","chosen","T_1","T_2")
    }

    return(data_sim)
  }

}
