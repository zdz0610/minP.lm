
#' @title One time simulation
#'
#' @param n_sim number of simulations
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


one_time_simulation<-function(n_sim,beta_0,beta_1,beta_2,mu,sigma,na,nb,permuted=FALSE){
  ## generate the data ##
  #true model

  if (!is.numeric(beta_0))
    stop("'beta_0' must be numeric")
  if (!is.numeric(beta_1))
    stop("'beta_1' must be numeric")
  if (!is.numeric(beta_2))
    stop("'beta_2' must be numeric")
  if (!is.numeric(mu))
    stop("'mu' must be numeric")
  if (!((length(sigma) == 1L) && is.finite(sigma) &&
        (sigma > 0) ))
    stop("'sigma' must be a finite number greater than 0")
  if (!((length(na) == 1L) && is.finite(na) &&
        (na > 0) ))
    stop("'na' must be a finite integer greater than 0")
  if (!((length(nb) == 1L) && is.finite(nb) &&
        (nb> 0) ))
    stop("'nb' must be a finite integer greater than 0")

  x0<-rep(0,na+nb)
  x1<-rep(1,na+nb)
  x2<-rep(1,na+nb)

  e<-rnorm(n=(na+nb),mean=0,sd=sigma)
  pre_x1<-rbinom(n=(na+nb),size =1,prob=0.5)
  pre_x2<-rbinom(n=(na+nb),size=1,prob=0.5)


  for(i in 1:(na+nb)){
    if(i<=na){
      x0[i]<--1
    }
    else{
      x0[i]<-1
    }
    if(pre_x1[i]==0){
      x1[i]=-1
    }
    if(pre_x2[i]==0){
      x2[i]=-1
    }

  }

  y<-mu+beta_0*x0+beta_1*x1+beta_2*x2+e
  ##end of generation##

  ## calculate minP and other statistics ##
  #get the p-value of model 1
  model_1<-lm(y~as.factor(x0)+x1)
  p_1<-summary(model_1)$coefficients[2,4]
  T_1<-summary(model_1)$coefficients[2,3]
  #get the p-value of model 2
  model_2<-lm(y~as.factor(x0)+x2)
  p_2<-summary(model_2)$coefficients[2,4]
  T_2<-summary(model_2)$coefficients[2,3]
  #observed value of minP
  minP_obs<-min(p_1,p_2)
  if(p_1<=p_2){
    chosen<-1
  }else{
    chosen<-2
  }
  if(permuted){
    #Permutations are generated by Monte Caro Simulation Method

    #permutation size as 5,000
    min_p_permutated<- numeric(5000)

    yy<-sapply(
      X=min_p_permutated, FUN=function(a){
        sample(y, size=length(y), replace=FALSE)
      }
    )

    t1_p_permutated<-apply(
      X=yy,2,FUN=function(a){
        model_1<-lm(a~as.factor(x0)+x1)
        p_1_per<-summary(model_1)$coefficients[2,4]
        p_1_per
      }
    )

    t2_p_permutated<-apply(
      X=yy,2,FUN=function(a){
        model_2<-lm(a~as.factor(x0)+x2)
        p_2_per<-summary(model_2)$coefficients[2,4]
        p_2_per
      }
    )

    min_p_b <- pmin(t1_p_permutated, t2_p_permutated)
    #get the permuted pvalue of the minp
    min_p_corrected<- (sum(minP_obs>= min_p_b))/(length(min_p_b))

  }
  if(permuted){
    return(c(min_p_corrected,minP_obs,p_1,p_2,chosen,T_1,T_2))
  }else{
    return(c(minP_obs,p_1,p_2,chosen,T_1,T_2))
  }

}
