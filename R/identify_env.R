#' Identify the environment from new samples of community species
#' 
#' This function uses the result of [select_indicator_species()] to identify a new 
#' set of samples from the community
#'
#' @param com.to.identify A new sample or group of samples to identify the 
#' environment that they belong to.
#' @param indicator.species The indicator species such as that returned by
#'  [select_indicator_species()]
#' 
#' 
#' @param group vector of the sample grouping of the original community.
#' @param alfa Significance level used for the test.
#' 
#'
#' @return A list with two components: a data.frame with the environment estimation 
#' value for each group and a vector with the belonging environment estimated. 
#' If two environments coincides in the max value then returns NA.
#' @export
#'
#' @examples
#' 
#' data(soilandfauna)
#'  
#' # Select community (species) data
#' 
#' com <- soilandfauna[,18:60]
#' 
#' # Select a subset of samples belonging to the same environment
#' 
#' subcom <- com[3:10,]
#' 
#' # Estimate 
#' 
#' # Select grouping factor
#' 
#' group <- soilandfauna[,1]
#' 
#' identify_env(subcom, group)
#' 
identify_env <- function(com.to.identify, indicator.species, group ,alfa = 0.05){
  
  # Estima las especies indicadoras
  
  #indicator.species <- select_indicator_species(com,group,alfa)
  
  pcond.indic <- indicator.species$pcond[,indicator.species$names]

  com[com>0] <- 1 # Transforma la matriz a presencia-ausencia

  com.indic <- com[,indicator.species$names] 
  
  com.to.identify[com.to.identify>0] <- 1
  
  n <- nrow(indicator.species$pcond) 
  
  lim.sup<-(1/n)+qnorm(1-alfa)*sqrt(((1/n)*(1-(1/n)))/colSums(com.indic))
  lim.inf<-(1/n)-qnorm(1-alfa)*sqrt(((1/n)*(1-(1/n)))/colSums(com.indic))
  
  
  D <- pcond.indic
  
  Dt <- t(D)
  
  
  Dt[Dt > lim.sup | Dt < lim.inf] <- Dt[Dt > lim.sup | Dt < lim.inf] - 1/n

  
  Dt[Dt < lim.sup & Dt > lim.inf] <- 0
  
  com.to.identify.sel <- com.to.identify[,indicator.species$names]
  
  A <- colSums(com.to.identify.sel)
  
  env.estimation <- A %*% Dt
  
  ifelse(sum(env.estimation - max(env.estimation)==0)!=1, belonging.env <-  NA, 
         belonging.env <-  colnames(env.estimation)[apply(env.estimation, 1, which.max)])
  
  out <- list(env.estimation = as.data.frame(env.estimation),
                               belonging.env = belonging.env)
  
  out
  
}
