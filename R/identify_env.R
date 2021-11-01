#' Identify the environment from new samples of community species
#' 
#' This function use the result of `select_indicator_species` to identify a new 
#' set of samples of the community
#'
#' @param com The original community used to select indicator species.
#' @param com.to.identify A new sample o group of samples to identify the 
#' environment that they belong.
#' @param group vector of the samples grouping
#' @param alfa Significance level used for the test.
#' 
#'
#' @return A list with the the environemnt estimation value for each group 
#' and the belonging environment estimated.
#' @export
#'
#' @examples
#' 
#' data(soilandfauna)
#'  
#' # Select community (species) data
#' 
#' com <- soilandfauna[,18:50]
#' 
#' # Select a subset of samples belonging to the same environment
#' 
#' subcom <- com[3:10,]
#' 
#' # Select grouping factor
#' 
#' group <- soilandfauna[,1]
#' 
#' identify_env(com, subcom, group)
#' 
identify_env <- function(com, com.to.identify, group ,alfa = 0.05){
  
  # Estima las especies indicadoras
  
  indicator.species <- select_indicator_species(com,group,alfa)
  
  pcond.indic <- indicator.species$pcond[,indicator.species$names]

  com[com>0] <- 1 # Transforma la matriz a presencia-ausencia

  com.indic <- com[,indicator.species$names] 
  
  com.to.identify[com.to.identify>0] <- 1 # Transforma la matriz a presencia-ausencia
  
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
  
  out <- list(env.estimation = as.data.frame(env.estimation),
                               belonging.env = colnames(env.estimation)[apply(env.estimation, 1, 
                                                                      which.max)])
  
  out
  
}
