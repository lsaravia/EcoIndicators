#' Identify the environment from new samples of community species
#' 
#' This function use the result of `select_indicator_species` to identify a new 
#' set of samples of the community
#'
#' @param com The original community used to select indicator species.
#' @param com.to.identify A new sample o group of samples to identify the 
#' environment that they belong.
#' @param indicator.species Result of `selec_indicator_species`.
#' @param alfa Significance level used for the test.
#' 
#'
#' @return The environment.
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
#' # Read data of new community matrix with abundances of the same
#' # species as 'com'.
#' 
#' newcom <- read.delim("data/newcom.txt")
#' 
#' # Select grouping factor
#' group <- soilandfauna[,1]
#' 
#' identify_env(com, newsamples, select_indicator_species(com,group))
#' 
identify_env <- function(com, com.to.identify, indicator.species,alfa = 0.05){
  
  # Creamos un subconjunto de las probabilidades condicionales que 
  # corresponden a las especies indicadoras
  
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
  
  est.env <- A %*% Dt
  
  select.est.env <- colnames(est.env)[apply(est.env, 1, which.max)]
  
  select.est.env
  
}