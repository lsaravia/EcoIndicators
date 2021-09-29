#' Identify the environment from new samples of community species
#' 
#' This function use the result of `select_indicator.species` to identify a new 
#' set of samples of the community
#'
#' @param com The original community used to select indicator species
#' @param com.to.identify A new sample o group of samples to identify the 
#' environment that they belong.
#' @param indicator.species Result of `selec_indicator_species`
#' @param alfa significance level used for the test
#' 
#'
#' @return The environment.
#' @export
#'
#' @examples #' 
#' 
#' #' da <- read.delim("Data/data.txt")
#' 
#' # Select community (species) data
#' 
#' com <- da[,18:50]
#' 
#' newsamples <- 1
#' 

identify_env <- function(com, com.to.identify, indicator.species,alfa = 0.05){
  
  require(vegan)
  
  # Agregar warnings y stop 
  # revisando algunas condiciones de los argumentos
  
  # Creamos un subconjunto de las probabilidades condicionales con las que 
  # corresponden a las especies indicadoras
  
  pcond.indic <- indicator.species$pcond[,indicator.species$names ] #subset(indicator.species$pcond,select = indicator.species$col.indic)

  


  com.pa <- decostand(com,method = "pa")

  com.pa.indic <- com.pa[,indicator.species$names] 
  
  n <- nrow(indicator.species$pcond) 
  
  lim.sup<-(1/n)+qnorm(1-alfa)*sqrt(((1/n)*(1-(1/n)))/colSums(com.pa.indic))
  lim.inf<-(1/n)-qnorm(1-alfa)*sqrt(((1/n)*(1-(1/n)))/colSums(com.pa.indic))
  
  
  D <- pcond.indic
  
  Dt <- t(D)
  
  
  Dt[Dt > lim.sup | Dt < lim.inf] <- Dt[Dt > lim.sup | Dt < lim.inf] - 1/n

  
  Dt[Dt < lim.sup & Dt > lim.inf] <- 0
  

  
  com2id.pa <- decostand(com.to.identify,method="pa")
  

  com2id.sel <- com2id.pa[,indicator.species$names]
  
  A <- colSums(com2id.sel)
  
  
  
  est.env <- A %*% Dt
  
  select.est.env <- colnames(est.env)[apply(est.env, 1, which.max)]
  
  select.est.env
  
}