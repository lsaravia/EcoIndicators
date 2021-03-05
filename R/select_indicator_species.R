#' Select indicator species
#' 
#' This function select species that indicate with a given probability the 
#' pertenence to an environment using a matrix of species samples at different
#' environments using a chisqrt test with a given significance level. 
#'
#' @param com matrix or data.frame with species in columns and samples in rows
#' @param group vector of the environments of the samples
#' @param alfa significance level used for the test
#'
#' @return A data.frame with the species that are above
#' @export
#'
#' @examples
#' da <- read.delim("Data/data.txt")
#' ############################# CHANGE THIS TO EMBEDDED PACKAGE DATA
#' 
#' # Select community (species) data
#' com <- da[,18:50]
#' # Select environmental data
#' env <- da[,3:17]
#' 
#' # Grouping factor
#' group <- da[,1]
#' 
#' select_indicator_species(com,group,0.05)
#' 
select_indicator_species <- function(com,group,alfa=0.05) {
  
  require(vegan)
  
  group <- as.factor(group)
  
  # Agregar warnings y stop 
  # revisando algunas condiciones de los argumentos
  
  
  # Calcula la matriz de apariciones o de presencia/ausencia
  
  com.pa <- decostand(com,method = "pa")
  
 
  
  # Calcula la matriz de probabilidades condicionales  P(estar en el ambiente...| apareci? el especimen....)
  
  p.cond <- sweep(aggregate(com.pa,by=list(group),sum)[,-1],2,colSums(com.pa),'/')
  rownames(p.cond) <- levels(group)
  
  # Calcula la matriz de independencia
  
  # Si me guío por el paper, 
  
  observed <- aggregate(com.pa,by=list(group),sum)[,-1]
  expected <- colSums(com.pa)/nlevels(group)
  
  
  indep <- colSums(sweep((sweep(observed,2,expected,'-')^2),2,expected,'/'))
  
  # Calcula un vector lógico de indicadores indicando cuándo es mayor
  # el valor de independencia respecto del calculado por la distribución chisq
  
  indic <-  indep > qchisq(1-alfa, nlevels(group)-1, ncp=0, lower.tail = TRUE, log.p = FALSE)
  
  # Selecciona dentro del vector de nombres los que corresponden a especies indicadoras
  
  names.indic <- names(indic)[indic==TRUE & !is.na(indic)]
  
  n.col.indic <- which(indic==1, arr.ind = T)

  return(list(names=names.indic, pcond=p.cond))
  
}
  