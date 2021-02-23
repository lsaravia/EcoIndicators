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
  
  obs <- aggregate(com.pa,by=list(group),sum)[,-1]
  esp <- colSums(com.pa)/nlevels(group)
  
  indep <- colSums(sweep((sweep(obs,2,esp,'-')^2),2,esp,'/'))
  
  
  # Calcula un vector lógico de indicadores indicando cuándo es mayor
  # el valor de independencia respecto del calculado por la distribución chisq
  
  indic <-  indep > qchisq(1-alfa, nlevels(group)-1, ncp=0, lower.tail = TRUE, log.p = FALSE)
  
  # Selecciona dentro del vector de nombres los que corresponden a especies indicadoras
  
  names.indic <- names(indic)[indic==TRUE & !is.na(indic)]
  
  n.col.indic <- which(indic==1, arr.ind = T)

  return(list(names=names.indic, pcond=p.cond, col.indic = n.col.indic))
  
}
  