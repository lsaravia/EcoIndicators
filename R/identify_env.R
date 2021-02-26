#' Identify the environment that 
#'
#' @param com The original community used to select indicator species
#' @param com.to.identify A new sample o group of samples to identify the e
#' environment that they belong.
#' @param indicator.species Result of 'selec_indicator_species'
#' @param alfa significance level used for the test
#' 
#'
#' @return The environment.
#' @export
#'
#' @examples
identify_env <- function(com, com.to.identify, indicator.species,alfa = 0.05){
  
  require(vegan)
  
  # Agregar warnings y stop 
  # revisando algunas condiciones de los argumentos
  
  # Creamos un subconjunto de las probabilidades condicionales con las que 
  # corresponden a las especies indicadoras
  
  pcond.indic <- subset(indicator.species$pcond,select = indicator.species$col.indic)

  

  # Creamos un subconjunto de la comunidad original, transformada a 
  # matriz de presencias y ausencias sólo con las especies indicadoras

  com.pa <- decostand(com,method = "pa")

  com.pa.indic <- subset(com.pa,select = indicator.species$col.indic)

  
  #-----------------------------------------------------
  # Creamos la matriz de coeficientes (D en el paper)
  # ----------------------------------------------------

  # Revisar este test:
  
  # En el programa orginal el límite inferior y superior se calcula con 
  
  # lim.sup<-(1/numero.ambientes)+qnorm(1-nivel.signific.puntual)*sqrt(((1/numero.ambientes)*(1-(1/numero.ambientes)))/sum(matriz.de.apariciones[,h]))
  # lim.inf<-(1/numero.ambientes)-qnorm(1-nivel.signific.puntual)*sqrt(((1/numero.ambientes)*(1-(1/numero.ambientes)))/sum(matriz.de.apariciones[,h]))
  
  # que está dividido al final por: sum(matriz.de.apariciones[,h]),
  
  # yo le veo el problema de que toma 'h' del entorno, entonces no sabemos qué valor toma
  # y que da como resultado un escalar que se resta luego a todas las especies indicadoras por igual.
  
  # Entiendo que tiene que haber un límite inferior y superior para cada especie indicadora. 
  # Eso se obtiene en la siguiente cuenta dividiendo por el vector 
  # colSums(com.pa.indic)
  # que es la suma de las columnas (las especies) del subconjunto de especies indicadoras
  # estandarizada como matriz de presencias y ausencias (com.pa.indic)
  
  
  
  n <- indicator.species$ngroups # n es el número de ambientes
 
  lim.sup<-(1/n)+qnorm(1-alfa2)*sqrt(((1/n)*(1-(1/n)))/colSums(com.pa.indic))
  lim.inf<-(1/n)-qnorm(1-alfa2)*sqrt(((1/n)*(1-(1/n)))/colSums(com.pa.indic))
  
  # En vez de crear una matriz de ceros y después completarla, 
  # me parece más prolijo, crearla primero igual al subcojunto de la matriz
  # de probabilidades condicionales de las especies indicadoras. 
  
  D <- pcond.indic
  
  # Calculamos la transpuesta
  
  Dt <- t(D)
  
  # Dado que R completa y compara las matrices por columnas y, 
  # viendo que: el número de filas de Dt es igual al número de especies indicadoras
  # y es igual a la longitud del vector lim.sup y lim.inf
  
  # en las siguientes líneas asignamos:
  
  # A los elementos de Dt que están por encima de lim.sup o por debajo de lim.inf
  # les restamos 1/n
  
  
  Dt[Dt > lim.sup | Dt < lim.inf] <- Dt[Dt > lim.sup | Dt < lim.inf] - 1/n
  
  # A los elementos comprendidos entre lim.inf y lim.sup
  # los convertimos en 0
  
  Dt[Dt < lim.sup & Dt > lim.inf] <- 0
  
  # ------------------------------------------------------------------
  
  # Transformamos la comunidad nueva a presencia-ausencia
  
  com2id.pa <- decostand(com.to.identify,method="pa")
  
  # Y seleccionamos un subcojunto con las especies indicadoras
  
  com2id.sel <- subset(com2id.pa,select = sp.indic$col.indic)
  
  # Calculamos la matriz A del paper con la suma de las columnas de especies indicadoras
  # de la comunidad a identificar (com2id.sel)
  
  A <- colSums(com2id.sel)
  
  # Realizamos la estimación del ambiente 
  # con el producto de la matriz A por la transpuesta de D (Dt)
  
  est.env <- A %*% Dt
  
  # Seleccionamos del vector de estimación del ambiente (est.env), 
  # el nombre de la columna que corresponde al mayor valor.
  
  select.est.env <- colnames(est.env)[apply(est.env, 1, which.max)]
  
  return(list(Environment = select.est.env))
  
  
}