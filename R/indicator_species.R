#' Performs a species indicator analysis following  
#' Dufrene, M. and P. Legendre. 1997. Species assemblages and indicator species:
#' The need for a flexible asymmetrical approach. Ecological Monographs 67: 345-366 
#' McCune, B. & Grace, J. B. Analysis of Ecological Communities 2002, 300 (page 198)
#' 
#' @param com A matrix of community data
#' @param group A vector indicating grouping of observations in the community data
#'
#' @return 
#' @export
#'
#' @examples



espindic <- function(com,group){
require(vegan)

  group <- as.factor(group)
  
if (!is.data.frame(com)){
stop("el argumento debe ser tipo data.frame")
}
nmuestras <- nrow(com)
nespecies <- ncol(com)

niveles <- levels(group)
nniveles <- nlevels(group)

#calcula la abundancia media de cada especie en cada nivel del factor
# a <- 1:nespecies
# for (i in 1:nniveles)
# {
# a <- rbind(a, apply(subset(com,group==niveles[i]),2,mean))
# }
# abundmedia <- a[-1,]

abundmedia <- aggregate(com,by=list(group),FUN = mean)[,-1]

#Luego calcula la abundancia relativa de cada especie en cada nivel del factor

# a <- 1:nespecies
# for (i in 1:nniveles){
# a <- rbind(a,abundmedia[i,]/apply(abundmedia,2,sum))
# }
# abundrel <- a[-1,]

abundrel <- sweep(abundmedia,2,colSums(abundmedia),'/')

#Transforma la matriz de com original en una de presencia-ausencia y luego calcula la frecuencia relativa
# a <- 1:nespecies
# for (i in 1:nniveles){
# a <- rbind(a, apply(subset(decostand(com,"pa"),group==niveles[i]),2,mean))
# }
# frecrel <- a[-1,]

com.pa <- decostand(com,method = "pa")

frecrel <- aggregate(com.pa,by=list(group),mean)[,-1]


#El valor indicativo de cada especie se calcula como el producto de la abund relativa por la frecuencia relativa, y se expresa en porcentaje.

espindic <- abundrel*frecrel*100
espindic <- sort(apply(abundrel*frecrel*100,2,max),decreasing=T)

#names(espindic) <- names(com)
#rownames(espindic) <-niveles
espindic
}




espindicmc <- function(com, group, veces=1000){
#Calcula el valor indicador de cada especie
espindic_actual <- espindic(com,group)

espindic_num<-0

#Calcula el valor indicador reasignando al azar las unidades de muestreo al factor considerado

#Cada vez suma 1 al acumulador "espindic_num" en el caso de que el resultado al azar sea mayor
# que el valor indicador calculado.
for (i in 1:veces){
  espindic_alazar <- espindic(com,sample(group))
  espindic_num <- espindic_num + as.integer((espindic_alazar - espindic_actual)>=0)
}

#Calcula la proporción de veces que el valor indicador para el conjunto de com al azar
# fue mayor o igual que el calculado para el conjunto original.

espindic_prob <- espindic_num/veces
names(espindic_prob) <- names(espindic_actual)
espindic_prob
}


com.espindic <- espindic(com[rowSums(com)>0,colSums(com)>0],env$Ambiente[rowSums(com)>0])
barplot(com.espindic)
