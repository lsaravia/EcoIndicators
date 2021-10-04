#' Performs a species indicator analysis
#' 
#'   @references Dufrene, M. and P. Legendre. 1997. Species assemblages and indicator species:
#' The need for a flexible asymmetrical approach. Ecological Monographs 67: 345-366 
#' 
#' McCune, B. & Grace, J. B. Analysis of Ecological Communities 2002, 300 (page 198)
#' 
#' @param com A matrix of community data
#' @param group A vector indicating grouping of observations in the community data
#'
#' @return The indicator species
#' @export
#'
#' @examples 
#' 
#' #' da <- read.delim("data/data.txt")
#' 
#' # Select community (species) data
#' 
#' com <- da[,18:50]
#' 
#' Grouping factor
#' 
#' group <- da[,1]
#' 
#' 
indicator_species <- function(com,group){
require(vegan)

  group <- as.factor(group)
  
if (!is.data.frame(com)){
stop("el argumento debe ser tipo data.frame")
}
nmuestras <- nrow(com)
nespecies <- ncol(com)

niveles <- levels(group)
nniveles <- nlevels(group)


abundmedia <- aggregate(com,by=list(group),FUN = mean)[,-1]

abundrel <- sweep(abundmedia,2,colSums(abundmedia),'/')

com.pa <- decostand(com,method = "pa")

frecrel <- aggregate(com.pa,by=list(group),mean)[,-1]

espindic <- abundrel*frecrel*100

espindic <- sort(apply(abundrel*frecrel*100,2,max),decreasing=T)

espindic
}




espindicmc <- function(com, group, veces=1000){
#Calcula el valor indicador de cada especie
espindic_actual <- indicator_species(com,group)

espindic_num<-0

for (i in 1:veces){
  espindic_alazar <- indicator_species(com,sample(group))
  espindic_num <- espindic_num + as.integer((espindic_alazar - espindic_actual)>=0)
}

espindic_prob <- espindic_num/veces
names(espindic_prob) <- names(espindic_actual)
espindic_prob
}


com.espindic <- indicator_species(com[rowSums(com)>0,colSums(com)>0],env$Ambiente[rowSums(com)>0])
barplot(com.espindic)
