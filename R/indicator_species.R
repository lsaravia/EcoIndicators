#' Performs a species indicator analysis
#' 
#'   @references 
#'   
#'   Dufrene, M. and P. Legendre. 1997. Species assemblages and indicator species:
#' The need for a flexible asymmetrical approach. Ecological Monographs 67: 345-366
#' 
#'   McCune, B. & Grace, 
#' J. B. Analysis of Ecological Communities 2002, 300 (page 198)
#' 
#' @param com A matrix of community data
#' @param group A vector indicating grouping of observations in the community data
#'
#' @return The indicator species
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
#' # Select Grouping factor
#' 
#' group <- soilandfauna[,1]
#' 
#' out <- indicator_species(com,group)
#' 
#' barplot(out)
#' 
indicator_species <- function(com,group,times=100){

  group <- as.factor(group)
  
  com <- as.data.frame(com)
  
#if (!is.data.frame(com)){
#stop("The argument 'com' must be a data.frame")
#}
  
nmuestras <- nrow(com)
nespecies <- ncol(com)

niveles <- levels(group)
nniveles <- nlevels(group)


abundmedia <- aggregate(com,by=list(group),FUN = mean)[,-1]

abundrel <- sweep(abundmedia,2,colSums(abundmedia),'/')

com[com>0] <- 1

frecrel <- aggregate(com,by=list(group),mean)[,-1]

espindic <- abundrel*frecrel*100

espindic <- sort(apply(abundrel*frecrel*100,2,max),decreasing=T)

#espindic.prob <- 

#espindic_num<-0

#for (i in 1:veces){
#  espindic_alazar <- indicator_species(com,sample(group))
#  espindic_num <- espindic_num + as.integer((espindic_alazar - espindic_actual)>=0)
#}

#espindic_prob <- espindic_num/veces
#names(espindic_prob) <- names(espindic_actual)
#espindic_prob

espindic
}




#espindicmc <- function(com, group, veces=1000){
#Calcula el valor indicador de cada especie
#espindic_actual <- indicator_species(com,group)


#}


#com.espindic <- indicator_species(com[rowSums(com)>0,colSums(com)>0],group[rowSums(com)>0])
#