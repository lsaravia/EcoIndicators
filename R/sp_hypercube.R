#' A partir de un vector o matriz de datos ambientales
#' devuelve la presencia de una especie en hipercubos de una resolución definida
#'
#' @param env Vector or matrix of quantitative environmental data
#' @param sp A vector of abundance of a species registered in the same 
#' samples as environmetal data
#' @param resolution To divide the range of each environmental variable
#'
#' @return An object of class ftable with the abundance of a species 
#' in an grid of environmental variables
#' @export
#'
#' @examples 
#' 
#' #' da <- read.delim("Data/data.txt")
#' 
#' # Select community (species) data
#' 
#' com <- da[,18:50]
#' 
#' # Select environmental data
#' 
#' env <- da[,3:17]
#' 
#' # Obtain the presence of the Onychiuridae species in a grid 
#' # of environmental variables 
#' 
#' sp_hypercube(env[,c("P","MO","N")],com.pa[,"onychiuridae"],5)
#' 
#' # Obtain the simultaneous presence of the four species in a grid 
#' # of environmental variables
#' 
#' sp_hypercube(env[,c("P","MO","N")],com.pa[,"onychiuridae"] * com.pa[,"isotomidae"]* com.pa[,"eupodoidea"]* com.pa[,"Aporos"],5)
#' 
#' 
sp_hypercube <- function(env,sp,resolution=3){
  
  require(vegan)
  
  if (!is.vector(sp)){
    stop("Expected a vector of species abundances")
  }
  
  sp.pa <- decostand(sp, method = "pa")
 
  env.factorized <- as_niche_factor(env,resolution)
  
  if(is.vector(env)){
    sp.hypercube <- ftable(env.factorized[sp.pa==1])
    sp.hypercube
  }
  else{
    sp.hypercube <- ftable(env.factorized[sp.pa==1,1:ncol(env.factorized)])
    sp.hypercube
  }
  
  }

