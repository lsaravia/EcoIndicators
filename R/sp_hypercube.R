#' A partir de un vector o matriz de datos ambientales
#' devuelve la presencia de una especie en hipercubos de una resolución definida
#'
#' @param env Vector or matrix of quantitative environmental data
#' @param com A vector of abundance of a species registered in the same 
#' samples as environmetal data
#' @param resolution To divide the range of each environmental variable
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
sp_hypercube <- function(env,com,resolution=3){
  
  require(vegan)
  
  if (!is.vector(com)){
    stop("Por el momento solo acepta datos de abundancia de una sola especie")
  }
  
  com.pa <- decostand(com, method = "pa")
 
  env.factorized <- as_niche_factor(env,resolution)
  
  if(is.vector(env)){
    sp.hypercube <- ftable(env.factorized[com.pa==1])
    sp.hypercube
  }
  else{
    sp.hypercube <- ftable(env.factorized[com.pa==1,1:ncol(env.factorized)])
    sp.hypercube
  }
  
  }


