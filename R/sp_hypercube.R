
#' Title
#'
#' @param env 
#' @param com 
#' @param resolution 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
sp_hypercube <- function(env,com,resolution){
  
  require(vegan)
  
  if (!is.vector(com)){
    stop("Por el momento solo acepta datos de abundancia de una sola especie")
  }
  
  com.pa <- decostand(com, method = "pa")
  
  # Tabla con el número de muestras en las que está presente la especie
  # por cada cubo.
  
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


