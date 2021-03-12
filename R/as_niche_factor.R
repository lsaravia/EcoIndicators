#' Title Converts a matrix of numeric quantitative environemtal vectors as a 
#' factorized version of a desired resolution
#'
#' @param env A matrix or data.frame of quantitative environmental data
#' @param resolution Number of intervals in which divide the range of each 
#' variable
#'
#' @return A data.frame 
#' @export
#'
#' @examples
as_niche_factor <- function(env,resolution){
  
 
  
  myseq <- function(x, resolution, dec=3){ #revisar el tema 'dec'
    result <- seq(from = min(x), to = max(x), 
                  length.out = resolution)
    return(round(result,dec))
  }
  
  # matríz con las divisiones del rango en función de la resolución
  
  if(is.vector(env)){
    env.factorized <- cut(env,myseq(env,resolution),include.lowest = TRUE)
    env.factorized
  }
  else{
    
  
  
  hypercube <-apply(env, 2, myseq,resolution)

  # Se transforma cada vector parámetros ambientales en un factor con los rangos
  # definidos por la matriz hypercube
  
  env.factorized <- rep(0,nrow(env))
  
  # no encontré como hacerlo sin un bucle for, pasando sucesivamente a 'cut'
  # un argumento que es un vector, el de los puntos de corte.
  
  for (i in 1:ncol(env)){
    
    env.factorized <- data.frame(env.factorized,cut(env[,i],hypercube[,i],include.lowest = TRUE))
    
  }
  
  env.factorized <- env.factorized[,-1]
  colnames(env.factorized) <- colnames(env)
  
  env.factorized
  }
}

