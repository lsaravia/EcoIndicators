#' Converts a vector or matrix of quantitative environmental data as a 
#' factorized version of a desired resolution
#'
#' @param env A matrix or data.frame of quantitative environmental data
#' @param resolution Number of intervals in which divide the range of each variable
#' 
#'
#' @return 
#' @export
#'
#' @examples
#' 
as_niche_factor <- function(env,resolution=3){
  
  myseq <- function(x, resolution, dec=3){ #revisar el tema 'dec'
    result <- seq(from = min(x), to = max(x), 
                  length.out = resolution)
    return(round(result,dec))
  }
  
  if(is.vector(env)){
    env.factorized <- cut(env,myseq(env,resolution),include.lowest = TRUE)
    env.factorized
  }
  else{
    
  
  
  hypercube <-apply(env, 2, myseq,resolution)

  
  env.factorized <- rep(0,nrow(env))
  
  
  for (i in 1:ncol(env)){
    
    env.factorized <- data.frame(env.factorized,cut(env[,i],hypercube[,i],include.lowest = TRUE))
    
  }
  
  env.factorized <- env.factorized[,-1]
  colnames(env.factorized) <- colnames(env)
  
  env.factorized
  }
}

