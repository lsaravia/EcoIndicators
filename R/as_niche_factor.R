#' Converts a vector or matrix of quantitative environmental data as a 
#' factorized version with a desired numbers of partitions.
#'
#' @param env A matrix or data.frame of quantitative environmental data
#' @param partitions Number of intervals in which to divide the range of each variable
#' 
#'
#' @return 
#' @export
#'
#' @examples
#' 
as_niche_factor <- function(env,partitions=3){
  
  myseq <- function(x, partitions, dec=3){ #revisar el tema 'dec'
    result <- seq(from = min(x), to = max(x), 
                  length.out = partitions)
    return(round(result,dec))
  }
  
  if(is.vector(env)){
    env.factorized <- cut(env,myseq(env,partitions),include.lowest = TRUE)
    env.factorized
  }
  else{
    
  
  
  hypercube <-apply(env, 2, myseq,partitions)

  
  env.factorized <- rep(0,nrow(env))
  
  
  for (i in 1:ncol(env)){
    
    env.factorized <- data.frame(env.factorized,cut(env[,i],hypercube[,i],
                                                    include.lowest = TRUE))
    
  }
  
  env.factorized <- env.factorized[,-1]
  colnames(env.factorized) <- colnames(env)
  
  env.factorized
  }
}

