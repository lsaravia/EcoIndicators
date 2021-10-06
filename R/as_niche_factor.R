#' Converts a vector or matrix of quantitative environmental data as a 
#' factorized version with a desired numbers of partitions.
#'
#' @param env A matrix or data.frame of quantitative environmental data.
#' @param partitions Number of intervals in which to divide the range of each variable.
#'
#' @return A data.frame of numeric variables converted to factor
#' @export
#'
#' @examples
#' 
#' # Read data
#' 
#' da <- read.delim("data/data.txt")
#' 
#' # Select environmental data
#' 
#' env <- da[,3:17]
#' 
#' as_niche_factor(env)
#' 
as_niche_factor <- function(env,partitions=3){
  
  #env <- round(env, digits = digits)
  
  mycut <- function(x, partitions){ 
    result <- cut(x, seq(from = min(x), to = max(x), 
                  length.out = partitions),include.lowest = TRUE)
    return(result)
  }
  
  if(is.vector(env)){
   env.factorized  <- cut(env$DAP,seq(from = min(env$DAP), to = max(env$DAP), 
                                      length.out = partitions),include.lowest = TRUE)
   env.factorized
  }
  else{
  
  env.factorized <-lapply(env, mycut, partitions)
  
  as.data.frame(env.factorized)
  }
}

