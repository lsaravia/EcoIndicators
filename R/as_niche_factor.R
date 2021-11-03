#' Converts a vector or matrix of quantitative environmental data as a 
#' factorized version with a desired numbers of partitions.
#'
#' @param env A matrix or data.frame of quantitative environmental data.
#' @param partitions Number of intervals in which to divide the 
#' range of each variable.
#'
#' @return A data.frame of numeric variables converted to factor
#' @export
#'
#' @examples
#' 
#' data(soilandfauna)
#' 
#' # Select environmental data
#' 
#' env <- soilandfauna[,c("Bd","P","EC")]
#' 
#' as_niche_factor(env)
#' 
as_niche_factor <- function(env,partitions=3){
  
  mycut <- function(x, partitions){ 
    result <- cut(x, seq(from = min(x), to = max(x), 
                  length.out = partitions),include.lowest = TRUE)
    return(result)
  }
  
  if(is.vector(env)){
   env.factorized  <- cut(env,seq(from = min(env), to = max(env), 
                                      length.out = partitions),include.lowest = TRUE)
   env.factorized
  }
  else{
  
  env.factorized <-lapply(env, mycut, partitions)
  
  as.data.frame(env.factorized)
  }
}

