#' Returns the frequency of any taxonomic unit in a selected combination 
#' of environmental factors
#'
#' @param env Vector or matrix of quantitative environmental data
#' @param sp A vector of the abundance or presence of a species or taxonomic unit 
#' registered in the same samples as environmental data.
#' @param partitions Number of intervals in which to divide the 
#' range of each variable.
#' @return An object of class 'ftable' with the abundance of a species 
#' in a grid of environmental variables.
#' 
#' @export
#' @examples 
#' 
#' data(soilandfauna)
#' 
#' # Select community (species) data
#' 
#' com <- soilandfauna[,18:60]
#' 
#' # Select environmental data
#' 
#' env <- soilandfauna[,3:17]
#' 
#' # Obtain the presence of the Onychiuridae species in a grid 
#' # of environmental variables 
#' 
#' sp_hypercube(env[,c("P","OM","N")],com[,"Onychiuridae"],5)
#' 
#' # Obtain the simultaneous presence of the four species in a grid 
#' # of environmental variables
#' 
#' sp_hypercube(env[,c("P","OM","N")],com[,"Onychiuridae"] *
#'  com[,"Isotomidae"] * com[,"Eupodoidea"]* com[,"Aporrectodea_rosea"],5)
sp_hypercube <- function(env,sp,partitions=3){
  
  if (!is.vector(sp)){
    stop("Expected a vector of species abundances")
  }
  
  sp[sp>0] <- 1
  partitions <- partitions + 1
  
  env.factorized <- as_niche_factor(env,partitions)
  
  if(is.vector(env)){
    sp.hypercube <- ftable(env.factorized[sp==1])
    sp.hypercube
  }
  else{
    sp.hypercube <- ftable(env.factorized[sp==1,1:ncol(env.factorized)])
    sp.hypercube
  }
  
  }


