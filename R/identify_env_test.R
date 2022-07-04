#' Tests the accuracy of the environment identification of a set of samples
#' 
#' This function tests the accuracy of the environment estimation
#'
#' @param com The community data used to select indicator species.
#' @param group Vector of the sample grouping of the community data.
#' @param times to repeat the test.
#' @param n Subsample size.
#' @param alfa Significance level used for the test.
#' 
#'
#' @return A list with the accuracy of the estimation for each group.
#' @export
#'
#' @examples
#' 
#' data(soilandfauna)
#'  
#' # Select community (species) data
#' 
#' com <- soilandfauna[,18:60]
#' 
#' # Select grouping factor
#' group <- soilandfauna[,1]
#' 
#' identify_env_test(com, group, times = 100)
#' 
identify_env_test <- function(com, group, times = 999, n = 5, alfa=0.05){
  
  acu <- lapply(unique(group), function(x) {
    com.gr <- com[group==x,]
    R <- rep(0,times)
    R <- lapply(1:times, function(x){identify_env(com,
                                                   com.gr[sample(nrow(com.gr),
                                                                 size = n,replace = TRUE),],
                                                   group)$belonging.env
      })
    out <-sum(R==x)
    data.frame(group= x, accuracy= out/times)
  })
  acu
}