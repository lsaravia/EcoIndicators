#' Test the accuracy of the environment identification of a set of samples
#' 
#' This function use 
#'
#' @param com The original community used to select indicator species.
#' @param group vector of the samples grouping
#' @param times to repeat the test
#' @param n Subsample size 
#' @param alfa Significance level used for the test.
#' 
#'
#' @return A list with the accuracy of the estimation for each group
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
#' identify_env_test(com, group)
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