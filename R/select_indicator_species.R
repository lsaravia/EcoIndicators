#' Select indicator species
#' 
#' Selection of species or taxonomic units, depending on the taxonomic resolution
#'  in the original data set, that indicate with a given probability of
#' belonging to an environment. 
#' 
#' @param com Community data. matrix or data.frame with species in columns and samples in rows.
#' @param group Vector of the sample grouping of the community data.
#' @param alfa Significance level used for the test.
#'
#' @return A list with two components: a vector of the name of the 
#' indicator species selected 
#' and a vector of the conditional probability of each species 
#' or taxonomic unit in the data.
#' 
#' @import stats
#' @export
#'
#' @examples
#' 
#' data(soilandfauna)
#'  
#' # Select community (species) data
#' com <- soilandfauna[,18:50]
#' 
#' # Select grouping factor
#' group <- soilandfauna[,1]
#' 
#' select_indicator_species(com,group,0.05)
#' 
select_indicator_species <- function(com,group,alfa=0.05) {
  
  group <- as.factor(group)
  
  com[com>0] <- 1
  
  p.cond <- sweep(aggregate(com,by=list(group),sum)[,-1],2,colSums(com),'/')
  
  rownames(p.cond) <- levels(group)
  
  observed <- aggregate(com,by=list(group),sum)[,-1]
  expected <- colSums(com)/nlevels(group)
  
  
  indep <- colSums(sweep((sweep(observed,2,expected,'-')^2),2,expected,'/'))
  
  indic <-  indep > qchisq(1-alfa, nlevels(group)-1, ncp=0, lower.tail = TRUE, log.p = FALSE)
  
  names.indic <- names(indic)[indic==TRUE & !is.na(indic)]
  
  n.col.indic <- which(indic==1, arr.ind = T)

  return(list(names=names.indic, pcond=p.cond))
  
}
  