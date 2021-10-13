identify_env_test <- function(com, group, times = 100){
  
  n <- 1 # no tendría que ser variable?
  
  indic.sp <- select_indicator_species(com,group)
  
  acu <- lapply(unique(group), function(x) {
    com.gr <- com[group==x,]
    R <- rep(0,times)
    R <- lapply(1:times, function(x){ identify_env(com,com.gr[sample(nrow(com.gr),
                                                                     size = n,replace = TRUE),],indic.sp)
      })
    out <-sum(R==x)
    data.frame(group= x, accuracy= out/veces)
  })
  acu
}