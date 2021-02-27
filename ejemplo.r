#
# The file data.txt have all the information about species and environments  
#
source("R/select_indicator_species.R")
source("R/identify_env.R")

da <- read.delim("Data/data.txt")
str(da)


# Select community (species) data
com <- da[,18:50]
# Select environmental data
env <- da[,3:17]

# Grouping factor
group <- da[,1]

# Plot of probabilities
#
indic <- select_indicator_species(com,group,0.005)
require(tidyverse)
pcond <- tibble::rownames_to_column(indic$pcond, var = "Env")
pcond <- pcond %>% pivot_longer(!Env, names_to = "Species", values_to = "Prob")
ggplot(pcond,aes(Species,Prob, color=Env)) + geom_point() + theme_bw() + scale_color_viridis_d() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

newcom <- read.delim("Data/comnueva.txt")


# From which environment are these new samples
#
env.id <- identify_env(com,newcom,indic)
