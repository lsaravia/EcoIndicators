#
# The file data.txt have all the information about species and environments  
#
source("R/select_indicator_species.R")

da <- read.delim("Data/data.txt")
str(da)


# Select community (species) data
com <- da[,18:50]
# Select environmental data
env <- da[,3:17]

# Grouping factor
group <- da[,1]

source("R/select_indicator_species.R")
select_indicator_species(com,group,0.05)
