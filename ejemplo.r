source("R/func.r")

#
# El archivo data.txt esta copiado directamente del excel 
#
da <- read.delim("Data/data.txt")
str(da)

select_indicator_species(da[,1],da[,18:50])

source("R/select_indicator_species.R")

com <- read.table("Data/com.txt", header = TRUE,dec = ".")
env <- read.table("Data/env.txt",header = T, dec = ".")
group <-  as.factor(env$Ambiente)

select_indicator_species(com,env[,-1],group)
