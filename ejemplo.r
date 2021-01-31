source("R/func.r")

#
# El archivo data.txt esta copiado directamente del excel 
#
da <- read.delim("Data/data.txt")
str(da)

select_indicator_species(da[,1],da[,18:50])
