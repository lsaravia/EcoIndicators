indicator_species_test <- function(com,group,times=100)

espindic_actual <- indicator_species(com,group)

espindic_num <- matrix(0,nrow(espindic_actual),ncol(espindic_actual))

for (i in 1:times){
  espindic_alazar <- indicator_species(com,sample(group))
  espindic_num <- espindic_num + as.integer((espindic_alazar - espindic_actual)>=0)
}

espindic_prob <- espindic_num/times
names(espindic_prob) <- names(espindic_actual)
espindic_prob


#espindicmc <- function(com, group, veces=1000){
#Calcula el valor indicador de cada especie


#}


#com.espindic <- indicator_species(com[rowSums(com)>0,colSums(com)>0],group[rowSums(com)>0])
#