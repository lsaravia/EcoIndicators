###########################################################################################################
###########################################################################################################
###########################################################################################################
#################        INGRESO DE DATOS
###########################################################################################################
###########################################################################################################
###########################################################################################################

# -----------------------------
## Andrés.
## los comentarios tienen que ver con simplificaciones de los procedimientos y reescrituras
## para eliminar los bucles for, tratando de usar funciones 

## En principio me parece que la matriz inicial tiene que estar separada en dos argumentos

## por un lado una matriz 'Ambiental' en la que la primer columna es un factor con los ambientes y 
## luego las variables físico químicas

## Por otro lado una matriz de la comunidad.

## opcionalmente puede ser ingresado como argumento el factor de ambientes.

## Me parece más ordenado poner nombres más cortos 
# -----------------------------------------------------

# Defino la matiz "numeros.completos" que contiene solo los datos (de los qu?micos y de las especies) pero no los nombres
# de las filas (los ambientes) ni las columnas (los par?metros f?sico-qu?micos y las especies)
<<<<<<< HEAD


# Original
 Data<-read.table("Data/data.txt", header=FALSE)
 numeros.completos<-as.matrix(Data)
 
#Andres -------------------
com <- read.table("Data/com.txt", header = TRUE,dec = ".")
env <- read.table("Data/env.txt",header = T, dec = ".")
group <-  as.factor(env$Ambiente)
# ----------------------


#creo los nombres de las filas (los ambientes)
#Original
 nombrefilas1<-read.table("Data/Nombre filas.txt", header=FALSE)
 nombrefilas<-t(nombrefilas1)
 Ambientes<-nombrefilas



#creo los nombres de las columnas (que son los par?metros f?sico-qu?micos y las especiess)

#Original
 nombrecolumnas1<-read.table("Data/Nombre columnas.txt", header=FALSE)
 nombrecolumnas2<-t(nombrecolumnas1)
 nombrecolumnas<-t(nombrecolumnas2)

#=======
Data<-read.table("Data/Data_old.txt", header=FALSE)
numeros.completos<-as.matrix(Data)

#creo los nombres de las filas (los ambientes)
nombrefilas1<-read.table("Data/Nombre filas.txt", header=FALSE)
nombrefilas<-t(nombrefilas1)
Ambientes<-nombrefilas

#creo los nombres de las columnas (que son los par?metros f?sico-qu?micos y las especiess)
nombrecolumnas1<-read.table("Data/Nombre columnas.txt", header=FALSE)
nombrecolumnas2<-t(nombrecolumnas1)
nombrecolumnas<-t(nombrecolumnas2)
#>>>>>>> 8209edafec7259b3447448f024c782a5ac822dd2

# Agrego nombre de filas y columnas a la matriz "n?meros.completos"
colnames(numeros.completos)<-nombrecolumnas
rownames(numeros.completos)<-nombrefilas
numeros.completos

#Se indica la cantidad de par?metros f?sico-qu?micos que hay en la base de datos (las primeras columnas)

# Andrés ---------------------------

n.fq<-15

# ----------------------------------



#Se indica la cantidad de especies o familias que hay en la base de datos (las ?ltimas columnas)


#Andrés ------------------------------------

n.biota<-43

# ----------------------------------

#Se recorta la matriz "numeros.completos" para dejar solo los datos de las especies (se recorta la parte de los par?metros f?sico-qu?micos)
#se construye la matriz "n?meros" 

numeros.inicial<-numeros.completos
for(i in 1:n.fq){numeros.inicial<-numeros.inicial[,-1]}
numeros<-numeros.inicial
numeros


### Se recorta el nombre de las columnas para dejar solo el nombre de las especies (se saca el nombre de los qu?micos)

biota.inicial<-nombrecolumnas
for(i in 1:n.fq){biota.inicial<-biota.inicial[-1]}
biota<-biota.inicial
biota




###########################################################################################################
###########################################################################################################
###########################################################################################################
##########  PRIMERA PARTE: SELECCI?N Y USO DE ESPECIERS INDICADORAS
###########################################################################################################
###########################################################################################################
  ###########################################################################################################


# N?meros que se van a necesitar




#Original
numero.filas<-length(numeros[,1])
numero.filas
numero.columnas<-length(numeros[1,])
numero.columnas


#Andrés (no se si esto será necesario después) ---------------------------------------------
n.filas <- nrow(com)
n.col <- ncol(com)

# -------------------------------------------------------------


#Se distinguen los ambientes en la matriz


niveles.ambientes.2<-c(Ambientes[1])
cont<-1
for(j in 2:numero.filas){if(niveles.ambientes.2[cont]!=Ambientes[j]){niveles.ambientes.2<-c(niveles.ambientes.2,Ambientes[j])
cont<-cont+1}}
niveles.ambientes.2

ambientes.como.factores<-factor(Ambientes, levels=niveles.ambientes.2, ordered=TRUE)
ambientes.como.factores
niveles.ambientes<-levels(ambientes.como.factores)
niveles.ambientes
numero.ambientes<-length(niveles.ambientes)
numero.ambientes

# vector que tiene en cada coordenada los "cortes" de cada ambiente (donde las muestras cambian de un ambiente a otro)

cortes.inicial<-rep(0,numero.ambientes+1)
for(i in 1:numero.ambientes){cortes.inicial[i+1]<-(sum(Ambientes==niveles.ambientes[i])+cortes.inicial[i])}
cortes<-cortes.inicial
cortes










#Se traduce la matriz "n?meros" a ceros y unos. Se crea la "matriz.de.apariciones"
#matriz que tiene 0 o 1 de acuerdo a si hubo o no apariciones del especimen en la muestra (en el paper la matriz "E")

matriz.de.apariciones.inicial<-matrix(0,numero.filas,numero.columnas)
for(k in 1:numero.columnas){
  for(j in 1:numero.filas){
    if(numeros[j,k]>0){matriz.de.apariciones.inicial[j,k]<-1}
  }
}
matriz.de.apariciones<-matriz.de.apariciones.inicial
colnames(matriz.de.apariciones)<-biota
rownames(matriz.de.apariciones)<-Ambientes
matriz.de.apariciones

#Andrés ----------------------------------
library(vegan)
com.pa <- decostand(com,method = "pa")

#com.pa == matriz.de.apariciones

# ---------------------------------------

#  Matriz con las probabilidades condicionales P(estar en el ambiente...| apareci? el especimen....)

matriz.de.probabilid.condicional.inicial<-matrix(0,numero.ambientes,numero.columnas)
for(l in 1:numero.ambientes){
  for(h in 1:numero.columnas){
    matriz.de.probabilid.condicional.inicial[l,h]<-((tapply(matriz.de.apariciones[,h],ambientes.como.factores,sum)[l])/sum(matriz.de.apariciones[,h])) 
  }
}
matriz.de.probabilid.condicional<-matriz.de.probabilid.condicional.inicial
matriz.de.probabilid.condicional

#Andrés ------------------------------------------------------------------------

p.cond <- sweep(aggregate(com.pa,by=list(group),sum)[,-1],2,colSums(com.pa),'/')


# --------------------------------------------------------------------------------

#Vector de apariciones totales. Muestraeil n?mero total de muestras en las que apareci? cada especie

vectores.de.apariciones.totales.inicial<-rep(0,numero.columnas)
for(i in 1:numero.columnas){vectores.de.apariciones.totales.inicial[i]<-sum(matriz.de.apariciones[,i])}
vectores.de.apariciones.totales<-vectores.de.apariciones.totales.inicial
vectores.de.apariciones.totales 

# Andrés ------------------
# directamente es == 
  
  colSums(com.pa)
# ---------------------------------


# Gr?fico que muestra la matriz de probabilidad condicional


par(mfrow=c(1,1))

plot(1:length(matriz.de.probabilid.condicional[1,]), matriz.de.probabilid.condicional[1,], 
     type="p",pch=19,xlim=c(0, length(matriz.de.probabilid.condicional[1,])),ylim = c(-0.2, 3), 
     col = 1, lty = 1,lwd=2,axes = TRUE, xlab="", ylab="",main="P(estar en el ambiente --- | apareci? el especimen ---)")
for(k in 2:numero.ambientes){points(1:length(matriz.de.probabilid.condicional[1,]), matriz.de.probabilid.condicional[k,],pch=19,col=k,lwd=2)}
legend("topleft",col=1:numero.ambientes,legend =niveles.ambientes, lwd=2, bty = "n")
axis(1, at = seq(1, length(vectores.de.apariciones.totales), by = 1), las=2)
abline(h=0)
abline(h=1/numero.ambientes)
abline(h=1)
text(1:length(vectores.de.apariciones.totales), 1.2, vectores.de.apariciones.totales)


# Andrés
par(mfrow=c(1,1))

plot(1:length(p.cond[1,]), p.cond[1,], 
     type="p",pch=19,xlim=c(0, length(p.cond[1,])),ylim = c(-0.2, 3), 
     col = 1, lty = 1,lwd=2,axes = TRUE, xlab="", ylab="",main="P(estar en el ambiente --- | apareci? el especimen ---)")
for(k in 2:nlevels(group)){points(1:length(p.cond[1,]), p.cond[k,],pch=19,col=k,lwd=2)}
legend("topleft",col=1:nlevels(group),legend =levels(group), lwd=2, bty = "n")
axis(1, at = seq(1, length(colSums(com.pa)), by = 1), las=2)
abline(h=0)
abline(h=1/nlevels(group))
abline(h=1)
text(1:length(colSums(com.pa)), 1.2, colSums(com.pa))






# Selecci?n de indicadores


#se elije el nivel de significaci?n del test
nivel.signific.independencia<-0.05


# Andrés -----------------------

alfa <- 0.05

# ---------------------------

independencia.inicial<-rep(0,numero.columnas)
for(i in 1:numero.columnas){
  independencia.inicial[i]<-sum(((tapply(matriz.de.apariciones[,i],ambientes.como.factores,sum)-(vectores.de.apariciones.totales[i]/numero.ambientes))^2)/(vectores.de.apariciones.totales[i]/numero.ambientes))
}
independencia<-independencia.inicial
independencia

# Andrés ----------------------------------------
# i <- 1
# obs <- tapply(matriz.de.apariciones[,i],ambientes.como.factores,sum)
# sum(((tapply(matriz.de.apariciones[,i],ambientes.como.factores,sum)-(vectores.de.apariciones.totales[i]/numero.ambientes))^2)/(vectores.de.apariciones.totales[i]/numero.ambientes))
# 
# obs <- tapply(matriz.de.apariciones[,i],ambientes.como.factores,sum)
# esp <- vectores.de.apariciones.totales[i]/numero.ambientes
# 
# sum((obs-esp)^2/esp)


obs <- aggregate(com.pa,by=list(group),sum)[,-1]
esp <- colSums(com.pa)/nlevels(group)

indep <- colSums(sweep((sweep(obs,2,esp,'-')^2),2,esp,'/'))

# -------------------------------------------------



qchisq(1-0.005  , numero.ambientes-1, ncp=0, lower.tail = T, log.p = F)

vector.de.indicadores.inicial<-rep(0,numero.columnas)
for(k in 1:numero.columnas){
  if(vectores.de.apariciones.totales[k]!=0){
    if(independencia[k]>qchisq(1-nivel.signific.independencia, numero.ambientes-1, ncp=0, lower.tail = T, log.p = F)){vector.de.indicadores.inicial[k]<-1}
  }
}
vector.de.indicadores<-vector.de.indicadores.inicial
vector.de.indicadores #Vector que indica (con un 1) en que columna de la matriz "numeros" hay una especie indicadora


# Andrés ------------------------------------

indic <-  indep > qchisq(1-alfa, nlevels(group)-1, ncp=0, lower.tail = TRUE, log.p = FALSE)

# ------------------------------------------



nombre.de.indicadores.inicial<-c(0)
for(s in 1:numero.columnas){if(vector.de.indicadores[s]==1){nombre.de.indicadores.inicial<-c(nombre.de.indicadores.inicial,biota[s])}}
nombre.de.indicadores.inicial<-nombre.de.indicadores.inicial[-1]
nombre.de.indicadores<-nombre.de.indicadores.inicial
nombre.de.indicadores #nombre de las especies indicadoras"


# Andrés ----------------------------

names(indic)[indic==TRUE & !is.na(indic)]

#----------------------------------

posicion.de.indicadores.inicial<-rep(0,numero.columnas)
for(k in 1:numero.columnas){if(vector.de.indicadores[k]==1){posicion.de.indicadores.inicial[k]<-k}}
posicion.de.indicadores.inicial<-posicion.de.indicadores.inicial[posicion.de.indicadores.inicial!=0]
posicion.de.indicadores<-posicion.de.indicadores.inicial
posicion.de.indicadores #el n?mero de columna de la matriz "numeros" correspondiente a las especies indicadoras



#Se recorta la matriz de probabilidad condicional y solo se miran las columnas de las especies indicadoras


matriz.de.proba.condicional.de.indicadores.inicial<-matrix(0,numero.ambientes,1)
for(l in 1:numero.columnas){
  if(vector.de.indicadores[l]==1){matriz.de.proba.condicional.de.indicadores.inicial<-cbind(matriz.de.proba.condicional.de.indicadores.inicial,matriz.de.probabilid.condicional[,l])}
}
matriz.de.proba.condicional.de.indicadores.inicial<-matriz.de.proba.condicional.de.indicadores.inicial[,-1]
matriz.de.proba.condicional.de.indicadores<-matriz.de.proba.condicional.de.indicadores.inicial
rownames(matriz.de.proba.condicional.de.indicadores)<-niveles.ambientes
colnames(matriz.de.proba.condicional.de.indicadores)<-nombre.de.indicadores
matriz.de.proba.condicional.de.indicadores


# Andrés ------------------------------------------
n.col.indic <- which(indic==1, arr.ind = T)

p.cond.indic <- subset(p.cond,select = n.col.indic)


---------------------------------


#Se recorta la matriz de apariciones y solo se miran las columnas de las especies indicadoras

matriz.de.apariciones.de.indicadores.inicial<-matrix(0,numero.filas,1)
for(l in 1:numero.columnas){
  if(vector.de.indicadores[l]==1){matriz.de.apariciones.de.indicadores.inicial<-cbind(matriz.de.apariciones.de.indicadores.inicial,matriz.de.apariciones[,l])}
}
matriz.de.apariciones.de.indicadores.inicial<-matriz.de.apariciones.de.indicadores.inicial[,-1]
matriz.de.apariciones.de.indicadores<-matriz.de.apariciones.de.indicadores.inicial
rownames(matriz.de.apariciones.de.indicadores)<-Ambientes
colnames(matriz.de.apariciones.de.indicadores)<-nombre.de.indicadores
matriz.de.apariciones.de.indicadores


# Andrés ------------------------------------------

com.pa.indic <- subset(com.pa,select = which(indic==1, arr.ind = T))


---------------------------------



#An?lisis ?til en algunos casos:
#An?lisis Factorial de correspondencias de los indicadores y los ambientes

an.fact.indicadores.inicial<-matrix(0,numero.ambientes,length(posicion.de.indicadores))
for(t in 1:numero.ambientes){
  for(k in 1:length(posicion.de.indicadores)){an.fact.indicadores.inicial[t,k]<-tapply(matriz.de.apariciones.de.indicadores[,k],ambientes.como.factores,sum)[t]}
}
an.fact.indicadores<-an.fact.indicadores.inicial
an.fact.indicadores




#install.packages("ca")
library(ca)
prop.table(an.fact.indicadores, 1)
prop.table(an.fact.indicadores, 2)
summary(ca(an.fact.indicadores))
ca(an.fact.indicadores)
plot(ca(an.fact.indicadores))


# Armo la matriz de coeficientes (en el paper la matriz D) es la que se usar? para estimar a que ambiente pertenecen nuevas muestras del suelo

#Se elije el nivel de significaci?n del test
nivel.signific.puntual<-0.1

# Se aparta la especie indicadora de lo esperado?? esa sería la idea de este test.


lim.sup<-(1/numero.ambientes)+qnorm(1-nivel.signific.puntual)*sqrt(((1/numero.ambientes)*(1-(1/numero.ambientes)))/sum(matriz.de.apariciones[,h]))
lim.inf<-(1/numero.ambientes)-qnorm(1-nivel.signific.puntual)*sqrt(((1/numero.ambientes)*(1-(1/numero.ambientes)))/sum(matriz.de.apariciones[,h]))
matriz.de.coeficientes.inicial<-matrix(0,numero.ambientes,sum(vector.de.indicadores))
for(i in 1:numero.ambientes){
  for(j in 1:sum(vector.de.indicadores)){
    if(matriz.de.proba.condicional.de.indicadores[i,j]>lim.sup){matriz.de.coeficientes.inicial[i,j]<-matriz.de.proba.condicional.de.indicadores[i,j]-(1/numero.ambientes)}
    if(matriz.de.proba.condicional.de.indicadores[i,j]<lim.inf){matriz.de.coeficientes.inicial[i,j]<-matriz.de.proba.condicional.de.indicadores[i,j]-(1/numero.ambientes)}
  }
}
matriz.de.coeficientes<-matriz.de.coeficientes.inicial
rownames(matriz.de.coeficientes)<-niveles.ambientes
colnames(matriz.de.coeficientes)<-nombre.de.indicadores
matriz.de.coeficientes

# Andrés ------------------------------
# no tiene que ser el mismo este alfa?

alfa2 <- 0.05

# en la fórmula anterior el límite inferior y superior se calcula dividiendo por sum(matriz.de.apariciones[,h])
# y el h no está definido, porque h quedó en memoria igual a 1, calculó la suma de la primer columna.

# se espera que haya un valor de lim.sup y lim.inf para cada especie indicadora o para todas?

n <- nlevels(group)

lim.sup<-(1/n)+qnorm(1-alfa2)*sqrt(((1/n)*(1-(1/n)))/colSums(com.pa.indic))
lim.inf<-(1/n)-qnorm(1-alfa2)*sqrt(((1/n)*(1-(1/n)))/colSums(com.pa.indic))


D <- p.cond.indic

Dt <- t(D)


Dt[Dt > lim.sup | Dt < lim.inf] <- Dt[Dt > lim.sup | Dt < lim.inf] - 1/nlevels(group)

Dt[Dt < lim.sup & Dt > lim.inf] <- 0

## Dt es la transpuesta de la matriz de coeficientes


# ----------------------------------------------------

############################################################################################################################
############################################################################################################################
#Funci?n que dada un conjunto de muestras nuevas, estima a que ambiente pertenece
############################################################################################################################
############################################################################################################################


### Implementar la estimación de a qué ambiente pertenece.
# tomando: nuevas muestras, matriz de probabilidad condicional de las especies indicadoras, 
# 

# Se leen las nuevas muestras (que en principio tienen tanto valores de par?metros f?sico-qu?mico como de especies y familias)

nuevas.muestras.inicial<-read.table("Data/NuevasMuestras.txt", header=FALSE)
nuevas.muestras<-as.matrix(nuevas.muestras.inicial)

# Andrés ------

nuevas.muestras <- read.table("Data/NuevasMuestras.txt", header=FALSE)

env.nueva <- nuevas.muestras[,1:15]

com.nueva <- nuevas.muestras[,16:58]
names(com.nueva) <- names(com)

write.table(com.nueva,file = "Data/comnueva.txt")

com.nueva <- read.table("Data/comnueva.txt", header = TRUE)
# ------------------

#se traduce a 0 y 1 de acuerdo a si hubo apariciones o no

nuevas.muestras.inicial<-matrix(0,length(nuevas.muestras[,1]),length(nuevas.muestras[1,]))
for(k in 1:length(nuevas.muestras[1,])){
  for(j in 1:length(nuevas.muestras[,1])){
    if(nuevas.muestras[j,k]>0){nuevas.muestras.inicial[j,k]<-1}
  }
}
nuevas.muestras<-nuevas.muestras.inicial

# Andrés ---------------------

com.nueva.pa <- decostand(com.nueva,method="pa")

# ---------------------------






## Nos quedamos solo con las especies indicadoras

nuevas.muestras.A<-nuevas.muestras[,posicion.de.indicadores+n.fq]
nuevas.muestras.A



# Andrés ---------------------

  sp.indic <- select_indicator_species(com,env$Ambiente)

  #tomo una parte de la funnción para seleccionar especies 

  alfa <- 0.05
  indic <-  indep > qchisq(1-alfa, nlevels(group)-1, ncp=0, lower.tail = TRUE, log.p = FALSE)

  com.nueva.sel <- subset(com.nueva.pa,select = which(indic==1, arr.ind = T))
  
  #nuevas.muestras.A == com.nueva.sel con este procedimiento son iguales.
  
  

# ---------------------------
  
  
  
  

#Se suman las columnas de la matriz anterior

matriz.A.inicial<-matrix(0,1,length(nuevas.muestras.A[1,]))
for(j in 1:length(nuevas.muestras.A[1,])){matriz.A.inicial[1,j]<-sum(nuevas.muestras.A[,j])}
matriz.A<-matriz.A.inicial
matriz.A


# Andrés ------------------------------

com.nueva.sel.sum <- colSums(com.nueva.sel)


#  com.nueva.sel.sum == matriz.A son iguales

# -------------------------------------



#########################################################################################################################################
#########################################################################################################################################
# producto entre la matriz A y la matriz de coeficientes (el n?mero m?s alto es la predicci?n del ambiente al que pertenecen las muestras)
#########################################################################################################################################
#########################################################################################################################################

estimacion.ambientes<-matriz.A%*%t(matriz.de.coeficientes)
estimacion.ambientes

# Andrés --------------------------------------------

est.env <- com.nueva.sel.sum %*% Dt

colnames(est.env)[apply(est.env, 1, which.max)]

# ---------------------------------------------------



############################################################################################################################
############################################################################################################################
# Puesta a prueba del algoritmo (usando la base de datos existente)
############################################################################################################################
############################################################################################################################










#Se aplica el algoritmo a cada muestra individual de la base de datos. Se toman SOLO LAS MUESTRAS EN LAS QUE APARECE AL MENOS UNA ESPECIE INDICADORA
#

###############################
# "Pasos intermedios"
###############################

# Primero se seleccionan las filas de la matriz "matriz.de.apariciones.de.indicadores" en las que aparece alguna especie indicadora

matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.inicial<-matrix(0,1,length(posicion.de.indicadores)+1)
nombre.filas.donde.aparecen.indicadores.inicial<-c(0)
for(j in 1:length(Ambientes)){
  if(sum(matriz.de.apariciones.de.indicadores[j,])!=0){
    matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.inicial<-rbind(matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.inicial,c(j,matriz.de.apariciones.de.indicadores[j,]))
    nombre.filas.donde.aparecen.indicadores.inicial<-c(nombre.filas.donde.aparecen.indicadores.inicial,Ambientes[1,j])
  } 
}
matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores<-matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.inicial[-1,]
matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores
nombre.filas.donde.aparecen.indicadores<-nombre.filas.donde.aparecen.indicadores.inicial[-1]
nombre.filas.donde.aparecen.indicadores

numero.filas.indicadores<-length(matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores[,1])

# Calculo los cortes en esa matriz

cortes.inicial.indicadores<-rep(0,numero.ambientes+1)
cortes.inicial.indicadores
for(i in 1:numero.ambientes){cortes.inicial.indicadores[i+1]<-(sum(nombre.filas.donde.aparecen.indicadores==niveles.ambientes[i])+cortes.inicial.indicadores[i])}
cortes.indicadores<-cortes.inicial.indicadores
cortes.indicadores #en que filas cambian los ambientes en la matriz "matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores"

# Me fijo el n?mero de muestras en las que aparece al menos una especie indicadora en cada ambiente y tomo el m?nimo de esos valores

hasta.donde.indicadores<-min((cortes.indicadores-c(0,cortes.indicadores[-length(cortes.indicadores)]))[-1])
hasta.donde.indicadores


# Calculo la matriz resultados solo mirando las filas que contienen al menos una especie indicadora

matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada.inicial<-matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores
matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada.inicial<-matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada.inicial[,-1]
matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada<-matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada.inicial
rownames(matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada)<-nombre.filas.donde.aparecen.indicadores
colnames(matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada)<-nombre.de.indicadores
matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada


resultado.solo.donde.aparecen.indicadoras<-matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada%*%t(matriz.de.proba.condicional.de.indicadores-(1/numero.ambientes))
resultado.solo.donde.aparecen.indicadoras
resultado<-resultado.solo.donde.aparecen.indicadoras
resultado




# Matriz que tiene un 1 si coincide la predicci?n con el ambiente, un 0 si no decide y un -1 si no coincide

confianza.inicial<-matrix(0,numero.filas.indicadores,1)
for(i in 1:(length(cortes)-1)){
  for(n in (cortes.indicadores[i]+1):cortes.indicadores[i+1]){
    if((max(resultado[n,])==resultado[n,i])&(sum((rep(max(resultado[n,]),length(cortes.indicadores)-1)-resultado[n,])==0)==1)){confianza.inicial[n,1]<-1}
  }
}
for(i in 1:(length(cortes.indicadores)-1)){
  for(n in (cortes.indicadores[i]+1):cortes.indicadores[i+1]){
    if(max(resultado[n,])!=resultado[n,i]){confianza.inicial[n,1]<--1}
  }
}
confianza<-confianza.inicial
confianza

############################################################################################################################################
#Valores y porcentaje de aciertos, indecisiones y desaciertos aplicando el algoritmo a cada muestra de la base de datos en forma individual
############################################################################################################################################

aciertos<-confianza==1
no.decide<-confianza==0
errores<-confianza==-1
matriz.resultados<-rbind(c(sum(aciertos),sum(no.decide),sum(errores)),c(sum(aciertos)/numero.filas.indicadores,sum(no.decide)/numero.filas.indicadores,sum(errores)/numero.filas.indicadores))
colnames(matriz.resultados)<-c("Aciertos", "No decide", "Desaciertos")
rownames(matriz.resultados)<-c("Valores", "Porcentajes")
matriz.resultados



#############################
# Tomando de a varias muestras
#############################
#Se toman al azar "n" muestras de cada ambiente (muestras en las que aparece al menos una especie indicadora) y se apica el algoritmo
#(el valor m?ximo de n viene dado por el m?nimo n?mero de muestras en las que aparece al menos una especie indicadora de cada ambiente)
#Se registran los aciertos, indecisiones y desacieros




# Gr?fico con acierto, indecisiones y desaciertos a medida que aumento el n?mero de muestras que tomo

hasta.donde<-min(c(hasta.donde.indicadores,20))
porcentaje.aciertos.con.ene.muestras<-rep(0,hasta.donde)
porcentaje.indeciciones.con.ene.muestras<-rep(0,hasta.donde)
porcentaje.pifiadas.con.ene.muestras<-rep(0,hasta.donde)

for(e in 1:hasta.donde){
  
  numero.de.muestras.n<-e
  
  de.a.ene.n.inicial<-matrix(0,numero.filas.indicadores,numero.ambientes)
  
  for(l in 1:(length(cortes.indicadores)-1)){
    for(i in (cortes.indicadores[l]+1):cortes.indicadores[l+1]){
      extraigo.n<-sample((cortes.indicadores[l]+1):cortes.indicadores[l+1],numero.de.muestras.n,replace=FALSE)
      suma.de.muestras.n<-rep(0,length(posicion.de.indicadores))
      for(j in 1:numero.de.muestras.n){suma.de.muestras.n<-suma.de.muestras.n+matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores.arreglada[extraigo.n[j],]}
      resultado.de.a.ene.n<-suma.de.muestras.n%*%t(matriz.de.proba.condicional.de.indicadores-(1/numero.ambientes))
      de.a.ene.n.inicial[i,]<-resultado.de.a.ene.n
    }
  }
  de.a.ene.n<-de.a.ene.n.inicial
  rownames(de.a.ene.n)<-matriz.de.apariciones.de.indicadores.donde.aparecen.indicadores[,1]
  colnames(de.a.ene.n)<-niveles.ambientes
  
  confianza.de.a.ene.n.inicial<-matrix(0,numero.filas.indicadores,1)
  for(i in 1:(length(cortes.indicadores)-1)){
    for(n in (cortes.indicadores[i]+1):cortes.indicadores[i+1]){
      if((max(de.a.ene.n[n,])==de.a.ene.n[n,i])&(sum((rep(max(de.a.ene.n[n,]),length(cortes.indicadores)-1)-de.a.ene.n[n,])==0)==1)){confianza.de.a.ene.n.inicial[n,1]<-1}
    }
  }
  for(i in 1:(length(cortes.indicadores)-1)){
    for(n in (cortes.indicadores[i]+1):cortes.indicadores[i+1]){
      if(max(de.a.ene.n[n,])!=de.a.ene.n[n,i]){confianza.de.a.ene.n.inicial[n,1]<--1}
    }
  }
  confianza.de.a.ene.n<-confianza.de.a.ene.n.inicial
  confianza.de.a.ene.n
  
  aciertos.de.a.ene.n<-confianza.de.a.ene.n==1
  sum(aciertos.de.a.ene.n)
  sum(aciertos.de.a.ene.n)/numero.filas.indicadores
  no.decide.de.a.ene.n<-confianza.de.a.ene.n==0
  sum(no.decide.de.a.ene.n)
  sum(no.decide.de.a.ene.n)/numero.filas.indicadores
  errores.de.a.ene.n<-confianza.de.a.ene.n==-1
  sum(errores.de.a.ene.n)
  sum(errores.de.a.ene.n)/numero.filas.indicadores
  
  porcentaje.aciertos.con.ene.muestras[e]<-sum(aciertos.de.a.ene.n)/numero.filas.indicadores
  porcentaje.indeciciones.con.ene.muestras[e]<-sum(no.decide.de.a.ene.n)/numero.filas.indicadores
  porcentaje.pifiadas.con.ene.muestras[e]<-sum(errores.de.a.ene.n)/numero.filas.indicadores
  
}


porcentaje.aciertos.con.ene.muestras
porcentaje.indeciciones.con.ene.muestras
porcentaje.pifiadas.con.ene.muestras



par(mfrow=c(1,3))
plot(1:hasta.donde, porcentaje.aciertos.con.ene.muestras, 
     type="p",pch=19,xlim=c(0, hasta.donde),ylim = c(-0.2, 1.5), 
     col = "red", lty = 1,lwd=2,axes = TRUE, xlab="", ylab="",main="Porcentaje de aciertos respecto de la cantidad de muestras")
axis(1, at = seq(1,hasta.donde, by = 1), las=2)
abline(h=0)
abline(h=0.8)
abline(h=0.9)
abline(h=1)
plot(1:hasta.donde, porcentaje.indeciciones.con.ene.muestras, 
     type="p",pch=19,xlim=c(0, hasta.donde),ylim = c(-0.2, 1.5), 
     col = "red", lty = 1,lwd=2,axes = TRUE, xlab="", ylab="",main="Porcentaje de indeciciones respecto de la cantidad de muestras")
axis(1, at = seq(1,hasta.donde, by = 1), las=2)
abline(h=0)
abline(h=0.2)
abline(h=0.1)
abline(h=1)
plot(1:hasta.donde, porcentaje.pifiadas.con.ene.muestras, 
     type="p",pch=19,xlim=c(0, hasta.donde),ylim = c(-0.2, 1.5), 
     col = "red", lty = 1,lwd=2,axes = TRUE, xlab="", ylab="",main="Porcentaje de errores respecto de la cantidad de muestras")
axis(1, at = seq(1,hasta.donde, by = 1), las=2)
abline(h=0)
abline(h=0.2)
abline(h=0.1)
abline(h=1)


###########################################################################################################
###########################################################################################################
###########################################################################################################
### Segunda Parte: Relaci?n entre par?metros f?sico-qu?micos y especies y familias presentes
###########################################################################################################
###########################################################################################################
###########################################################################################################

#Funci?n que dada una especie o familia "k" devuelve en la primer columna las muestras en las que aparece
#y en la otra columna el n?mero espec?menes encontrados en esa muestra. El n?mero "k" correspone al n?mero 
#de la columna de la base de datos (de la matriz n?meros.completos) correspondiente a la especie elegida

columnas.con.bicho<-function(k){
  m.inicial<-cbind(rep(0,sum(numeros.completos[,k]>0)),rep(0,sum(numeros.completos[,k]>0)))
  contador<-1
  for(i in 1:length(nombrefilas)){
    if(numeros.completos[i,k]!=0){
      m.inicial[contador,1]<-i
      m.inicial[contador,2]<-numeros.completos[i,k]
      contador<-contador+1
    }
  }
  m<-m.inicial
  return(m)
}

#Ejemplo
columnas.con.bicho(6)

#Funci?n que toma un par?metro f?sico-qu?mico t y una especie o familia k y devuelve una matriz con las apariciones de la especie con el valor
#del qu?mico asociado. En le primer columna aparece el valor del qu?mico (ordenados de menor a mayor)
#y en la segunda la cantidad de apariciones de la eapecie para ese valor del qu?mico t



quim.bich<-function(t,k){
  m.inicial<-cbind(rep(0,sum(numeros.completos[,k]>0)),rep(0,sum(numeros.completos[,k]>0)))
  contador<-1
  for(i in 1:length(nombrefilas)){
    if(numeros.completos[i,k]!=0){
      m.inicial[contador,1]<-numeros.completos[i,t]
      m.inicial[contador,2]<-numeros.completos[i,k]
      contador<-contador+1
    }
  }
  m<-m.inicial
  m1.ord<-sort(m[,1])
  m2.ord<-m[,2][order(m[,1])]
  m.ord<-cbind(m1.ord,m2.ord)
  colnames(m.ord)<-c(nombrecolumnas[t],nombrecolumnas[k])
  return(m.ord)
}

#Ejemplo
quim.bich(1,4)




# Lo mismo que antes pero sin ordenar de menor a mayor el qu?mico (en el orden en que aparecen en la base de datos)

quim.bich.sin.ordenar<-function(t,k){
  m.inicial<-cbind(rep(0,sum(numeros.completos[,k]>0)),rep(0,sum(numeros.completos[,k]>0)))
  contador<-1
  for(i in 1:length(nombrefilas)){
    if(numeros.completos[i,k]!=0){
      m.inicial[contador,1]<-numeros.completos[i,t]
      m.inicial[contador,2]<-numeros.completos[i,k]
      contador<-contador+1
    }
  }
  m<-m.inicial
  colnames(m)<-c(nombrecolumnas[t],nombrecolumnas[k])
  return(m)
}

#Ejemplo
quim.bich.sin.ordenar(1,4)

#Defino un vector con los qu?micos que quiero analizar

qu<-c(1,2)


# Funci?n que dada la especie de la columna "k" devuelve los valores de los qu?micos elegidos en las muestras en las que hay apariciones

BiyQui<-function(h){
  nn.inicial<-quim.bich.sin.ordenar(qu[1],h)[,1]
  for(j in 2:length(qu)){
    nn.inicial<-cbind(nn.inicial,quim.bich.sin.ordenar(qu[j],h)[,1])
  }
  nn<-nn.inicial
  return(nn)
}
#Ejemplo
BiyQui(4)


# Se calculan los "cortes" para los "cubos" de los qu?micos 
# El m?nimo es el valor m?nimo del qu?mico en todas las muestras y lo mismo con el m?ximo 
# Se define el n?mero de cortes y la cantidad de decimales a usar


numero.de.cortes<-2

decimales.de.los.cortes<-2

cortes<-function(j){
  q<-numeros.completos[,j]
  mi<-c(min(numeros.completos[,j]),max(numeros.completos[,j]))
  cort.inicial<-c(min(mi))
  for(l in 1:numero.de.cortes){cort.inicial<-c(cort.inicial,min(mi)+(((max(mi)-min(mi))/numero.de.cortes)*l))}
  cort<-cort<-round(cort.inicial, decimales.de.los.cortes)
  return(cort)
}
matriz.cortes.inicial<-matrix(0,numero.de.cortes+1,1)
for(k in 1:n.fq){
  matriz.cortes.inicial<-cbind(matriz.cortes.inicial,cortes(k))  
}
matriz.cortes<-matriz.cortes.inicial[,-1]
matriz.cortes



#Se seleccionan las columnas de la matriz anterior correspondiente a los qu?micos elegidos

matriz.cubitos.inicial<-matrix(0,numero.de.cortes+1,1)
for(t in 1:length(qu)){matriz.cubitos.inicial<-cbind(matriz.cubitos.inicial,matriz.cortes[,qu[t]])}
matriz.cubitos<-matriz.cubitos.inicial[,-1]
matriz.cubitos

############################################################################################
# La matriz anterior "matriz.cubitos" puede sustituirse por una matriz elegida por el usuario
#############################################################################################

# Porcentaje de mediciones de los qu?micos que caen en cada uno de los  cubitos discriminados por ambiente???????????????????????????????????

#Estudio de la distribuci?n de cada muestra en los cubos de los qu?micos elegidos

#Matriz que numera cada cubo, representado por las (n?mero de qu?micos elegidos)^(n?mero de cortes) combinaciones de
# longitud "n?mero de qu?micos elegidos" formada por los n?meros del 1 al "n?mero de cortes"

library(gtools)
prueba.cubito.inicial<-c(1:numero.de.cortes)
permutaciones <- permutations(numero.de.cortes, length(qu), prueba.cubito.inicial,repeats.allowed=TRUE)
prueba.cubito.inicial<-t(permutaciones)
prueba.cubito<-rbind(prueba.cubito.inicial,1:(numero.de.cortes)^(length(qu)))
prueba.cubito



# N?mero  (tomando todos los ambientes) de apariciones de la especie "b" en cada cubito 
# Defino una funci?n a"pariciones.cubitos(b)" que da el cubito en el que se encuentra de cada aparici?n de la especie 


apariciones.cubitos<-function(b){
  apariciones.cubitos.inicial<-matrix(0,1,length(BiyQui(b)[,1]))
  for(n in 1:length(BiyQui(b)[,1])){
    for(i in 1:length(prueba.cubito[1,])){
      ve.in<-rep(0,numero.de.cortes)
      for(s in 1:numero.de.cortes){if((matriz.cubitos[prueba.cubito[s,i],s]<=BiyQui(b)[n,s])&(BiyQui(b)[n,s]<=matriz.cubitos[prueba.cubito[s,i]+1,s])){ve.in[s]<-1}}
      if(prod(ve.in)==1){apariciones.cubitos.inicial[n]<-prueba.cubito[length(prueba.cubito[,1]),i]}
    }
  }
  apariciones.cubitos<-apariciones.cubitos.inicial
  return(apariciones.cubitos)
}

#Ejemplo
apariciones.cubitos(4)

#Funci?n que indica cuantas veces apareci? la especie "b" en cada cubo

cantidad.apariciones.en.cada.cubo<-function(x){
  cantidad.apariciones.en.cada.cubo.inicial<-matrix(0,1,length(prueba.cubito[1,]))
  if(sum(numeros.completos[,x]!=0)){
    APCU<-apariciones.cubitos(x)
    for(j in 1:length(prueba.cubito[1,])){cantidad.apariciones.en.cada.cubo.inicial[1,j]<-(sum(APCU==j))}
  }
  cantidad.apariciones.en.cada.cubo<-cantidad.apariciones.en.cada.cubo.inicial
  cantidad.apariciones.en.cada.cubo
  return(cantidad.apariciones.en.cada.cubo)
}

#Ejemplo
cantidad.apariciones.en.cada.cubo(4)


# Matriz donde cada fila es la cantidad de apariciones de la especie en cada cubo 
##############################################################################################################################
##############################################################################################################################
## IMPORTANTE: Este paso insume mucho tiempo, por eso luego de calcular la matriz, se exporta y luego se importa para
##             tomar los datos de la matriz ya calculada
##############################################################################################################################
##############################################################################################################################

matriz.apariciones.en.cada.cubo.todas.las.especies.inicial<-matrix(0,n.biota,length(prueba.cubito[1,]))
for(j in 1:n.biota){
  matriz.apariciones.en.cada.cubo.todas.las.especies.inicial[j,]<-cantidad.apariciones.en.cada.cubo(j+n.fq)
}
matriz.apariciones.en.cada.cubo.todas.las.especies<-matriz.apariciones.en.cada.cubo.todas.las.especies.inicial
matriz.apariciones.en.cada.cubo.todas.las.especies

#Se exporta la matriz calculada
write.csv(matriz.apariciones.en.cada.cubo.todas.las.especies, file="matriz.apariciones.en.cada.cubo.todas.las.especies.csv",row.names = F)
#Se lee la matriz calculada
matriz.apariciones.en.cada.cubo.todas.las.especies.final<-read.table("matriz.apariciones.en.cada.cubo.todas.las.especies.txt", header=FALSE)
matriz.apariciones.en.cada.cubo.todas.las.especies.final.final<-as.matrix(matriz.apariciones.en.cada.cubo.todas.las.especies.final)
matriz.apariciones.en.cada.cubo.todas.las.especies.final.final


# Funci?n que dada una muestra indica que especies aparecen

especies.que.aparecen.en.la.muestra<-function(y){
  especies.que.aparecen.en.la.muestra.inicial<-rep(0,1)
  for(f in (n.fq+1):(n.fq+n.biota)){if(numeros.completos[y,f]!=0){especies.que.aparecen.en.la.muestra.inicial<-c(especies.que.aparecen.en.la.muestra.inicial,f)}}
  if(length(especies.que.aparecen.en.la.muestra.inicial)==1){especies.que.aparecen.en.la.muestra.inicial<-c(0,0)}
  especies.que.aparecen.en.la.muestra<-especies.que.aparecen.en.la.muestra.inicial[-1]
  return(especies.que.aparecen.en.la.muestra)
}

#Ejemplo
especies.que.aparecen.en.la.muestra(8)

# Funci?n que dada una muestra indica los nombres de las especies que aparecen

nombre.de.las.especies.que.aparecen.en.la.muestra<-function(y){
  nombre.de.las.especies.que.aparecen.en.la.muestra.inicial<-rep(0,length(especies.que.aparecen.en.la.muestra(y)))
  for(f in 1:length(especies.que.aparecen.en.la.muestra(y))){
    nombre.de.las.especies.que.aparecen.en.la.muestra.inicial[f]<-nombrecolumnas[especies.que.aparecen.en.la.muestra(y)[f]]
  }
  nombre.de.las.especies.que.aparecen.en.la.muestra<-nombre.de.las.especies.que.aparecen.en.la.muestra.inicial
  return(nombre.de.las.especies.que.aparecen.en.la.muestra)
}

#Ejemplo
nombre.de.las.especies.que.aparecen.en.la.muestra(8)


#matriz que da las especies que aparecen en cada una de las muestras

largo.matriz.inicial<-0
for(i in 1:length(numeros.completos[,1])){
  especies.que.aparecen.en.la.muestra(i)
  if(length(especies.que.aparecen.en.la.muestra(i))>largo.matriz.inicial){largo.matriz.inicial<-length(especies.que.aparecen.en.la.muestra(i))}}
largo.matriz<-largo.matriz.inicial
largo.matriz

matriz.especies.que.aparecen.inicial<-matrix(0,length(numeros.completos[,1]),n.fq)
for(j in 1:length(numeros.completos[,1])){
  matriz.especies.que.aparecen.inicial[j,]<-c(especies.que.aparecen.en.la.muestra(j),rep(0,largo.matriz-length(especies.que.aparecen.en.la.muestra(j)))) 
}
matriz.especies.que.aparecen<-matriz.especies.que.aparecen.inicial
matriz.especies.que.aparecen




# Matriz que dada una muestra me da las especies que aparecieron en esa muestra y en que cubos aparecieron

apariciones.en.cada.cubo.en.la.muestra<-function(t){
  apariciones.en.cada.cubo.en.la.muestra.inicial<-matrix(0,1,length(prueba.cubito[1,]))
  for(b in 1:length(especies.que.aparecen.en.la.muestra(t))){apariciones.en.cada.cubo.en.la.muestra.inicial<-rbind(apariciones.en.cada.cubo.en.la.muestra.inicial,matriz.apariciones.en.cada.cubo.todas.las.especies.final.final[especies.que.aparecen.en.la.muestra(t)[b]-n.fq,])}
  apariciones.en.cada.cubo.en.la.muestra<-apariciones.en.cada.cubo.en.la.muestra.inicial[-1,]
  rownames(apariciones.en.cada.cubo.en.la.muestra)<-nombre.de.las.especies.que.aparecen.en.la.muestra(t)
  return(apariciones.en.cada.cubo.en.la.muestra)
}

#Ejemplo
apariciones.en.cada.cubo.en.la.muestra(8)


#Vector que, dada una muestra, devuelve en que cubos aparecen TODAS las especies que aparecen en esa muestra (en que
# cubos aparecen todas juntas)


posibles.cubos.de.la.muestra<-function(w){
  posibles.cubos.de.la.muestra.inicial<-rep(0,1)
  for(h in 1:length(prueba.cubito[1,])){if(prod(apariciones.en.cada.cubo.en.la.muestra(w)[,h])!=0){posibles.cubos.de.la.muestra.inicial<-c(posibles.cubos.de.la.muestra.inicial,h)}}
  posibles.cubos.de.la.muestra<-posibles.cubos.de.la.muestra.inicial[-1]
  return(posibles.cubos.de.la.muestra)
}

#Ejemplo
posibles.cubos.de.la.muestra(1)


#Vector que muestra en que cubito cae cada muestra

cubito.de.cada.muestra.inicial<-rep(0,length(prueba.cubito[1,]))
for(n in 1:length(numeros.completos[,1])){
  for(i in 1:length(prueba.cubito[1,])){
    ve.mu<-rep(0,numero.de.cortes)
    for(s in 1:numero.de.cortes){if(((matriz.cubitos[prueba.cubito[s,i],s]<=numeros.completos[n,qu[s]])&(numeros.completos[n,qu[s]]<=matriz.cubitos[prueba.cubito[s,i]+1,s]))){ve.mu[s]<-1}}
    if(prod(ve.mu)==1){cubito.de.cada.muestra.inicial[n]<-prueba.cubito[length(prueba.cubito[,1]),i]}
  }
}
cubito.de.cada.muestra<-cubito.de.cada.muestra.inicial
cubito.de.cada.muestra



#Funci?n que dada una cantidad de especies E1, E2, ..., En 
#devuelve en que muestras aparecen todas al mismo tiempo (eso se se?ala mediante un uno o un cero) y en que cubo est?n


aparicion.conjunta<-function(v){
  aparicion.conjunta.inicial<-matrix(0,1,length(numeros.completos[,1]))
  aparicion.conjunta.inicial<-rbind(aparicion.conjunta.inicial,cubito.de.cada.muestra)
  rownames(aparicion.conjunta.inicial)<-NULL
  if(v[1]!=0){
    for(m in 1:length(numeros.completos[,1])){
      vector.auxiliar<-rep(0,length(v))
      for(j in 1:length(v)){
        vector.auxiliar[j]<-numeros.completos[m,v[j]]
      }
      if(prod(vector.auxiliar)!=0){aparicion.conjunta.inicial[1,m]<-1}
    }
  }
  aparicion.conjunta<-aparicion.conjunta.inicial
  return(aparicion.conjunta)
}

#Ejemplo
aparicion.conjunta(c(4,7))



#Funci?n que dada una cantidad de especies E1, E2, ..., En 
#devuelve en que cubos aparecen todas al mismo tiempo (es decir, existe una muestra, que est? en un 
# determinado cubo, donde aparecen todas juntas )


cubos.en.donde.aparecen.todas<-function(s){
  rjmh<-aparicion.conjunta(s)
  rjm<-(rjmh[1,]==0)
  rj<-rjmh[2,][rjm==F]
  aver<-sort(rj)
  cubos.en.donde.aparecen.todas<-aver[!duplicated(aver)]
  return(cubos.en.donde.aparecen.todas)
}

#Ejemplo
cubos.en.donde.aparecen.todas(c(4,7))




#Funci?n que dada una cantidad de especies E1, E2, ..., En 
#devuelve la cantidad de veces que esto sucede (contando todas las muestras) en cada cubo

aparicion.conjunta.en.cada.cubo<-function(w){
  aparicion.conjunta.en.cada.cubo.inicial<-rep(0,length(prueba.cubito[1,]))
  for(r in 1:length(prueba.cubito[1,])){
    contador<-0
    for(s in 1:length(numeros.completos[,1])){if((aparicion.conjunta(w)[1,s]!=0)&(aparicion.conjunta(w)[2,s]==r)){contador<-contador+1}}
    aparicion.conjunta.en.cada.cubo.inicial[r]<-contador
  }
  aparicion.conjunta.en.cada.cubo<-aparicion.conjunta.en.cada.cubo.inicial
  return(aparicion.conjunta.en.cada.cubo)
}

#Ejemplo
aparicion.conjunta.en.cada.cubo(c(4,7))


#Funci?n que dada una cantidad de especies E1, E2, ..., En 
#devuelve la probabilidad de estar en el cubo Cj sabiendo que aparecieron esas especies

proba.de.estar.en.tal.cubo.sabiendo.que.aparecieron.tales.especies<-function(z){
  rr<-aparicion.conjunta.en.cada.cubo(z)
  if(sum(rr)!=0){proba.de.estar.en.tal.cubo.sabiendo.que.aparecieron.tales.especies<-rr/sum(rr)}else{proba.de.estar.en.tal.cubo.sabiendo.que.aparecieron.tales.especies<-rep(0,length(prueba.cubito[1,]))}
  return(proba.de.estar.en.tal.cubo.sabiendo.que.aparecieron.tales.especies)
}

#Ejemplo
proba.de.estar.en.tal.cubo.sabiendo.que.aparecieron.tales.especies(c(4,7))




#Matriz que tiene el cubo al que pertenece cada muestra en su primer columna
#y la probabilidad de estar en cada cubo en las siguientes columnas (en esa muestra aparecen varias especies y todas ellas
# juntas aparecen con cierta probabbilidad en cada cubo)
################################################
##IMPORTANTE: Este paso tambi?n insume tiempo ##
###############################################

predicciones.inicial<-rep(0,length(prueba.cubito[1,]))
for(u in 1:length(numeros.completos[,1])){predicciones.inicial<-rbind(predicciones.inicial,proba.de.estar.en.tal.cubo.sabiendo.que.aparecieron.tales.especies(especies.que.aparecen.en.la.muestra(u)))}  
predicciones.inicial<-predicciones.inicial[-1,]
c.d.c.m<-as.matrix(cubito.de.cada.muestra)
predicciones<-cbind(c.d.c.m,predicciones.inicial)
predicciones

write.csv(predicciones, file="predicciones.csv",row.names = F)

predicciones.final<-read.table("predicciones.txt", header=FALSE)
predicciones.final.final<-as.matrix(predicciones.final)
predicciones.final.final

#Matriz que da el n?mero de posibles cubos en los que aparecen todas las especies que hay en esa muestra en la primer columna 
#y la cantidad de especies que aparecieron en esa muestra en la segunda columna 


numero.de.posibles.cubos.en.cada.muestra.inicial<-matrix(0,length(numeros.completos[,1]),2)
for(s in 1:length(numeros.completos[,1])){
  numero.de.posibles.cubos.en.cada.muestra.inicial[s,1]<-(sum(predicciones.final.final[s,]!=0)-1)
}
for(t in 1:length(numeros.completos[,1])){
  numero.de.posibles.cubos.en.cada.muestra.inicial[t,2]<-length(especies.que.aparecen.en.la.muestra(t))
}
numero.de.posibles.cubos.en.cada.muestra<-numero.de.posibles.cubos.en.cada.muestra.inicial
numero.de.posibles.cubos.en.cada.muestra



#Funci?n que dado el n?mero de un cubito, devuelve entre que par?metros se mueve

del.numero.del.cubito.a.sus.parametros<-function(i){
  aux.inicial<-c(matriz.cubitos[prueba.cubito[1,i],1],matriz.cubitos[prueba.cubito[1,i]+1,1])
  for(r in 2:length(qu)){aux.inicial<-rbind(aux.inicial,c(matriz.cubitos[prueba.cubito[r,i],r],matriz.cubitos[prueba.cubito[r,i]+1,r]))}
  aux<-aux.inicial
  nom.inicial<-c(nombrecolumnas[qu[1]])
  for(s in 2:length(qu)){nom.inicial<-c(nom.inicial,nombrecolumnas[qu[s]])}
  rownames(aux)<-nom.inicial
  del.numero.del.cubito.a.sus.parametros<-aux
  return(del.numero.del.cubito.a.sus.parametros)
}

#Ejemplo
del.numero.del.cubito.a.sus.parametros(3)


######################################################################################################
######################################################################################################
#Funci?n que dada una cantidad de especies E1, E2, ..., En 
#devuelve en que cubos aparecen todas al mismo tiempo (es decir, existe una muestra, que est? en un 
# determinado cubo, donde aparecen todas juntas ) e indica entre que par?metros se mueve cada cubo
######################################################################################################
######################################################################################################


par?metros.en.donde.aparecen.todas<-function(g){
  for(f in 1:length(cubos.en.donde.aparecen.todas(g))){print(del.numero.del.cubito.a.sus.parametros(cubos.en.donde.aparecen.todas(g)[f]))}
}

#Ejemplo
par?metros.en.donde.aparecen.todas(c(4,6))
par?metros.en.donde.aparecen.todas(c(5,6))
par?metros.en.donde.aparecen.todas(c(4))



 ##################################################################################################




















 










