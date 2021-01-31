
select_indicator_species <- function( ambientes, especies, alfa) {
  #
  # No funciona realmente porque copie el codigo sin modificar los nombres
  #
  independencia.inicial<-rep(0,numero.columnas)
  for(i in 1:numero.columnas){
    independencia.inicial[i]<-sum(((tapply(matriz.de.apariciones[,i],ambientes.como.factores,sum)-(vectores.de.apariciones.totales[i]/numero.ambientes))^2)/(vectores.de.apariciones.totales[i]/numero.ambientes))
  }
  independencia<-independencia.inicial
  independencia
  qchisq(1-0.005, numero.ambientes-1, ncp=0, lower.tail = T, log.p = F)
  
  vector.de.indicadores.inicial<-rep(0,numero.columnas)
  for(k in 1:numero.columnas){
    if(vectores.de.apariciones.totales[k]!=0){
      if(independencia[k]>qchisq(1-alfa, numero.ambientes-1, ncp=0, lower.tail = T, log.p = F)){vector.de.indicadores.inicial[k]<-1}
    }
  }
  vector.de.indicadores<-vector.de.indicadores.inicial
  vector.de.indicadores #Vector que indica (con un 1) en que columna de la matriz "numeros" hay una especie indicadora
  
  
  nombre.de.indicadores.inicial<-c(0)
  for(s in 1:numero.columnas){if(vector.de.indicadores[s]==1){nombre.de.indicadores.inicial<-c(nombre.de.indicadores.inicial,Biota[s])}}
  nombre.de.indicadores.inicial<-nombre.de.indicadores.inicial[-1]
  nombre.de.indicadores<-nombre.de.indicadores.inicial
  nombre.de.indicadores #nombre de las especies indicadoras"
  
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
  
  
}
