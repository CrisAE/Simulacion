#Intento fallido de los efectos de la dimensión en el tiempo de regreso al origen del movimiento browniano

dimension=8
exponente=10
pos=0
rep=50

for(e in 5:exponente){
  pasos=2**e
  for(dim in 1:dimension){
    for(replica in 1:rep){
      pos=rep(0,dim)
      esOrigen=FALSE
      for(t in 1:pasos){
        cambiar=sample(1:dimension,1)
        if(runif(1)<0.5){
          pos[cambiar]=pos[cambiar]+1
        }else{pos[cambiar]=pos[cambiar]-1}
      }
        if(all(pos==0)){esOrigen=TRUE
        return(TRUE)
    }
  }
}