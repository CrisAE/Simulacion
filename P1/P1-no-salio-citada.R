#Intento fallido de los efectos de la dimensión en el tiempo de regreso al origen del movimiento browniano
#Basado en el código de claratepa (https://github.com/claratepa/Simulacion/blob/master/Practica1/pr1sim.R) y simulación basada en loops mencionada en (https://robotwealth.com/efficiently-simulating-geometric-brownian-motion-in-r/)
#Con adaptación en las líneas 5-8, 15, 17, 22-23.

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