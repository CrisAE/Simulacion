#Práctica 3: teoría de colas. Código base tomado de Ref. [1 y 3].

#Determinación del número de núcleos disponibles en el sistema.
library(parallel)
detectCores()

#Examinación de los tiempos de ejecución con diferentes ordenamientos.
numprimos=read.csv("primes1-fragm.txt",header=FALSE)
n=dim(numprimos)
print(length(numprimos))
noprimos=numprimos+1 #Se vuelven pares, así que dejan de ser primos
trabajo=c(numprimos,noprimos)

primo=function(n){
  if(n==1||n==2){
    return(TRUE)
  }
  if(n%%2==0){
    return(FALSE)
  }
  for(i in seq(3,max(3,ceiling(sqrt(n))),2)){
    if((n%%i)==0){
      return(FALSE)
    }
  }
  return(TRUE)
}

original=trabajo
invertido=rev(trabajo)
aleatorio=sample(original)
replicas=10
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores()-1))
ot=numeric()
it=numeric()
at=numeric()
for(r in 1:replicas){
  ot=c(ot,system.time(foreach(n=original, .combine=c) %dopar% primo(n))[3])
  it=c(it,system.time(foreach(n=invertido, .combine=c) %dopar% primo(n))[3])
  at=c(at,system.time(foreach(n=aleatorio, .combine=c) %dopar% primo(n))[3])
}
stopImplicitCluster()
summary(ot)
summary(it)
summary(at)

tiempo=t(rbind(ot,it,at))
png("p3.png")
boxplot(tiempo,xlab="Ordenamientos",ylab="Tiempo de ejecución (s)",col=rainbow(8),border="black")
graphics.off()

names(tiempo)=c("Original","Invertido","Aleatorio")
write.table(tiempo,file="p3-tiempo.txt",quote=FALSE,sep="\t",row.names=FALSE)




