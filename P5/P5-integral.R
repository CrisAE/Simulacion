#Práctica 5: método de Monte-Carlo. Código base en Ref. [1].

muestras=c(10,100,1000,10000,100000,500000)
replica=10
inicio= -6
final= -inicio
aum=0.25
x=seq(inicio,final,aum)
valor=0.048834
i=1
porc=numeric(length(muestras*replica))
suppressMessages(library(distr))
f = function(x) { return(1 / (exp(x) + exp(-x))) }
g = function(x) { return((2 / pi) * f(x)) }
desde = 3
hasta = 7
cuantos = 500
generador = r(AbscontDistribution(d = g)) # creamos un generador            
parte = function() {
  valores = generador(muestra)
  return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for(muestra in muestras){
  for(r in 1:replica){
montecarlo = foreach(i = 1:cuantos, .combine=c) %dopar% parte()
integral = sum(montecarlo) / (cuantos * muestra)
resultado=((pi / 2) * integral)
error=abs(((valor-resultado)/valor)*100)
porc[i]=error
print(resultado)
print(error)
i=i+1
  }
}
stopImplicitCluster()

res=matrix(porc,ncol=6,nrow=replica)
png("p5-error.png")
lbls=c("10","100","1000","10000","100000","500000")
boxplot(res,col=rainbow(6),names=lbls,ylab="Porcentaje de error",xlab="Tamaño de muestra")
abline(h=0.048834,col="red")
graphics.off()
