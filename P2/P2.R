#Práctica 2: autómata celular. Código base tomado de Ref. [1].

#Juego de la vida, en matriz booleana.
dim=20 
num=dim^2
aum=0.10
prob=numeric(length((0.9/0.10)-1))

datos=data.frame()

for(cor in 1:((0.9/0.10)-1)){
  actual=matrix(1*(runif(num)<aum),nrow=dim,ncol=dim,byrow=TRUE)
  suppressMessages(library("sna"))
  png("inicioP2.png")
  plot.sociomatrix(actual,diaglab=FALSE,main="Inicio")
  graphics.off()
  
  print(actual)
  paso=function(pos){
    fila=floor((pos-1)/dim)+1
    columna=((pos-1)%%dim)+1
    vecindad=actual[max(fila-1,1):min(fila+1,dim),
                    max(columna-1,1):min(columna+1,dim)]
    return(1*((sum(vecindad)-actual[fila,columna])==3))
  }
  aum=aum+0.10
  for(iteracion in 1:50){
    valores=numeric()
    for(i in 1:num){valores=c(valores,paso(i))}
    datos=rbind(datos,c(cor,iteracion,vivos))
    vivos=sum(valores)
    cat(iteracion,vivos,'\n')
    if(vivos==0){       
      print("Ya no queda nadie vivo.")
      prob[cor]=iteracion
      
      break
  }
names(datos)=c("Cor","Iteración","Vivos")
write.table(datos,"datos.txt",sep="\t",quote=F,row.names=F)
actual=matrix(valores,nrow=dim,ncol=dim,byrow=TRUE)
salida=paste("p2_t",iteracion,".png",sep="")
tiempo=paste("Paso",iteracion)
png(salida)
plot.sociomatrix(actual,diaglab=FALSE,main=tiempo)
graphics.off()
}
}

#Colapso mayor
colapso=numeric()
for(i in 1:length(datos$Vivos)-1){colapso[i]=datos[i,3]-datos[(i+1),3]}
print(colapso)

png('total.png')
boxplot(datos$Vivos~datos$Iteración,xlab="Iteración",ylab="Vivos",col=rainbow(9),border="black")
dev.off()

prob0_1=datos[c(2:4),c(2:3)]
png('prob0-1.png')
barplot(prob0_1$Vivos~prob0_1$Iteración,main="Probabilidad inicial de 0.1",xlab="Iteración",ylab="Vivos",col=heat.colors(3),border="black")
dev.off()

prob0_2=datos[c(5:53),c(2:3)]
png('prob0-2.png')
barplot(prob0_2$Vivos~prob0_2$Iteración,main="Probabilidad inicial de 0.2",xlab="Iteración",ylab="Vivos",col=rainbow(9),border="black")
dev.off()

prob0_3=datos[c(55:103),c(2:3)]
png('prob0-3.png')
barplot(prob0_3$Vivos~prob0_3$Iteración,main="Probabilidad inicial de 0.3",xlab="Iteración",ylab="Vivos",col=rainbow(9),border="black")
dev.off()

prob0_4=datos[c(105:116),c(2:3)]
png('prob0-4.png')
barplot(prob0_4$Vivos~prob0_4$Iteración,main="Probabilidad inicial de 0.4",xlab="Iteración",ylab="Vivos",col=rainbow(9),border="black")
dev.off()

prob0_5=datos[c(117:165),c(2:3)]
png('prob0-5.png')
barplot(prob0_5$Vivos~prob0_5$Iteración,main="Probabilidad inicial de 0.5",xlab="Iteración",ylab="Vivos",col=rainbow(9),border="black")
dev.off()

prob0_6=datos[c(167:171),c(2:3)]
png('prob0-6.png')
barplot(prob0_6$Vivos~prob0_6$Iteración,main="Probabilidad inicial de 0.6",xlab="Iteración",ylab="Vivos",col=rainbow(9),border="black")
dev.off()

prob0_7=datos[c(172:177),c(2:3)]
png('prob0-7.png')
barplot(prob0_7$Vivos~prob0_7$Iteración,main="Probabilidad inicial de 0.7",xlab="Iteración",ylab="Vivos",col=rainbow(9),border="black")
dev.off()

prob0_8=datos[c(178:179),c(2:3)]
png('prob0-8.png')
barplot(prob0_8$Vivos~prob0_8$Iteración,main="Probabilidad inicial de 0.8",xlab="Iteración",ylab="Vivos",col=rainbow(9),border="black")
dev.off()
