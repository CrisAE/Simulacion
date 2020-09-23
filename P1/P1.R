#Práctica 1: movimiento browniano, tiempo de regreso al origen
#Cita del código: por comunicación personal con Dra. Elisa Shaeffer

#Tiempo de regreso al origen en pasos
regOri=data.frame()
NoregOri=data.frame()
for(e in 1:5){
  pasos=2**e
  for(dim in 1:8){
    for(r in 1:50){
      pos=rep(0,dim)
      regreso=FALSE
      for(t in 1:pasos){
        cambiar=sample(1:dim,1)
        if(runif(1)<0.5){
          pos[cambiar]=pos[cambiar]+1
        }else{
          pos[cambiar]=pos[cambiar]-1
        }
        if(all(pos==0)){
          regOri=rbind(regOri,c(pasos,dim,t))
          regreso=TRUE
          break
        }
      }
      if(!regreso){NoregOri=rbind(NoregOri,c(pasos,dim))}
    }
  }
}
names(regOri)=c('Pasos','Dimensión','Tiempo')
names(NoregOri)=c('Pasos','Dimensión')
dim(regOri)
dim(NoregOri)

#Generación del gráfico
write.table(regOri,"regOri.txt",sep="\t",quote=F,row.names = F)
write.table(NoregOri,"NoregOri.txt",sep = "\t",quote=F,row.names=F)
png('Practica1.png')
boxplot(regOri$Tiempo~regOri$Dimensión,xlab = "Dimensión",ylab = "Tiempo en pasos",col=rainbow(8),border="black")
dev.off()
