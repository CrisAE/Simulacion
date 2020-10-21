#Práctica 5: método Monte-Carlo. Código base en Ref. [4,6].

library(countcolors)
library(jpeg)
coca=readJPEG('C:/Users/Cristina BH/Documents/coca.jpg')
print(coca)

#Definición de rangos de color para usar en el paquete countcolors de R
white.center = c(1, 1, 1)
red.center=c(1,0,0)
#Cuenta exacta de pixeles por cada rango de color definido con el paquete countcolors de R
coca.white=countcolors::sphericalRange(coca, center = white.center, radius = 0.5, color.pixels = FALSE, plotting = TRUE, target.color="magenta")  
blanco=coca.white$pixel.count
coca.red=countcolors::sphericalRange(coca, center = red.center, radius = 0.3, color.pixels = FALSE, plotting = TRUE, target.color="blue")  
rojo=coca.red$pixel.count

#Si se asume que un pixel equivale a 10cm^2 del mural, y que un litro de pintura rinde 10m^2:
pixeles=c(blanco,rojo)
pixeles
pintura=pixeles*0.001 #Litros de pintura
pintura

#Estimación con método Monte-Carlo
runs=1000
datos=data.frame()
for(r in 1:length(runs)){
  for(s in 1:10000){
    blue = coca[,,3]
    x =sample (blue, runs[r])
    y=sum(x < 0.5)
    print(y)
    datos=rbind(datos,c(s,runs[r],y))
  }
}
names(datos)=c("Réplica","Muestra","Pixeles")
write.table(datos,"datosp.txt",sep="\t",quote=F,row.names=F)
pix = datos [,3]
pixm = mean(pix)
montecarlorojo = pixm*(160000/runs)
montecarlorojo
mcblanco=160000-montecarlorojo
mcblanco
#Litros de pintura
pixelesmc=c(mcblanco,montecarlorojo)
pixelesmc
pinturamc=pixelesmc*0.001 
pinturamc
