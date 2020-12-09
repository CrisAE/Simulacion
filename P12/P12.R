#Práctica 12: red neuronal. Código base en Ref.[1].

library(tidyr)

binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

#Se agregan las probabilidades para cada color
negro=c(0.995,0.90,0.80)
gris=c(0.92,0.85,0.70)
blanco=c(0.002,0.010,0.015)

datos=data.frame()
for(neg in negro){
  for(gri in gris){
    for(bla in blanco){
      for(rep in 1:10){
        modelos <- read.csv("digits.txt", sep=" ", header=FALSE, stringsAsFactors=F)
        modelos[modelos=='n'] <- neg
        modelos[modelos=='g'] <- gri
        modelos[modelos=='b'] <- bla
        
        r <- 5
        c <- 3
        dim <- r * c
        
        tasa <- 0.15
        tranqui <- 0.99
        
        tope <- 9
        digitos <- 0:tope
        k <- length(digitos)
        contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
        rownames(contadores) <- 0:tope
        colnames(contadores) <- c(0:tope, NA)
        
        n <- floor(log(k-1, 2)) + 1
        neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
        
        for (t in 1:5000) { # entrenamiento
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,]
          correcto <- binario(d, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            if (deseada != resultado) {
              ajuste <- tasa * (deseada - resultado)
              tasa <- tranqui * tasa
              neuronas[i,] <- w + ajuste * pixeles
            }
          }
        }
        
        for (t in 1:300) { # prueba
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
          correcto <- binario(d, n)
          salida <- rep(FALSE, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            salida[i] <- resultado
          }
          r <- min(decimal(salida, n), k) # todos los no-existentes van al final
          contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
        }
        print(contadores)
        precision = diag(contadores) / colSums(contadores[,1:10])
        recall = diag(contadores) / rowSums(contadores)
        f1 = ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
        datos=rbind(datos,c(rep,neg,gri,bla,f1))
      }
    }
  }
}

names(datos) <- c("Réplica", "Negro","Gris","Blanco","Cero", "Uno","Dos","Tres","Cuatro","Cinco","Seis","Siete","Ocho","Nueve")
write.table(datos,"datos.txt",sep="\t",quote=F,row.names = F)

N1G1B1=datos[1:10,5:14]
N1G1B2=datos[11:20,5:14]
N1G1B3=datos[21:30,5:14]
N1G2B1=datos[31:40,5:14]
N1G2B2=datos[41:50,5:14]
N1G2B3=datos[51:60,5:14]
N1G3B1=datos[61:70,5:14]
N1G3B2=datos[71:80,5:14]
N1G3B3=datos[81:90,5:14]

N2G1B1=datos[91:100,5:14]
N2G1B2=datos[101:110,5:14]
N2G1B3=datos[111:120,5:14]
N2G2B1=datos[121:130,5:14]
N2G2B2=datos[131:140,5:14]
N2G2B3=datos[141:150,5:14]
N2G3B1=datos[151:160,5:14]
N2G3B2=datos[161:170,5:14]
N2G3B3=datos[171:180,5:14]

N3G1B1=datos[181:190,5:14]
N3G1B2=datos[191:200,5:14]
N3G1B3=datos[201:210,5:14]
N3G2B1=datos[211:220,5:14]
N3G2B2=datos[221:230,5:14]
N3G2B3=datos[231:240,5:14]
N3G3B1=datos[241:250,5:14]
N3G3B2=datos[251:260,5:14]
N3G3B3=datos[261:270,5:14]

png("p12-digitos.png")
boxplot(N1G1B1,N1G1B2,N1G1B3,N1G2B1,N1G2B2,N1G2B3,N1G3B1,N1G3B2,N1G3B3,
        N2G1B1,N2G1B2,N2G1B3,N2G2B1,N2G2B2,N2G2B3,N2G3B1,N2G3B2,N2G3B3,
        N3G1B1,N3G1B2,N3G1B3,N3G2B1,N3G2B2,N3G2B3,N3G3B1,N3G3B2,N3G3B3,col=rainbow(10), xlab="Dígitos", ylab="Valor F",names=c("0","1","2","3","4","5","6","7","8","9"))
graphics.off()

N1G1B1=gather(datos[1:10,5:14])
N1G1B2=gather(datos[11:20,5:14])
N1G1B3=gather(datos[21:30,5:14])
N1G2B1=gather(datos[31:40,5:14])
N1G2B2=gather(datos[41:50,5:14])
N1G2B3=gather(datos[51:60,5:14])
N1G3B1=gather(datos[61:70,5:14])
N1G3B2=gather(datos[71:80,5:14])
N1G3B3=gather(datos[81:90,5:14])

N2G1B1=gather(datos[91:100,5:14])
N2G1B2=gather(datos[101:110,5:14])
N2G1B3=gather(datos[111:120,5:14])
N2G2B1=gather(datos[121:130,5:14])
N2G2B2=gather(datos[131:140,5:14])
N2G2B3=gather(datos[141:150,5:14])
N2G3B1=gather(datos[151:160,5:14])
N2G3B2=gather(datos[161:170,5:14])
N2G3B3=gather(datos[171:180,5:14])

N3G1B1=gather(datos[181:190,5:14])
N3G1B2=gather(datos[191:200,5:14])
N3G1B3=gather(datos[201:210,5:14])
N3G2B1=gather(datos[211:220,5:14])
N3G2B2=gather(datos[221:230,5:14])
N3G2B3=gather(datos[231:240,5:14])
N3G3B1=gather(datos[241:250,5:14])
N3G3B2=gather(datos[251:260,5:14])
N3G3B3=gather(datos[261:270,5:14])

vf1=cbind(N1G1B1$value,N1G1B2$value,N1G1B3$value,N1G2B1$value,N1G2B2$value,N1G2B3$value,N1G3B1$value,N1G3B2$value,N1G3B3$value,
        N2G1B1$value,N2G1B2$value,N2G1B3$value,N2G2B1$value,N2G2B2$value,N2G2B3$value,N2G3B1$value,N2G3B2$value,N2G3B3$value,
        N3G1B1$value,N3G1B2$value,N3G1B3$value,N3G2B1$value,N3G2B2$value,N3G2B3$value,N3G3B1$value,N3G3B2$value,N3G3B3$value)

png("p12-factor.png",width = 900, height = 500)
boxplot(vf1,col=rainbow(27), ylab="Valor F",names=c("N1G1B1","N1G1B2","N1G1B3","N1G2B1","N1G2B2","N1G2B3","N1G3B1","N1G3B2","N1G3B3",
                                                                          "N2G1B1","N2G1B2","N2G1B3","N2G2B1","N2G2B2","N2G2B3","N2G3B1","N2G3B2","N2G3B3",
                                                                          "N3G1B1","N3G1B2","N3G1B3","N3G2B1","N3G2B2","N3G2B3","N3G3B1","N3G3B2","N3G3B3"),cex.names=0.5,las=2)
graphics.off()






