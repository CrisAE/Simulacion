g <- function(x, y) {
  func<- (((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)^2/100)
  return(func)
}

valg <- c()
temperaturas <- c(0,10,15,20,30)
for(tem in temperaturas){
  low <- -3
  high <- 3
  step <- 0.25
  replicas <- 100
  t <- tem
  ep <- 0.99
  replica <- function(t){
    curr <- c(runif(1, low, high), runif(1, low, high))
    best <- curr
    for (tiempo in 1:t) {
      delta <- runif(1, 0, step)
      x1 <- curr + c(-delta,0)
      x2 <- curr + c(delta,0)
      y1 <- curr + c(0,-delta)
      y2 <- curr + c(0,delta)
      puntos <- c(x1,x2,y1,y2)
      for(k in 1:8){
        if(puntos[k] < (-5)){
          puntos[k] <- puntos[k]+10
        }
        if(puntos[k] > 5){
          puntos[k] <- puntos[k]-10
        }
      }
      vecx <- c()
      vecy <- c()
      for(p in 1:8){
        if(p %% 2 == 0){
          vecy <- c(vecy,puntos[p])
        }else{
          vecx <- c(vecx,puntos[p])
        }
      }
      u <- sample(1:4,1)
      x.p <- c(vecx[u],vecy[u])
      delt <- g(x.p[1],x.p[2]) - g(curr[1],curr[2])
      if(delt > 0){
        curr <- x.p
      }else{
        if(runif(1)< exp((delt) / (t * ep))){
          curr <- x.p
          if(t == 1){
            t <-t
          }else{
            t <- t-1
          }
        }
      }
      
      if(g(curr[1],curr[2]) > g(best[1],best[2])){
        best <- curr
      }
    }
    return(best)
  }
  
  
  tmax <- 100
  resultados <- c()
  for(r in 1:100){
    resultados <- c(resultados, replica(tmax))
  }
  vecx <- c()
  vecy <- c()
  aux <- 200
  for(p in 1:aux){
    if(p %% 2 == 0){
      vecy <- c(vecy,resultados[p])
    }else{
      vecx <- c(vecx,resultados[p])
    }
  }
  
  valores <- c()
  for(q in 1:100){
    valores <- c(valores, g(vecx[q], vecy[q]))
  }
  valg <- c(valg, valores)
  
}

v1 <- c(valg[1:100])
v2 <- c(valg[101:200])
v3 <- c(valg[201:300])
v4 <- c(valg[301:400])
v5 <- c(valg[401:500])
datos <- data.frame(v1, v2, v3, v4, v5)

png("p7-0.99.png")
colnames(datos)<- c(0,10,15,20,30)
boxplot(datos, col=rainbow(5), xlab="Temperatura", ylab="Valor en g", cex.lab = 1.5, cex.axis= 1.5)
graphics.off()