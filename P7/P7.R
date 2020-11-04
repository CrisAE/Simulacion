#Práctica 7: búsqueda local. Código base en Ref. [].

library(reshape2) 
library(lattice)
library(parallel)

g <- function(x, y) {
  return (((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)^2/100)
}
x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, g)
png("p7_2.png", width=700, height=700)
persp(x, y, z, shade=0.2, col='orange', theta=40, phi=30)
graphics.off()

low <- -3
high <- 3
step <- 0.25
replicas <- 100

replica <- function(t){
  puntosxy<- c()
  curr <- c( x = runif(1, min = low, max = high), y = runif(1, min = low, max = high))
  best <- curr
  for (tiempo in 1:t) {
    delta <- runif(1, 0, step)
    left <- curr + c(-delta,0) 
    right <- curr + c(delta,0) 
    up <- curr + c(0,-delta) 
    down <- curr + c(0,delta) 
    puntos <- c(left, right, up, down)
    
    for(k in 1:8){
      if(puntos[k] < (-3)){
        puntos[k] <- puntos[k]+3 
      }
      if(puntos[k] > 3){
        puntos[k] <- puntos[k]-3
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
    valg <- c()
    for(q in 1:4){
      valg <- c(valg, g(vecx[q], vecy[q]) )
    }
    dm <- which.max(valg)
    curr <- c(vecx[dm], vecy[dm])
    puntosxy <- c(puntosxy, vecx[dm],vecy[dm])
  }
  return(puntosxy)
}

resultado <- c()
for(q in 1:30){
  resultado <- c(resultado, replica(100))
}

vx <- c()
vy <- c()
for(p in 1:6000){
  if(p %% 2 == 0){
    vy <- c(vy,resultado[p])
  }else{
    vx <- c(vx,resultado[p])
  }
}

vx1 <- c(vx[1:100])
vx2 <- c(vx[101:200])
vx3 <- c(vx[201:300])
vx4 <- c(vx[301:400])
vx5 <- c(vx[401:500])
vx6 <- c(vx[501:600])
vx7 <- c(vx[601:700])
vx8 <- c(vx[701:800])
vx9 <- c(vx[801:900])
vx10 <- c(vx[901:1000])
vx11 <- c(vx[1001:1100])
vx12 <- c(vx[1101:1200])
vx13 <- c(vx[1201:1300])
vx14 <- c(vx[1301:1400])
vx15 <- c(vx[1401:1500])
vx16 <- c(vx[1501:1600])
vx17 <- c(vx[1601:1700])
vx18 <- c(vx[1701:1800])
vx19 <- c(vx[1801:1900])
vx20 <- c(vx[1901:2000])
vx21 <- c(vx[2001:2100])
vx22 <- c(vx[2101:2200])
vx23 <- c(vx[2201:2300])
vx24 <- c(vx[2301:2400])
vx25 <- c(vx[2401:2500])
vx26 <- c(vx[2501:2600])
vx27 <- c(vx[2601:2700])
vx28 <- c(vx[2701:2800])
vx29 <- c(vx[2801:2900])
vx30 <- c(vx[2901:3000])


vy1 <- c(vy[1:100])
vy2 <- c(vy[101:200])
vy3 <- c(vy[201:300])
vy4 <- c(vy[301:400])
vy5 <- c(vy[401:500])
vy6 <- c(vy[501:600])
vy7 <- c(vy[601:700])
vy8 <- c(vy[701:800])
vy9 <- c(vy[801:900])
vy10 <- c(vy[901:1000])
vy11 <- c(vy[1000:1100])
vy12 <- c(vy[1101:1200])
vy13 <- c(vy[1201:1300])
vy14 <- c(vy[1301:1400])
vy15 <- c(vy[1401:1500])
vy16 <- c(vy[1501:1600])
vy17 <- c(vy[1601:1700])
vy18 <- c(vy[1701:1800])
vy19 <- c(vy[1801:1900])
vy20 <- c(vy[1901:2000])
vy21 <- c(vy[2001:2100])
vy22 <- c(vy[2101:2200])
vy23 <- c(vy[2201:2300])
vy24 <- c(vy[2301:2400])
vy25 <- c(vy[2401:2500])
vy26 <- c(vy[2501:2600])
vy27 <- c(vy[2601:2700])
vy28 <- c(vy[2701:2800])
vy29 <- c(vy[2801:2900])
vy30 <- c(vy[2901:3000])


suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for(j in 1:100){
  x <- seq(-6, 5, 0.25)
  y <-  x
  z <- outer(x, y, g)
  dimnames(z) <- list(x, y)
  d <- melt(z)
  names(d) <- c("x", "y", "z")
  if(j < 10){
    nombre <-  paste0("p7_100", j, ".png", sep="")
  }else if(j>= 10 & j < 100){ 
    nombre <-  paste0("p7_10", j, ".png", sep="") }else{
      nombre <-  paste0("p7_1", j, ".png", sep="")
    }
  png(nombre, width=500, height=500)
  plot(levelplot(z ~ x * y, data = d))
  trellis.focus("panel", 1, 1)
  lpoints(vx1[j], vy1[j], pch=1, col="black")
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1)
  lpoints(vx2[j], vy2[j], pch=2, col="black")
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1)
  lpoints(vx3[j], vy3[j], pch=3, col="black")
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1)
  lpoints(vx4[j], vy4[j], pch=4, col="black")
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1)
  lpoints(vx5[j], vy5[j], pch=5, col="black")
  trellis.unfocus()
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx6[j], vy6[j], pch=6, col="black", cex=1)
  trellis.unfocus()
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx7[j], vy7[j], pch=7, col="black", cex=1)
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1, highlight=FALSE)
  lpoints(vx8[j], vy8[j], pch=8, col="black", cex=1)
  trellis.unfocus()
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx9[j], vy9[j], pch=9, col="black", cex=1)
  trellis.unfocus()
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx10[j], vy10[j], pch=10, col="black", cex=1)
  trellis.unfocus()
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx11[j], vy11[j], pch=11, col="black", cex=1)
  trellis.unfocus()
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx12[j], vy12[j], pch=12, col="black", cex=1)
  trellis.unfocus()
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx13[j], vy13[j], pch=13, col="black", cex=1)
  trellis.unfocus()
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx14[j], vy14[j], pch=14, col="black", cex=1)
  trellis.unfocus()
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx15[j], vy15[j], pch=15, col="black", cex=1)
  trellis.unfocus()
  
  graphics.off() 
}