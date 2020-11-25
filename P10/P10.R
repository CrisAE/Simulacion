#Práctica 10: algoritmo genético.Código base en Ref. [1].

library(testit)

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        tabla[acum, objeto + 1] <- tabla[acum, objeto]                
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}


factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1,media,desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

#Reglas
generador.pesos.exp.ind <- function(cuantos, min, max) {
  return(sort(round(normalizar(rexp(cuantos)) * (max - min) + min)))
}

generador.valores.exp.ind <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    valores <- c(valores, rexp(1))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

generador.valores.cor.pos <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1,media,desv)*pesos[i])
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

generador.valores.cor.neg <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1,media,desv)/pesos[i])
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

#Con selección de ruleta
tiempo=data.frame()
n <- 50 
pesos <- generador.pesos.exp.ind(n, 15, 80)   
valores <- generador.valores.exp.ind(pesos, 10, 500)   #Se varía con los correlacionados positivo y negativo
capacidad <- round(sum(pesos) * 0.65)
for(r in 1:3){
  opt1=Sys.time()
  optimo <- knapsack(capacidad, pesos, valores)
  opt2=Sys.time()
  opt=opt2-opt1
  Topt=as.numeric(opt)
  exacto=optimo/Topt
  init <- 200
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  assert(tam == init)
  pm <- 0.05
  rep <- 50
  tmax <- 50
  mejores <- double()
  for (iter in 1:tmax) {
    t1=Sys.time()
    fitness=numeric()
    for (i in 1:tam){fitness=rbind(fitness,c(objetivo(p[i,],valores)))}
    fitness=fitness/sum(fitness)
    p$obj <- NULL
    p$fact <- NULL
    for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
      if (runif(1) < pm) {
        p <- rbind(p, mutacion(p[i,], n))
      }
    }
    for (i in 1:rep) { # una cantidad fija de reproducciones
      padres <- sample(1:tam, 2, replace=FALSE,prob = fitness) #Reproducción con selección de ruleta
      hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
      p <- rbind(p, hijos[1:n]) # primer hijo
      p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
    }
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
    for (i in 1:tam) {       
      obj <- c(obj, objetivo(p[i,], valores))
      fact <- c(fact, factible(p[i,], pesos, capacidad))
    }
    p <- cbind(p, obj)
    p <- cbind(p, fact)
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
    Vgen=mejor
    t2=Sys.time()
    tm=t2-t1
    tf=as.numeric(tm)
    genetico=Vgen/tf
    if(genetico>exacto){
      break 
    }
    tiempo=rbind(tiempo,c(r,iter,mejor,genetico,optimo,exacto))
  }
}
names(tiempo)=rbind("Réplica","Iteración","Mejor","Genético","Óptimo","Exacto")
datos=write.table(tiempo, "Rdatos50-I1.txt",sep="\t",quote=F,row.names = F)

###SIN selección de ruleta###
tiempo=data.frame()
n <- 50 
pesos <- generador.pesos.exp.ind(n, 15, 80)   
valores <- generador.valores.exp.ind(pesos, 10, 500)   #Se varía con los correlacionados positivo y negativo
capacidad <- round(sum(pesos) * 0.65)
for(r in 1:3){
  opt1=Sys.time()
  optimo <- knapsack(capacidad, pesos, valores)
  opt2=Sys.time()
  opt=opt2-opt1
  Topt=as.numeric(opt)
  exacto=optimo/Topt
  init <- 200
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  assert(tam == init)
  pm <- 0.05
  rep <- 50
  tmax <- 50
  mejores <- double()
  for (iter in 1:tmax) {
    t1=Sys.time()
    p$obj <- NULL
    p$fact <- NULL
    for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
      if (runif(1) < pm) {
        p <- rbind(p, mutacion(p[i,], n))
      }
    }
    for (i in 1:rep) { # una cantidad fija de reproducciones
      padres <- sample(1:tam, 2, replace=FALSE) 
      hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
      p <- rbind(p, hijos[1:n]) # primer hijo
      p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
    }
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
    for (i in 1:tam) {       
      obj <- c(obj, objetivo(p[i,], valores))
      fact <- c(fact, factible(p[i,], pesos, capacidad))
    }
    p <- cbind(p, obj)
    p <- cbind(p, fact)
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
    Vgen=mejor
    t2=Sys.time()
    tm=t2-t1
    tf=as.numeric(tm)
    genetico=Vgen/tf
    if(genetico>exacto){
      break 
    }
    tiempo=rbind(tiempo,c(r,iter,mejor,genetico,optimo,exacto))
  }
}
names(tiempo)=rbind("Réplica","Iteración","Mejor","Genético","Óptimo","Exacto")
datos=write.table(tiempo, "datos50-I1.txt",sep="\t",quote=F,row.names = F)

#Gráficos y análisis estadístico
ri1=read.csv("Rdatos50-I1.txt",header=TRUE,sep="\t")
ri2=read.csv("Rdatos50-I2.txt",header=TRUE,sep="\t")
ri3=read.csv("Rdatos50-I3.txt",header=TRUE,sep="\t")
i1=read.csv("datos50-I1.txt",header=TRUE,sep="\t")
i2=read.csv("datos50-I2.txt",header=TRUE,sep="\t")
i3=read.csv("datos50-I3.txt",header=TRUE,sep="\t")

mv=cbind(ri1$Mejor,i1$Mejor,ri2$Mejor,i2$Mejor,ri3$Mejor,i3$Mejor)
png("p10-mv.png")
boxplot(mv, col=rainbow(5), xlab="Instancias", ylab="Mejor valor",names=c("RI1","I1","RI2","I2","RI3","I3"))
graphics.off() 

times=cbind(ri1$Genético,i1$Genético,ri2$Genético,i2$Genético,ri3$Genético,i3$Genético)
png("p10-time.png")
boxplot(times, col=rainbow(5), xlab="Instancias", ylab="Tiempo (s)",names=c("RI1","I1","RI2","I2","RI3","I3"))
graphics.off() 

er=cbind(ri1$Genético,i1$Genético,ri2$Genético,i2$Genético,ri3$Genético,i3$Genético)
er1=cbind(ri1$Exacto,i1$Exacto,ri2$Exacto,i2$Exacto,ri3$Exacto,i3$Exacto)
er2=abs(((er-er1)/er))
png("p10-error.png")
boxplot(er2, col=rainbow(5), xlab="Instancias", ylim= c(0,50),ylab="Porcentaje de error",names=c("RI1","I1","RI2","I2","RI3","I3"))
graphics.off()

kruskal.test(ri1$Mejor~i1$Mejor)
kruskal.test(ri2$Mejor~i2$Mejor)
kruskal.test(ri3$Mejor~i3$Mejor)
kruskal.test(ri1$Genético~i1$Genético)
kruskal.test(ri2$Genético~i2$Genético)
kruskal.test(ri3$Genético~i3$Genético)

