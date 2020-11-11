#Práctica 8: modelo de urnas. Código base en Ref. [1].

install.packages("testit")
library(testit) 

replica=10
datos=data.frame()

for(k in c(20000,30000,50000)){
  for(n in c(2000000,4000000,6000000,10000000)){
    for(r in 1:replica){
      originales <- rnorm(k)
      cumulos <- originales - min(originales) + 1
      cumulos <- round(n * cumulos / sum(cumulos))
      assert(min(cumulos) > 0)
      diferencia <- n - sum(cumulos)
      if (diferencia > 0) {
        for (i in 1:diferencia) {
          p <- sample(1:k, 1)
          cumulos[p] <- cumulos[p] + 1
        }
      } else if (diferencia < 0) {
        for (i in 1:-diferencia) {
          p <- sample(1:k, 1)
          if (cumulos[p] > 1) {
            cumulos[p] <- cumulos[p] - 1
          }
        }
      }
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      assert(sum(cumulos) == n)
      c <- median(cumulos) # tamanio critico de cumulos
      d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
      rotura <- function(x) {
        return (1 / (1 + exp((c - x) / d)))
      }
      union <- function(x) {
        return (exp(-x / c))
      }
      romperse <- function(tam, cuantos) {
        romper <- round(rotura(tam) * cuantos) # independientes
        resultado <- rep(tam, cuantos - romper) # los demas
        if (romper > 0) {
          for (cumulo in 1:romper) { # agregar las rotas
            t <- 1
            if (tam > 2) { # sample no jala con un solo valor
              t <- sample(1:(tam-1), 1)
            }
            resultado <- c(resultado, t, tam - t)
          }
        }
        assert(sum(resultado) == tam * cuantos) # no hubo perdidas
        return(resultado)
      }
      unirse <- function(tam, cuantos) {
        unir <- round(union(tam) * cuantos) # independientes
        if (unir > 0) {
          division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
          assert(sum(abs(division)) == tam * cuantos)
          return(division)
        } else {
          return(rep(tam, cuantos))
        }
      }
      
      
      freq <- as.data.frame(table(cumulos))
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      duracion=15                              #Después 30 y 50 iteraciones
      digitos <- floor(log(duracion, 10)) + 1
      for (paso in 1:duracion) {
        assert(sum(cumulos) == n)
        cumulos <- integer()
        for (i in 1:dim(freq)[1]) { # fase de rotura
          urna <- freq[i,]
          if (urna$tam > 1) { # no tiene caso romper si no se puede
            cumulos <- c(cumulos, romperse(urna$tam, urna$num))
          } else {
            cumulos <- c(cumulos, rep(1, urna$num))
          }
        }
        assert(sum(cumulos) == n)
        assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
        freq <- as.data.frame(table(cumulos)) # actualizar urnas
        names(freq) <- c("tam", "num")
        freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
        assert(sum(freq$num * freq$tam) == n)
        cumulos <- integer()
        for (i in 1:dim(freq)[1]) { # fase de union
          urna <- freq[i,]
          cumulos <- c(cumulos, unirse(urna$tam, urna$num))
        }
        assert(sum(abs(cumulos)) == n)
        assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
        juntarse <- -cumulos[cumulos < 0]
        cumulos <- cumulos[cumulos > 0]
        assert(sum(cumulos) + sum(juntarse) == n)
        nt <- length(juntarse)
        if (nt > 0) {
          if (nt > 1) {
            juntarse <- sample(juntarse)
            for (i in 1:floor(nt / 2) ) {
              cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
            }
          }
          if (nt %% 2 == 1) {
            cumulos <- c(cumulos, juntarse[nt])
          }
        }
        assert(sum(cumulos) == n)
        freq <- as.data.frame(table(cumulos))
        names(freq) <- c("tam", "num")
        freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
        assert(sum(freq$num * freq$tam) == n)  
      }
      filtro=subset(freq,tam>c,num)   #Partículas mayores al tamaño crítico c
      porc=(sum(filtro)/sum(freq$num)*100)  #Porcentaje de partículas filtradas
      datos=rbind(datos, c(porc,r,n,k))
    }
  }
}

#Generación de gráficos
names(datos)=rbind("Porcentaje","Rep","Partículas","Cúmulos")
write.table(datos,"datos.txt",sep="\t",quote=F,row.names = F)
datos15=read.csv("datos15.txt",header=TRUE,sep="\t")
png("p8-15-part.png")
boxplot(datos15$Porcentaje~datos15$Partículas, col=rainbow(5), ylim=c(34.0,36.0), xlab="Partículas iniciales (n)", ylab="Porcentaje filtrado (>c)",names=c("2,000,000","4,000,000","6,000,000","10,000,000"))
graphics.off() 
png("p8-15-cum.png")
boxplot(datos15$Porcentaje~datos15$Cúmulos, col=rainbow(5), ylim=c(34.0,36.0), xlab="Cúmulos (k)", ylab="Porcentaje filtrado (>c)",names=c("20,000","30,000","50,000"))
graphics.off() 
datos30=read.csv("datos30.txt",header=TRUE,sep="\t")
png("p8-30-part.png")
boxplot(datos30$Porcentaje~datos30$Partículas, col=rainbow(5), ylim=c(34.0,36.0), xlab="Partículas iniciales (n)", ylab="Porcentaje filtrado (>c)",names=c("2,000,000","4,000,000","6,000,000","10,000,000"))
graphics.off() 
png("p8-30-cum.png")
boxplot(datos30$Porcentaje~datos30$Cúmulos, col=rainbow(5), ylim=c(34.0,36.0), xlab="Cúmulos (k)", ylab="Porcentaje filtrado (>c)",names=c("20,000","30,000","50,000"))
graphics.off() 
datos50=read.csv("datos50.txt",header=TRUE,sep="\t")
png("p8-50-part.png")
boxplot(datos50$Porcentaje~datos50$Partículas, col=rainbow(5), ylim=c(34.0,36.0), xlab="Partículas iniciales (n)", ylab="Porcentaje filtrado (>c)",names=c("2,000,000","4,000,000","6,000,000","10,000,000"))
graphics.off() 
png("p8-50-cum.png")
boxplot(datos50$Porcentaje~datos50$Cúmulos, col=rainbow(5), ylim=c(34.0,36.0), xlab="Cúmulos (k)", ylab="Porcentaje filtrado (>c)",names=c("20,000","30,000","50,000"))
graphics.off()

#Análisis estadístico
datos=rbind(datos15,datos30,datos50)
anova <- aov(datos$Porcentaje ~ datos$Partículas*datos$Cúmulos,)
summary(anova)




