#Práctica 11: frentes de Pareto. Código base en Ref. [1].

pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

vc <- 4
md <- 3
tc <- 5
fo = seq(2, 12, by=1) # cuantas funciones objetivo

resultados=data.frame()
for (k in fo){
  for(rep in 1:20){
    obj <- list()
    for (i in 1:k) {
      obj[[i]] <- poli(vc, md, tc)
    }
    minim <- (runif(k) > 0.5)
    sign <- (1 + -2 * minim)
    n <- 200 # cuantas soluciones aleatorias
    sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
    val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
    for (i in 1:n) { # evaluamos las soluciones
      for (j in 1:k) { # para todos los objetivos
        val[i, j] <- eval(obj[[j]], sol[i,], tc)
      }
    }
    mejor1 <- which.max(sign[1] * val[,1])
    mejor2 <- which.max(sign[2] * val[,2])
    no.dom <- logical()
    dominadores <- integer()
    for (i in 1:n) {
      d <- logical()
      for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,])) #Quité k
      }
      cuantos <- sum(d)
      dominadores <- c(dominadores, cuantos)
      no.dom <- c(no.dom, cuantos == 0) # nadie le domina
    }
    frente <- subset(val, no.dom) # solamente las no dominadas
    porcentaje=(sum(no.dom)/n)*100 #Porcentaje de soluciones
    resultados=rbind(resultados, c(k, rep, porcentaje)) 
  }
}
names(resultados) <- c("Func.Obj.", "Réplica", "Porcentaje")
write.table(resultados,"resultados.txt",sep="\t",quote=F,row.names = F)
resultados$Func.Obj. = as.factor(resultados$Func.Obj.)

png("p11.png", height = 10, width = 20, units = "cm", res = 900)
ggplot(resultados, aes(x=Func.Obj., y=Porcentaje, fill=Func.Obj.)) + geom_violin(scale = "width") + 
  scale_fill_brewer(palette="Paired")+ theme_light() + 
  geom_boxplot(inherit.aes = TRUE, fill="grey", color="black", width = 0.1)+
  xlab("Funciones objetivo")+
  ylab("Porcentaje de soluciones")
graphics.off()

esnorm = shapiro.test(resultados$Porcentaje)
esnorm$p.value < 0.05
#Se toma alfa como 0.05
kw = kruskal.test(resultados$Porcentaje~resultados$Func.Obj.)
pww = pairwise.wilcox.test(resultados$Porcentaje, resultados$Func.Obj., p.adjust.method = "holm")
pww$p.value < 0.05 #Diferencia significativa



