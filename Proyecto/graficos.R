#Análisis de los datos obtenidos a partir de la simulación.

library(ggplot2)
#RMSD
rmsd=read.csv("rmsd.txt",header=TRUE,sep="\t")
png("rmsd.png")
p1 = ggplot() + 
  geom_line(aes(y = RMSD, x = Time), data = rmsd) +
  scale_x_continuous(breaks=seq(0,100,10)) + 
  theme(text=element_text(family="Tahoma"))
p1 + labs(x = "Tiempo (ns)", y = "RMSD (nm)")
graphics.off()

#RMSF
rmsf=read.csv("rmsf-res.txt",header=TRUE,sep="\t")
png("rmsf.png")
p2 = ggplot() + 
  geom_line(aes(y = nm, x = Residue), data = rmsf) +
  scale_x_continuous(breaks=seq(0,80,5))  
  p2 + labs(x = "Número de residuo", y = "Fluctuación (nm)")
graphics.off()