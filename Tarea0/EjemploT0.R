#Cosas básicas
x=33
y=34
z=x+y
z
datos=c(22,14,55,74,89,12,1,10,41,90,38,20)
length(datos)
min(datos)
max(datos)
sum(datos)
ord=sort(datos)
ord[3]
ord[7]
mean(datos)
summary(datos)
quantile(datos,0.5)
table(datos)

var(datos)
sd(datos)
#Matrices, aritmética, funciones, ect.
M=matrix(rep(1,9),nrow=3,ncol=3)
M
a=c(1,4,7)
b=c(1,6,8)
a==b

all(a==b)
any(a==b)

a+b
b**a
a%%b
r=3.4
floor(r)
ceiling(r)
round(r)
class(r)

sqrt(r)
exp(r)
sin(r)
cos(r)
log(r)
log(r,5)

factorial(3)
choose(1,5)

s=seq(1,100,5)
s

q=2:14
q

tail(s)
head(s)

p=3
o=NULL
if(p>4){o=p*3}else{o=5-p}
o
p==o

if(o!=1){print("no es uno")}
for(i in 1:8){print(3**1)}
while(p>0){print("menos uno"); p=p-1}
p

#Subrutinas, números pseudoaleatorios, etc.
rutina=function(x)2**x
rutina(4)
desc=function(y,z){d=y*z/100;de=z-d;return(de)}
desc(50,100)

runif(3)
runif(3,4,9)
runif(3)<0.5
sample(1:10,5)
rnorm(10)
rnorm(10,mean=5,sd=0.5)

f=c(22,64,93,68)
g=c(10,37,7,29)
plot(f,g)
plot(f,g,type="l")
boxplot(f,g)
hist(f)
