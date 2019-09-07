titanic <- read.csv(file="C:\\Users\\Clau\\Documents\\ESTUDIO\\Estaca\\Lab1\\titanic_data.csv",sep = ",")

titanic <- titanic[!(is.na(titanic$Age)),]

#a)
Hom <- titanic[ which(titanic$Sex=='male'), ]
Muj <- titanic[ which(titanic$Sex=='female'), ]

#b)
HomS <- Hom[which(Hom$Survived==1),]
MujS <- Muj[which(Muj$Survived==1),]
boxplot(HomS$Age, MujS$Age,names=c("Hombres","Mujeres"),col = c("yellow","red"),ylim=c(0,65),border = "brown")

#d)
Surv <- titanic[which(titanic$Survived==1),]
#Porcentaje de sobrevivientes y el precio que pagaron en total.
library(lattice)
histogram(Surv$Fare)
#Se calcula la frecuencia absoluta de sobrevivientes dependiendo del costo.
Nuev<-titanic
Nuev$Fare=as.integer(Nuev$Fare)
Surv$Fare=as.integer(Surv$Fare)
n1=Nuev[which(0<=Nuev$Fare & Nuev$Fare<=50),]
n2=Nuev[which(50<Nuev$Fare & Nuev$Fare<=100),]
n3=Nuev[which(100<Nuev$Fare & Nuev$Fare<=150),]
n4=Nuev[which(150<Nuev$Fare & Nuev$Fare<=200),]
n5=Nuev[which(200<Nuev$Fare & Nuev$Fare<=250),]
n6=Nuev[which(250<Nuev$Fare & Nuev$Fare<=300),]
n7=Nuev[which(300<Nuev$Fare & Nuev$Fare<=350),]
n8=Nuev[which(350<Nuev$Fare & Nuev$Fare<=400),]
n9=Nuev[which(400<Nuev$Fare & Nuev$Fare<=450),]
n10=Nuev[which(450<Nuev$Fare),]
s1=Surv[which(0<=Surv$Fare & Surv$Fare<=50),]
s2=Surv[which(50<Surv$Fare & Surv$Fare<=100),]
s3=Surv[which(100<Surv$Fare & Surv$Fare<=150),]
s4=Surv[which(150<Surv$Fare & Surv$Fare<=200),]
s5=Surv[which(200<Surv$Fare & Surv$Fare<=250),]
s6=Surv[which(250<Surv$Fare & Surv$Fare<=300),]
s7=Surv[which(300<Surv$Fare & Surv$Fare<=350),]
s8=Surv[which(350<Surv$Fare & Surv$Fare<=400),]
s9=Surv[which(400<Surv$Fare & Surv$Fare<=450),]
s10=Surv[which(450<=Surv$Fare),]
freq=c(nrow(s1)/nrow(n1),nrow(s2)/nrow(n2),nrow(s3)/nrow(n3),nrow(s4)/nrow(n4),nrow(s5)/nrow(n5),nrow(s6)/nrow(n6),nrow(s7)/nrow(n7),nrow(s8)/nrow(n8),nrow(s9)/nrow(n9),nrow(s10)/nrow(n10))
numb=c(50,100,150,200,250,300,350,400,450,500)

dat <- data.frame(x=numb, y=freq)
barplot(dat$y, names.arg=dat$x, ylim=c(0,1),main="Frecuencia de sobrevivencia según precio",xlab="Precio pagado",ylab="Frecuencia",col=rainbow(10))

#e)
Cab <- titanic[!(titanic$Cabin==""), ]

#f)
#Se calcula la cantidad de letras que hay en todas las cabin
  
Le = NULL
Su = NULL
  
for (x in LETTERS){n=str_count(Cab$Cabin,x)
Le=append(Le,x)
Su=append(Su,sum(n))}
  
Let=data.frame(Le,Su)

  
#g)

Cab2 <- Surv[!(Surv$Cabin==""), ]

Le = NULL
Su = NULL

for (x in LETTERS){n=str_count(Cab2$Cabin,x)
Le=append(Le,x)
Su=append(Su,sum(n))}

Let2=data.frame(Le,Su)

barplot(Let$Su,  ylim=c(0,70),names.arg=Let$Le,col="blue",legend="total",main="Sobrevivientes según Letra de Cabin")
par(new=TRUE)
barplot(Let2$Su,  ylim=c(0,70),names.arg=Let2$Le,col="red",legend="sobrevivientes",xlab="Letra Cabin",ylab="Cantidad de personas",main="Sobrevivientes según Letra de Cabin")


#h) Concluir
