zika <- read.csv(file="C:\\Users\\Clau\\Documents\\ESTUDIO\\Estaca\\Lab1\\cdc_zika.csv",sep = ",")

#a)
zika<-zika[!(is.na(zika$report_date) | is.na(zika$value)), ] #Borra las que no cumplen

#b)
zika$value = as.numeric(as.character(zika$value))
zika<-zika[!(is.na(zika$report_date) | is.na(zika$value)), ]


#c) Falta encontrar como son completamente nulas.
zika$time_period=NULL
zika$time_period_type=NULL

#e)
USA <-zika[which(grepl("^United_States", zika$location)),]
Arg <-zika[which(grepl("^Argentina", zika$location)),]
Col <-zika[which(grepl("^Colombia", zika$location)),]
Br <-zika[which(grepl("^Brazil", zika$location)),]
Mex <-zika[which(grepl("^Mexico", zika$location)),]

#f) 
USA <- USA[!(USA$value==0), ]
Arg <- Arg[!(Arg$value==0), ]
Col <- Col[!(Col$value==0), ]
Br <- Br[!(Br$value==0), ]
Mex <- Mex[!(Mex$value==0), ]

#g) 
USA <- USA[which(grepl("zika",USA$data_field) & (grepl("reported",USA$data_field) | grepl("lab",USA$data_field))), ]
Arg <- Arg[which(grepl("cumulative_confirmed",Arg$data_field)), ]
Col <- Col[which(grepl('confirmed',Col$data_field)), ]
Br <- Br[which(grepl('zika',Br$data_field)),]
Mex <- Mex[which(grepl('confirmed',Mex$data_field)),]

#h)
USA$location='United_States'
Arg$location='Argentina'
Col$location='Colombia'
Br$location='Brazil'
Mex$location='Mexico'

#i)
#Mediana
median(zika$value)

m1=median(USA$value)
m2=median(Arg$value)
m3=median(Col$value)
m4=median(Br$value)
m5=median(Mex$value)


#No estoy segura de la varianza.
v1=var(USA$value)
v2=var(Arg$value)
v3=var(Col$value)
v4=var(Br$value)
v5=var(Mex$value)

#Media
mean(zika$value)

me1=mean(USA$value)
me2=mean(Arg$value)
me3=mean(Col$value)
me4=mean(Br$value)
me5=mean(Mex$value)

#Cuartiles 1y 3 que nolos he separado.
q1=quantile(USA$value)
q2=quantile(Arg$value)
q3=quantile(Col$value)
q4=quantile(Br$value)
q5=quantile(Mex$value)


#j)

boxplot(USA$value, Arg$value, Col$value,Br$value,Mex$value,names=c('United States','Argentina','Colombia','Brazil','Mexico'),col=rainbow(5),ylim=c(0.30,50000),ylab ="Casos reportados", xlab ="Country",log='y')
#l) Concluir.

