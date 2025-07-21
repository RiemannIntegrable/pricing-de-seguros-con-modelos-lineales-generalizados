library(dplyr); library(ggplot2); library(GGally); library(ggpubr)
library(cowplot); library(ggmosaic); library(tweedie); library(statmod)

produccion=read.table("base3.csv",header=TRUE,sep=",",encoding="latin1")
str(produccion); attach(produccion); dim(produccion)
produccion$Modelo=factor(produccion$Modelo)
produccion$Sexo_Aseg=factor(produccion$Sexo_Aseg)
produccion$Edadre=factor(produccion$Edadre)
produccion$agrupadare=factor(produccion$agrupadare)
produccion$colorre=factor(produccion$colorre)
base31=produccion %>%
filter(Hasta1<as.Date("2013-01-01") | Desde1>as.Date("2012-12-31")) %>%
mutate(Expo=as.numeric(as.Date(Hasta1)-as.Date(Desde1)))
base321=produccion %>%
filter(Desde1<as.Date("2013-01-01") & Hasta1>as.Date("2012-12-31")) %>%
mutate(Expo1=as.numeric(as.Date("2012-12-31")-as.Date(Desde1)),
Expo2=as.numeric(as.Date(Hasta1)-as.Date("2012-12-31"))) %>%
mutate(Pagos1=Pagos*Expo1/(Expo1+Expo2),Pagos2=Pagos*Expo2/(Expo1+Expo2))
base322=base321
base321=base321 %>%
select(Modelo,Sexo_Aseg,Edadre,agrupadare,colorre,Expo=Expo1,Pagos=Pagos1)
base322=base322 %>%
select(Modelo,Sexo_Aseg,Edadre,agrupadare,colorre,Expo=Expo2,Pagos=Pagos2)
base31 =base31 %>%
select(Modelo,Sexo_Aseg,Edadre,agrupadare,colorre,Expo,Pagos)
p1 = bind_rows(base31,base321,base322); dim(p1)
summary(droplevels(p1))
p1 <- within(p1,{reclamacion <- ifelse(Pagos>0,1,0)})
p1$reclamacion=factor(p1$reclamacion,levels=c("0","1"),labels=c("No","Sí"))
M ="Modelo del vehículo"
S = "Género del tomador de la póliza"
E = "Edad del tomador de la póliza"
A = "Agrupación del vehículo"
C = "Color del vehículo"
R = "Reclamación"
N = "Número de pólizas"
funcion <- function(var1,var2){
p <- ggplot(p1, aes(x = var1)) + geom_bar(width=0.70)+xlab(var2)+
ylab(N)+theme_bw()
return(p)
}
pl1 <- funcion(p1$Modelo,M)
pl2 <- funcion(p1$agrupadare,A)
pl3 <- funcion(p1$Sexo_Aseg,S)
pl4 <- funcion(p1$reclamacion,R)
pl5 <- funcion(p1$Edadre,E)
pl6 <- funcion(p1$colorre,C)
plot_grid(pl1,pl2,pl3,pl4,nrow = 2)
plot_grid(pl5,pl6,nrow = 2)
p1$cociente=p1$Pagos/p1$Expo
p2 <- subset(p1,Pagos>0)
Monto <- "Monto pagado/Exposición (en días)"
p17<-ggplot(p1, aes(x=cociente)) + geom_histogram(color="black",
fill="white",bins=100)+ylab(N)+xlab(Monto)+theme_bw()
p18<-ggplot(p2, aes(x=cociente)) + geom_histogram(color="black",
fill="white",bins=100)+geom_vline(aes(xintercept=mean(cociente)),
color="blue", linetype="dashed", size=1)+ylab(N)+xlab(Monto)+theme_bw()
plot_grid(p17,p18,nrow = 1)
##
#salida <- tweedie.profile(p1$cociente ~ 1, p.vec=seq(1.1, 1.7, by=0.1),
# do.ci=TRUE )
#salida$p.max
potencia=1.6
contrasts(p1$Sexo_Aseg)=contr.treatment(levels(p1$Sexo_Aseg),base=2,
contrasts=TRUE,sparse=FALSE)
contrasts(p1$Edadre)=contr.treatment(levels(p1$Edadre),base=2,
contrasts=TRUE,sparse=FALSE)
mod5=glm(Pagos ~ Modelo+Sexo_Aseg+Edadre+agrupadare+offset(log(Expo)),
family=tweedie(var.power=potencia,link.power=0),data=p1)
options(scipen=999)
summary(mod5); exp(mod5$coef)