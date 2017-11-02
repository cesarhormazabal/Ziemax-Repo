library(readxl)
Accesibilidad<-read_excel("Accesibilidad.xls")
View(Accesibilidad)
library(readr)
UbicacionGeografica2016 <- read_csv("UbicacionGeografica2016.csv")
View(UbicacionGeografica2016)
library(dplyr)
Colegios_Puntaje_2012 <- read_excel("Colegios.xls", 
                       sheet = "SIMCE IIM 2012 Leng")
Colegios_Mat_Total <- read_excel("Colegios.xls")

Colegios_Lideres <- read_excel("base datos redes final.xlsx")

Colegios_Redes <- read_excel("base datos redes final.xlsx", 
                                     sheet = "Redes")

Accesibilidad%>% mutate(Comuna=toupper(Comuna))%>%mutate(Comuna=chartr("ÁÉÍÓÚ","AEIOU",Comuna))->Accesibilidad 
#Accesibilidad%>%group_by(Comuna)%>%summarise(conteo=n())%>%filter(conteo>1)
UbicacionGeografica2016%>%mutate(NOM_COM_RBD=chartr("ÁÉÍÓÚ","AEIOU",NOM_COM_RBD))%>%left_join(Accesibilidad,by=c("NOM_COM_RBD" = "Comuna"))->UbicacionGeografica2016

Colegios_Puntaje_2012%>%select(RBD,PROM_LECT,PROM_MAT)->Colegios_Puntaje_2012;View(Colegios_Puntaje_2012)
Colegios_Mat_Total[,c(1,3,5,17,18)]->Colegios_Mat_Total;View(Colegios_Mat_Total)

UbicacionGeografica2016%>%left_join(Colegios_Puntaje_2012,by="RBD")%>%left_join(Colegios_Mat_Total,by="RBD")->Ubicacion_Actualizada

library(ggplot2)
View(Ubicacion_Actualizada)
names(Ubicacion_Actualizada)

names(Ubicacion_Actualizada)%>%toupper()->names_data;
gsub(" ", "",chartr("ÁÉÍÓÚ","AEIOU",names_data) ,fixed = TRUE)->names(Ubicacion_Actualizada)
#names(Ubicacion_Actualizada)
names(Ubicacion_Actualizada)[50]<-"IMPLEMENTADO"

?select(Ubicacion_Actualizada, RBD,REGION,MAT_TOTAL,PROM_MAT,PROM_LECT,IMPLEMENTADO)

Ubicacion_Actualizada%>%group_by(NOM_COM_RBD)%>%summarise(Matricula_Comuna=sum(MAT_TOTAL,na.rm=TRUE))->Mat_Comuna

Ubicacion_Actualizada%>%group_by(RUT_SOSTENEDOR)%>%summarise(Matricula_Sostenedor=sum(MAT_TOTAL,na.rm=TRUE))->Mat_Sostenedor

Ubicacion_Actualizada%>%left_join(Colegios_Redes,by="RBD")%>%mutate(Grupo=ifelse(is.na(Grupo),RUT_SOSTENEDOR,Grupo))%>%group_by(Grupo)%>%summarise(Matricula_Grupo=sum(MAT_TOTAL,na.rm=TRUE))->Mat_Grupo
Ubicacion_Actualizada%>%left_join(Colegios_Redes,by="RBD")%>%mutate(Grupo=ifelse(is.na(Grupo),RUT_SOSTENEDOR,Grupo))->Ubicacion_Actualizada
Ubicacion_Actualizada%>%left_join(filter(Mat_Comuna,!is.na(Matricula_Comuna)),by="NOM_COM_RBD")%>%left_join(Mat_Grupo,by="Grupo")->Ubicacion_Actualizada;Ubicacion_Actualizada%>%View()    


################# Cluster ##############################################
names(Ubicacion_Actualizada)

select(
  Ubicacion_Actualizada,
  RBD,
  NOM_COM_RBD,
  MAT_TOTAL,
  Matricula_Comuna,
  Matricula_Grupo,
  GRADOACCESIBILIDAD,
  PROM_LECT,
  PROM_MAT
)%>%filter(!is.na(MAT_TOTAL)) -> Result_1

names(Result_1)


library(data.table)

set.seed(20)
Result_1<-data.table(Result_1)



?kmeans
comunas<-levels(as.factor(Result_1$NOM_COM_RBD))

tablafinal<-data.frame(RBD=character(),
                       NOM_COM_RBD=character(), 
                       MAT_TOTAL=numeric(), 
                       Matricula_Comuna=numeric(),
                       Matricula_Grupo=numeric(),
                       GRADOACCESIBILIDAD=numeric(),
                       PROM_LECT=numeric(),
                       PROM_MAT=numeric(),
                       cluster=numeric(),
                       stringsAsFactors=FALSE) 
colnames(tablafinal)<-names(Result_1)

for(comuna in comunas)
{
  Result_1 %>% filter(comunas[1] == NOM_COM_RBD)-> Result_2
  scale<-scale(Result_2[,c("Matricula_Grupo",
                           "MAT_TOTAL",
                           "PROM_LECT",
                           "PROM_MAT")])
  cbind(Result_2[,"GRADOACCESIBILIDAD"],scale)->Result_2
  Result_2[is.na(Result_2[,"PROM_LECT"]),"PROM_LECT"]<- median(Result_2[,"PROM_LECT"],na.rm=TRUE)
  Result_2[is.na(Result_2[,"PROM_MAT"]),"PROM_MAT"]<- median(Result_2[,"PROM_MAT"],na.rm=TRUE)
  
  
  Kmeans_comuna <- kmeans(Result_2[,c("Matricula_Grupo",
                                      "MAT_TOTAL",
                                      "PROM_LECT",
                                      "PROM_MAT",
                                      "GRADOACCESIBILIDAD")],centers=5)
  Result_2$cluster <-Kmeans_comuna$cluster
  Result_1$cluster <-Result_2$cluster
}



