#################### Librerías #############################################
rm(list=ls())
pacman::p_load(readxl,readr,dplyr,ggplot2,Rglpk,tidyverse,cluster)
source("Funciones.R")

#################### Archivos #############################################

read_excel("Accesibilidad.xls")%>% 
  mutate(COMUNA=toupper(COMUNA))%>%
  mutate(COMUNA=chartr("ÁÉÍÓÚ","AEIOU",COMUNA))->Accesibilidad

read_csv("UbicacionGeografica2016.csv")%>%filter(ENS_01==110 | ENS_01==310 |
                                                   ENS_02==110 | ENS_02==310 |
                                                   ENS_03==110 | ENS_03==310 |
                                                   ENS_04==110 | ENS_04==310 |
                                                   ENS_05==110 | ENS_05==310 |
                                                   ENS_06==110 | ENS_06==310 |
                                                   ENS_07==110 | ENS_07==310 |
                                                   ENS_08==110 | ENS_08==310 |
                                                   ENS_09==110 | ENS_09==310 
                                                 )%>% filter(ESTADO_ESTAB==1)%>%
  mutate(NOM_COM_RBD=chartr("ÁÉÍÓÚ","AEIOU",NOM_COM_RBD))%>%
  left_join(Accesibilidad,by=c("NOM_COM_RBD" = "COMUNA"))%>%
   select(RBD,NOM_RBD,
         MRUN,RUT_SOSTENEDOR,
         P_JURIDICA,COD_REG_RBD,
         COD_COM_RBD,NOM_COM_RBD,
         COD_DEPE, COD_DEPE2,
         RURAL_RBD,CONVENIO_PIE,
         LATITUD,LONGITUD,
         ORI_RELIGIOSA,PAGO_MATRICULA,
         PAGO_MENSUAL,GRADO_ACCESIBILIDAD,
         NOMBRE_ACCESIBILIDAD,NECESIDAD_AVION)->UbicacionGeografica2016

Colegios_Puntaje_2012 <-
  read_excel("Colegios.xls", sheet = "SIMCE IIM 2012 Leng")


read_excel("VistaResumenMatriculaEE2016.xlsx") %>% 
  select(RBD, COD_DEPE, COD_DEPE2, MAT_TOTAL) ->
  Colegios_Mat_Total
# View(Colegios_Mat_Total)

Colegios_Lideres <- read_excel("base datos redes final.xlsx")

Colegios_Redes <- read_excel("base datos redes final.xlsx", 
                                     sheet = "Redes")

## Regla de tres para calcular la SEP ###
## Valor USE 25385 ###
## Tomando 0.70575 USE EMERGENTE o RECUPERACION ###
##         1.4115  USE AUTONOMO ###

read_excel("VistaresumenprioritariospreferentesybeneficiariosSEPporEE2016.xlsx") %>%
  filter(CONVENIO_SEP == "1") %>% mutate(
    monto = ifelse(
      CLASIFICACION_SEP == "AUTONOMO",
      1.4115 * 25385 * as.numeric(N_BEN),
      0.70575 * 25385 * as.numeric(N_BEN)
    )
  ) %>% select(RBD, N_BEN, monto) -> Colegios_Ley_SEP;

Dim_Servicios <- read_excel("C:/Users/Stef/Desktop/Proyectos/ZIEMAX/Proyecto Ziemax 2/Data_Ejemplo.xlsx", 
                            sheet = "DIM Servicios", col_types = c("text", 
                                                                   "text", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric"))
colnames(Dim_Servicios) <-
  c("SERVICIO", "TIPO_SERVICIO", "PP","PC",
    "COSTO_TRASLADO","HORAS_SERVICIO","EXPERTISE","COSTO_HORA","BENEFICIO_COLEGIO",
    "JERARQUIA_SERVICIO","REPETIBLE_HASTA","NIVEL","CURSO",
    "COSTO_ZIE_HH_INFOR","COSTO_ZIE_HH","EXPERTISE2","COSTO_EXP_ZIEMAX","LIMITE_CANTIDAD",
    "EXPERTISE_INFOR")

Dim_asesor <- read_excel("C:/Users/Stef/Desktop/Proyectos/ZIEMAX/Proyecto Ziemax 2/Data_Ejemplo.xlsx", 
                         sheet = "Dim Asesor", col_types = c("numeric", 
                                                             "numeric", "numeric", "text", "numeric", 
                                                             "numeric"))
colnames(Dim_asesor) <-
  c("ASESOR", "EXPERTISE", "COSTO_HORA","NOMBRE","COSTO_ZIEMAX","SUELDO")


##################### Limpieza de Bases ################################################


#### Unir Bases ###
## PUNTAJE SIMCE
# summary(UbicacionGeografica2016)
# summary(Colegios_Ley_SEP)
colnames(Colegios_Ley_SEP) <-
  c("RBD", "NUMERO_BENEFICIADOS", "MONTO_SEP")

Colegios_Mat_Total$RBD <- as.numeric(Colegios_Mat_Total$RBD)
Colegios_Ley_SEP$RBD <- as.numeric(Colegios_Ley_SEP$RBD)
Colegios_Ley_SEP$NUMERO_BENEFICIADOS <- as.numeric(Colegios_Ley_SEP$NUMERO_BENEFICIADOS)
Colegios_Ley_SEP$MONTO_SEP <- as.numeric(Colegios_Ley_SEP$MONTO_SEP)




UbicacionGeografica2016%>%
  left_join(Colegios_Puntaje_2012,by="RBD")%>%   # DATA PUNTAJE
  left_join(Colegios_Mat_Total,by="RBD")%>%      # DATA MATRICULAS
  left_join(Colegios_Lideres,by="RBD")%>%        # DATA COLEGIOS LIDERES
  left_join(Colegios_Redes,by="RBD")%>%          # DATA REDES DE COLEGIOS
  left_join(Colegios_Ley_SEP,by="RBD")%>%        # COLEGIOS LEY SEP
  mutate(
    MONTO_SEP = ifelse(is.na(MONTO_SEP), 0, MONTO_SEP),
    NUMERO_BENEFICIADOS = ifelse(is.na(NUMERO_BENEFICIADOS), 0, NUMERO_BENEFICIADOS),
    RED_DE_ESCUELAS_LIDERES = ifelse(is.na(RED_DE_ESCUELAS_LIDERES), 0, RED_DE_ESCUELAS_LIDERES)
  )%>%
  rowwise() %>% mutate(DEPENDENCIA = switch_R(
    condiciones = c(
      COD_DEPE2.y == 1,
      COD_DEPE2.y ==2,
      COD_DEPE2.y == 3,
      COD_DEPE2.y ==4),
    consecuencias = c("Municipal",
                      "Particular Subvencionado",
                      "Particular Pagado",
                      "Administración Delegada",
                      NA)))%>%
  mutate(RURAL_RBD = switch_R(
    condiciones = c(
      RURAL_RBD == 1,
      RURAL_RBD ==0),
    consecuencias = c("RURAL",
                      "URBANO",
                      NA)))%>%
  mutate(ORI_RELIGIOSA = switch_R(
    condiciones = c(
      ORI_RELIGIOSA == 1,
      ORI_RELIGIOSA == 2,
      ORI_RELIGIOSA == 3,
      ORI_RELIGIOSA == 4,
      ORI_RELIGIOSA == 5,
      ORI_RELIGIOSA == 6,
      ORI_RELIGIOSA == 7,
      ORI_RELIGIOSA == 8,
      ORI_RELIGIOSA == 9),
    consecuencias = c("Laica",
                      "Católica",
                      "Evangélica",
                      "Musulmana",
                      "Judía",
                      "Budista",
                      "Otro",
                      NA,
                      NA,
                      NA)))%>%
    select(RBD,NOM_RBD,
           MRUN,RUT_SOSTENEDOR,
           COD_REG_RBD,COD_COM_RBD,NOM_COM_RBD,
           DEPENDENCIA,
           RURAL_RBD,CONVENIO_PIE,
           LATITUD,LONGITUD,
           ORI_RELIGIOSA,PAGO_MATRICULA,
           PAGO_MENSUAL,NOMBRE_ACCESIBILIDAD,
           NECESIDAD_AVION,
           PROM_LECT,PROM_MAT,           # PROMEDIO NOTAS SIMCE 2012
           MAT_TOTAL,                    # MATRICULAS TOTALES
           RED_DE_ESCUELAS_LIDERES,      # COLEGIOS LIDERES
           Grupo,                        # RED COLEGIOS
           NUMERO_BENEFICIADOS,MONTO_SEP # COLEGIOS LEY SEP
           )->Ubicacion_Actualizada; #View(Ubicacion_Actualizada)

names(Ubicacion_Actualizada)%>%toupper()->names_data;
gsub(" ", "",chartr("ÁÉÍÓÚ","AEIOU",names_data) ,fixed = TRUE)->names(Ubicacion_Actualizada)


############## Generación de variables agrupadas ####################

ungroup(Ubicacion_Actualizada)->Ubicacion_Actualizada
Ubicacion_Actualizada$NOM_COM_RBD<-as.factor(Ubicacion_Actualizada$NOM_COM_RBD)
Ubicacion_Actualizada$MAT_TOTAL<-as.numeric(Ubicacion_Actualizada$MAT_TOTAL)
  
##Matricula Por Comuna ###
Ubicacion_Actualizada %>%
  group_by(NOM_COM_RBD) %>%
  summarise(Matricula_Comuna = sum(MAT_TOTAL, na.rm = TRUE)) -> Mat_Comuna;#View(Mat_Comuna)

## Matrícula Por Sostenedor ##
Ubicacion_Actualizada%>%
  group_by(RUT_SOSTENEDOR)%>%
  summarise(Matricula_Sostenedor=sum(MAT_TOTAL,na.rm=TRUE))->Mat_Sostenedor;#View(Mat_Sostenedor)

## Agregar Matricula por Sostenedor a Base Principal
Ubicacion_Actualizada%>%
  left_join(Colegios_Redes,by="RBD")%>%
  mutate(GRUPO=ifelse(is.na(GRUPO),RUT_SOSTENEDOR,GRUPO))%>%
  group_by(GRUPO)%>%
  summarise(Matricula_Grupo=sum(MAT_TOTAL,na.rm=TRUE))->Mat_Grupo


Ubicacion_Actualizada%>%
  left_join(Colegios_Redes,by="RBD")%>%
  mutate(GRUPO=ifelse(is.na(GRUPO),RUT_SOSTENEDOR,GRUPO))->Ubicacion_Actualizada

Ubicacion_Actualizada%>%
  left_join(filter(Mat_Comuna,!is.na(Matricula_Comuna)),by="NOM_COM_RBD")%>%
  left_join(Mat_Grupo,by="GRUPO")->Ubicacion_Actualizada;
Ubicacion_Actualizada%>%View()    

#### Agregar Grado Accesibilidad y necesidad de avion

### Agregar Colegios ley  SEP

Ubicacion_Actualizada%>%
  left_join(Colegios_Ley_SEP,by="RBD")%>%View()  
  # left_join(Mat_Grupo,by="Grupo")->Ubicacion_Actualizada;
Ubicacion_Actualizada%>%View()    

########################## Selección de variables a utilizar ###################################
names(Ubicacion_Actualizada)

# Pago Matricula
Ubicacion_Actualizada%>%rowwise()%>%mutate(PAGO_MATRICULA_2=switch_R(condiciones=c(PAGO_MATRICULA=="$1.000 A $10.000",
                                                                                   PAGO_MATRICULA=="$10.001 A $25.000",
                                                                                   PAGO_MATRICULA=="$25.001 A $50.000",
                                                                                   PAGO_MATRICULA=="$50.001 A $100.000",
                                                                                   PAGO_MATRICULA=="GRATUITO",
                                                                                   PAGO_MATRICULA=="MAS DE $100.000",
                                                                                   PAGO_MATRICULA=="SIN INFORMACION")
                                                                     ,consecuencias=c(5000,
                                                                                      17500,
                                                                                      37500,
                                                                                      75000,
                                                                                      0,
                                                                                      100000,
                                                                                      NA,
                                                                                      NA)
))->Ubicacion_Actualizada;Ubicacion_Actualizada%>%summary()

## Pago Mensual
Ubicacion_Actualizada%>%rowwise()%>%mutate(PAGO_MENSUAL_2=switch_R(condiciones=c(PAGO_MENSUAL=="$1.000 A $10.000",
                                                                                 PAGO_MENSUAL=="$10.001 A $25.000",
                                                                                 PAGO_MENSUAL=="$25.001 A $50.000",
                                                                                 PAGO_MENSUAL=="$50.001 A $100.000",
                                                                                 PAGO_MENSUAL=="GRATUITO",
                                                                                 PAGO_MENSUAL=="MAS DE $100.000",
                                                                                 PAGO_MENSUAL=="SIN INFORMACION")
                                                                     ,consecuencias=c(5000,
                                                                                      17500,
                                                                                      37500,
                                                                                      75000,
                                                                                      0,
                                                                                      100000,
                                                                                      NA,
                                                                                      NA)
))->Ubicacion_Actualizada;Ubicacion_Actualizada%>%summary()

# Pago Distancia

Ubicacion_Actualizada$NOMBRE_ACCESIBILIDAD<-levels(as.factor(Ubicacion_Actualizada$NOMBRE_ACCESIBILIDAD))
summary(Ubicacion_Actualizada$NOMBRE_ACCESIBILIDAD)
# Ubicacion_Actualizada%>%rowwise()%>%mutate(NOMBRE_ACCESIBILIDAD=switch_R(condiciones=c(NOMBRE_ACCESIBILIDAD=="Accesible",
#                                                                                        NOMBRE_ACCESIBILIDAD=="Medianamente",
#                                                                                        NOMBRE_ACCESIBILIDAD=="No Accesible")
#                                                                    ,consecuencias=c(4000,
#                                                                                     15000,
#                                                                                     0,
#                                                                                     0)
# ))->Ubicacion_Actualizada_TMP;





length(Ubicacion_Actualizada_TMP$RBD)
summary(Ubicacion_Actualizada_TMP)

############################# Creacion Primeros Cluster de Servicios ##############
?kmeans()
Ubicacion_Actualizada$NOMBRE_ACCESIBILIDAD<-as.numeric(Ubicacion_Actualizada$PAGO_MATRICULA)
Ubicacion_Actualizada$DEPENDENCIA<-as.factor(Ubicacion_Actualizada$DEPENDENCIA)


set.seed(20)
kmeans_Colegios<- kmeans(scale(Ubicacion_Actualizada[, c("Matricula_Grupo","MAT_TOTAL","MONTO_SEP","NUMERO_BENEFICIADOS")]),5,nstart = 20)
Ubicacion_Actualizada$cluster <-factor(kmeans_Colegios$cluster)


summary(filter(Ubicacion_Actualizada$NOMBRE_ACCESIBILIDAD,cluster==1))
kmeans_Colegios$size
write.csv(Ubicacion_Actualizada,"Ubicacion_Actualizada.csv")
ggplot(Ubicacion_Actualizada, aes(x = MAT_TOTAL, y = Matricula_Grupo)) +
  geom_point(
    mapping = aes(
      x = MAT_TOTAL,
      y = Matricula_Grupo,
      color = Ubicacion_Actualizada$cluster
    )
    ##  ,   position = "jitter"
  ) +
  labs(
    title = "Cluster de Colegios",
    #subtitle = "Basado RFM y Kmeans",
    x = "Cantidad de Alumnos por Colegio",
    y = "Matriculas por Grupo",
    color = "Cluster Kmeans"
  ) + 
  scale_color_manual(values = c("#9370DB", 
                                "#FFFF00", 
                                "#FAAC58", 
                                "#40FF00", 
                                "#FA5882",
                                "#013ADF"))





################## Creación primeros grupos de servicio ############





Ubicacion_Actualizada%>%mutate(PAGO_COLEGIO=(PAGO_MENSUAL_2*10+PAGO_MATRICULA_2)*MAT_TOTAL)%>%group_by(cluster,NOMBRE_ACCESIBILIDAD,RED_DE_ESCUELAS_LIDERES,DEPENDENCIA)%>%summarise(Monto_Escuela=sum(PAGO_COLEGIO+MONTO_SEP,na.rm=TRUE), MATRICULA_TOTAL=sum(MAT_TOTAL,na.rm=TRUE),MONTO_ALUMNO=sum(PAGO_COLEGIO+MONTO_SEP,na.rm=TRUE)/sum(MAT_TOTAL,na.rm=TRUE),N_TOTAL_COLEGIOS=n())%>%ungroup()->GRUPOS_COLEGIOS

valores_servicios<-cbind(rep(0,nrow(GRUPOS_COLEGIOS)*nrow(Dim_Servicios)),rep(1:19,nrow(GRUPOS_COLEGIOS)),rep(1:61,nrow(Dim_Servicios)))
colnames(valores_servicios)<-c("Valor","Servicios","Colegios")
k=1
for(i in 1:nrow(Dim_Servicios))
{
  for(j in 1:nrow(GRUPOS_COLEGIOS))
  {
    valores_servicios[k] = Dim_Servicios$PP[i] * GRUPOS_COLEGIOS$MATRICULA_TOTAL[j] +
      Dim_Servicios$PC[i] * GRUPOS_COLEGIOS$N_TOTAL_COLEGIOS[j] -
      Dim_Servicios$COSTO_TRASLADO[i] * switch_R(
        condiciones = c(
          GRUPOS_COLEGIOS$NOMBRE_ACCESIBILIDAD[j] == "Accesible",
          GRUPOS_COLEGIOS$NOMBRE_ACCESIBILIDAD[j] ==
            "Medianamente",
          GRUPOS_COLEGIOS$NOMBRE_ACCESIBILIDAD[j] ==
            "No Accesible"
        )
        ,
        consecuencias = c(4000,
                          100000,
                          2000000,
                          0)
      )*GRUPOS_COLEGIOS$N_TOTAL_COLEGIOS[j]
    -(Dim_Servicios$COSTO_ZIE_HH[i]*Dim_Servicios$COSTO_EXP_ZIEMAX[i]+
        Dim_Servicios$COSTO_ZIE_HH_INFOR[i]*Dim_Servicios$EXPERTISE_INFOR[i])*GRUPOS_COLEGIOS$N_TOTAL_COLEGIOS[j]
    
    k=k+1
  }
}

View(valores_servicios)
Dim_Servicios%>%View()

write.csv(GRUPOS_COLEGIOS,"GRUPOS_COLEGIOS.csv")
write.csv(valores_servicios,"valores_servicios.csv")
###################################################################






Colegios_muestra<-sample(1:nrow(Ubicacion_Actualizada_TMP),.20*nrow(Ubicacion_Actualizada_TMP))
Ubicacion_Actualizada_TMP<-Ubicacion_Actualizada_TMP[Colegios_muestra,]

Servicios_muestra<-sample(1:nrow(Dim_Servicios),.40*nrow(Dim_Servicios))
Dim_Servicios_Muestra<-Dim_Servicios[Servicios_muestra,]

####### Función Objetivo ################################

### Vectores de Costo Colegios

Precio_PC_Colegio<-matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD),ncol=length(Dim_Servicios_Muestra$SERVICIO))
Precio_PP_Colegio<-matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD),ncol=length(Dim_Servicios_Muestra$SERVICIO))
Precio_Traslado  <-matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD),ncol=length(Dim_Servicios_Muestra$SERVICIO))

### Vectores de Costo Ziemax

Costo_HHProfesor_Ziemax   <-matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD),ncol=length(Dim_Servicios_Muestra$SERVICIO))
Costo_HHInformatica_Ziemax<-matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD),ncol=length(Dim_Servicios_Muestra$SERVICIO))
Costo_Traslado_Ziemax     <-matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD),ncol=length(Dim_Servicios_Muestra$SERVICIO))


### Def Funcion Objetivo
## Max = Precio - Costo

Total=length(Dim_Servicios_Muestra$SERVICIO)*length(Ubicacion_Actualizada_TMP$RBD)
FunObj_Matriz  <-matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD),ncol=length(Dim_Servicios_Muestra$SERVICIO))
Fun_Obj <-matrix(nrow=1,ncol=length(Dim_Servicios_Muestra$SERVICIO)*length(Ubicacion_Actualizada_TMP$RBD)) 


for(i in 1:length(Ubicacion_Actualizada_TMP$RBD)) 
  { for(j in 1:length(Dim_Servicios_Muestra$SERVICIO))
    {
    
    ### Vectores de Costo para colegio
    
    Precio_PC_Colegio[i,j]= Dim_Servicios_Muestra$PC[j]*
                              if (Dim_Servicios_Muestra$TIPO_SERVICIO[j]=="presencial") 
                                {
                                  ifelse(Ubicacion_Actualizada_TMP$NOMBRE_ACCESIBILIDAD[i]==0,0,1)
                                } else {1}
      
    Precio_PP_Colegio[i,j]= Dim_Servicios_Muestra$PP[j]*
                          ifelse(is.na(Dim_Servicios_Muestra$COSTO_HORA[j]),0,Dim_Servicios_Muestra$COSTO_HORA[j])*
                          ifelse(is.na(Dim_Servicios_Muestra$HORAS_SERVICIO[j]),0,Dim_Servicios_Muestra$HORAS_SERVICIO[j])*
                          (Ubicacion_Actualizada_TMP$MAT_TOTAL[i])*0.1*
                          if (Dim_Servicios_Muestra$TIPO_SERVICIO[j]=="presencial") 
                            {
                              ifelse(Ubicacion_Actualizada_TMP$NOMBRE_ACCESIBILIDAD[i]==0,0,1)
                            } else {1}
    
    ## Solo la mitad del precio de traslado de pasa al cliente
    Precio_Traslado[i,j]=Dim_Servicios_Muestra$COSTO_TRASLADO[j]*
                        (Ubicacion_Actualizada_TMP$NOMBRE_ACCESIBILIDAD[i])/2
    
    ### Vectores de Costo para Ziemax
    
    Costo_HHInformatica_Ziemax[i,j]=  Dim_Servicios_Muestra$COSTO_ZIE_HH_INFOR[j]*Dim_Servicios_Muestra$EXPERTISE_INFOR[j]
    
    Costo_HHProfesor_Ziemax[i,j]   =  Dim_Servicios_Muestra$COSTO_ZIE_HH[j]*Dim_Servicios_Muestra$COSTO_EXP_ZIEMAX[j]*                           
                                         if (Dim_Servicios_Muestra$TIPO_SERVICIO[j]=="presencial") 
                                           {
                                            ifelse(Ubicacion_Actualizada_TMP$NOMBRE_ACCESIBILIDAD[i]==0,0,1)
                                           } else {1}
    
    Costo_Traslado_Ziemax[i,j]     =  Dim_Servicios_Muestra$COSTO_TRASLADO[j]*
                                      Ubicacion_Actualizada_TMP$NOMBRE_ACCESIBILIDAD[i]

    } 
  }



### Función Objetivo

for(i in 1:length(Ubicacion_Actualizada_TMP$RBD)) 
  { for(j in 1:length(Dim_Servicios_Muestra$SERVICIO))
      { 
            FunObj_Matriz[i,j]=Precio_PC_Colegio[i,j]+Precio_PP_Colegio[i,j]+Precio_Traslado[i,j]-
                               Costo_HHProfesor_Ziemax[i,j]-Costo_HHInformatica_Ziemax[i,j]-Costo_Traslado_Ziemax[i,j]
            
      } 
}

for(i in 1:length(Ubicacion_Actualizada_TMP$RBD)) 
  { for(j in 1:length(Dim_Servicios_Muestra$SERVICIO))
      { 
      for(k in 1:Total)
        {
          Fun_Obj[k]=FunObj_Matriz[i,j]
          } 
      } 
  }

View(Fun_Obj)

############ Restricciones ###########################################################

##1. No se pueden repetir los servicios: No puede haber dos servicios iguales
Matriz_Unicidad<-matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD)*length(Dim_Servicios_Muestra$SERVICIO),
                        ncol=length(Ubicacion_Actualizada_TMP$RBD)*length(Dim_Servicios_Muestra$SERVICIO))

##2. Cada Colegio debe tener por lo menos un servicio
Matriz_existe<--matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD)*length(Dim_Servicios_Muestra$SERVICIO),
                       ncol=length(Ubicacion_Actualizada_TMP$RBD)*length(Dim_Servicios_Muestra$SERVICIO))

##3. Algunas combinaciones no pueden darse (relacionado a la jerarquia)

Matriz_Limite_Combi<--matrix(nrow=length(Ubicacion_Actualizada_TMP$RBD)*length(Dim_Servicios_Muestra$SERVICIO),
                             ncol=length(Ubicacion_Actualizada_TMP$RBD)*length(Dim_Servicios_Muestra$SERVICIO))


#### Definiciones de Restricciones

Matriz_Unicidad<-diag(rep(1,length(Ubicacion_Actualizada_TMP$RBD)*length(Dim_Servicios_Muestra$SERVICIO)))


for(i in 1:length(Ubicacion_Actualizada_TMP$RBD))

  sum()
  Matriz_existe[i,j]

##4. Variables enteras Entera
tipos<- matrix(ncol=1,nrow=(length(Ubicacion_Actualizada_TMP$RBD)+
                            length(Ubicacion_Actualizada_TMP$RBD)+
                            length(Ubicacion_Actualizada_TMP$RBD)))
for(k in 1:Total) { tipos[k]="I"} 

## direccion
#direccion<- matrix(ncol=1,nrow=length(Dim_Servicios_Muestra$SERVICIO)*length(Ubicacion_Actualizada_TMP$RBD))


############ Solucion ################################################################

Resultado_Ziemax <-
  Rglpk_solve_LP(
    Fun_Obj,                   # Funcion Objetivo
    restricciones,             # Restricciones
    direccion,                 # Desigualdad
    LadoDerecho_restricciones, # Lado derecho de restricciones
    types = tipos,             # Tipo de variables (entera o continua)
    max = TRUE                  # Maximizacion o minimizacion
  )

