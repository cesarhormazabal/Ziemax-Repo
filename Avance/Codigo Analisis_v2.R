#################### Librerías #############################################
rm(list=ls())
pacman::p_load(readxl,readr,dplyr,ggplot2)
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
View(Colegios_Mat_Total)

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
View(Colegios_Ley_SEP)


Dim_Servicios <- read_excel("Data_Ejemplo.xlsx", 
                            sheet = "DIM Servicios", col_types = c("text", 
                                                                   "text", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric"))

Dim_asesor <- read_excel("Data_Ejemplo.xlsx", 
                         sheet = "Dim Asesor", col_types = c("text", 
                                                             "numeric", "numeric"))


##################### Limpieza de Bases ################################################


#### Unir Bases ###
## PUNTAJE SIMCE
# summary(UbicacionGeografica2016)
# summary(Colegios_Ley_SEP)
Colegios_Mat_Total$RBD <- as.numeric(Colegios_Mat_Total$RBD)
Colegios_Ley_SEP$RBD <- as.numeric(Colegios_Ley_SEP$RBD)
Colegios_Ley_SEP$NUMERO_BENEFICIADOS <- as.numeric(Colegios_Ley_SEP$NUMERO_BENEFICIADOS)
Colegios_Ley_SEP$MONTO_SEP <- as.numeric(Colegios_Ley_SEP$MONTO_SEP)

colnames(Colegios_Ley_SEP) <-
  c("RBD", "NUMERO_BENEFICIADOS", "MONTO_SEP")


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
           )->Ubicacion_Actualizada; View(Ubicacion_Actualizada)

names(Ubicacion_Actualizada)%>%toupper()->names_data;
gsub(" ", "",chartr("ÁÉÍÓÚ","AEIOU",names_data) ,fixed = TRUE)->names(Ubicacion_Actualizada)


############## Generación de variables agrupadas ####################

ungroup(Ubicacion_Actualizada)->Ubicacion_Actualizada
Ubicacion_Actualizada$NOM_COM_RBD<-as.factor(Ubicacion_Actualizada$NOM_COM_RBD)
Ubicacion_Actualizada$MAT_TOTAL<-as.numeric(Ubicacion_Actualizada$MAT_TOTAL)
  
##Matricula Por Comuna ###
Ubicacion_Actualizada %>%
  group_by(NOM_COM_RBD) %>%
  summarise(Matricula_Comuna = sum(MAT_TOTAL, na.rm = TRUE)) -> Mat_Comuna;View(Mat_Comuna)

## Matrícula Por Sostenedor ##
Ubicacion_Actualizada%>%
  group_by(RUT_SOSTENEDOR)%>%
  summarise(Matricula_Sostenedor=sum(MAT_TOTAL,na.rm=TRUE))->Mat_Sostenedor;View(Mat_Sostenedor)

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