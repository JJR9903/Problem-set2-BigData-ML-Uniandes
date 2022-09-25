## Autores: Juan José Rincón , Juan Andres Ospina, Juanita Chacón 
## Descripción: Desarrollo 2 problem set /Big Data and Machine Leanring for applied economics
## Universidad de los Andes 2022-2
## Creation Date: 14/09/2022
####################################

#### setting the work space 

rm(list=ls())

# este setwd por ahora no esta funcionando, toca que cada uno lo haga independiente mientras logro que funcione. att: Juan Jose
dir_set <- function(){
  if(Sys.info()["user"]=="JuanJose"){
    setwd("/Users/JuanJose/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Uniandes/9 Semestre - 1 PEG/Big Data/Problems Set/Problem-set2-BigData-ML-Uniandes")
  }
  else if(Sys.info()["user"]=="PC-PORTATIL"){
    setwd("C:/Users/PC-PORTATIL/OneDrive/Documentos/GitHub/Problem-set2-BigData-ML-Uniandes")
  }
  else{
    setwd("C:/Users/Usuario/Documents/GitHub/Problem-set2-BigData-ML-Uniandes")
  }
}

dir_set()

pacman:: p_load(rvest, tidyverse, skimr, stargazer,cowplot,car,boot,caret, here,tidyverse,fastDummies,tidymodels,glmnet)

train_hogares <- readRDS("data/train_hogares_full.Rds")
test_hogares <- readRDS("data/test_hogares_full.Rds")

set.seed(1234)

train<-model.matrix(object = ~ Ingtotugarr + Clase + Dominio + P5010 + P5090 + P5100 + P5130 + P5140 + 
                      Nper + Depto + Hombres + Mujeres + Pareja + Hijos + Nietos + 
                      EdadPromedio + SSalud + Trabajan + Estudiantes + Subsidios + 
                      HorasTrabajo + CotizaPension + OtroTrabajo + DeseaTrabajarMas + 
                      PrimerTrabajo + DesReciente + Ingresos_AlquilerPensiones + 
                      Ingresos_Paternidad + OtrosIngresos + AyudasEco + Pet + Oc + 
                      Des + Ina + Pea + JH_Ing + JH_Mujer + JH_Edad + JH_RSS_S + 
                      JH_NEduc + JH_Trabaja + JH_HorasTrabajo + JH_CotizaPension + 
                      JH_OtroTrabajo + JH_DeseaTrabajarMas + JH_PrimerTrabajo + 
                      JH_DesReciente + JH_Oc + JH_Des + JH_Ina,data=train_hogares)%>%as.data.frame()

y_train<-train[,'Ingtotugarr']
x_train<-train[,-c(1,2)]



###################### REVISION DE CLASE DESBALANCEADA Y  AJUSTE ############################





######################  DEFINICION DE METRICAS DE MEDICION MODELOS ############################







###################### ENTRENAMIENTO DE LOS MODELOS ############################

########### PREDICCIÓN INGRESO ##########

##### LASSO #####
Lasso<-glmnet(x=x_train,y=y_train,alpha=1,nlambda=100,standarize=FALSE,trace.it = TRUE)


##### RIDGE #####

##### ELASTIC NET #####

##### PCA #####

##### REGRESSION TREES #####

##### REGRESSION TREES RANDOM FOREST #####

##### K-N-N  #####


########### PREDICCIÓN POBREZA ##########

##### LOGIT #####

##### LOGIT-LASSO #####

##### RANDOM FOREST #####

##### RANDOM FOREST BAGGING #####

##### GB TREES #####

##### K-N-N  #####
