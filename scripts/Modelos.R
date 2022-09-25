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

y_train<-train_hogares[,'Ingtotugarr']
p_train<-train_hogares[,'Pobre']
x<-names(test_hogares)
for (i in c('P5000','P5100','P5130','P5140','Npersug','Li','Lp','Fex_c','Fex_depto','Ingtot_hogar','JH_Ing')){ x=x[ !x == i]}
x_train<-train_hogares[,x]
x_test<-test_hogares[,x]


###################### REVISION DE CLASE DESBALANCEADA Y  AJUSTE ############################





######################  DEFINICION DE METRICAS DE MEDICION MODELOS ############################







###################### ENTRENAMIENTO DE LOS MODELOS ############################

########### PREDICCIÓN INGRESO ##########

##### LASSO #####

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
