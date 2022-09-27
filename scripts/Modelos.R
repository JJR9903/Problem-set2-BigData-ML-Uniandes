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

pacman:: p_load(rvest, tidyverse, skimr, stargazer,cowplot,car,boot,caret, here,tidyverse,fastDummies,tidymodels,glmnet, MLmetrics, themis)


train_hogares <- readRDS(paste0(getwd(),"/stores/train_hogares_full.Rds"))
test_hogares <- readRDS(paste0(getwd(),"/stores/test_hogares_full.Rds"))

y_train<-train_hogares[,'Ingtotugarr']
p_train<-train_hogares[,'Pobre']
x<-names(test_hogares)
for (i in c('P5000','Npersug','Li','Lp','Fex_c','Fex_depto','Ingtot_hogar','JH_Ing')){ x=x[ !x == i]}
x_train<-train_hogares[,x]
x_test<-test_hogares[,x]


###################### REVISION DE CLASE DESBALANCEADA Y  AJUSTE ############################
prop.table(table(train_hogares$Pobre))
 ## Se aprecia que s�lo el 20% de la base es pobre


#Se separa el sample de train en dos
smp_size <- floor(0.7*nrow(train_hogares))
set.seed(666)
train_ind <- sample(1:nrow(train_hogares), size = smp_size)

train <- train_hogares[train_ind, ]
test <- train_hogares[-train_ind, ]



##OverSample
train_hogares$Pobre<- factor(train_hogares$Pobre)
train_h2 <- recipe(Pobre ~ P5000+P5010+SSalud+Trabajan+Estudiantes+CotizaPension+DeseaTrabajarMas+AyudasEco, data = train_hogares) %>%
  themis::step_smote(Pobre, over_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)
#Corroboramos que ahora la mitad de la muestra sea pobre
prop.table(table(train_h2$Pobre))
nrow(train_h2)
nrow(train_hogares)


##UnderSample
train_h3 <- recipe(Pobre ~ P5000+P5010+SSalud+Trabajan+Estudiantes+CotizaPension+DeseaTrabajarMas+AyudasEco, data = train_hogares) %>%
  themis::step_downsample(Pobre) %>%
  prep() %>%
  bake(new_data = NULL)
#Corroboramos que ahora la mitad de la muestra sea pobre
prop.table(table(train_h3$Pobre))
nrow(train_h3)
nrow(train_hogares)




##Opti
thresholds <- seq(0.1, 0.9, length.out = 100)
for (t in thresholds) {
  y_pred_t <- as.numeric(probs_outsample1 > t)
  f1_t <- F1_Score(y_true = test$infielTRUE, y_pred = y_pred_t,
                   positive = 1)
  fila <- data.frame(t = t, F1 = f1_t)
  opt_t <- bind_rows(opt_t, fila)
}

mejor_t <-  opt_t$t[which(opt_t$F1 == max(opt_t$F1, na.rm = T))]


######################DEFINICION DE METRICAS DE MEDICION MODELOS ############################
  






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
#Hay que definir Ctrl
Ctrl<-trainControl(method="CV")
set.seed(1410)
forest <- train(Pobre~ SSalud+Subsidios,+CotizaPension+P5010, data = train_hogares,
  method = "rf",
  trControl = Ctrl,
  family = "binomial",
  metric="Sens"
  #preProcess = c("center", "scale")
)

##### RANDOM FOREST BAGGING #####

##### GB TREES #####

##### K-N-N  #####
