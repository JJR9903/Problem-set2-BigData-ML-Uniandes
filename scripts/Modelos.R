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
  else if(Sys.info()["user"]=="juan.rincon"){
    setwd("C:/Users/juan.rincon/OneDrive - Universidad de los Andes/Uniandes/9 Semestre - 1 PEG/Big Data/Problems Set/Problem-set2-BigData-ML-Uniandes")
  }
  else{
    setwd("C:/Users/Usuario/Documents/GitHub/Problem-set2-BigData-ML-Uniandes")
  }
}

dir_set()

pacman:: p_load(tidyverse,stargazer,caret,tidymodels,glmnet,parallel,doParallel,MLmetrics, themis)
#pacman:: p_load(tidyverse,stargazer,caret,tidymodels,glmnet,parallel,doParallel,tsutils,glmnetUtils,MLmetrics, themis)


train_hogares <- readRDS("stores/train_hogares_full.Rds")
#test_hogares <- readRDS("stores/test_hogares_full.Rds")

#train_hogares <- readRDS(paste0(getwd(),"/stores/train_hogares_full.Rds"))
#test_hogares <- readRDS(paste0(getwd(),"/stores/test_hogares_full.Rds"))
#y_train<-train_hogares[,'Ingtotugarr']
#p_train<-train_hogares[,'Pobre']



###################### ENTRENAMIENTO DE LOS MODELOS ############################
train<-model.matrix(object = ~ Ingtotugarr + Pobre + Lp + Npersug + Clase  + Dominio + P5010 + P5090 + P5100 + P5130 + P5140 + 
                      Nper + Depto + Hombres + Mujeres + Pareja + Hijos + Nietos + 
                      EdadPromedio + SSalud + Trabajan + Estudiantes + Subsidios + 
                      HorasTrabajo + CotizaPension + OtroTrabajo + DeseaTrabajarMas + 
                      PrimerTrabajo + DesReciente + Ingresos_AlquilerPensiones + 
                      Ingresos_Paternidad + OtrosIngresos + AyudasEco + Pet + Oc + 
                      Des + Ina + Pea + JH_Ing + JH_Mujer + JH_Edad + JH_RSS_S + 
                      JH_NEduc + JH_Trabaja + JH_HorasTrabajo + JH_CotizaPension + 
                      JH_OtroTrabajo + JH_DeseaTrabajarMas + JH_PrimerTrabajo + 
                      JH_DesReciente + JH_Oc + JH_Des + JH_Ina,data=train_hogares)

Train_y<-data.frame(train[,c('Lp','Pobre')],rowIndex=as.numeric(rownames(train)))

######################  DEFINICION DE METRICAS DE MEDICION MODELOS ############################
FPR_FNR_C <- function (data, lev = NULL, model = NULL){ 
  cm<-caret::confusionMatrix(as.factor(data$pred),reference = as.factor(data$obs))
  fp<-cm[["table"]]["1","0"]
  fn<-cm[["table"]]["0","1"]
  tn<-cm[["table"]]["0","0"]
  tp<-cm[["table"]]["1","1"]
  out<-(fn/(fn+tp)*0.75)+(fp/(fp+tn)*0.25)  
  out
}

FPR <- function (data, lev = NULL, model = NULL){ 
  cm<-caret::confusionMatrix(as.factor(data$pred),reference = as.factor(data$obs))
  fp<-cm[["table"]]["1","0"]
  fn<-cm[["table"]]["0","1"]
  tn<-cm[["table"]]["0","0"]
  tp<-cm[["table"]]["1","1"]
  out<-fp/(fp+tn)
  out
}

FNR <- function (data, lev = NULL, model = NULL){ 
  cm<-caret::confusionMatrix(as.factor(data$pred),reference = as.factor(data$obs))
  fp<-cm[["table"]]["1","0"]
  fn<-cm[["table"]]["0","1"]
  tn<-cm[["table"]]["0","0"]
  tp<-cm[["table"]]["1","1"]
  out<-fn/(fn+tp)
  out
}

########### PREDICCIÓN INGRESO ##########
model_Ing<-formula(Ingtotugarr~ Clase + DominioBARRANQUILLA+DominioBOGOTA+
                     DominioBUCARAMANGA+DominioCALI+DominioCARTAGENA+DominioCUCUTA+
                     DominioFLORENCIA+DominioIBAGUE+DominioMANIZALES+DominioMEDELLIN+
                     DominioMONTERIA+DominioNEIVA+DominioPASTO+DominioPEREIRA+DominioPOPAYAN+
                     DominioQUIBDO+`DominioRESTO URBANO`+DominioRIOHACHA+DominioRURAL+
                     `DominioSANTA MARTA`+DominioSINCELEJO+DominioTUNJA+DominioVALLEDUPAR+
                     DominioVILLAVICENCIO + P5010 + P50902+P50903+P50904+P50905+P50906 + 
                     P5100 + P5130 + P5140 + Nper + Depto08 + Depto11+Depto13+Depto15+Depto17+
                     Depto18+Depto19+Depto20+Depto23+ Depto25+Depto27+Depto41+Depto44+Depto47+
                     Depto50+Depto52+Depto54+Depto63+ Depto66+Depto68+Depto70+Depto73+Depto76+
                     Hombres + Mujeres + Pareja + Hijos + Nietos + EdadPromedio + SSalud + 
                     Trabajan + Estudiantes + Subsidios + HorasTrabajo + CotizaPension + 
                     OtroTrabajo + DeseaTrabajarMas + PrimerTrabajo + DesReciente + 
                     Ingresos_AlquilerPensiones + Ingresos_Paternidad + OtrosIngresos + 
                     AyudasEco + Pet + Oc + Des + Ina + Pea + JH_Ing + JH_Mujer + JH_Edad + 
                     JH_RSS_S + JH_NEduc2+JH_NEduc3+JH_NEduc4+JH_NEduc5+JH_NEduc6+JH_NEduc9 + 
                     JH_Trabaja + JH_HorasTrabajo + JH_CotizaPension + JH_OtroTrabajo + 
                     JH_DeseaTrabajarMas + JH_PrimerTrabajo + JH_DesReciente + JH_Oc + JH_Des + JH_Ina)


##### LASSO #####
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)
             
lasso<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=1,nlambda=1000,standarize=F)
lambdas_LCV<-lasso[["lambda"]]
Lasso_LCV <-train(model_Ing,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = 1,lambda=lambdas_LCV))
y_hat_lasso<-Lasso_LCV$pred%>%
  left_join(Train_y, by = "rowIndex")%>%
  mutate(P_pred=ifelse(pred<=Lp,1,0))

y_hat_l<-y_hat_lasso%>%
  group_by(lambda,Resample)%>%
  summarize(MSE=sum((obs-pred)^2),
            RMSE=sqrt(sum((obs-pred)^2)),
            FPR_FNR_C=FPR_FNR_C(data.frame(pred=P_pred,obs=Pobre)),
            FNR=FNR(data.frame(pred=P_pred,obs=Pobre)),
            FPR=FPR(data.frame(pred=P_pred,obs=Pobre)))%>%
  group_by(lambda)%>%
  summarise(MSE=mean(MSE),
            RMSE=mean(RMSE),
            FPR_FNR_C=mean(FPR_FNR_C),
            FNR=mean(FNR),
            FPR=mean(FPR))

lambda_mse<-y_hat_l[which.min(y_hat_l$MSE),1] 
lambda_rmse<-y_hat_l[which.min(y_hat_l$RMSE),1] 
lambda_fp_fn_r<-y_hat_l[which.min(y_hat_l$FPR_FNR_C),1] 
lambda_fp<-y_hat_l[which.min(y_hat_l$FPR),1] 
lambda_fn<-y_hat_l[which.min(y_hat_l$FNR),1] 
HyperP<-data.frame(FP_FN_R=lambda_fp_fn_r,FNR=lambda_fn,FPR=lambda_fp,MSE=lambda_mse,RMSE=lambda_rmse)
stargazer(HyperP,type="text",summary=F,out = "views/LassoReg_HyperP.txt")

Lasso_mse<-min(y_hat_l$MSE)
lasso_rmse<-min(y_hat_l$RMSE)
lasso_fp_fn_r<-min(y_hat_l$FPR_FNR_C)
lasso_fpr<-min(y_hat_l$FPR)
lasso_fnr<-min(y_hat_l$FNR)

Metricas<-data.frame(FP_FN_R=lasso_fp_fn_r,FNR=lasso_fnr,FPR=lasso_fpr,MSE=Lasso_mse,RMSE=lasso_rmse)
stargazer(Metricas,type="text",summary=F,out = "views/LassoReg.txt")
stopCluster(cl)

rm(lambda_mse,lambda_rmse,lambdas_LCV,lasso_cv,lasso_fp_fn_r,Lasso_LCV,Lasso_mse,lasso_rmse,Metricas,y_hat_l,y_hat_lasso,HyperP,lambda_fp,lambda_fn,lasso_fpr,lasso_fnr)
#variables del modelo de lasso 
Lasso<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=1,lambda=lambda_fp_fn_r,standarize=F)
Lasso$beta
rm(lambda_fp_fn_r)

##### RIDGE #####
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)

ridge<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=0,nlambda=1000,standarize=F)
lambdas_RCV<-ridge[["lambda"]]
Ridge_CV <-train(model_Ing,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = 0,lambda=lambdas_RCV))
y_hat_Ridge<-Ridge_CV$pred%>%
  left_join(Train_y, by = "rowIndex")%>%
  mutate(P_pred=ifelse(pred<=Lp,1,0))

y_hat_R<-y_hat_Ridge%>%
  group_by(lambda,Resample)%>%
  summarize(MSE=sum((obs-pred)^2),
            RMSE=sqrt(sum((obs-pred)^2)),
            FPR_FNR_C=FPR_FNR_C(data.frame(pred=P_pred,obs=Pobre)),
            FNR=FNR(data.frame(pred=P_pred,obs=Pobre)),
            FPR=FPR(data.frame(pred=P_pred,obs=Pobre)))%>%
  group_by(lambda)%>%
  summarise(MSE=mean(MSE),
            RMSE=mean(RMSE),
            FPR_FNR_C=mean(FPR_FNR_C),
            FNR=mean(FNR),
            FPR=mean(FPR))

lambda_mse<-y_hat_R[which.min(y_hat_R$MSE),1] 
lambda_rmse<-y_hat_R[which.min(y_hat_R$RMSE),1] 
lambda_fp_fn_r<-y_hat_R[which.min(y_hat_R$FPR_FNR_C),1] 
lambda_fp<-y_hat_R[which.min(y_hat_R$FPR),1] 
lambda_fn<-y_hat_R[which.min(y_hat_R$FNR),1] 
HyperP<-data.frame(FP_FN_R=lambda_fp_fn_r,FNR=lambda_fn,FPR=lambda_fp,MSE=lambda_mse,RMSE=lambda_rmse)
stargazer(HyperP,type="text",summary=F,out = "views/LassoReg_HyperP.txt")

ridge_mse<-min(y_hat_R$MSE)
ridge_rmse<-min(y_hat_R$RMSE)
ridge_fp_fn_r<-min(y_hat_R$FPR_FNR_C)
ridge_fpr<-min(y_hat_l$FPR)
ridge_fnr<-min(y_hat_l$FNR)

Metricas<-data.frame(FP_FN_R=ridge_fp_fn_r,FNR=ridge_fnr,FPR=ridge_fpr,MSE=ridge_mse,RMSE=ridge_rmse)
stargazer(Metricas,type="text",summary=F,out = "views/RidgeReg.txt")
stopCluster(cl)

rm(lambda_mse,lambda_rmse,lambdas_RCV,ridge_cv,Ridge_CV,ridge_mse,ridge_rmse,ridge_fp_fn_r,Metricas,y_hat_R,y_hat_Ridge,HyperP,lambda_fn,lambda_fP,ridge_fpr,ridge_fnr)
#variables del modelo de ridge 
Ridge<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=0,lambda=lambda_fp_fn_r,standarize=F)
Ridge$beta
rm(lambda_fp_fn_r)



##### ELASTIC NET #####
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)
alphas=seq(0,1,0.001)
HyperP<-data.frame(alpha=alphas,lambda=NA,FPR_FNR_C=NA)
for (i in alphas){
  en<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=i,nlambda=100,standarize=F)
  lambdas_EN<-en[["lambda"]]
  EN_CV <-train(model_Ing,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = i,lambda=lambdas_EN))
  y_hat_EN<-EN_CV$pred%>%
    left_join(Train_y, by = "rowIndex")%>%
    mutate(P_pred=ifelse(pred<=Lp,1,0))
  
  y_hat_EN<-y_hat_EN%>%
    group_by(lambda,Resample)%>%
    summarize(FPR_FNR_C=FPR_FNR_C(data.frame(pred=P_pred,obs=Pobre)))%>%
    group_by(lambda)%>%
    summarise(FPR_FNR_C=mean(FPR_FNR_C))
  
  HyperP[HyperP$alpha==i,2]<-y_hat_EN[which.min(y_hat_EN$FPR_FNR_C),1]
  HyperP[HyperP$alpha==i,3]<-min(y_hat_EN$FPR_FNR_C)
  
  print(i)
  rm(y_hat_EN,EN_CV,lambdas_EN,en)
}

HyperP<-HyperP[which.min(HyperP$FPR_FNR_C),]

EN_CV <-train(model_Ing,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = as.numeric(HyperP['alpha']),lambda=as.numeric(HyperP['lambda'])))
y_hat_EN<-EN_CV$pred%>%
  left_join(Train_y, by = "rowIndex")%>%
  mutate(P_pred=ifelse(pred<=Lp,1,0))

y_hat_EN<-y_hat_EN%>%
  summarize(MSE=sum((obs-pred)^2),
            RMSE=sqrt(sum((obs-pred)^2)),
            FPR_FNR_C=FPR_FNR_C(data.frame(pred=P_pred,obs=Pobre)),
            FNR=FNR(data.frame(pred=P_pred,obs=Pobre)),
            FPR=FPR(data.frame(pred=P_pred,obs=Pobre)))


Metricas<-data.frame(FP_FN_R=y_hat_EN$FPR_FNR_C,FNR=y_hat_EN$FNR,FPR=y_hat_EN$FPR,MSE=y_hat_EN$MSE,RMSE=y_hat_EN$RMSE)
stargazer(Metricas,type="text",summary=F,out = "views/ENReg.txt")
stopCluster(cl)
rm(EN_CV,HyperP,Metricas,y_hat_EN,i,alphas)


##### REGRESSION TREES #####

##### REGRESSION TREES RANDOM FOREST #####








########### PREDICCIÓN POBREZA ##########
#Stratified Cross Validation
##### LOGIT #####

##### LOGIT-LASSO #####
Lasso <-train(model_Ing,data=train,trControl = trainControl(method = "cv", number = 5 ,savePredictions = 'final'),method = "glmnet",metric=metric_set(fp_fn_rate_vec), tuneGrid = expand.grid(alpha = 1,lambda=seq(0,1,0.01)))
Lasso[["bestTune"]][['alpha']]
Lasso[["bestTune"]][['lambda']]
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
mod <- train(Pobre ~ ., data = train ,
             method = "glmnet",
             metric = "FPR_FNR_C",
             trControl = trainControl(summaryFunction = FPR_FNR_C))









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

