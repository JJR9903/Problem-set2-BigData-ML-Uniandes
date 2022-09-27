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

pacman:: p_load(tidyverse,skimr,stargazer,fastDummies,caret,tidymodels,glmnet,parallel,doParallel,tsutils,glmnetUtils)


train_hogares <- readRDS("stores/train_hogares_full.Rds")
#test_hogares <- readRDS("stores/test_hogares_full.Rds")

set.seed(1234)


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

#y_train<-train[,'Ingtotugarr']
#x_train<-train[,-c(1:5)]



###################### REVISION DE CLASE DESBALANCEADA Y  AJUSTE ############################
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
  FPR<-fp/(fp+tn)
  FNR<-fn/(fn+tp)
  out<-(FNR*0.75)+(FPR*0.25)  
  out
}



###################### ENTRENAMIENTO DE LOS MODELOS ############################

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

lasso_cv<-cv.glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),trace.it = T,type.measure = "mse")
lambdas_LCV<-c(seq(lasso_cv$lambda.min,max(lasso_cv$lambda),length.out=1000),lasso_cv$lambda.1se)
Lasso_LCV <-train(model_Ing,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = 1,lambda=lambdas_LCV))
y_hat_lasso<-Lasso_LCV$pred%>%
  left_join(Train_y, by = "rowIndex")%>%
  mutate(P_pred=ifelse(pred<=Lp,1,0))

y_hat_l<-y_hat_lasso%>%
  group_by(lambda,Resample)%>%
  summarize(MSE=sum((obs-pred)^2),
            RMSE=sqrt(sum((obs-pred)^2)),
            FPR_FNR_C=FPR_FNR_C(data.frame(pred=P_pred,obs=Pobre)))%>%
  group_by(lambda)%>%
  summarise(MSE=mean(MSE),
            RMSE=mean(RMSE),
            FPR_FNR_C=mean(FPR_FNR_C))

lambda_mse<-y_hat_l[which.min(y_hat_l$MSE),1] # 948
lambda_rmse<-y_hat_l[which.min(y_hat_l$RMSE),1] # 948
lambda_fp_fn_r<-y_hat_l[which.min(y_hat_l$FPR_FNR_C),1] # 948

Lasso_mse<-min(y_hat_l$MSE)
lasso_rmse<-min(y_hat_l$RMSE)
lasso_fp_fn_r<-min(y_hat_l$FPR_FNR_C)

Metricas<-data.frame(FP_FN_R=lasso_fp_fn_r,MSE=Lasso_mse,RMSE=lasso_rmse)
stargazer(Metricas,type="text",summary=F,out = "views/LassoReg.txt")
stopCluster(cl)

rm(lambda_fp_fn_r,lambda_mse,lambda_rmse,lambdas_LCV,lasso_cv,lasso_fp_fn_r,Lasso_LCV,Lasso_mse,lasso_rmse,Metricas,y_hat_l,y_hat_lasso )

##### RIDGE #####
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)

ridge_cv<-cv.glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=0,trace.it = T,type.measure = "mse")
lambdas_RCV<-c(seq(ridge_cv$lambda.min,max(ridge_cv$lambda),length.out=1000),ridge_cv$lambda.1se)
Ridge_CV <-train(model_Ing,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = 0,lambda=lambdas_RCV))
y_hat_Ridge<-Ridge_CV$pred%>%
  left_join(Train_y, by = "rowIndex")%>%
  mutate(P_pred=ifelse(pred<=Lp,1,0))

y_hat_R<-y_hat_Ridge%>%
  group_by(lambda,Resample)%>%
  summarize(MSE=sum((obs-pred)^2),
            RMSE=sqrt(sum((obs-pred)^2)),
            FPR_FNR_C=FPR_FNR_C(data.frame(pred=P_pred,obs=Pobre)))%>%
  group_by(lambda)%>%
  summarise(MSE=mean(MSE),
            RMSE=mean(RMSE),
            FPR_FNR_C=mean(FPR_FNR_C))

lambda_mse<-y_hat_R[which.min(y_hat_R$MSE),1] # 948
lambda_rmse<-y_hat_R[which.min(y_hat_R$RMSE),1] # 948
lambda_fp_fn_r<-y_hat_R[which.min(y_hat_R$FPR_FNR_C),1] # 948

ridge_mse<-min(y_hat_R$MSE)
ridge_rmse<-min(y_hat_R$RMSE)
ridge_fp_fn_r<-min(y_hat_R$FPR_FNR_C)

Metricas<-data.frame(FP_FN_R=ridge_fp_fn_r,MSE=ridge_mse,RMSE=ridge_rmse)
stargazer(Metricas,type="text",summary=F,out = "views/RidgeReg.txt")
stopCluster(cl)

rm(lambda_fp_fn_r,lambda_mse,lambda_rmse,lambdas_RCV,ridge_cv,Ridge_CV,ridge_mse,ridge_rmse,ridge_fp_fn_r,Metricas,y_hat_R,y_hat_Ridge )

##### ELASTIC NET #####
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)

en_cv<-cva.glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),trace.it = T,type.measure = "mse")
lambdas_EN<-c(seq(en_cv$lambda.min,max(en_cv$lambda),length.out=1000),en_cv$lambda.1se)
alphas_EN<-en_cv[["alpha"]]
EN_CV <-train(model_Ing,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = alphas_EN,lambda=lambdas_EN))
y_hat_EN<-EN_CV$pred%>%
  left_join(Train_y, by = "rowIndex")%>%
  mutate(P_pred=ifelse(pred<=Lp,1,0))

y_hat_EN<-y_hat_EN%>%
  group_by(lambda,alpha,Resample)%>%
  summarize(MSE=sum((obs-pred)^2),
            RMSE=sqrt(sum((obs-pred)^2)),
            FPR_FNR_C=FPR_FNR_C(data.frame(pred=P_pred,obs=Pobre)))%>%
  group_by(lambda,alpha)%>%
  summarise(MSE=mean(MSE),
            RMSE=mean(RMSE),
            FPR_FNR_C=mean(FPR_FNR_C))

lambda_mse<-y_hat_EN[which.min(y_hat_EN$MSE),1] # 948
lambda_rmse<-y_hat_EN[which.min(y_hat_EN$RMSE),1] # 948
lambda_fp_fn_r<-y_hat_EN[which.min(y_hat_EN$FPR_FNR_C),1] # 948

alpha_mse<-y_hat_EN[which.min(y_hat_EN$MSE),2] # 948
alpha_rmse<-y_hat_EN[which.min(y_hat_EN$RMSE),2] # 948
alpha_fp_fn_r<-y_hat_EN[which.min(y_hat_EN$FPR_FNR_C),2] # 948

EN_mse<-min(y_hat_R$MSE)
EN_rmse<-min(y_hat_R$RMSE)
EN_fp_fn_r<-min(y_hat_R$FPR_FNR_C)

Metricas<-data.frame(FP_FN_R=EN_fp_fn_r,MSE=EN_mse,RMSE=EN_rmse)
stargazer(Metricas,type="text",summary=F,out = "views/ENReg.txt")
stopCluster(cl)


rm(lambda_fp_fn_r,lambda_mse,lambda_rmse,lambdas_EN,alphas_EN,en_cv,EN_CV,EN_mse,EN_mse,EN_mse,Metricas,y_hat_EN,alpha_mse,alpha_rmse,alpha_fp_fn_r)


##### PCA #####

##### REGRESSION TREES #####

##### REGRESSION TREES RANDOM FOREST #####

##### K-N-N  #####


########### PREDICCIÓN POBREZA ##########
#Stratified Cross Validation
##### LOGIT #####

##### LOGIT-LASSO #####
Lasso <-train(model_Ing,data=train,trControl = trainControl(method = "cv", number = 5 ,savePredictions = 'final'),method = "glmnet",metric=metric_set(fp_fn_rate_vec), tuneGrid = expand.grid(alpha = 1,lambda=seq(0,1,0.01)))
Lasso[["bestTune"]][['alpha']]
Lasso[["bestTune"]][['lambda']]
##### RANDOM FOREST #####

##### RANDOM FOREST BAGGING #####

##### GB TREES #####

##### K-N-N  #####
mod <- train(Pobre ~ ., data = train ,
             method = "glmnet",
             metric = "FPR_FNR_C",
             trControl = trainControl(summaryFunction = FPR_FNR_C))
