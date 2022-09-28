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

pacman:: p_load(tidyverse,stargazer,caret,tidymodels,glmnet,parallel,doParallel,MLmetrics, themis,rattle,rlang,randomForest)

set.seed(1234)
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 

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
                      Des + Ina + Pea  + JH_Mujer + JH_Edad + JH_RSS_S + 
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

###### NEW METRIC 
fp_fn_vec <- function(truth, estimate, na_rm = TRUE, values=Train_y) {
  
  fp_fn_impl <- function(truth,estimate,values) {
    estimate<-data.frame(estimate)%>%setNames("estimate")%>%
      rownames_to_column(var = "rowIndex")%>%
      mutate(rowIndex = as.numeric(rowIndex))%>%
      left_join(Train_y, by = "rowIndex")%>%
      mutate(estimate=ifelse(estimate<=Lp,1,0))
    
    cm<-caret::confusionMatrix(as.factor(estimate$estimate),reference = as.factor(estimate$Pobre))
    fp<-cm[["table"]]["1","0"]
    fn<-cm[["table"]]["0","1"]
    tn<-cm[["table"]]["0","0"]
    tp<-cm[["table"]]["1","1"]
    out<-(fn/(fn+tp)*0.75)+(fp/(fp+tn)*0.25)  
    out
    
  }
  
  metric_vec_template(
    metric_impl = fp_fn_impl,
    truth = truth, 
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    values=Train_y
  )
  
}
fp_fn <- function(data, ...) {
  UseMethod("fp_fn")
}

fp_fn <- new_numeric_metric(fp_fn, direction = "minimize")

fp_fn.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  
  metric_summarizer(
    metric_nm = "fp_fn",
    metric_fn = fp_fn_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate), 
    na_rm = na_rm,
    ...
  )
  
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
                     AyudasEco + Pet + Oc + Des + Ina + Pea + JH_Mujer + JH_Edad + 
                     JH_RSS_S + JH_NEduc2+JH_NEduc3+JH_NEduc4+JH_NEduc5+JH_NEduc6+JH_NEduc9 + 
                     JH_Trabaja + JH_HorasTrabajo + JH_CotizaPension + JH_OtroTrabajo + 
                     JH_DeseaTrabajarMas + JH_PrimerTrabajo + JH_DesReciente + JH_Oc + JH_Des + JH_Ina)


##### LASSO #####
registerDoParallel(cl)
lasso<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=1,nlambda=10,standarize=F)
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

#variables del modelo de lasso 
Lasso<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=1,lambda=lambda_fp_fn_r,standarize=F)
Betas<-coef(Lasso,  exact = FALSE,x=x, y=y)
Betas<-Betas@Dimnames[[1]][which(Betas!= 0)]
Nbetas_Lasso<-length(Betas)

Metricas<-data.frame(FP_FN_R=lasso_fp_fn_r,FNR=lasso_fnr,FPR=lasso_fpr,MSE=Lasso_mse,RMSE=lasso_rmse,N_Betas=Nbetas_Lasso)
stargazer(Metricas,type="text",summary=F,out = "views/LassoReg.txt")
stopCluster(cl)


betas=Betas[Betas!='(Intercept)']

betas <- sapply(betas, function(i) { paste0(i, "+") })
betas <- paste(betas, collapse = '')
betas  <- substr(betas, 1, nchar(betas)-1)

model_Ing_lasso<-formula(Ingtotugarr~Clase+DominioBOGOTA+DominioBUCARAMANGA+DominioCARTAGENA+DominioFLORENCIA+DominioIBAGUE+DominioMANIZALES+DominioMEDELLIN+DominioNEIVA+DominioPASTO+DominioPOPAYAN+DominioQUIBDO+
                           `DominioRESTO URBANO`+DominioRURAL+DominioSINCELEJO+DominioVALLEDUPAR+DominioVILLAVICENCIO+P5010+P50902+P50903+P50904+P50905+P50906+P5100+P5130+P5140+Nper+Depto08+Depto11+Depto15+
                           Depto18+Depto19+Depto20+Depto23+Depto25+Depto27+Depto41+Depto44+Depto47+Depto50+Depto52+Depto63+Depto66+Depto68+Depto70+Depto73+Depto76+Hombres+Pareja+Hijos+Nietos+EdadPromedio+SSalud+
                           Trabajan+Estudiantes+Subsidios+HorasTrabajo+CotizaPension+OtroTrabajo+DeseaTrabajarMas+PrimerTrabajo+DesReciente+Ingresos_AlquilerPensiones+Ingresos_Paternidad+OtrosIngresos+AyudasEco+
                           Oc+Des+Ina+JH_Mujer+JH_Edad+JH_RSS_S+JH_NEduc2+JH_NEduc3+JH_NEduc4+JH_NEduc5+JH_NEduc6+JH_NEduc9+JH_HorasTrabajo+JH_CotizaPension+JH_OtroTrabajo+JH_DeseaTrabajarMas+JH_PrimerTrabajo+
                           JH_DesReciente+JH_Oc+JH_Des)



train_lasso<-train[,Betas]    
rm(lambda_fp_fn_r,lambda_mse,lambda_rmse,lambdas_LCV,lasso_cv,lasso_fp_fn_r,Lasso_LCV,Lasso_mse,lasso_rmse,Metricas,y_hat_l,y_hat_lasso,HyperP,lambda_fp,lambda_fn,lasso_fpr,lasso_fnr,betas,Betas,Nbetas_Lasso,Lasso,lasso)

##### RIDGE #####
registerDoParallel(cl)

ridge<-glmnet(x=train_lasso,y=as.matrix(train[,'Ingtotugarr']),alpha=0,nlambda=1000,standarize=F)
lambdas_RCV<-ridge[["lambda"]]
Ridge_CV <-train(model_Ing_lasso,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = 0,lambda=lambdas_RCV))
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
stargazer(HyperP,type="text",summary=F,out = "views/RidgeReg_HyperP.txt")

ridge_mse<-min(y_hat_R$MSE)
ridge_rmse<-min(y_hat_R$RMSE)
ridge_fp_fn_r<-min(y_hat_R$FPR_FNR_C)
ridge_fpr<-min(y_hat_R$FPR)
ridge_fnr<-min(y_hat_R$FNR)

#variables del modelo de ridge 
Ridge<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=0,lambda=lambda_fp_fn_r,standarize=F)
Betas<-coef(Ridge,  exact = FALSE,x=x, y=y)
Betas<-Betas@Dimnames[[1]][which(Betas!= 0)]
Nbetas_Ridge<-length(Betas)

Metricas<-data.frame(FP_FN_R=ridge_fp_fn_r,FNR=ridge_fnr,FPR=ridge_fpr,MSE=ridge_mse,RMSE=ridge_rmse,N_Betas=Nbetas_Ridge)
stargazer(Metricas,type="text",summary=F,out = "views/RidgeReg.txt")
stopCluster(cl)

rm(lambda_fp_fn_r,lambda_mse,lambda_rmse,lambdas_RCV,ridge,Ridge_CV,ridge_mse,ridge_rmse,ridge_fp_fn_r,Metricas,y_hat_R,y_hat_Ridge,HyperP,lambda_fn,lambda_fp,ridge_fpr,ridge_fnr,Ridge,Betas,Nbetas_Ridge)




##### ELASTIC NET #####
registerDoParallel(cl)
alphas=seq(0,1,0.01)
HyperP<-data.frame(alpha=alphas,lambda=NA,FPR_FNR_C=NA)
for (i in alphas){
  en<-glmnet(x=train_lasso,y=as.matrix(train[,'Ingtotugarr']),alpha=i,nlambda=100,standarize=F)
  lambdas_EN<-en[["lambda"]]
  EN_CV <-train(model_Ing_lasso,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = i,lambda=lambdas_EN))
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
stargazer(HyperP,type="text",summary=F,out = "views/ENReg_HyperP.txt")

EN_CV <-train(model_Ing_lasso,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = as.numeric(HyperP['alpha']),lambda=as.numeric(HyperP['lambda'])))
y_hat_EN<-EN_CV$pred%>%
  left_join(Train_y, by = "rowIndex")%>%
  mutate(P_pred=ifelse(pred<=Lp,1,0))

y_hat_EN<-y_hat_EN%>%
  summarize(MSE=sum((obs-pred)^2),
            RMSE=sqrt(sum((obs-pred)^2)),
            FPR_FNR_C=FPR_FNR_C(data.frame(pred=P_pred,obs=Pobre)),
            FNR=FNR(data.frame(pred=P_pred,obs=Pobre)),
            FPR=FPR(data.frame(pred=P_pred,obs=Pobre)))

EN<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=as.numeric(HyperP['alpha']),lambda=as.numeric(HyperP['lambda']),standarize=F)
Betas<-coef(EN,  exact = FALSE,x=x, y=y)
Betas<-Betas@Dimnames[[1]][which(Betas!= 0)]
Nbetas_EN<-length(Betas)

Metricas<-data.frame(FP_FN_R=y_hat_EN$FPR_FNR_C,FNR=y_hat_EN$FNR,FPR=y_hat_EN$FPR,MSE=y_hat_EN$MSE,RMSE=y_hat_EN$RMSE,N_Betas=Nbetas_EN)
stargazer(Metricas,type="text",summary=F,out = "views/ENReg.txt")

stopCluster(cl)
rm(EN_CV,HyperP,Metricas,y_hat_EN,i,alphas,Betas,EN,Nbetas_EN)


 
##### LASSO CON VARIABLES RECOMENDADAS POR LA LITERARURA #####
##model matrix 
train_lasso_li<-model.matrix(object = ~ Ingtotugarr + Pobre + Lp + Npersug +JH_Edad2+JH_Edad+Depto + Hombres + Mujeres+
                      SSalud+Clase+ P_o + JH_RSS_S + P5100+ P5090+CotizaPension , data=train_hogares)

##formula 
model_Ing_lasso_li<-formula(Ingtotugarr~JH_Edad2+JH_Edad+ Depto08 + Depto11+Depto13+Depto15+Depto17+
                              Depto18+Depto19+Depto20+Depto23+ Depto25+Depto27+Depto41+Depto44+Depto47+
                              Depto50+Depto52+Depto54+Depto63+ Depto66+Depto68+Depto70+Depto73+Depto76+
                              Hombres + Mujeres+ SSalud+Clase+ P_o + JH_RSS_S + P5100+P50902+P50903+P50904+P50905+P50906 +CotizaPension)

registerDoParallel(cl)

lasso<-glmnet(x=as.matrix(train_lasso_li[,-c(1:5)]),y=as.matrix(train_lasso_li[,'Ingtotugarr']),alpha=1,nlambda=1000,standarize=F)
lambdas_LCV<-lasso[["lambda"]]
Lasso_LCV <-train(model_Ing_lasso_li,data=train_lasso_li,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", tuneGrid = expand.grid(alpha = 1,lambda=lambdas_LCV))
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
stargazer(HyperP,type="text",summary=F,out = "views/LassoReg_li_HyperP.txt")

Lasso_mse<-min(y_hat_l$MSE)
lasso_rmse<-min(y_hat_l$RMSE)
lasso_fp_fn_r<-min(y_hat_l$FPR_FNR_C)
lasso_fpr<-min(y_hat_l$FPR)
lasso_fnr<-min(y_hat_l$FNR)

#variables del modelo de lasso 
Lasso<-glmnet(x=as.matrix(train_lasso_li[,-c(1:5)]),y=as.matrix(train_lasso_li[,'Ingtotugarr']),alpha=1,lambda=lambda_fp_fn_r,standarize=F)
Betas<-coef(Lasso,  exact = FALSE,x=x, y=y)
Betas<-Betas@Dimnames[[1]][which(Betas!= 0)]
Nbetas_Lasso<-length(Betas)

Metricas<-data.frame(FP_FN_R=lasso_fp_fn_r,FNR=lasso_fnr,FPR=lasso_fpr,MSE=Lasso_mse,RMSE=lasso_rmse,N_Betas=Nbetas_Lasso)
stargazer(Metricas,type="text",summary=F,out = "views/LassoReg_li.txt")
stopCluster(cl)

rm(lambda_fp_fn_r,lambda_mse,lambda_rmse,lambdas_LCV,lasso,Lasso,lasso_fp_fn_r,Lasso_LCV,Lasso_mse,lasso_rmse,Metricas,y_hat_l,y_hat_lasso,HyperP,lambda_fp,lambda_fn,lasso_fpr,lasso_fnr,betas,Nbetas_Lasso,train_lasso_li,model_Ing_lasso_li)


##### REGRESSION TREES RANDOM FOREST #####
registerDoParallel(cl)
modelo1 <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

modelo1_fit <- fit(modelo1, Ingtotugarr ~ ., data = as.data.frame(train))
stopCluster(cl)

fancyRpartPlot(modelo1_fit$fit, main = "Árbol sin fine tuning", sub = "")

importancia <- varImp(modelo1_fit$fit)
importancia <- importancia %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))

y_hat_insample <- predict(modelo1_fit, as.data.frame(train))%>%
  rownames_to_column(var = "rowIndex")%>%
  mutate(rowIndex = as.numeric(rowIndex))%>%
  left_join(Train_y, by = "rowIndex")%>%
  mutate(P_pred=ifelse(.pred<=Lp,1,0))

FPR_FNR_C(data.frame(pred=y_hat_insample$P_pred,obs=y_hat_insample$Pobre))


######
forest <- train( model_Ing,  data=train, method='rf', trControl=trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),tuneGrid = expand.grid(alpha = 1,lambda=lambdas_LCV))
ntree=c(100)

######

######
modelo2 <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_grid <- crossing(
  cost_complexity = c(0.0001),
  min_n = c(2, 14, 27),
  tree_depth = c(4, 8, 16)
)

registerDoParallel(cl)
Train<-as.data.frame(train)
folds <- vfold_cv(Train, strata = Pobre, v = 5)



modelo2_cv <- tune_grid(
  modelo2,
  model_Ing,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(fp_fn),
  control = control_grid(event_level = 'second')
)








########### PREDICCIÓN POBREZA ##########
#Stratified Cross Validation
##### LOGIT #####

##### LOGIT-LASSO #####
registerDoParallel(cl)

L_Lasso<-glmnet(x=train_lasso,y=as.matrix(train[,'Ingtotugarr']),alpha=0,nlambda=1000,standarize=F, family="Binomial")
L_Lasso_RCV<-ridge[["lambda"]]
L_Lasso_CV <-train(model_Ing_lasso,data=train,trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'all',selectionFunction="best"),method = "glmnet", family="Binomial", tuneGrid = expand.grid(alpha = 0,lambda=lambdas_RCV))
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
stargazer(HyperP,type="text",summary=F,out = "views/RidgeReg_HyperP.txt")

ridge_mse<-min(y_hat_R$MSE)
ridge_rmse<-min(y_hat_R$RMSE)
ridge_fp_fn_r<-min(y_hat_R$FPR_FNR_C)
ridge_fpr<-min(y_hat_R$FPR)
ridge_fnr<-min(y_hat_R$FNR)

#variables del modelo de ridge 
Ridge<-glmnet(x=as.matrix(train[,-c(1:5)]),y=as.matrix(train[,'Ingtotugarr']),alpha=0,lambda=lambda_fp_fn_r,standarize=F)
Betas<-coef(Ridge,  exact = FALSE,x=x, y=y)
Betas<-Betas@Dimnames[[1]][which(Betas!= 0)]
Nbetas_Ridge<-length(Betas)

Metricas<-data.frame(FP_FN_R=ridge_fp_fn_r,FNR=ridge_fnr,FPR=ridge_fpr,MSE=ridge_mse,RMSE=ridge_rmse,N_Betas=Nbetas_Ridge)
stargazer(Metricas,type="text",summary=F,out = "views/RidgeReg.txt")
stopCluster(cl)

rm(lambda_fp_fn_r,lambda_mse,lambda_rmse,lambdas_RCV,ridge,Ridge_CV,ridge_mse,ridge_rmse,ridge_fp_fn_r,Metricas,y_hat_R,y_hat_Ridge,HyperP,lambda_fn,lambda_fp,ridge_fpr,ridge_fnr,Ridge,Betas,Nbetas_Ridge)



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
  #Se debe poner el modelo que se usa
train_h2 <- recipe(Pobre ~ P5000+P5010+SSalud+Trabajan+Estudiantes+CotizaPension+DeseaTrabajarMas+AyudasEco, data = train_hogares) %>%
  themis::step_smote(Pobre, over_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)
#Corroboramos que ahora la mitad de la muestra sea pobre
prop.table(table(train_h2$Pobre))
#Aquí sabemos en qué porcentaje aumentó la muestra
(nrow(train_h2)-nrow(train_hogares))/nrow(train_hogares)*100


##UnderSample
train_h3 <- recipe(Pobre ~ P5000+P5010+SSalud+Trabajan+Estudiantes+CotizaPension+DeseaTrabajarMas+AyudasEco, data = train_hogares) %>%
  themis::step_downsample(Pobre) %>%
  prep() %>%
  bake(new_data = NULL)
#Corroboramos que ahora la mitad de la muestra sea pobre
prop.table(table(train_h3$Pobre))
nrow(train_h3)
nrow(train_hogares)

##Optimizar umbral de decisión
  
  
 #Optimizador
thresholds <- seq(0.1, 0.9, length.out = 100)
  opt_t<-dat.frame()
for (t in thresholds) {
  y_pred_t <- as.numeric(probs_outsample1 > t)
  f1_t <- F1_Score(y_true = train_hogares$Pobre, y_pred = y_pred_t,
                   positive = 1)
  fila <- data.frame(t = t, F1 = f1_t)
  opt_t <- bind_rows(opt_t, fila)
}

mejor_t <-  opt_t$t[which(opt_t$F1 == max(opt_t$F1, na.rm = T))]

