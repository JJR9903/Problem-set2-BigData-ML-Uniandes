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

pacman:: p_load(tidyverse,skimr,fastDummies,labelled)

######## FUNCIONES ####
NotInTest<-function(y){ tryCatch( error = function(cnd) { NA }, y ) }

settingVariables_Personas<- function(personas){
  personas=personas
  factor_variables_personas<-c('P6020','P6050','P6090','P6100','P6210','P6240','P6585s1','P6585s2','P6585s3','P6585s4','P6920','P7040','P7090','P7310','P7422','P7495','P7500s3','P7505','P7510s1','P7510s2','P7510s3')        
  personas[,factor_variables_personas] <- lapply(personas[,factor_variables_personas] , factor)
  
  dummy_variables_personas<-c('P6020','P6090','P6585s1','P6585s2','P6585s3','P6585s4','P7040','P7090','P7310','P7422','P7495','P7500s3','P7505','P7510s1','P7510s2','P7510s3')
  personas[,dummy_variables_personas] <-sapply(personas[,dummy_variables_personas],  function(x) replace(x, x=="9", NA))
  personas[,dummy_variables_personas] <-sapply(personas[,dummy_variables_personas],  function(x) replace(x, x=="2", '0'))
  personas[,dummy_variables_personas] <- lapply(personas[,dummy_variables_personas] , as.numeric)
  
  personas<-mutate(personas,P6585 = ifelse(personas$P6585s1==1 |personas$P6585s2==1 |personas$P6585s3==1 |personas$P6585s4==1,1,0))
  personas<-mutate(personas,P7510 = ifelse(personas$P7510s1==1 |personas$P7510s2==1 |personas$P7510s3==1,1,0))
  
  factor_variables_personas<-c('P6020','P6210')        
  personas<-dummy_cols(personas,factor_variables_personas,remove_most_frequent_dummy=F,ignore_na=T,split="_",remove_selected_columns=FALSE)
  factor_variables_personas<-c('P6050','P6100','P6240','P6920')        
  personas<-dummy_cols(personas,factor_variables_personas,remove_most_frequent_dummy=F,ignore_na=T,split="_",remove_selected_columns=TRUE)
  
  return(personas)
} 

settingVariables_Hogares<-function(personas,Hogares){
  sum_Hogar<-personas %>% 
    group_by(id) %>% 
    summarize(Ingtot_hogar  = NotInTest(sum(Ingtot,na.rm = TRUE)),
              Hombres       = sum(P6020_1,na.rm = TRUE),
              Mujeres       = sum(P6020_0,na.rm = TRUE),
              Pareja        = sum(P6050_2,na.rm = TRUE),
              Hijos         = sum(P6050_3,na.rm = TRUE),
              Nietos        = sum(P6050_4,na.rm = TRUE),
              EdadPromedio  = mean(P6040,na.rm = TRUE),
              SSalud        = sum(P6090,na.rm = TRUE),
              Trabajan      = sum(P6240_1,na.rm = TRUE),
              Estudiantes   = sum(P6240_3,na.rm = TRUE),
              Subsidios     = ifelse(any(P6585,na.rm = TRUE),1,0),
              HorasTrabajo  = sum(P6800,na.rm = TRUE),
              CotizaPension = ifelse(any(P6920_1,na.rm = TRUE),1,0),
              Pensionado = ifelse(any(P6920_3,na.rm = TRUE),1,0),
              OtroTrabajo   = sum(P7040,na.rm = TRUE),
              DeseaTrabajarMas   = sum(P7090,na.rm = TRUE),
              PrimerTrabajo = sum(P7310,na.rm = TRUE),
              DesReciente   = sum(P7422,na.rm = TRUE),
              Ingresos_AlquilerPensiones   = ifelse(any(P7495,na.rm = TRUE),1,0),
              Ingresos_Paternidad   = ifelse(any(P7500s3,na.rm = TRUE),1,0),
              OtrosIngresos= ifelse(any(P7505,na.rm = TRUE),1,0),
              AyudasEco= ifelse(any(P7510,na.rm = TRUE),1,0),
              Pet          = sum(Pet,na.rm = TRUE),
              Oc           = sum(Oc,na.rm = TRUE),
              Des          = sum(Des,na.rm = TRUE),
              Ina          = sum(Ina,na.rm = TRUE),
              Pea          = sum(Oc,na.rm = TRUE)+sum(Des,na.rm = TRUE)
    )
  
  JH<-personas %>% 
    filter(P6050_1==1)%>%
    group_by(id) %>% 
    summarize(JH_Ing           = NotInTest(mean(Ingtot,na.rm = TRUE)),
              JH_Mujer         = mean(P6020_0,na.rm = TRUE),
              JH_Edad          = mean(P6040,na.rm = TRUE),
              JH_RSS_S         = ifelse(is.na(P6100_3),0,P6100_3),
              JH_NEduc         = P6210,
              JH_Trabaja       = ifelse(P6240_1==1,1,0),
              JH_HorasTrabajo  = ifelse(is.na(P6800),0,P6800),
              JH_CotizaPension = ifelse(is.na(P6920_1),0,P6920_1),
              JH_Pensionado    = ifelse(is.na(P6920_3),0,P6920_3),
              JH_OtroTrabajo   = ifelse(is.na(P7040),0,P7040),
              JH_DeseaTrabajarMas   = ifelse(is.na(P7090),0,P7090),
              JH_PrimerTrabajo = ifelse(is.na(P7310),0,P7310),
              JH_DesReciente   = ifelse(is.na(P7422),0,P7422),
              JH_Oc            = ifelse(is.na(Oc),0,Oc),
              JH_Des           = ifelse(is.na(Des),0,Des),
              JH_Ina           = ifelse(is.na(Ina),0,Ina)
              
    )
  
  
  
  Hogares<- left_join(Hogares, sum_Hogar,by="id")
  Hogares<- left_join(Hogares, JH,by="id")
  

  Hogares[,"Clase"]<-sapply(Hogares[,"Clase"],  function(x) replace(x, x=="2", '0'))
  Hogares[,"Clase"] <- lapply(Hogares[,"Clase"] , as.numeric)
  
  factor_variables_Hogares<-c('Dominio','Depto','P5090','JH_NEduc')        
  Hogares[,factor_variables_Hogares] <- lapply(Hogares[,factor_variables_Hogares] , factor)

  #remplazar por 0 los missing values (ceros porque significa que no tienen ingresos o egresos de esos rubros)
  Hogares$P5100[is.na(Hogares$P5100)]<-0
  Hogares$P5130[is.na(Hogares$P5130)]<-0
  Hogares$P5140[is.na(Hogares$P5140)]<-0
  
  #estandarizar 
  estandarizar<-c('P5100','P5130','P5140','P5140','P5130','P5100','P5010','Nper','Hombres','Mujeres','Hijos','Nietos','EdadPromedio','SSalud','Trabajan','Estudiantes','HorasTrabajo','OtroTrabajo','DeseaTrabajarMas','PrimerTrabajo','DesReciente','Pet','Oc','Des','Ina','Pea','JH_Edad','JH_HorasTrabajo')
  Hogares<- Hogares %>%           
    mutate_at(estandarizar, ~(scale(.) %>% as.vector))
  
  Hogares$Lp=Hogares$Lp*Hogares$Npersug
  
  return(Hogares)
}


####IMPORTAR BASES 
train_hogares <- remove_val_labels(readRDS("stores/data/train_hogares.Rds"))
train_personas <- remove_val_labels(readRDS("stores/data/train_personas.Rds"))
test_hogares <- remove_val_labels(readRDS("stores/data/test_hogares.Rds"))
test_personas <- remove_val_labels(readRDS("stores/data/test_personas.Rds"))
###### PRE PROCESAMIENTO #### 

## Personas ##
train_personas<-settingVariables_Personas(train_personas)
test_personas<-settingVariables_Personas(test_personas)

###### Merge con Hogares#### 
train_hogares<-settingVariables_Hogares(train_personas,train_hogares)
test_hogares<-settingVariables_Hogares(test_personas,test_hogares)

saveRDS(train_hogares, file = "stores/train_hogares_full.rds")
saveRDS(test_hogares, file = "stores/test_hogares_full.rds")

rm(train_hogares,test_hogares,train_personas,test_personas)
