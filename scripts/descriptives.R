## Autores: Juan José Rincón , Juan Andres Ospina, Juanita Chacón 
## Descripción: Desarrollo 2 problem set /Big Data and Machine Leanring for applied economics
## Universidad de los Andes 2022-2
## Creation Date: 14/09/2022
#### setting the work space ####################################

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


pacman:: p_load(sf, sp, gridExtra, cowplot, classInt, gridExtra, grid,lessR,RColorBrewer,tidyverse,stargazer,caret,tidymodels,glmnet,parallel,doParallel,MLmetrics,themis,rattle,rlang,randomForest,mlr,rpart,rpart.plot,kableExtra)

train_hogares <- readRDS("stores/train_hogares_full.Rds")


##Distribuci�n de la pobreza
No_Pobres<-nrow(train_hogares[train_hogares$Pobre == 0,])/nrow(train_hogares)*100
Pobres<-nrow(train_hogares[train_hogares$Pobre == 1,])/nrow(train_hogares)*100

pie(x = c(No_Pobres,Pobres),labels = c('No Pobres(20%)','Pobres(80%)'),radius = 1,col =c('lightblue','pink') )


Pobreza<-data.frame(pobres=table((train_hogares$Pobre)))


train_hogares_2<-train_hogares%>%
  mutate(Pobre=case_when(Pobre==1~"No Pobres",
                         Pobre==0~ "Pobres",
                         ))

PieChart(x=Pobre,data=train_hogares_2,hole=0,color = c("pink","purple"))

#Por sexo

sexo_base<-ggplot(data=train_hogares_2,aes(x=JH_Mujer,y=Pobre))+geom_bar(stat="identity")

pobreza_sexo<-ggplot(data=train_hogares_2,aes(y=JH_Mujer,x=Pobre))+geom_bar(stat = "identity", col="pink")+theme(axis.text.y = element_blank())


#Por n�mero de hijos

pobreza_hijos<-ggplot(data=train_hogares)+geom_col(aes(x=Hijos,y=Pobre),col="pink")+theme(axis.text = element_blank())


#dpto
for(i in c(08,11,13,15,17,18,19,20,23,25,27,41,44,47,50,52,54,63,66,68,70,73,76)){
 a=parse("train_hogares")
  return(a)
}



