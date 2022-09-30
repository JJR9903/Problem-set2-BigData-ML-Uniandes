##model matrix 
train<-model.matrix(object = ~ Ingtotugarr+JH_Edad2+JH_Edad+Depto + Hombres + Mujeres+
                      SSalud+Clase+ P_o + JH_RSS_S + P5100+ P5090+CotizaPension+ , data=train_hogares)

##formula 
model_Ing<-formula(Ingtotugarr~JH_Edad2+JH_Edad+Depto + Hombres + Mujeres+
                     SSalud+Clase+ P_o + JH_RSS_S + P5100+ P5090+CotizaPension)
## Entrenamiento del modelo 

train<-model.matrix(object = ~ Ingtotugarr JH_Edad2+JH_Edad+Depto + Hombres + Mujeres+
                      SSalud+Clase+ P_o + JH_RSS_S + P5100+ P5090+CotizaPension)

Train_y<-data.frame(train[,c('Lp','Pobre')],rowIndex=as.numeric(rownames(train)))

##dento de las recomendaciones ta poner una interacción con seguridad social y la interacción de Clase 
##con depto pero no se si valgala pena hacela, revisemos sin eso primero 