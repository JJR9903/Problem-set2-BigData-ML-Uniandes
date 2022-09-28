##model matrix 
train<-model.matrix(object = ~ Ingtotugarr + Npersug + Clase  + Dominio + P5010 + P5090 + P5100 + P5130 + P5140 + 
                      Nper + Depto + Hombres + Mujeres  + Pareja + Hijos + Nietos + 
                     + SSalud + Trabajan + Estudiantes + Subsidios + 
                      HorasTrabajo + CotizaPension + OtroTrabajo + DeseaTrabajarMas + 
                      PrimerTrabajo + DesReciente + Ingresos_AlquilerPensiones + 
                      Ingresos_Paternidad + OtrosIngresos + AyudasEco + Pet + Oc + 
                      Des + Ina + Pea + JH_Ing + JH_Mujer + JH_Edad + JH_RSS_S + 
                      JH_NEduc + JH_Trabaja + JH_HorasTrabajo + JH_CotizaPension + 
                      JH_OtroTrabajo + JH_DeseaTrabajarMas + JH_PrimerTrabajo + JH_Edad2 +
                      JH_DesReciente + JH_Oc + JH_Des + JH_Ina,data=train_hogares)

##model matrix 
train<-model.matrix(object = ~ Ingtotugarr+JH_Edad2+JH_Edad+Depto + Hombres + Mujeres+
                      SSalud+ P_o + JH_RSS_S + P5100+ P5090+CotizaPension+ , data=train_hogares)