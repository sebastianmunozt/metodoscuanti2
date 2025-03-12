#00. Cargar paquetes

pacman::p_load(openxlsx,tidyverse,readxl, fastDummies, purrr, stargazer)

#01. Cargar datos

muni<-read_xlsx("IMS.xlsx")

#0.2 Recodificaciones-------------

#Tipo de vivienda

muni$TIPODEVIVIENDA <- factor(muni$TIPODEVIVIENDA, levels = c("1", "2", "3"), labels = c("Casa", "Departamento", "Pieza en casa"))

#Territorio

muni$TERRITORIO <- factor(muni$TERRITORIO, levels = c("1","2","3","4","5","6"), labels = c("Nororiente",
                                                                                           "Norponiente",
                                                                                           "Centro oriente",
                                                                                           "Centro poniente",
                                                                                           "Suroriente",
                                                                                           "Surponiente"))
#Sexo
muni$SEXO<- factor(muni$SEXO, levels = c("1","2"), labels = c("Mujer","Hombre"))

#p1. Tramo de edad

muni$P1 <- factor(muni$P1, levels = c("1","2","3","4","5"), labels = c("18 a 29 años",
                                                                       "30 a 44 años",
                                                                       "45 a 59 años",
                                                                       "60 a 74 años",
                                                                       "75 años o más"))
#p2. Nacionalidad

muni$P2 <- factor(muni$P2, levels = c("1","2"), labels=c("Chilena (exclusivamente)","Otra"))


#Correlación

cor(muni$ÍND_PROBLEMASAMBIENTALES,muni$ÍND_BSPERSONA)


#Regresión lineal--------------


#Filtro: Nororiente
muni_nororiente<-muni %>% filter(TERRITORIO=="Nororiente") 
muni_norponiente <- muni %>%   filter(TERRITORIO=="Norponiente")
muni_centroriente <- muni %>%  filter(TERRITORIO=="Centro oriente")
muni_centroponiente <- muni %>%  filter(TERRITORIO == "Centro poniente")
muni_suroriente <- muni %>%  filter(TERRITORIO == "Suroriente")
muni_surponiente <- muni %>%  filter(TERRITORIO == "Surponiente")

#Regresiones ÍND_BSPERSONA


m11<-lm(ÍND_BSPERSONA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_nororiente)
m12<-lm(ÍND_BSPERSONA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_norponiente)
m13<-lm(ÍND_BSPERSONA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_centroriente)
m14<-lm(ÍND_BSPERSONA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_centroponiente)
m15<-lm(ÍND_BSPERSONA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_suroriente)
m16<-lm(ÍND_BSPERSONA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_surponiente)



#Ver resultados

summary(m11)
summary(m12)
summary(m13)
summary(m14)
summary(m15)

#Tabla fancy con stargazer

models_list <- list(m11,
                    m12,
                    m13,
                    m14,
                    m15,
                    m16)


stargazer(models_list,
          type = "html",
          intercept.bottom = TRUE,
          intercept.top = FALSE,
          ci = FALSE,
          digits = 3,
          notes = "*** p < 0.001; ** p < 0.01; * p < 0.05",
          model.names = TRUE,
          single.row = TRUE,
          column.labels = c("Nororiente", "Norponiente", "Centro Oriente", "Centro Poniente", "Sur Oriente", "Sur Poniente"),
          dep.var.labels = c("Índice de Bienestar Persona"),
          covariate.labels = c("Intercepto (constante)","Tipo de vivienda Departamento ","Tipo de vivienda Pieza en casa ", "Sexo Hombre", "30 a 44 años","45 a 59 años", "60 a 74 años", "75 años o más", "Nacionalidad Extranjera"))





#Regresiones ÍND_BSPERSONA


m21<-lm(ÍND_BSCOMUNA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_nororiente)
m22<-lm(ÍND_BSCOMUNA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_norponiente)
m23<-lm(ÍND_BSCOMUNA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_centroriente)
m24<-lm(ÍND_BSCOMUNA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_centroponiente)
m25<-lm(ÍND_BSCOMUNA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_suroriente)
m26<-lm(ÍND_BSCOMUNA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_surponiente)



#Tabla fancy con stargazer

models_list <- list(m21,
                    m22,
                    m23,
                    m24,
                    m25,
                    m26)


stargazer(models_list,
          type = "html",
          intercept.bottom = TRUE,
          intercept.top = FALSE,
          ci = FALSE,
          digits = 3,
          notes = "*** p < 0.001; ** p < 0.01; * p < 0.05",
          model.names = TRUE,
          single.row = TRUE,
          column.labels = c("Nororiente", "Norponiente", "Centro Oriente", "Centro Poniente", "Sur Oriente", "Sur Poniente"),
          dep.var.labels = c("índice de Bienestar Comuna"),
          covariate.labels = c("Intercepto (constante)","Tipo de vivienda Departamento ","Tipo de vivienda Pieza en casa ", "Sexo Hombre", "30 a 44 años","45 a 59 años", "60 a 74 años", "75 años o más", "Nacionalidad Extranjera"))




#Regresiones ÍND_SITVIVIENDA


m31<-lm(ÍND_SITVIVIENDA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_nororiente)
m32<-lm(ÍND_SITVIVIENDA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_norponiente)
m33<-lm(ÍND_SITVIVIENDA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_centroriente)
m34<-lm(ÍND_SITVIVIENDA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_centroponiente)
m35<-lm(ÍND_SITVIVIENDA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_suroriente)
m36<-lm(ÍND_SITVIVIENDA ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_surponiente)



#Tabla fancy con stargazer

models_list <- list(m31,
                    m32,
                    m33,
                    m34,
                    m35,
                    m36)


stargazer(models_list,
          type = "html",
          intercept.bottom = TRUE,
          intercept.top = FALSE,
          ci = FALSE,
          digits = 3,
          notes = "*** p < 0.001; ** p < 0.01; * p < 0.05",
          model.names = TRUE,
          single.row = TRUE,
          column.labels = c("Nororiente", "Norponiente", "Centro Oriente", "Centro Poniente", "Sur Oriente", "Sur Poniente"),
          dep.var.labels = c("índice de Situación Vivienda"),
          covariate.labels = c("Intercepto (constante)","Tipo de vivienda Departamento ","Tipo de vivienda Pieza en casa ", "Sexo Hombre", "30 a 44 años","45 a 59 años", "60 a 74 años", "75 años o más", "Nacionalidad Extranjera"))



#Regresiones ÍND_SITUACIONES


m41<-lm(ÍND_SITUACIONES ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_nororiente)
m42<-lm(ÍND_SITUACIONES ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_norponiente)
m43<-lm(ÍND_SITUACIONES ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_centroriente)
m44<-lm(ÍND_SITUACIONES ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_centroponiente)
m45<-lm(ÍND_SITUACIONES ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_suroriente)
m46<-lm(ÍND_SITUACIONES ~ TIPODEVIVIENDA+SEXO+P1+P2, weights= `FACTOR 1`, data= muni_surponiente)



#Tabla fancy con stargazer

models_list <- list(m41,
                    m42,
                    m43,
                    m44,
                    m45,
                    m46)


stargazer(models_list,
          type = "html",
          intercept.bottom = TRUE,
          intercept.top = FALSE,
          ci = FALSE,
          digits = 3,
          notes = "*** p < 0.001; ** p < 0.01; * p < 0.05",
          model.names = TRUE,
          single.row = TRUE,
          column.labels = c("Nororiente", "Norponiente", "Centro Oriente", "Centro Poniente", "Sur Oriente", "Sur Poniente"),
          dep.var.labels = c("índice de Situaciones"),
          covariate.labels = c("Intercepto (constante)","Tipo de vivienda Departamento ","Tipo de vivienda Pieza en casa ", "Sexo Hombre", "30 a 44 años","45 a 59 años", "60 a 74 años", "75 años o más", "Nacionalidad Extranjera"))