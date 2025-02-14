#Cargar todas las librer�as####
library(readr)
library(umap)
library(ggplot2)
library(plotly)
library(Hmisc)
library(PerformanceAnalytics)
library(psych)
library(heatmaply)
library(ggridges)
library(gridExtra)
library(corrplot)
library(corrgram)
library(ggbiplot)
library(plyr)
library(scales)
library(grid)
library(factoextra)
library(FactoMineR)
library(tidyverse)
library(cluster)
library(NbClust)
library(tidyr)
library(missForest)
library(effectsize)
library(haven)
library(apaTables)
library(DescTools)
library(report)
library(dplyr)
library(rstatix)
library(FSA)
library(rcompanion)


#Cargar base de datos####
BD <- read_csv("C:/Users/Admin/Desktop/Semestre_Enero_Julio_2021/Verano/Base de datos final para trabajar (1).csv")
BD<- read_csv("C:/Users/Carlos L Drago/Downloads/Base de datos final para trabajar.csv")
BD0<-data.frame(BD)


####Reemplazar por valores medios####
BD0$Peso[is.na(BD0$Peso)]<-mean(BD0$Peso,na.rm = TRUE)
BD0$Altura[is.na(BD0$Altura)]<-mean(BD0$Altura,na.rm = TRUE)
BD0$IMC[is.na(BD0$IMC)]<-mean(BD0$IMC,na.rm = TRUE)
BD0$Cintura [is.na(BD0$Cintura)]<-mean(BD0$Cintura,na.rm = TRUE)
BD0$Cadera[is.na(BD0$Cadera)]<-mean(BD0$Cadera,na.rm = TRUE)
BD0$Presi�n.sist�lica[is.na(BD0$Presi�n.sist�lica)]<-mean(BD0$Presi�n.sist�lica,na.rm = TRUE)
BD0$Presi�n.diast�lica[is.na(BD0$Presi�n.diast�lica)]<-mean(BD0$Presi�n.diast�lica,na.rm = TRUE)
BD0$Presi�n.media[is.na(BD0$Presi�n.media)]<-mean(BD0$Presi�n.media,na.rm = TRUE)
BD0$Menopausia[is.na(BD0$Menopausia)]<-mean(BD0$Menopausia,na.rm = TRUE)
BD0$Estr�s[is.na(BD0$Estr�s)]<-mean(BD0$Estr�s,na.rm = TRUE)
BD0$Nido.vac�o[is.na(BD0$Nido.vac�o)]<-mean(BD0$Nido.vac�o,na.rm = TRUE)
BD0$FSH.mIU.mL.[is.na(BD0$FSH.mIU.mL.)]<-mean(BD0$FSH.mIU.mL.,na.rm = TRUE)
BD0$Menarca[is.na(BD0$Menarca)]<-mean(BD0$Menarca,na.rm = TRUE)


#Colocando etiquetas para filtrar
BD0$Clase.BMI<-factor(BD0$Clase.BMI,
                      levels = c('0','1','2'),
                      labels = c('Normopeso','Sobrepeso','Obesidad'))

#Ejercicio
BD0$Ejercicio<-factor(BD0$Ejercicio,
                      levels = c('0','1'),
                      labels = c('No hace','S� hace'))

#Fumar
BD0$Fumar<-factor(BD0$Fumar,
                  levels = c('0','1'),
                  labels = c('No fuma','S� fuma'))

#Alcohol
BD0$Alcohol<-factor(BD0$Alcohol,
                    levels = c('0','1'),
                    labels = c('No toma','S� toma'))

#Clasificaci�n Straw

BD0$Clasificaci�n.Straw<-factor(BD0$Clasificaci�n.Straw,
                                levels = c('0','1','2'),
                                labels = c('Perimenopausia','Posmenopausia temprana','Posmenopausia tard�a'))

#Estadio menopausia

BD0$Estadio.de.la.menopausia<-factor(BD0$Estadio.de.la.menopausia,
                                     levels = c('0','1'),
                                     labels = c('Premenopausica','Menopausica'))

#Bochornos
BD0$Bochornos<-factor(BD0$Bochornos,
                      levels = c('0','1'),
                      labels = c('Ausentes','Presentes'))


#Correlaci�n entre las variables####

#Conjunto de datos sin variables categ�ricas
BD1<-BD0[,-28:-34]



#Clasificaci�n seg�n el resto de categor�as con base de datos completa##############################################

#Seg�n IMC
IMC.CN<-BD0 %>%
  filter(Clase.BMI =="Normopeso")
IMC.CN_<-IMC.CN[1:12,1:27]
IMC.CS<-BD0 %>%
  filter(Clase.BMI =="Sobrepeso")
IMC.CS_<-IMC.CS[1:39,1:27]
IMC.CO<-BD0 %>%
  filter(Clase.BMI =="Obesidad")
IMC.CO_<-IMC.CO[1:29,1:27]


#Seg�n actividad f�sica
Peri_2<-Peri %>%
  filter(Ejercicio =="S� hace")

Peri_2.1<-Peri %>%
  filter(Ejercicio =="No hace")


#Seg�n fumar
Peri_3<-Peri %>%
  filter(Fumar =="S� fuma")

Peri_3.1<-Peri %>%
  filter(Fumar =="No fuma")


#Seg�n tomar
Peri_4<-Peri %>%
  filter(Alcohol =="S� toma")
view(Peri_4)
Peri_4.1<-Peri %>%
  filter(Alcohol =="No toma")
view(Peri_4.1)

#Seg�n bochornos
Peri_5<-Peri %>%
  filter(Bochornos =="Presentes")
view(Peri_5)
Peri_5.1<-Peri %>%
  filter(Bochornos =="Ausentes")
view(Peri_5.1)

#Matriz de correlaci�n COMPLETA seg�n IMC#### 

#Normopeso
CorrelacionC<-round(cor(IMC.CN_), 2)
corrplot(CorrelacionC, method="number", type="upper", is.corr = TRUE, tl.cex = 0.3, number.cex = 0.2, cl.cex = 0.5) 
#Correlaci�n mixta COMPLETA
corrplot(CorrelacionC, method="number", type="upper", is.corr = FALSE, tl.cex = 0.3, number.cex = 0.3, cl.cex = 0.5)
corrplot.mixed(CorrelacionC, tl.cex = 0.1, number.cex = 0.2, cl.cex = 0.3)

CorrelacionC_<-as.data.frame(CorrelacionC)

CorrelacionC_1<-filter(CorrelacionC_,CorrelacionC_$S100B >='0.2' & CorrelacionC_$S100B <'1')


CorrelacionC_1.1<-filter(CorrelacionC_,CorrelacionC_$S100B >='-0.2' & CorrelacionC_$S100B <'-1')


Correlaci�nC_1.1.1<-rbind(CorrelacionC_1,CorrelacionC_1.1)


#Sobrepeso
CorrelacionC2<-round(cor(IMC.CS_), 2)
corrplot(CorrelacionC2, method="number", type="upper", is.corr = TRUE, tl.cex = 0.3, number.cex = 0.2, cl.cex = 0.5) 
#Correlaci�n mixta COMPLETA
corrplot(CorrelacionC2, method="number", type="upper", is.corr = FALSE, tl.cex = 0.3, number.cex = 0.3, cl.cex = 0.5)
corrplot.mixed(CorrelacionC2, tl.cex = 0.1, number.cex = 0.2, cl.cex = 0.3)

CorrelacionC_2<-as.data.frame(CorrelacionC2)

CorrelacionC_2.1<-filter(CorrelacionC_2,CorrelacionC_2$S100B >='0.2' & CorrelacionC_2$S100B <'1')


CorrelacionC_2.2<-filter(CorrelacionC_2,CorrelacionC_2$S100B >='-0.2' & CorrelacionC_2$S100B <'-1')


Correlaci�nC_2.1.1<-rbind(CorrelacionC_2.1,CorrelacionC_2.2)


#Obesidad
CorrelacionC3<-round(cor(IMC.CO_), 2)
corrplot(CorrelacionC3, method="number", type="upper", is.corr = TRUE, tl.cex = 0.3, number.cex = 0.2, cl.cex = 0.5) 
#Correlaci�n mixta COMPLETA
corrplot(CorrelacionC3, method="number", type="upper", is.corr = FALSE, tl.cex = 0.3, number.cex = 0.3, cl.cex = 0.5)
corrplot.mixed(CorrelacionC3, tl.cex = 0.1, number.cex = 0.2, cl.cex = 0.3)

CorrelacionC_3<-as.data.frame(CorrelacionC3)
View(CorrelacionC_3)
CorrelacionC_3.1<-filter(CorrelacionC_3,CorrelacionC_3$S100B >='0.2' & CorrelacionC_3$S100B <'1')
View(CorrelacionC_3.1)

CorrelacionC_3.2<-filter(CorrelacionC_3,CorrelacionC_3$S100B >='-0.2' & CorrelacionC_3$S100B <'-1')
View(CorrelacionC_3.2)

Correlaci�nC_3.1.1<-rbind(CorrelacionC_3.1,CorrelacionC_3.2)
view(Correlaci�nC_3.1.1)


#Conjuntos de bases de datos seg�n clasificaci�n straw##############################################################

#Secci�n de perimenopausia
Peri<-BD0 %>%
  filter(Clasificaci�n.Straw == "Perimenopausia")
Peri_<-Peri
  PeriF<-Peri_[1:31, 1:27]


#Secci�n de Menopausia temprana
PosTe<-BD0 %>%
  filter(Clasificaci�n.Straw == "Posmenopausia temprana")
view(PosTe)
PosTeF<-PosTe[1:27,1:27]
View(PosTeF) #Conjunto menopausia temprana

#Secci�n de menopausia tard�a
PosTA<-BD0 %>%
  filter(Clasificaci�n.Straw == "Posmenopausia tard�a")
PosTAF<-PosTA[1:22,1:27]
View(PosTAF) #Conjunto menopausia tard�a

#Secci�n de posmenopausia temprana y tard�a

PosTT<-BD0[-1:-31,-28:-34] #Conjunto de posmenopausia temprana y tard�a
view(PosTT)

#Matrices de correlaci�n####


#Matriz de correlaci�n PERIMENOPAUSIA###################################################################################

CorrelacionPER<-round(cor(PeriF), 2)

#Tabla de correlaciones
CorrelacionPER_<-as.data.frame(CorrelacionPER)
CorrelacionPER_1.1<-filter(CorrelacionPER_, CorrelacionPER_$S100B>='0.2' & CorrelacionPER_$S100B<'1')
CorrelacionPER_1.2<-filter(CorrelacionPER_, CorrelacionPER_$S100B>='-0.2' & CorrelacionPER_$S100B<='-1')
CorrelacionPER_1.3<-rbind(CorrelacionPER_1.1,CorrelacionPER_1.2)
view(CorrelacionPER_1.3)
#Ploteo de correlaciones
corrplot(CorrelacionPER, method="number", type="upper", is.corr = TRUE, tl.cex = 0.3, number.cex = 0.2, cl.cex = 0.5)
corrplot(CorrelacionPER, method="number", type="upper", is.corr = FALSE, tl.cex = 0.3, number.cex = 0.3, cl.cex = 0.5)
#Correlaci�n mixta PERIMENOPAUSIA
corrplot.mixed(CorrelacionPER, tl.cex = 0.1, number.cex = 0.2, cl.cex = 0.3)
pairs(PeriF)

#Matriz de correlaci�n POSMENOPAUSIA TEMPRANA###########################################################################
CorrelacionPERTE<-round(cor(PosTeF), 2)

#Tabla de correlaciones �tiles
CorrelacionPERTE_<-as.data.frame(CorrelacionPERTE)
CorrelacionPERTE_1.1<-filter(CorrelacionPERTE_, CorrelacionPERTE_$S100B>='0.2' & CorrelacionPERTE_$S100B<'1')
CorrelacionPERTE_1.2<-filter(CorrelacionPERTE_, CorrelacionPERTE_$S100B>='-0.2' & CorrelacionPERTE_$S100B<='-1')
CorrelacionPERTE_1.3<-rbind(CorrelacionPERTE_1.1,CorrelacionPERTE_1.2)
View(CorrelacionPERTE_1.3)

#Correlaci�n simple
corrplot(CorrelacionPERTE, method="number", type="upper", is.corr = TRUE, tl.cex = 0.3, number.cex = 0.2, cl.cex = 0.5) 
corrplot(CorrelacionPERTE, method="number", type="upper", is.corr = FALSE, tl.cex = 0.3, number.cex = 0.3, cl.cex = 0.5)
#Correlaci�n mixta POSMENOPAUSIA TEMPRANA
corrplot.mixed(CorrelacionPERTE, tl.cex = 0.1, number.cex = 0.2, cl.cex = 0.3)
pairs(PosTeF)

#Matriz de correlaci�n POSMENOPAUSIA TARDIA#############################################################################
CorrelacionPERTA<-round(cor(PosTAF), 2)

#Tabla de correlaciones �tiles
CorrelacionPERTA_<-as.data.frame(CorrelacionPERTA)
CorrelacionPERTA_1.1<-filter(CorrelacionPERTA_, CorrelacionPERTA_$S100B>='0.2' & CorrelacionPERTA_$S100B<'1')
CorrelacionPERTA_1.2<-filter(CorrelacionPERTA_, CorrelacionPERTA_$S100B>='-0.2' & CorrelacionPERTA_$S100B<='-1')
CorrelacionPERTA_1.3<-rbind(CorrelacionPERTA_1.1,CorrelacionPERTA_1.2)
View(CorrelacionPERTA_1.3)

#Correlaci�n simple
corrplot(CorrelacionPERTA, method="number", type="upper", is.corr = TRUE, tl.cex = 0.3, number.cex = 0.2, cl.cex = 0.5) 
corrplot(CorrelacionPERTA, method="number", type="upper", is.corr = FALSE, tl.cex = 0.3, number.cex = 0.3, cl.cex = 0.5)
#Correlaci�n mixta POSMENOPAUSIA TARDIA
corrplot.mixed(CorrelacionPERTA, tl.cex = 0.1, number.cex = 0.2, cl.cex = 0.3)


#Matriz de correlaci�n POSMENOPAUSIA TEMPRANA Y TARDIA##################################################################
CorrelacionTT<-round(cor(PosTT), 2)

#Tabla de correlaciones �tiles
CorrelacionTT_<-as.data.frame(CorrelacionTT)
CorrelacionTT_1.1<-filter(CorrelacionTT_, CorrelacionTT_$S100B>='0.2' & CorrelacionTT_$S100B<'1')
CorrelacionTT_1.2<-filter(CorrelacionTT_, CorrelacionTT_$S100B>='-0.2' & CorrelacionTT_$S100B<='-1')
CorrelacionTT_1.3<-rbind(CorrelacionTT_1.1,CorrelacionTT_1.2)
view(CorrelacionTT_1.3)

#Correlaci�n simple
corrplot(CorrelacionTT, method="number", type="upper", is.corr = TRUE, tl.cex = 0.3, number.cex = 0.2, cl.cex = 0.5) 
corrplot(CorrelacionTT, method="number", type="upper", is.corr = FALSE, tl.cex = 0.3, number.cex = 0.3, cl.cex = 0.5)
#Correlaci�n mixta POSMENOPAUSIA TEMPRANA Y TARD�A
corrplot.mixed(CorrelacionTT, tl.cex = 0.1, number.cex = 0.2, cl.cex = 0.3)
pairs(PosTT)


#Regresiones lineales para obtener valores de P, T y las otras cosas para perimenopausia####

####Modelo 1
R1<-lm(PeriF$S100B~PeriF$Peso, data=PeriF)
summary(R1)


####Modelo 2
R2<-lm(PeriF$S100B~PeriF$IMC, data=PeriF)
summary(R2)

####Modelo 3 ###Da un asterisco
R3<-lm(PeriF$S100B~PeriF$Cintura, data=PeriF)
summary(R3)
sd(PeriF$Cintura)
0.4562+PeriF$Cintura*0.3102
PeriF$S100B
####Modelo 4 ###Da un punto
R4<-lm(PeriF$S100B~PeriF$Cadera, data=PeriF)
summary(R4)

#Modelo 5
R5<-lm(PeriF$S100B~PeriF$Creatinina, data=PeriF)
summary(R5)

#Modelo 6
R6<-lm(PeriF$S100B~PeriF$DHEAS, data=PeriF)
summary(R6)

#Regresiones lineales para obtener valores de P, T y las otras cosas posmenopausia temprana y tard�a####
NomCol5<-colnames(PosTT)
BD1.5<-data.frame(cor(PosTT))

####Modelo 7 ###Da un punto
R7<-lm(PosTT$S100B~PosTT$Peso, data=PosTT)
summary(R7)

####Modelo 8
R8<-lm(PosTT$S100B~PosTT$IMC, data=PosTT)
summary(R8)

####Modelo 9
R9<-lm(PosTT$S100B~PosTT$Cintura, data=PosTT)
summary(R9)

####Modelo 10 ###Da un asterisco
R10<-lm(PosTT$S100B~PosTT$Cadera, data=PosTT)
summary(R10)
sd(PosTT$Cadera)
-23.8437+PosTT$Cadera*0.5413
PosTT$S100B

####Modelo 11
R11<-lm(PosTT$S100B~PosTT$Presi�n.sist�lica, data=PosTT)
summary(R11)


####Modelo 12 ###Da un asterisco
R12<-lm(PosTT$S100B~PosTT$Presi�n.diast�lica, data=PosTT)
summary(R12)
sd(PosTT$Presi�n.diast�lica)
-28.6964+PosTT$Presi�n.diast�lica*0.8210
PosTT$S100B
####Modelo 13 
R13<-lm(PosTT$S100B~PosTT$Presi�n.media, data=PosTT)
summary(R13)


#Modelo 14 ###Da un aster�sco
R14<-lm(PosTT$S100B~PosTT$Colesterol, data=PosTT)
summary(R14)
sd(PosTT$Colesterol)
-2.04530+PosTT$Colesterol*0.16723
PosTT$S100B
#Modelo 15  ###Da un asterisco
R15<-lm(PosTT$S100B~PosTT$No.HDL, data=PosTT)
summary(R15)
sd(PosTT$No.HDL)
4.03962+PosTT$No.HDL*0.17612
PosTT$S100B
#Modelo 16 ###Da dos asteriscos
R16<-lm(PosTT$S100B~PosTT$LDL, data=PosTT)
summary(R16)
sd(PosTT$LDL)
3.43977+PosTT$LDL*0.21300
PosTT$S100B
#Modelo 17 ###Da un asterisco
R17<-lm(PosTT$S100B~PosTT$Depresi�n, data=PosTT)
summary(R17)
sd(PosTT$Depresi�n)
24.8955+PosTT$Depresi�n*1.3702
PosTT$S100B
#Modelo 18 ###Da dos asteriscos
R18<-lm(PosTT$S100B~PosTT$Estr�s, data=PosTT)
summary(R18)
sd(PosTT$Estr�s)
59.5163-0.8852*PosTT$Estr�s
PosTT$S100B
#Modelo 19 ###Da un asterisco
R19<-lm(PosTT$S100B~PosTT$DHEAS, data=PosTT)
summary(R19)
sd(PosTT$DHEAS)
23.6015+PosTT$DHEAS*0.7373
PosTT$S100B
#Regresiones lineales para obtener valores de P, T y las otras cosas posmenopausia temprana####


####Modelo 20
R20<-lm(PosTeF$S100B~PosTeF$Peso, data=PosTeF)
summary(R20)


####Modelo 21
R21<-lm(PosTeF$S100B~PosTeF$IMC, data=PosTeF)
summary(R21)

####Modelo 22 ###Da un asterisco
R22<-lm(PosTeF$S100B~PosTeF$Cadera, data=PosTeF)
summary(R22)
sd(PosTeF$Cadera)
-32.5320+PosTeF$Cadera*0.6086
PosTeF$S100B
#Modelo 23
R23<-lm(PosTeF$S100B~PosTeF$Colesterol, data=PosTeF)
summary(R23)

#Modelo 24
R24<-lm(PosTeF$S100B~PosTeF$HDL, data=PosTeF)
summary(R24)

#Modelo 25
R25<-lm(PosTeF$S100B~PosTeF$No.HDL, data=PosTeF)
summary(R25)

#Modelo 26 ###Da un punto
R26<-lm(PosTeF$S100B~PosTeF$LDL, data=PosTeF)
summary(R26)

#Modelo 27
R27<-lm(PosTeF$S100B~PosTeF$Creatinina, data=PosTeF)
summary(R27)

#Modelo 28
R28<-lm(PosTeF$S100B~PosTeF$Nido.vac�o, data=PosTeF)
summary(R28)

#Modelo 29
R29<-lm(PosTeF$S100B~PosTeF$Estr�s, data=PosTeF)
summary(R29)

#Modelo 30
R30<-lm(PosTeF$S100B~PosTeF$Sumisi�n, data=PosTeF)
summary(R30)

#Modelo 31
R31<-lm(PosTeF$S100B~PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R31)

#Modelo 32
R32<-lm(PosTeF$S100B~PosTeF$DHEAS, data=PosTeF)
summary(R32)

#Modelo 33
R33<-lm(PosTeF$S100B~PosTeF$Menopausia, data=PosTeF)
summary(R33)

#Modelo 34
R34<-lm(PosTeF$S100B~PosTeF$Glucosa, data=PosTeF)
summary(R34)

#Regresiones lineales para obtener valores de P, T y las otras cosas posmenopausia tard�a####

####Modelo 35
R35<-lm(PosTAF$S100B~PosTAF$IMC, data=PosTAF)
summary(R35)

####Modelo 36
R36<-lm(PosTAF$S100B~PosTAF$Cintura, data=PosTAF)
summary(R36)


####Modelo 37 ###Da un punto
R37<-lm(PosTAF$S100B~PosTAF$Presi�n.sist�lica, data=PosTAF)
summary(R37)

####Modelo 38 ###Da un asterisco
R38<-lm(PosTAF$S100B~PosTAF$Presi�n.diast�lica, data=PosTAF)
summary(R38)
sd(PosTAF$Presi�n.diast�lica)
-97.045+PosTAF$Presi�n.diast�lica*1.765
PosTAF$S100B
####Modelo 39 ###Da un asterisco
R39<-lm(PosTAF$S100B~PosTAF$Presi�n.media, data=PosTAF)
summary(R39)
sd(PosTAF$Presi�n.media)
-137.157+PosTAF$Presi�n.media*1.947
PosTAF$S100B
#Modelo 40 ###Da un punto
R40<-lm(PosTAF$S100B~PosTAF$Colesterol, data=PosTAF)
summary(R40)

#Modelo 41 
R41<-lm(PosTAF$S100B~PosTAF$No.HDL, data=PosTAF)
summary(R41)

#Modelo 42 ###Da un punto
R42<-lm(PosTAF$S100B~PosTAF$LDL, data=PosTAF)
summary(R42)

#Modelo 43 ###Da un punto
R43<-lm(PosTAF$S100B~PosTAF$Depresi�n, data=PosTAF)
summary(R43)

#Modelo 44
R44<-lm(PosTAF$S100B~PosTAF$Nido.vac�o, data=PosTAF)
summary(R44)

#Modelo 45 ###Da un asterisco
R45<-lm(PosTAF$S100B~PosTAF$Estr�s, data=PosTAF)
summary(R45)
sd(PosTAF$Estr�s)
76.4300+PosTAF$Estr�s*-1.3494
PosTAF$S100B
#Modelo 46 ###Da un punto
R46<-lm(PosTAF$S100B~PosTAF$DHEAS, data=PosTAF)
summary(R46)


####Modelos de regresi�n lineal m�ltiple####

#Intentos de modelos con variables perimenopausia

R1.1<-lm(PeriF$S100B~PeriF$Peso+PeriF$IMC+PeriF$Cintura+PeriF$Cadera+PeriF$DHEAS+PeriF$Creatinina, data=PeriF)
summary(R1.1)

R1.2<-lm(PeriF$S100B~PeriF$Peso+PeriF$IMC+PeriF$Cintura+PeriF$Cadera+PeriF$DHEAS, data=PeriF)
summary(R1.2)

R1.3<-lm(PeriF$S100B~PeriF$IMC+PeriF$Cintura+PeriF$Cadera+PeriF$DHEAS, data=PeriF)
summary(R1.3)

R1.4<-lm(PeriF$S100B~PeriF$Cintura+PeriF$Cadera+PeriF$DHEAS, data=PeriF)
summary(R1.4)

R1.5<-lm(PeriF$S100B~PeriF$Cintura+PeriF$Cadera+PeriF$DHEAS+PeriF$Creatinina, data=PeriF)
summary(R1.5)

R1.6<-lm(PeriF$S100B~PeriF$Cintura+PeriF$DHEAS, data=PeriF) #Mejor modelo para perimenopausia simple
summary(R1.6)

-13.2949436+0.3830256*PeriF$Cintura+0.3944542*PeriF$DHEAS
PeriF$S100B

#Intentos de modelos con variables de posmenopausia temprana y tard�a

R2.1<-lm(PosTT$S100B~PosTT$Peso+PosTT$IMC+PosTT$Cintura+PosTT$Cadera+PosTT$Presi�n.sist�lica+PosTT$Presi�n.diast�lica+PosTT$Presi�n.media+PosTT$Colesterol+PosTT$No.HDL+PosTT$LDL+PosTT$Depresi�n+PosTT$DHEAS+PosTT$Estr�s, data=PosTT)
summary(R2.1)

R2.2<-lm(PosTT$S100B~PosTT$Peso+PosTT$IMC+PosTT$Cadera+PosTT$Presi�n.sist�lica+PosTT$Presi�n.diast�lica+PosTT$Presi�n.media+PosTT$Colesterol+PosTT$No.HDL+PosTT$LDL+PosTT$Depresi�n+PosTT$DHEAS+PosTT$Estr�s, data=PosTT)
summary(R2.2)

R2.3<-lm(PosTT$S100B~PosTT$Peso+PosTT$IMC+PosTT$Cadera+PosTT$Presi�n.diast�lica+PosTT$Presi�n.media+PosTT$Colesterol+PosTT$No.HDL+PosTT$LDL+PosTT$Depresi�n+PosTT$DHEAS+PosTT$Estr�s, data=PosTT)
summary(R2.3)

R2.4<-lm(PosTT$S100B~PosTT$Peso+PosTT$IMC+PosTT$Cadera+PosTT$Presi�n.diast�lica+PosTT$Colesterol+PosTT$No.HDL+PosTT$LDL+PosTT$Depresi�n+PosTT$DHEAS+PosTT$Estr�s, data=PosTT)
summary(R2.4)

R2.5<-lm(PosTT$S100B~PosTT$Peso+PosTT$Cadera+PosTT$Presi�n.diast�lica+PosTT$Colesterol+PosTT$No.HDL+PosTT$LDL+PosTT$Depresi�n+PosTT$DHEAS+PosTT$Estr�s, data=PosTT)
summary(R2.5)

R2.6<-lm(PosTT$S100B~PosTT$Cadera+PosTT$Presi�n.diast�lica+PosTT$Colesterol+PosTT$No.HDL+PosTT$LDL+PosTT$Depresi�n+PosTT$DHEAS+PosTT$Estr�s, data=PosTT)
summary(R2.6)

R2.7<-lm(PosTT$S100B~PosTT$Cadera+PosTT$Colesterol+PosTT$No.HDL+PosTT$LDL+PosTT$Depresi�n+PosTT$DHEAS+PosTT$Estr�s, data=PosTT) 
summary(R2.7)

R2.8<-lm(PosTT$S100B~PosTT$Colesterol+PosTT$No.HDL+PosTT$LDL+PosTT$Depresi�n+PosTT$DHEAS+PosTT$Estr�s, data=PosTT) ####Mejor aproximaci�n de modelo para posmenopausia temprana y tard�a
summary(R2.8)
9.712264+PosTT$Colesterol*0.055265+PosTT$No.HDL*-0.005474+PosTT$LDL*0.131454+PosTT$Depresi�n*0.673416+0.559485*PosTT$DHEAS+PosTT$Estr�s*-0.518643
PosTT$S100B

R2.9<-lm(PosTT$S100B~PosTT$Colesterol+PosTT$No.HDL+PosTT$LDL+PosTT$Depresi�n+PosTT$DHEAS, data=PosTT) 
summary(R2.9)

#Intentos de modelos con variables de posmenopausia temprana 
#Modelo 32
R3.1<-lm(PosTeF$S100B~PosTeF$Peso+PosTeF$IMC+PosTeF$Cadera+PosTeF$Colesterol+PosTeF$No.HDL+PosTeF$LDL+PosTeF$DHEAS+PosTeF$Menopausia+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Nido.vac�o+PosTeF$Estr�s+PosTeF$Sumisi�n+PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R3.1)
R3.2<-lm(PosTeF$S100B~PosTeF$IMC+PosTeF$Cadera+PosTeF$Colesterol+PosTeF$No.HDL+PosTeF$LDL+PosTeF$DHEAS+PosTeF$Menopausia+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Nido.vac�o+PosTeF$Estr�s+PosTeF$Sumisi�n+PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R3.2)
R3.3<-lm(PosTeF$S100B~PosTeF$Cadera+PosTeF$Colesterol+PosTeF$No.HDL+PosTeF$LDL+PosTeF$DHEAS+PosTeF$Menopausia+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Nido.vac�o+PosTeF$Estr�s+PosTeF$Sumisi�n+PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R3.3)
R3.4<-lm(PosTeF$S100B~PosTeF$Cadera+PosTeF$No.HDL+PosTeF$LDL+PosTeF$DHEAS+PosTeF$Menopausia+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Nido.vac�o+PosTeF$Estr�s+PosTeF$Sumisi�n+PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R3.4)
R3.5<-lm(PosTeF$S100B~PosTeF$Cadera+PosTeF$LDL+PosTeF$DHEAS+PosTeF$Menopausia+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Nido.vac�o+PosTeF$Estr�s+PosTeF$Sumisi�n+PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R3.5)
R3.6<-lm(PosTeF$S100B~PosTeF$Cadera+PosTeF$LDL+PosTeF$Menopausia+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Nido.vac�o+PosTeF$Estr�s+PosTeF$Sumisi�n+PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R3.6)
R3.7<-lm(PosTeF$S100B~PosTeF$Cadera+PosTeF$LDL+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Nido.vac�o+PosTeF$Estr�s+PosTeF$Sumisi�n+PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R3.7)
R3.8<-lm(PosTeF$S100B~PosTeF$Cadera+PosTeF$LDL+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Estr�s+PosTeF$Sumisi�n+PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R3.8)
R3.9<-lm(PosTeF$S100B~PosTeF$Cadera+PosTeF$LDL+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Estr�s+PosTeF$FSH.mIU.mL., data=PosTeF)
summary(R3.9)
R3.10<-lm(PosTeF$S100B~PosTeF$Cadera+PosTeF$LDL+PosTeF$Glucosa+PosTeF$Creatinina+PosTeF$Sumisi�n+PosTeF$FSH.mIU.mL., data=PosTeF) #Mejor modelo con m�s varibles
summary(R3.10)
38.44329+0.36224*PosTeF$Cadera+0.17246*PosTeF$LDL-0.39886*PosTeF$Glucosa-26.18770*PosTeF$Creatinina-0.26213*PosTeF$Sumisi�n-0.06857*PosTeF$FSH.mIU.mL.
PosTeF$S100B
R3.11<-lm(PosTeF$S100B~PosTeF$LDL+PosTeF$Cadera, data=PosTeF) #Mejor modelo con menos variables
summary(R3.11)
-39.42453+PosTeF$LDL*0.11173+PosTeF$Cadera*0.53197
PosTeF$S100B
#Intentos de modelos con variables de posmenopausia tard�a
R4.1<-lm(PosTAF$S100B~PosTAF$IMC+PosTAF$Cintura+PosTAF$Presi�n.sist�lica+PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$Colesterol+PosTAF$No.HDL+PosTAF$LDL+PosTAF$Depresi�n+PosTAF$Nido.vac�o+PosTAF$DHEAS+PosTAF$Estr�s, data=PosTAF)
summary(R4.1)
R4.2<-lm(PosTAF$S100B~PosTAF$Cintura+PosTAF$Presi�n.sist�lica+PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$Colesterol+PosTAF$No.HDL+PosTAF$LDL+PosTAF$Depresi�n+PosTAF$Nido.vac�o+PosTAF$DHEAS+PosTAF$Estr�s, data=PosTAF)
summary(R4.2)
R4.3<-lm(PosTAF$S100B~PosTAF$Presi�n.sist�lica+PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$Colesterol+PosTAF$No.HDL+PosTAF$LDL+PosTAF$Depresi�n+PosTAF$Nido.vac�o+PosTAF$DHEAS+PosTAF$Estr�s, data=PosTAF)
summary(R4.3)
R4.4<-lm(PosTAF$S100B~PosTAF$Presi�n.sist�lica+PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$Colesterol+PosTAF$LDL+PosTAF$Depresi�n+PosTAF$Nido.vac�o+PosTAF$DHEAS+PosTAF$Estr�s, data=PosTAF)
summary(R4.4)
R4.5<-lm(PosTAF$S100B~PosTAF$Presi�n.sist�lica+PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$Colesterol+PosTAF$LDL+PosTAF$Depresi�n+PosTAF$DHEAS+PosTAF$Estr�s, data=PosTAF)
summary(R4.5)
R4.6<-lm(PosTAF$S100B~PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$Colesterol+PosTAF$LDL+PosTAF$Depresi�n+PosTAF$DHEAS+PosTAF$Estr�s, data=PosTAF)
summary(R4.6)
R4.7<-lm(PosTAF$S100B~PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$LDL+PosTAF$Depresi�n+PosTAF$DHEAS+PosTAF$Estr�s, data=PosTAF)
summary(R4.7)
R4.8<-lm(PosTAF$S100B~PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$Depresi�n+PosTAF$DHEAS+PosTAF$Estr�s, data=PosTAF) #Mejor modelo con m�s variables
summary(R4.8)
R4.9<-lm(PosTAF$S100B~PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$DHEAS+PosTAF$Estr�s, data=PosTAF) #Mejor modelo
summary(R4.9)
-56.8513+PosTAF$Presi�n.diast�lica*0.1870+PosTAF$Presi�n.media*1.1009+PosTAF$DHEAS*0.9082+PosTAF$Estr�s*-1.0071
PosTAF$S100B
R4.10<-lm(PosTAF$S100B~PosTAF$Presi�n.diast�lica+PosTAF$Presi�n.media+PosTAF$Estr�s, data=PosTAF)
summary(R4.10)



#An�lisis PCA para perimenopausia####
RPCA1<-prcomp(PeriF, scale = TRUE) #Resultados del PCA
summary(RPCA1)
#Data frame de componentes principales
CP<-RPCA1$x
CP<-as.data.frame(CP)
PeriF$PC1<-CP$PC1
PeriF$PC2<-CP$PC2

RPCA2<- PCA(X = PeriF, scale.unit = TRUE, ncp = 9, graph = TRUE)
summary(RPCA2)

#Visualizacion 2
fviz_eig(RPCA2) #visualizar eigenvalores (scree plot)
fviz_screeplot(RPCA2) #visualizar eigenvalores (scree plot)
fviz_pca_ind(RPCA2) #Representaci�n de observaciones sobre componentes principales.
fviz_pca_ind(RPCA2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE, labelsize=2)

fviz_pca_var(RPCA2) #Representaci�n de variables sobre componentes principales.

fviz_pca_var(RPCA2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(RPCA2,choice = "var") #Representa la contribuci�n de filas/columnas de los resultados de un pca.
fviz_contrib(RPCA2,choice = "ind")



#Clustering
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr")
ipak(packages)

#normalizar las puntuaciones
NP<-scale(PeriF)
NP
head(NP)#calcular la matriz de distacias
m.distancia <- get_dist(NP, method = "euclidean") #el m�todo aceptado tambi�n puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))#estimar el n�mero de cl�sters
#Elbow, silhouette o gap_stat method
fviz_nbclust(NP, kmeans, method = "wss")
fviz_nbclust(NP, kmeans, method = "silhouette")  #ccc, scott, marriot
fviz_nbclust(NP, kmeans, method = "gap_stat")#con esta funci�n se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust(NP, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

#K
k<-kmeans(NP, centers = 3, nstart = 25)
k
#plotear los cluster
fviz_cluster(k, data = NP, )
fviz_cluster(k, data = NP, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k, data = NP, ellipse.type = "norm", labelsize = 8)
fviz_cluster(k, data = NP, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())


res <- hcut(NP, k = 3, stand = TRUE)
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))

res2 <- hcut(NP, k = 4, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))

#Pasar clusters al dataframe inicial
PeriF %>%
  mutate(Cluster = k$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

NP<- PeriF
NP<- scale(NP)
NP<- as.data.frame(NP)
NP$clus<-as.factor(k$cluster)
NP

NP$clus<-factor(NP$clus)
names(PeriF)
data_long <- gather(NP, caracteristica, valor,Edad:DHEAS, factor_key=TRUE)
data_long

ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus),cex.) +
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")+labs(x="Caracter�stica", y="Valor")+theme(axis.title.x = element_text(size = rel(1)))

#An�lisis PCA para posmenopausia temprana y tard�a####

RPCA3<-prcomp(PosTT, scale = TRUE) #Resultados del PCA
summary(RPCA3)
#Data frame de componentes principales
CP1<-RPCA3$x
CP1<-as.data.frame(CP1)
PosTT$PC1<-CP1$PC1
PosTT$PC2<-CP1$PC2

RPCA4<- PCA(X = PosTT, scale.unit = TRUE, ncp = 9, graph = TRUE)
summary(RPCA4)

#Visualizacion 2
fviz_eig(RPCA4) #visualizar eigenvalores (scree plot)
fviz_screeplot(RPCA4) #visualizar eigenvalores (scree plot)
fviz_pca_ind(RPCA4) #Representaci�n de observaciones sobre componentes principales.
fviz_pca_ind(RPCA4,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE, labelsize=2)

fviz_pca_var(RPCA4) #Representaci�n de variables sobre componentes principales.

fviz_pca_var(RPCA4,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(RPCA4,choice = "var") #Representa la contribuci�n de filas/columnas de los resultados de un pca.
fviz_contrib(RPCA4,choice = "ind")

#Clustering
#Normalizar las puntuaciones
NP2<-scale(PosTT)
NP2
head(NP2)#calcular la matriz de distacias
m.distancia2 <- get_dist(NP2, method = "euclidean") #el m�todo aceptado tambi�n puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia2, gradient = list(low = "blue", mid = "white", high = "red"))#estimar el n�mero de cl�sters
#Elbow, silhouette o gap_stat method
fviz_nbclust(NP2, kmeans, method = "wss")
fviz_nbclust(NP2, kmeans, method = "silhouette")  #ccc, scott, marriot
fviz_nbclust(NP2, kmeans, method = "gap_stat")#con esta funci�n se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust2<-NbClust(NP2, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust2)

#K
k2<-kmeans(NP2, centers = 3, nstart = 25)
k2
#plotear los cluster
fviz_cluster(k2, data = NP2, )
fviz_cluster(k2, data = NP2, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = NP2, ellipse.type = "norm", labelsize = 8)
fviz_cluster(k2, data = NP2, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())


res2 <- hcut(NP2, k = 3, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))

res3 <- hcut(NP2, k = 4, stand = TRUE)
fviz_dend(res3, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))

#Pasar clusters al dataframe inicial
PosTT %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

NP2<- PosTT
NP2<- scale(NP2)
NP2<- as.data.frame(NP2)
NP2$clus<-as.factor(k2$cluster)
NP2

NP2$clus<-factor(NP2$clus)
names(PosTT)
data_long2 <- gather(NP2, caracteristica2, valor,Edad:DHEAS, factor_key=TRUE)
data_long2

ggplot(data_long2, aes(as.factor(x = caracteristica2), y = valor,group=clus, colour = clus),cex.) +
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")+labs(x="Caracter�stica", y="Valor")+theme(axis.title.x = element_text(size = rel(1)))

#An�lisis PCA para posmenopausia temprana####

RPCA5<-prcomp(PosTeF, scale = TRUE) #Resultados del PCA
summary(RPCA4)
#Data frame de componentes principales
CP3<-RPCA5$x
CP3<-as.data.frame(CP3)
PosTeF$PC1<-CP3$PC1
PosTeF$PC2<-CP3$PC2

RPCA6<- PCA(X = PosTeF, scale.unit = TRUE, ncp = 9, graph = TRUE)
summary(RPCA6)

#Visualizacion 2
fviz_eig(RPCA6) #visualizar eigenvalores (scree plot)
fviz_screeplot(RPCA6) #visualizar eigenvalores (scree plot)
fviz_pca_ind(RPCA6) #Representaci�n de observaciones sobre componentes principales.
fviz_pca_ind(RPCA6,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE, labelsize=2)

fviz_pca_var(RPCA6) #Representaci�n de variables sobre componentes principales.

fviz_pca_var(RPCA6,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(RPCA6,choice = "var") #Representa la contribuci�n de filas/columnas de los resultados de un pca.
fviz_contrib(RPCA6,choice = "ind")
View(PosTeF)

#Clustering
#Normalizar las puntuaciones
NP3<-scale(PosTeF)
NP3
head(NP3)#calcular la matriz de distacias
m.distancia3 <- get_dist(NP3, method = "euclidean") #el m�todo aceptado tambi�n puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia3, gradient = list(low = "blue", mid = "white", high = "red"))#estimar el n�mero de cl�sters
#Elbow, silhouette o gap_stat method
fviz_nbclust(NP3, kmeans, method = "wss")
fviz_nbclust(NP3, kmeans, method = "silhouette")  #ccc, scott, marriot
fviz_nbclust(NP3, kmeans, method = "gap_stat")#con esta funci�n se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust3<-NbClust(NP3, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust3)

#K
k3<-kmeans(NP3, centers = 3, nstart = 25)
k3
#plotear los cluster
fviz_cluster(k3, data = NP3, )
fviz_cluster(k3, data = NP3, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k3, data = NP3, ellipse.type = "norm", labelsize = 8)
fviz_cluster(k3, data = NP3, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())


res5 <- hcut(NP3, k = 3, stand = TRUE)
fviz_dend(res5, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))

res6 <- hcut(NP3, k = 4, stand = TRUE)
fviz_dend(res6, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))

#Pasar clusters al dataframe inicial
PosTeF %>%
  mutate(Cluster = k3$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

NP3<- PosTeF
NP3<- scale(NP3)
NP3<- as.data.frame(NP3)
NP3$clus<-as.factor(k3$cluster)
NP3

NP3$clus<-factor(NP3$clus)
names(PosTeF)
data_long3 <- gather(NP3, caracteristica3, valor,Edad:DHEAS, factor_key=TRUE)
data_long3

ggplot(data_long3, aes(as.factor(x = caracteristica3), y = valor,group=clus, colour = clus),cex.) +
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")+labs(x="Caracter�stica", y="Valor")+theme(axis.title.x = element_text(size = rel(1)))


#An�lisis PCA para posmenopausia tard�a####

RPCA6<-prcomp(PosTAF, scale = TRUE) #Resultados del PCA
summary(RPCA6)
#Data frame de componentes principales
CP4<-RPCA6$x
CP4<-as.data.frame(CP4)
PosTAF$PC1<-CP4$PC1
PosTAF$PC2<-CP4$PC2

RPCA7<- PCA(X = PosTAF, scale.unit = TRUE, ncp = 9, graph = TRUE)
summary(RPCA7)

#Visualizacion 2
fviz_eig(RPCA7) #visualizar eigenvalores (scree plot)
fviz_screeplot(RPCA7) #visualizar eigenvalores (scree plot)
fviz_pca_ind(RPCA7) #Representaci�n de observaciones sobre componentes principales.
fviz_pca_ind(RPCA7,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE, labelsize=2)

fviz_pca_var(RPCA7) #Representaci�n de variables sobre componentes principales.

fviz_pca_var(RPCA7,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(RPCA7,choice = "var") #Representa la contribuci�n de filas/columnas de los resultados de un pca.
fviz_contrib(RPCA7,choice = "ind")

#Clustering
#Normalizar las puntuaciones
NP4<-scale(PosTAF)
NP4
head(NP4)#calcular la matriz de distacias
m.distancia4 <- get_dist(NP4, method = "euclidean") #el m�todo aceptado tambi�n puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia4, gradient = list(low = "blue", mid = "white", high = "red"))#estimar el n�mero de cl�sters
#Elbow, silhouette o gap_stat method
fviz_nbclust(NP4, kmeans, method = "wss")
fviz_nbclust(NP4, kmeans, method = "silhouette")  #ccc, scott, marriot
fviz_nbclust(NP4, kmeans, method = "gap_stat")#con esta funci�n se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust4<-NbClust(NP4, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust4)

#K
k4<-kmeans(NP4, centers = 3, nstart = 25)
k4
#plotear los cluster
fviz_cluster(k4, data = NP4, )
fviz_cluster(k4, data = NP4, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k4, data = NP4, ellipse.type = "norm", labelsize = 8)
fviz_cluster(k4, data = NP4, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())


res7 <- hcut(NP4, k = 3, stand = TRUE)
fviz_dend(res7, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))

res8 <- hcut(NP4, k = 4, stand = TRUE)
fviz_dend(res8, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))

#Pasar clusters al dataframe inicial
PosTAF %>%
  mutate(Cluster = k4$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

NP4<- PosTAF
NP4<- scale(NP4)
NP4<- as.data.frame(NP4)
NP4$clus<-as.factor(k4$cluster)
NP4

NP4$clus<-factor(NP4$clus)
names(PosTAF)
data_long4 <- gather(NP4, caracteristica4, valor,Edad:DHEAS, factor_key=TRUE)
data_long4

ggplot(data_long4, aes(as.factor(x = caracteristica4), y = valor,group=clus, colour = clus),cex.) +
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")+labs(x="Caracter�stica", y="Valor")+theme(axis.title.x = element_text(size = rel(1)))





