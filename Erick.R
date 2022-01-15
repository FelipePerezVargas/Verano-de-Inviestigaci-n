#######################################################
#####                                             #####
#####             PROYECTO FINAL ERICK            #####
#####                                             #####
#######################################################
graphics.off() #Borramos los gráficos anteriores
rm(list = ls()) #Borramos todas las variables anteriores


#AVANCES CITOMETRIA

#Cargar librerías####
library(ggplot2)
library(Formula)
library(readr)
library(corrplot)
library(lattice)
library(survival)
library(Hmisc)
library(PerformanceAnalytics)
library(psych)
library(plotly)
library(heatmaply)
library(ggridges)
library(gridExtra)
library(naniar)
library(skimr)
#Crear los dataframe para trabajar ####
#Importar base de datos
#Crear primer dataframe
ND <- read_csv("C:/Users/Admin/Desktop/Semestre_Enero_Julio_2021/Verano/ND.csv")
ND <- data.frame(ND)
#Cambiamos los nombres de las variables 
names(ND)<-c('Edad', 'Peso', 'Altura', 'IMC',
             'grasa', 'Masa muscular', 'Cintura', 'Cadera',
             'Cuello' , 'TAS', 'TAD', 'TA', 'Fuerza de agarre',
             'Tiempo desde diagnostico', 'glucosa', 'HbA1c',
             'Col' , 'HDL', 'no-HDL', 'LDL', 'TG', 'Creat',
             'eTFG', 'Creat horaria', 'Microalb', 'Indice',
             'NCM_RAGE', 'NCM_S100A8/9', 'NCM_S100A12', 'NCM_S100B',
             'NCM_HMGB1', 'IM_RAGE', 'IM_S100A8/9', 'IM_S100A12',
             'IM_S100B', 'IM_HMGB1', 'CM_RAGE', 'CM_S100A8/9',
             'CM_S100A12', 'CM_S100B', 'CM_HMGB1', 'PMN_S100A8/9', 
             'PMN_S100A12', 'PMN_S100B', 'PMN_HMGB1', 'Sexo', 
             'clas IMC', 'Clasif_sarcopenia', 'Tabaquismo',
             'Alcoholismo', 'Ejercicio', 'Acantosis' , 'Dx',
             'Ctrl gluc', 'ERC', 'No')

rownames(ND)<-ND$No
ND$No<-NULL

#Asignar etiquetas a variables categoricas
ND$Sexo<-factor(ND$Sexo,levels = c('0','1'), labels = c('H','M'))
ND$`clas IMC`<-factor(ND$`clas IMC`,levels = c('0','1','2'), labels = c('Normopeso','Sobrepeso','Obesidad'))
ND$Clasif_sarcopenia<-factor(ND$Clasif_sarcopenia,levels = c('0','1','2'), labels = c('Baja','Media','Alta'))
ND$Tabaquismo<-factor(ND$Tabaquismo,levels = c('0','1'), labels = c('No','Si'))
ND$Alcoholismo<-factor(ND$Alcoholismo,levels = c('0','1'), labels = c('No','Si'))
ND$Ejercicio<-factor(ND$Ejercicio,levels = c('0','1'), labels = c('No','Si'))
ND$Acantosis<-factor(ND$Acantosis,levels = c('0','1'), labels = c('No','Si'))
ND$Dx<-factor(ND$Dx,levels = c('0','1'), labels = c('Sano','DM'))
ND$`Ctrl gluc`<-factor(ND$`Ctrl gluc`,levels = c('0','1','2','3'), labels = c('Sano','Buen control','Control regular', 'Mal control'))
ND$ERC<-factor(ND$ERC,levels = c('0','1'), labels = c('Sin DR','Con DR'))
factor(ND$Acantosis)
levels(ND$Acantosis)

boxplot(ND$Peso~ND[,46], ylab='Peso (kg)', xlab = paste(names(ND[55])), 
        main=paste("Boxplot ",names(ND[55])))


#Boxplot Tipo de IMC
for (i in 46:55) {
  boxplot(ND$Peso~ND[,i], ylab='Peso (kg)', xlab = paste(names(ND[i])),
          main=paste("Boxplot ",names(ND[i])))
}
#Boxplot Sarcopenia 
for (i in 46:55) {
  boxplot(ND$Edad~ND[,i], ylab='Edad (Años)', xlab = paste(names(ND[i])), col=,
          main=paste("Boxplot ",names(ND[i])))
}

for (i in 46:55) {
ggplot(ND, aes(x=Edad, y=ND[,]))+geom_boxplot()+coord_flip()
}

pl <- ggplot(ND, aes(x=ND$NCM_S100B, y=Sexo))+
  geom_boxplot(aes(fill=factor(Acantosis)))+
  theme_bw()+coord_flip();pl

pl1 <- ggplot(ND, aes(x=Peso, y=Acantosis))+
  geom_boxplot(aes(fill=factor(Sexo)))+
  theme_bw();pl1

library(ggpubr)

ggline(ND, x=names(ND)[ND$Edad],y=names(ND)[ND$Tabaquismo], add=c('d', 'f'),
       palette='jco')

# Boxplot Sexo 
pdf(file = "boxf.pdf", width = 8, height = 10) #Guarda el gráfico en formato PDF
for (j in 1:45) {
  for (i in 46:55) {
    boxplot(ND[,j]~ND[,i],col=, ylab=paste(names(ND[j])), xlab = paste(names(ND[i])),
            main=paste("Boxplot ",names(ND[i])))}
}
dev.off()

#Anova

  ran<- aov(ND$Edad~ND$`clas IMC`, data = ND)  
  print(ran)
  summary.aov(ran)



for (i in 46:55) {
ran<- aov(ND$Edad~ND[,], data = ND)  
print(ran)
summary.aov(ran)
pairwise.t.test(ND$Edad,ND$`clas IMC`, p.adjust.method ='BH' )  
TukeyHSD(ran) 
}
pairwise.t.test(ND$Edad,ND$Ejercicio, p.adjust.method ='BH' )  
TukeyHSD(ran) 



  
#Remplazar valores no asignados por medias
ND <- ND %>% 
  impute_mean_if(is.numeric)
ND1<-ND[,-56]
#Sacamos las estadísticas vitales de todas las variables 
describe(ND)
#Calculamos las correlaciones entre ellas 
cor_to<-cor(ND)
#Convertimos en datafrema
cor_to<-as.data.frame(cor_to)

#Boxplot

library(ggpubr)
ggline(ND, x=ND$grasa, y=ND$Acantosis)

boxplot(grasa ~ Acantosis, data = ND)

#Separación de base de datos por genero
mujer<-filter(ND1,ND1[,46]=='1')
hombre<-filter(ND1,ND1[,46]=='0')
#Separación de base de datos por nivel de IMC
pn<-filter(ND1,ND1[,47]=='0')
sp<-filter(ND1,ND1[,47]=='1')
ob<-filter(ND1,ND1[,47]=='2')
#Separación de datos por Sarcopenía
sar0<-filter(ND1, ND1[,48]=='0')
sar1<-filter(ND1, ND1[,48]=='1')
sar2<-filter(ND1, ND1[,48]=='2')
#separación de datos por tabaquismo
nf<-filter(ND1, ND1[,49]=='0')
sf<-filter(ND1, ND1[,49]=='1')
#Separación de datos por Alcoholismo 
nb<-filter(ND1,ND1[,50]=='0')
sb<-filter(ND1,ND1[,50]=='1')
#Separación de datos por Ejercicio
nhe<-filter(ND1,ND1[,51]=='0')
she<-filter(ND1,ND1[,51]=='1')
#Separación de datos por Acantosis
nac<-filter(ND1,ND1[,52]=='0')
sac<-filter(ND1,ND1[,52]=='1')
#Separación de datos por Dx
ndx<-filter(ND1, ND1[,53]=='0') #Sano
sdx<-filter(ND1, ND1[,53]=='1') #Dm
#Separación de datos por Glucosa
glu0<-filter(ND1,ND1[,54]=='0')
glu1<-filter(ND1,ND1[,54]=='1')
glu2<-filter(ND1,ND1[,54]=='2')
glu3<-filter(ND1,ND1[,54]=='3')
#Separación de datos por ERC
erc0<-filter(ND1,ND1[,55]=='0')
erc1<-filter(ND1,ND1[,55]=='1')


# Grupo 1  ####
correlacion1<-round(cor(ND1[,1:14]), 1)
pdf(file = "cor1.pdf", width = 8, height = 10) #Guarda el gráfico en formato PDF
chart.Correlation(correlacion1, histogram = TRUE, method = "pearson")
corrplot.mixed(correlacion1)
corrplot(correlacion1, method="number", type="upper", is.corr = FALSE)
corrplot(correlacion1, method="number", type="upper", is.corr = TRUE)
dev.off() #cierra la instrucción


#Grupo 2 ####
correlacion2<-round(cor(ND1[,15:26]), 1)
pdf(file = "cor2.pdf", width = 8, height = 10) #Guarda el gráfico en formato PDF
chart.Correlation(correlacion2, histogram = TRUE, method = "pearson")
corrplot.mixed(correlacion2)
corrplot(correlacion2, method="number", type="upper", is.corr = FALSE)
corrplot(correlacion2, method="number", type="upper", is.corr = TRUE)
dev.off() #cierra la instrucción

# Grupo 3 ####
correlacion3<-round(cor(ND1[,27:45]), 1)
pdf(file = "cor3.pdf", width = 8, height = 10) #Guarda el gráfico en formato PDF
chart.Correlation(correlacion3, histogram = TRUE, method = "pearson")
corrplot.mixed(correlacion3)
corrplot(correlacion3, method="number", type="upper", is.corr = FALSE)
corrplot(correlacion3, method="number", type="upper", is.corr = TRUE)
dev.off() #cierra la instrucción

#Grupo 4 ####
correlacion4<-round(cor(ND1[,46:55]), 1)
pdf(file = "cor4.pdf", width = 8, height = 10) #Guarda el gráfico en formato PDF
chart.Correlation(correlacion4, histogram = TRUE, method = "pearson")
corrplot.mixed(correlacion4)
corrplot(correlacion4, method="number", type="upper", is.corr = FALSE)
corrplot(correlacion4, method="number", type="upper", is.corr = TRUE)
dev.off() #cierra la instrucción

#Correlograma 
pdf(file = "Correlaciones en grupos de edad mayor a 35.pdf", width = 8, height = 10) #Guarda el grafico en formato PDF
corND2<-round(cor(ND2), 2)
corrplot(corND1, method="number", type="upper",
         is.corr = TRUE, tl.cex = 0.3, number.cex = 0.3)
corrplot.mixed(corND2, tl.cex = 0.3, number.cex = 0.3)
dev.off() #Cierra la instruccion


#Boxplots comparativos

pdf(file = "Boxplots comparativos.pdf", width = 8, height = 10) 
par(mfrow=c(3,2))
for (i in 1:45) {
  boxplot(ND2[,i] ~ ND1$ERC, main=paste('Boxplot para ', names(ND1[i]))
          ,col=i, ylab=names(ND1[i]), xlab ='ERC',notch = TRUE)
  boxplot(ND2[,i] ~ ND1$Dx, main=paste('Boxplot para ', names(ND1[i]))
          ,col=i, ylab=names(ND1[i]),xlab = 'Diagnostico', notch = TRUE)}
dev.off()

#Propuesta resultado correlaciones####
pdf(file = "resultado_cor.pdf", width = 8, height = 10) #Guarda el gráfico en formato PDF
corrplot.mixed(correlacion1)
corrplot.mixed(correlacion2)
corrplot.mixed(correlacion3)
corrplot.mixed(correlacion4)
dev.off() #cierra la instrucción

#Correlaciones ####
correlacion<-round(cor(ND1), 1)
correlacion<-as.data.frame(correlacion)
#Correlaciones NCMS100B ####
NCM<-filter(correlacion, correlacion$NCM_S100B>='0.3'& correlacion$NCM_S100B<='1')
NCM1<-filter(correlacion, correlacion$NCM_S100B>='-0.3'& correlacion$NCM_S100B<='-1')
NCM2<-rbind(NCM, NCM1)
#Correlaciones IMS100B ####
IM<-filter(correlacion, correlacion$IM_S100B >='0.3'& correlacion$IM_S100B <='1')
IM1<-filter(correlacion, correlacion$IM_S100B>='-0.3'& correlacion$IM_S100B <='-1')
IM2<-rbind(IM, IM1)    
#Correlaciones CMS100B ####
CM<-filter(correlacion, correlacion$CM_S100B>='0.3'& correlacion$CM_S100B<='1')
CM1<-filter(correlacion, correlacion$CM_S100B>='-0.3'& correlacion$CM_S100B<='-1')
CM2<-rbind(CM, CM1)
#Correlaciones PMNS100B ####
PMN<-filter(correlacion, correlacion$PMN_S100B>='0.3'& correlacion$PMN_S100B<='1')
PMN1<-filter(correlacion, correlacion$PMN_S100B>='-0.3'& correlacion$PMN_S100B<='-1')
PMN2<-rbind(PMN, PMN1)    

#Regresiones lineales ####
r1<-lm(ND1$NCM_S100B~ND1$HbA1c+ND1$eTFG+ND1$`NCM_S100A8/9`+ND1$IM_S100A12+
         ND1$IM_S100B+ND1$IM_HMGB1+ND1$`CM_S100A8/9`+ND1$CM_S100B+ND1$Ejercicio-1 )


#Variables seleccionadas de acuerdo a las correlaciones de Pearson
r1<-lm(ND1$NCM_RAGE ~ ND1$Altura+ND1$grasa+
         ND1$`Masa muscular`+ND1$Cuello+ND1$`Fuerza de agarre`+
         ND1$Creat+ND1$Clasif_sarcopenia)
summary(r1)
NCMRAGE<- r1$coefficients[1]-111.31*ND1$Altura+1.03*ND1$grasa+
  1.82*ND1$`Masa muscular`-2.12*ND1$Cuello+0.10*ND1$`Fuerza de agarre`-
  22.05*ND1$Creat

r2<-lm(ND1$`NCM_S100A8/9` ~ ND1$TAD+ND1$`Creat horaria`+
         ND1$Creat)

NCMS100A89<- r2$coefficients[1]-827.6*ND1$TAD+
  50884*ND1$`Creat horaria`-17692.9*ND1$Creat
NCMS100A89
ND1$`NCM_S100A8/9`

r3<-lm(ND1$NCM_S100B~ ND1$HbA1c+ND1$eTFG+ND1$ERC)#Este mas o menos funciona

NCMS100B<-r3$coefficients[1]+1384.27*ND1$HbA1c+161.36*ND1$eTFG


r4<-lm(ND1$IM_RAGE~ ND1$Edad+ND1$`Masa muscular`+
         ND1$Creat+ND1$Sexo+ND1$Alcoholismo) #Mas o menos funciona

IMRAGE<- r4$coefficients[1]-0.82*ND1$Edad+0.32*ND1$`Masa muscular`-
  32.47*ND1$Creat

r5<-lm(ND1$`IM_S100A8/9`~ ND1$Creat+ND1$Alcoholismo)
summary(r5)
IMS100A89<- r5$coefficients[1]-25973*ND1$Creat

r6<-lm(ND1$IM_S100A12~ ND1$Edad+ND1$eTFG)
summary(r6)

IMS100A12<- r6$coefficients[1]-57.02*ND1$Edad+181.16*ND1$eTFG


r7<-lm(ND1$IM_S100B~ ND1$Edad+ND1$Peso+ND1$`clas IMC`+
         ND1$grasa+ND1$Cintura+ND1$Cadera+ND1$HbA1c+ND1$Creat)
summary(r7)

IMS100B<-r7$coefficients[1]+373*ND1$Peso-305657*ND1$Creat+232.4*ND1$Cintura
IMS100B
ND1$IM_S100B

r8<-lm(ND1$CM_RAGE~ ND1$Edad+ND1$Altura+ND1$Cuello+
         ND1$LDL+ND1$Alcoholismo+ND1$Ejercicio)
summary(r8)

r9<-lm(ND1$CM_S100B~ND1$Peso+ND1$IMC+ND1$grasa+ND1$Cintura+
         ND1$Creat+ND1$Microalb+ND1$Dx+ND1$ERC)
summary(r9)
CMS100B<-r9$coefficients[1]+336*ND1$Peso-18405*ND1$Creat-
  547*ND1$IMC-63.25*ND1$grasa+40.8*ND1$Cintura+10.8*ND1$Microalb #Ahi la lleva

r10<-lm(ND1$CM_HMGB1~ND1$TAS+ND1$TA+ND1$Col+ND1$HDL+
          ND1$`no-HDL`+ND1$LDL)
summary(r10)
CMHMGB1<-r10$coefficients[1]-3.35*ND1$TAS-48*ND1$TA+
  1.35*ND1$Col+5.25*ND1$HDL+3.68*ND1$LDL
((CMHMGB1-ND1$CM_HMGB1)/ND1$CM_HMGB1)*100

r11<-lm(ND1$PMN_HMGB1~ND1$TAS+ND1$TA+ND1$Indice)
summary(r11)
PMNHMGB1<-r11$coefficients[1]+20.65*ND1$TAS-2.51*ND1$TA-
  98.49*ND1$Indice

#ANOVAs####

ListaPruebasND<-names(ND1)
for (a in 1:ncol(ND2)) {
  cat('\n Anova para',ListaPruebasND[a],"\n");print(summary(aov(formula = ND2[,a]~ND1$`Ctrl gluc`*
                                                                  ND1$Dx*ND1$ERC)))
}


#Intentos de PCA ####
pcaND2 <- prcomp(ND2,scale=T)
summary(pcaND2)

fviz_contrib(pcaND2,choice = "ind")

fviz_pca_biplot(pcaND2, repel = TRUE,
                #geom= "point", #o text o geom=c("point", "text")
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969", # Individuals color
                select.ind = list(contrib = 30))
fviz_pca_biplot(pcaND2, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969" ) # Individuals color)


grupo1ND2<-as.factor(ND1$Dx)
grupo2ND2<-as.factor(ND1$ERC)


fviz_pca_ind(pcaND2,
             col.ind = grupo2ND2, # color by groups
             palette = c("#00AFBB", "#FC4E07","#696969"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = FALSE
)

fviz_pca_biplot(pcaND2,
                col.ind = grupo1ND2, # color by groups
                palette = c("#00AFBB", "#FC4E07","#696969"),
                addEllipses = TRUE, # Concentration ellipses
                ellipse.type = "confidence",
                legend.title = "Groups",
                repel = FALSE
)
fviz_pca_biplot(pcaND2,
                col.ind = grupo2ND2, # color by groups
                palette = c("#00AFBB", "#FC4E07","#696969"),
                addEllipses = TRUE, # Concentration ellipses
                ellipse.type = "confidence",
                legend.title = "Groups",
                repel = F)

#Intento de grafico con t-sne ####
library(Rtsne)
library(caret)

tsneND<-Rtsne(as.matrix(ND2), dims=2,initial_dims=41, perplexity = 10, 
              partial_pca=F, theta=0.0, num_threads=1.0)
summary(tsneND)

dftsneND<-as.data.frame(tsneND$Y)

ggplot(dftsneND, aes(x=V1,y=V2))+
  geom_point(size=2)

plot(tsneND$Y, col = 2, pch = 19, cex = 1.5)

#Para mostrar población
DxND<-ND1$Dx
ND1$Dx<- as.factor(ND1$Dx)

colorsDx = rainbow(length(unique(DxND)))
names(colorsDx) = unique(ND1$Dx)

ERCND<-ND1$ERC
ND1$ERC<- as.factor(ND1$ERC)

colorsERC = rainbow(length(unique(ND1$ERC)))
names(colorsERC) = unique(ND1$ERC)

plot(tsneND$Y, t='n', main="tsne")
text(tsneND$Y, labels=DxMet, col=colorsDx[Met$Dx])

plot(tsneND$Y, t='n', main="tsne")
text(tsneND$Y, labels=ERCMet, col=colorsERC[Met$ERC])



