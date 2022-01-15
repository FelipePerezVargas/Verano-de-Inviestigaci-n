# AVANCES DE VERANO EN R

#Modelo predictivo metabolomica
#Cargar librerias####
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("forestmodel","see","jtools","olsrr","parameters",
              "stats","ggplot2", "palmerpenguins", "plot3D" ,
              "plot3Drgl","apaTables","gvlma","broomExtra",
              "performance","Formula","readr","corrplot",
              'lattice','survival','Hmisc','PerformanceAnalytics',
              'psych','plotly','heatmaply','ggridges','gridExtra',
              'naniar','missForest','tidyr','dplyr',"effectsize","haven",
              "apaTables", "DescTools","report","dplyr","rstatix","FSA",
              "rcompanion", 'stats',  'factoextra','Rtsne','caret','BiodiversityR',
              'randomForest')

ipak(packages)

#Crear los dataframe para trabajar ####
#Importar base de datos
Met<- read.csv("Verano UG/Metabolomica Verano.csv")
#  ò
Met<- read.csv("Verano UG\\Met1.csv")

#Remplazar valores no asignados por medias
Met <- Met %>% 
  impute_mean_if(is.numeric)
rownames(Met)<-Met$No
Met$No<-NULL
Met$Code<-NULL
#Asignar etiquetas a variables categoricas
Met$Sexo<-factor(Met$Sexo,levels = c('0','1'), labels = c('H','M'))
Met$Clas.IMC<-factor(Met$Clas.IMC,levels = c('0','1','2'), labels = c('Normoperso','Sobrepeso','Obesidad'))
Met$Tabaquismo<-factor(Met$Tabaquismo,levels = c('0','1'), labels = c('No','Si'))
Met$Alcoholismo<-factor(Met$Alcoholismo,levels = c('0','1'), labels = c('No','Si'))
Met$Ejercicio<-factor(Met$Ejercicio,levels = c('0','1'), labels = c('No','Si'))
Met$Acantosis<-factor(Met$Acantosis,levels = c('0','1'), labels = c('No','Si'))
Met$Dx<-factor(Met$Dx,levels = c('0','1'), labels = c('Sano','DM'))
Met$Ctrl.gluc<-factor(Met$Ctrl.gluc,levels = c('0','1','2','3'), labels = c('Sano','Buen control','Control regular', 'Mal control'))
Met$ERC<-factor(Met$ERC,levels = c('0','1','2'), labels = c('Sano','DM','ND'))

#Imputacion de variables categoricas
Met<-missForest(Met)
Met<-Met$ximp

#Crear dataframe con solo valores numericos (quitar variables categoricas)
Met1 <- Met[,1:(ncol(Met)-10)]


#Separar poblaciones####


Mets<-Met[Met$ERC=='Sano',]
Mets2<-Mets[,1:(ncol(Met)-10)]
Mets2


MetDs<-Met[Met$ERC=='DM',]
MetDs2<-MetDs[,1:(ncol(Met)-10)]
MetDs2


MetND<-Met[Met$ERC=='ND',]
MetND2<-MetND[,1:(ncol(Met)-10)]


#Crear Dataframe con medias, sd de las distintas poblaciones####
Res<-matrix(data=1:ncol(Met1),ncol=1,nrow=ncol(Met1))
Res<-as.data.frame(Res)
Res$Variable<-colnames(Met1)
Res$V1<-NULL
rownames(Res)<-Res$Variable
Res$Variable<-NULL


Res$Medias<- apply(Met1,2, mean)
Res$MD<-apply(Mets2,2,mean)
Res$Sano<-apply(Mets2,2,mean)
Res$DM<-apply(MetDs2,2,mean)
Res$ND<-apply(MetND2,2,mean)


Res$Ds<-apply(Met1,2,sd)
Res$SanoDs<-apply(Mets2,2,sd)
Res$DMDs<-apply(MetDs2,2,sd)
Res$NDDs<-apply(MetND2,2,sd)


#pvalor Shapiro-Wilk
Res$p.shapiro<-1:ncol(Met1)
for (i in 1:ncol(Met1)) {
  if(i==55)  next
  normalidad<-shapiro.test(Met[,i])
  Res[i,'p.shapiro']<-normalidad$p.value  
}
colnames(Met1)
shapiro.test(Met$X3.oxo.alpha.ionone.TR1)

#pvalor ANOVA no parametrico
Res$p.ANOVAnoParametrico<-1:ncol(Met1)
for (i in 1:ncol(Met1)) {
  
  Anova<-kruskal.test(formula = Met[,i]~ Met$ERC)
  Res[i,'p.ANOVAnoParametrico']<-Anova$p.value
}
summary(Anova)

#chisquare ANOVA no parametrico
Res$chisquare<-1:ncol(Met1)
for (i in 1:ncol(Met1)) {
  
  Anova<-kruskal.test(formula = Met[,i]~ Met$ERC)
  Res[i,'chisquare']<-as.numeric(Anova$statistic)
}

#p valores Post-Hoc no parametricos
Res

Res$z.DM.ND<-1:ncol(Met1)
Res$z.DM.Sano<-1:ncol(Met1)
Res$z.ND.Sano<-1:ncol(Met1)

Res$pu.DM.ND<-1:ncol(Met1)
Res$pu.DM.Sano<-1:ncol(Met1)
Res$pu.ND.Sano<-1:ncol(Met1)

Res$pa.DM.ND<-1:ncol(Met1)
Res$pa.DM.Sano<-1:ncol(Met1)
Res$pa.ND.Sano<-1:ncol(Met1)

for (i in 1:ncol(Met1)) {
  if(i==55) next
  dd<-dunnTest(Met[,i]~Met$ERC,method="bh");
  dd<-dd$res;
  ddz<-dd$Z;
  ddpu<-dd$P.unadj;
  ddpa<-dd$P.adj;
  Res[i,'z.DM.ND']<-ddz[1];
  Res[i,'z.DM.Sano']<-ddz[2];
  Res[i,'z.ND.Sano']<-ddz[3];
  
  Res[i,'pu.DM.ND']<-ddpu[1];
  Res[i,'pu.DM.Sano']<-ddpu[2];
  Res[i,'pu.ND.Sano']<-ddpu[3];
  
  Res[i,'pa.DM.ND']<-ddpa[1];
  Res[i,'pa.DM.Sano']<-ddpa[2];
  Res[i,'pa.ND.Sano']<-ddpa[3];
  
  
}
Res
write.csv(Res,"Met medias y pvalue 1.csv") # Los valores de las pruebas parametricas se remplazaron manualmente en excel




#ANOVA PARAMETRICO####


#Anova Parametrico
ListaPruebas<-colnames(Met1)
for (a in 1:ncol(Met1)) {
  cat('\n Anova para',ListaPruebas[a],"\n");print(summary(aov(formula = Met[,a]~
                                                                Met$ERC)))
}

#Posthoc Parametrico
for (a in 1:ncol(Met1)){
  an<-aov(formula = Met2[,a]~Met$ERC)
  cat('\n Tuckey para',ListaPruebas[a]);print(TukeyHSD(an)  )
}

#PCA ####

MetND$Methional<-NULL

PCAMet <- prcomp(Met1,scale=T)

z<-as.data.frame(PCAMet$x)

Met$PC1<-z$PC1
Met$PC2<-z$PC2

grupo2<-Met$ERC

#Graficos PCA

ggplot(data=Met, aes(x=PC1,y = PC2))+
  geom_point(size=3)

ggplot(data=MetPrueba, aes(x=PC1,y = PC2, color=ERC,shape=ERC))+
  geom_point(size=3)

fviz_contrib(PCAMet,choice = "ind")

fviz_pca_biplot(PCAMet, repel = TRUE,
                #geom= "point", #o text o geom=c("point", "text")
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969", # Individuals color
                select.ind = list(contrib = 30))
fviz_pca_biplot(PCAMet, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969" ) # Individuals color)


fviz_pca_ind(PCAMet,
             col.ind = grupo2, # color by groups
             palette = c("#00AFBB", "#FC4E07","#696969"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = FALSE
)




fviz_pca_biplot(PCAMet,
                col.ind = grupo2, # color by groups
                palette = c("#00AFBB", "#FC4E07","#696969"),
                addEllipses = TRUE, # Concentration ellipses
                ellipse.type = "confidence",
                legend.title = "Groups",
                repel = FALSE
)
fviz_pca_biplot(PCAMet,
                col.ind = grupo2, # color by groups
                palette = c("#00AFBB", "#FC4E07","#696969"),
                addEllipses = TRUE, # Concentration ellipses
                ellipse.type = "confidence",
                legend.title = "Groups",
                repel = F)

#PLS####  
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("mixOmics")

library(mixOmics)


Y<- Met$ERC
X<- Met1[,-42:-44]


srbct.plsda <- plsda(X,Y, ncomp = 10)

plotIndiv(srbct.plsda , comp = 1:2,
          group = Met$ERC, ind.names = FALSE, 
          ellipse = TRUE, legend = TRUE, title = 'PLSDA on SRBCT')


#PLSDA VIP
PLSDA.VIP(srbct.plsda,graph = T)

#PLS con fondo en grafico

background = background.predict(srbct.plsda, comp.predicted=2, dist = "max.dist") 
#optional: xlim = c(-40,40), ylim = c(-30,30))

plotIndiv(srbct.plsda, comp = 1:2,
          group = Met$ERC, ind.names = FALSE, title = "Maximum distance",
          legend = TRUE,  background = background)  

auc.plsda = auroc(srbct.plsda, roc.comp = 6)


splsda.srbct <- splsda(X, Y, ncomp = 10, keepX = 1:30) 
#plotLoadings(splsda.srbct, comp = 2, title = 'Loadings on comp 2', 
#            contrib = 'max', method = 'mean')
plotLoadings(splsda.srbct, comp = 1, title = 'Loadings on comp 1', 
             contrib = 'max', method = 'mean')

cim(splsda.srbct)
#pls perfect

set.seed(2543) 
perf.plsda.srbct <- perf(srbct.plsda, validation = "Mfold", folds = 5, 
                         progressBar = FALSE, auc = TRUE, nrepeat = 10) 

plot(perf.plsda.srbct, col = color.mixo(5:7), sd = TRUE, legend.position = "horizontal")


#Variables que correlacionan
plsda<-srbct.plsda
plsda<-plsda$variates
plsda<-plsda$X
ggplot(data=Met,aes(x=xvariate1,y=xvariate2,shape=ERC,color=ERC))+geom_point()
Met1$xvariate1<-plsda[,1]
Met1$xvariate2<-plsda[,2]

CorMet<-data.frame(cor(Met1, method = 'spearman'))
variablesMet<-colnames(Met1)

#|CorMet$CAP1<=-0.3
rownames(CorMet[CorMet$xvariate1 >0.3 & CorMet$xvariate1 <0.5,])
rownames(CorMet[CorMet$xvariate1 >0.5 & CorMet$xvariate1 <0.7,])
rownames(CorMet[CorMet$xvariate1 >0.7 & CorMet$xvariate1 <1,])



CorMet<-data.frame(cor(Met1, method = 'spearman'))
variablesMet<-colnames(Met1)

|CorMet$CAP1<=-0.3
rownames(CorMet[CorMet$CAP1 >0.7 & CorMet$CAP1 <1,])

#Discriminar metabolitos####
MetND1$Methional<-NULL
MetND1$X7.Angeloylheliotridine.TR1<-NULL
MetND1$X3.oxo.alpha.ionone.TR1<-NULL
MetND1$Heptane..hexadecafluoro.TR2.1<-NULL

PCAMet <- prcomp(MetND,scale=T) 
Componentes<-as.data.frame(PCAMet$x)

Met$PC1<-Componentes$PC1;Met1$PC1<-Componentes$PC1
Met$PC2<-Componentes$PC2;Met1$PC2<-Componentes$PC2

ggplot(data=MetND,aes(x=xvariate1,y=xvariate2,shape=ERC,color=ERC))+geom_point()

CorMet<-data.frame(cor(MetND1, method = 'spearman'))

#Obtener correlaciones con el eje discriminante resultado del PCA


rownames(CorMet[CorMet$PC1 >0.3 & CorMet$PC1 <0.5,])
rownames(CorMet[CorMet$PC1 >0.5 & CorMet$PC1 <0.7,])
rownames(CorMet[CorMet$PC1 >0.7 & CorMet$PC1 <1,])
