library(readr)
library(ggplot2)
library(plotly)
bd2 <- read_csv("C:/Users/Admin/Desktop/Semestre_Enero_Julio_2021/Verano/BD2.csv")


library(cluster.datasets)
data("all.mammals.milk.1956")
amm<-all.mammals.milk.1956
class(amm)
dim(amm)

dist<-dist(amm[,2:6])
round(as.matrix(dist)[1:6,1:6],3)
den<-as.dendrogram(hclust(dist))
plot(den)


nom<-amm
rownames(nom)<-nom$name
nom<-nom[,-1]
plot(as.dendrogram(hclust(dist(nom))), main='Dendograma', 
     col='red', ylab='Distancia')

ds1<-data.frame(bd2$Name, bd2$Age,bd2$Gender, bd2$Weight, bd2$Height,
                bd2$BMI, bd2$Fat_Percent, bd2$`Mass Muscle`, bd2$Waist, bd2$Hip,
                bd2$Neck, bd2$SBP, bd2$DBP, bd2$TA, bd2$`Handgrip streght`)
#Modificamos los nombres
colnames(ds1)<-c('Nombre','Edad', 'Genero', 'Peso','Altura','BMI','Grasa',
                 'Masa Muscular','Cintura','Hip','Cuello','SBP', 'DBP', 'TA',

# is.na(ds1)
# which(is.na(ds1)) #Encontar la posición del dato faltante
# sum(is.na(ds1)) # cuantos valores tiene la base de datos   
# complete.cases(ds1) #Saber cuantos casos tenemos completo (filas)
# sum(complete.cases(ds1))   # Cuantos casos completos hay
# na.omit(data)               
                 
                                  'Agarre')
nom<-ds1
nom<-nom[,c(-1,-3)]
plot(as.dendrogram(hclust(dist(nom))), main='Dendograma', 
     col='red', ylab='Distancia')

# Dissimilarity matrix
d <- dist(data, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
