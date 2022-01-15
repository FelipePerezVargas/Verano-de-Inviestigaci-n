############################################################
####                                                    ####
####                       Día tres                     ####
####                                                    ####
############################################################
#Como instalar un paquete
# install.packages('readr') #Instalar paquetes
library(readr)  # Cargar paquetes
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(psych)
library(ggplot2)
library(plotly)
library(heatmaply)
library(ggridges)
library(gridExtra)
library(corrgram)
graphics.off() #Borramos los gráficos anteriores
rm(list = ls()) #Borramos todas las variables anteriores
#Cargamos la base de datos usando la dirección 
bd2 <- read_csv("C:/Users/Admin/Desktop/Semestre_Enero_Julio_2021/Verano/BD2.csv")



class(bd2)     # Tipo de datos
dim(bd2)       # Dimensión del dataframe
length(bd2)    # Longitud de la base de datos o número de filas
str(bd2)       # Despliega una lista para saber que tipo de variable es
#Comprobamos si hay datos faltantes en la base de datos
is.na(bd2)
#which(is.na(s100b)) #Encontar la posición del dato faltante
sum(is.na(bd2)) # cuantos valores tiene la base de datos   
complete.cases(bd2) #Saber cuantos casos tenemos completo
sum(complete.cases(bd2))   # Cuantos casos completos hay

bd2[ ,65] #Seleccionamos la columna
bd2$Age    #Extraemos la columna o variable

is.na(bd2$PMN_HMGB1)
which(is.na(bd2$PMN_HMGB1))
mean(as.numeric(bd2$Age))
mean(as.numeric(bd2$Weight))
mean(as.numeric(bd2$PMN_HMGB1), na.rm=TRUE) 
var(as.numeric(bd2[,3]), na.rm=TRUE) # Valor medio sin valores faltante
sd(as.numeric(bd2[,3]), na.rm=TRUE) 

summary(bd2)

#Formamos una base de datos en formato data frame


ds1<-data.frame(bd2$Name, bd2$Age,bd2$Gender, bd2$Weight, bd2$Height,
                bd2$BMI, bd2$Fat_Percent, bd2$`Mass Muscle`, bd2$Waist, bd2$Hip,
                bd2$Neck, bd2$SBP, bd2$DBP, bd2$TA, bd2$`Handgrip streght`)
#Modificamos los nombres
colnames(ds1)<-c('Nombre','Edad', 'Genero', 'Peso','Altura','BMI','Grasa',
                 'Masa Muscular','Cintura','Hip','Cuello','SBP', 'DBP', 'TA',
                'Agarre')
# Comprobamos si hay datos faltantes en la base de datos
is.na(ds1)
which(is.na(ds1)) #Encontrar la posición del dato faltante
sum(is.na(ds1)) # cuantos valores tiene la base de datos   
complete.cases(ds1) #Saber cuantos casos tenemos completo (filas)
sum(complete.cases(ds1))   # Cuantos casos completos hay


describe(ds1)
v<-vector()
for (i in 3:4) {
  v[i]<-cor.test(ds1$Edad, ds1[,i], method = "pearson")
}


  v<-cor.test(ds1$Edad, ds1$Peso, method = "pearson")
v$p.value
v$statistic
print(paste('El valor de P para Edad-Peso es', round(v$p.value, 2)))
print(paste('El valor de P para Edad-Peso es', round(v$statistic, 2)))

  
v[1]
v$statistic
v$estimate  
describe(ds1)



v[i]<-cor.test(ds1$Edad, ds1[,i], method = "pearson")
#Correlación entre variables 
plot(ds1$Edad, ds1$BMI, pch = 19, col = "lightblue", main='Gráfica Edad vs IMC',
    xlab='Edad (Años)', ylab='Indice de Masa Corporal')
cor(ds1)
# Línea de regresión simple
abline(lm(ds1$BMI ~ ds1$Edad), col = "red", lwd = 3)
# Correlación de Pearson
text(paste("Correlación:", round(cor(ds1$Edad, ds1$BMI), 2)), x = 35, y = 35)
#Correlación entre todas las variables

# recta de regresión esta compformada por y =ax+b
# donde: a es la pendiente y b es el intercepto y x es la variable a explicar
r1<-lm(ds1$BMI ~ ds1$Edad) #calcula los coeficientes  a y b
summary(r1)
r2<-lm(ds1$Peso~ds1$BMI+ds1$Grasa+ds1$`Masa Muscular`+ds1$Cintura+ds1$Hip+ds1$Cuello)
summary(r2)

r2<-lm(ds1$Peso~ds1$BMI+ds1$Grasa+ds1$`Masa Muscular`)
summary(r2)
pm<-r1$coefficients[2]*BMI+r1$coefficients[3]*Edad;round(pm,2)

Edad<-35

r1[1]
pm<-r1$coefficients[1]+r1$coefficients[2]*Edad;round(pm,2)
print(paste('El índice de masa muscular para un paciente de eda', Edad, 
            'años es de:',round(pm,2)))
ds1<-ds1[,c(-1,-3)]
cor(ds1)
respca<-prcomp(ds1, scale = TRUE)
dim(respca$rotation) #Número de distintos componentes
head(respca$x)[,1:5] #los vectores de los scores.
respca$sdev #las desviaciones estándares de cada CP.
respca$sdev^2  ## Varianza explicada por cada componente
summary(respca)
#comprobemos la importancia del componente 1
xx<-respca$x
xx<-as.data.frame(xx)
ds1$PC1<-xx$PC1
ds1$PC2<-xx$PC2
cor(ds1)
#otra función
princomp(ds1,cor = TRUE) 

respca1 <- princomp(~ Edad + Peso,
                  data = ds1, na.action = na.exclude, cor = TRUE)

names(respca1)
respca1$sdev
summary(respca1)


library(FactoMineR)

#PCA() #PCA con resultados más detallados. Los valores ausentes se reemplazan por la media de cada columna.
#Pueden incluirse variables categóricas suplementarias. Estandariza automáticamente los datos.


respca2 <- PCA(X = ds1, scale.unit = FALSE, ncp = 6, graph = TRUE)

print(respca2)


head(respca2$eig) #como ejemplo

# Regresión lineal multiple
ybmi<-as.numeric(r1$coefficients[1])+as.numeric(r1$coefficients[2])*ds1$Edad


ds1<-ds1[,c(-1,-3)]
r2 <- lm(ds1$BMI ~ds1$Altura+ds1$Grasa
         +ds1$`Masa Muscular`, data = ds1)
summary(r2)

Altura<-1.50
Grasa<-44.5
MM<-  41.7
modelo<-r2$coefficients[1]+r2$coefficients[3]*Grasa
        +r2$coefficients[4]*MM
modelo

r2 <- lm(ds1$BMI ~ds1$Altura+ds1$Grasa
        +ds1$`Masa Muscular`, data = ds1)
bmi <- function(Altura, Grasa, MM) {
  modelo<-r2$coefficients[1]+r2$coefficients[3]*Grasa
  +r2$coefficients[4]*MM+r2$coefficients[2]*Altura
  return(modelo)
}
bmi()

Edad<-
Peso<-  
Altura<-
  
BMI<-0.12306*ds1$Edad+ 0.35384*ds1$Peso-1.85481 #Comparamos el modelo

r3<-lm(ds1$BMI ~ ds1$Edad + ds1$Peso+ds1$Altura, data = ds1)
summary(r3)
r4<-lm(ds1$BMI ~ ds1$Edad + ds1$Peso+ds1$Altura+ds1$Grasa, data = ds1)
summary(r4)


p1<-function(Edad, Peso){
  BMI<-0.12306*Edad+ 0.35384*Peso-1.85481
 print(paste('El BMI de ',bd2$Name,'es de', round(BMI,2)))
# print(paste('El BMI es de ', BMI))  
}

p1()


r5<-lm(ds1$BMI ~., data = ds2)
summary(r5)

r5<-lm(ds1$`Masa Muscular` ~., data = ds2)
summary(r5)

multi.hist(x = ds2, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")
library(GGally)
g<-ggpairs(ds2, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggplotly(g)


# Gráfico Peso vs índice de masa corporal
#Correlación entre variables 
plot(ds1$Peso, ds1$BMI, pch = 19, col = "lightblue", main='Gráfica Peso vs IMC ',
     xlab='Peso (Kg)', ylab='Indice de Masa Corporal')
# Línea de regresión simple
abline(lm(ds1$BMI ~ ds1$Peso), col = "red", lwd = 3)
# Correlación de Pearson
text(paste("Correlación:", round(cor(ds1$Peso, ds1$BMI), 2)), x = 60, y = 35)
#Correlación entre todas las variables




#Podemos remover, modificar o agregar columnas 
#Remover columnas
ds2<-ds1[,-1]                 # Remover una columna
ds2<-ds1[,-1:-3]              # Remover secuencia de columnas
ds2<-ds1[,c(-1,-3)]           # Remover selección de columna

pairs(ds2)
#Recortamos los nombres de las bases de datos
nombre<-c('Edad', 'Peso','Altura','BMI','Grasa',
  'Masa Muscular','Cintura','Hip','Cuello','SBP', 'DBP', 'TA',
  'Agarre')
cor(ds2)
plot(ds2)
summary(ds2)

# Losw ciclos for nos sirven para hacer multiples calculos 

x<-vector()
for (i in 1:10) {
  x[i]<-print(i**2+i**3)
}



m<-vector()
v<-vector()
d<-vector()
pdf(file = "box_1.pdf", width = 8, height = 10)
par(mfrow=c(2,2)) # Configuración de tus graficos
for (i in 1:13) {
  m[i]<-mean(ds2[,i]) 
  v[i]<-var(ds2[,i])
  d[i]<-sd(ds2[,i])
  boxplot(ds2[,i], main=paste('Boxplot para ', names(ds2[i]))
          ,col=i, xlab=names(ds2[i]),  notch = TRUE)
    hist(ds2[,i],
       # breaks = 20,   
       main = paste('Histograma para ', names(ds2[i])), 
       freq = FALSE,
       xlab = paste( names(ds2[i])),                                          
       ylab = "Frecuencia relativa", 
       # xlim = c(min(ds2[,2]),max(ds2[,2])), 
       # ylim = c(0,0.15),
       col = i,
       border = "red")
  lines(density(ds2[,i]), col='darkviolet', lwd=3)  # dibujamos la distribución normal empírica
  lines(density(ds2[,i], adjust=2), col='darkturquoise', lwd=3, lty=4) # suavizar  la curva
      }
dev.off()


round(cor(ds2),2)  

#Matriz numérica de correlaciones


ss<-rcorr(as.matrix(ds2))
#Data frame de la correlación
correlacion<-round(cor(ds2), 1)
corrplot(correlacion, method="number", type="upper", is.corr = TRUE)
corrplot(correlacion, method="number", type="upper", is.corr = FALSE)
corrplot.mixed(correlacion)
pdf(file = "cor_2.pdf", width = 8, height = 10) #Guarda eñ gráfico en formato PDF
chart.Correlation(correlacion, histogram = TRUE, method = "pearson")
dev.off() #cierra la instrucción
#Correlación entre las variables 
pdf(file = "cor_1.pdf", width = 8, height = 10)
chart.Correlation(ds2, histogram = F, pch = 19)
dev.off()

pdf(file = "cor_3.pdf", width = 8, height = 10)
corrplot.mixed(correlacion)
dev.off()

pairs.panels(ds2,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes


corrplot(cor(ds2),
        method = "circle",       
        order = "hclust",         # Método de ordenación de la matriz
        hclust.method = "ward.D", # Si order = "hclust", es el método de agrupación usado
        addrect = 3,              # Si order = "hclust", es el número de clusters
        rect.col = 3,             # Color de los rectángulos
        rect.lwd = 3)             # Ancho de línea de los rectángulos




par(mfrow = c(2, 3)) #Ajustamos la pantalla de salida
# Círculos
corrplot(cor(ds2), method = "circle",
         title = "method = 'circle'",
         tl.pos = "n", mar = c(2, 1, 3, 1)) 
# Cuadrados
corrplot(cor(ds2), method = "square",
         title = "method = 'square'",
         tl.pos = "n", mar = c(2, 1, 3, 1)) 
# Elipses
corrplot(cor(ds2), method = "ellipse",
         title = "method = 'ellipse'",
         tl.pos = "n", mar = c(2, 1, 3, 1)) 
# Correlaciones
corrplot(cor(ds2), method = "number",
         title = "method = 'number'",
         tl.pos = "n", mar = c(2, 1, 3, 1)) 
# Gráficos de sectores
corrplot(cor(ds2), method = "pie",
         title = "method = 'pie'",
         tl.pos = "n", mar = c(2, 1, 3, 1)) 
# Colores
corrplot(cor(ds2), method = "color",
         title = "method = 'color'",
         tl.pos = "n", mar = c(2, 1, 3, 1)) 

par(mfrow = c(1, 1)) #Regresamos a la configuración inicial

corrgram(ds2,
         order = TRUE,              # Si TRUE, reordena los datos en base a un ACP
         upper.panel = panel.pie,   # Función del panel sobre la diagonal
         lower.panel = panel.shade, # Función del panel bajo la diagonal
         text.panel = panel.txt,    # Función del panel de la diagonal
         main = "Correlograma")     # Título principal

corPlot(ds2, cex = 1.2, main = "Matriz de correlación")


# Vamos atrabajar con la libreria ggplot2 y plotly


g1<-ggplot(ds2, aes(x=Edad, y=Peso))+geom_point()
g1<-ggplotly(g1);g1

#Gráfico de dispersión con ajuste lineal
g1<-ggplot(ds2, aes(x=Grasa, y=BMI))+
  geom_point()+
  geom_smooth()
g1<-ggplotly(g1);g1

# Gráfico de cajas 

g2<-ggplot(ds2, aes(y=Agarre))+
  geom_boxplot(outlier.colour = "blue", outlier.shape = 8, outlier.size = 2
               , fill='pink');g2
g2<-ggplotly(g2);g2

g3<-ggplot(ds2, aes(x = Peso)) + geom_histogram()
ggplotly(g3)

# nombre<-c('Edad', 'Peso','Altura','BMI','Grasa',
          # 'Masa Muscular','Cintura','Hip','Cuello','SBP', 'DBP', 'TA',
          # 'Agarre')

p <- heatmaply(ds2, 
               #dendrogram = "row",
               xlab = "", ylab = "", 
               main = "",
               scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.1,
               label_names = c("Paciente", "Indicador:", "Valor"),
               fontsize_row = 5, fontsize_col = 5,
               labCol = colnames(ds2),
               labRow = rownames(ds2),
               heatmap_layers = theme(axis.line=element_blank())
);p

colnames(ds1)<-c('Nombre','Edad', 'Genero', 'Peso','Altura','BMI','Grasa',
                 'Masa Muscular','Cintura','Hip','Cuello','SBP', 'DBP', 'TA',
                 'Agarre')

row.names(ds2)<-ds1$Nombre


heatmaply_cor(cor(ds2),
  xlab = "Features",
  ylab = "Features",
  k_col = 2,
  k_row = 2
)








library(ggridges)
library(ggplot2)

# Diamonds dataset is provided by R natively
#head(diamonds)

# basic example
ggplot(ds2, aes(x = ds2[,3], y = bd2$`clas BMI`, fill = bd2$`clas BMI`)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")




# Diamonds dataset is provided by R natively
#head(diamonds)

# basic example
ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


library(psych)
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")


library(umap)
iris.data <-iris[, grep("Sepal|Petal", colnames(iris))]
iris.labels <- iris[, "Species"]
iris.umap <- umap(iris.data)
iris.umap
head(iris.umap$layout, 3)
iris.umap$knn
iris.umap$data
iris.umap$config  
plot.iris(iris.umap, iris.labels)
  
  
  
  