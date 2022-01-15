# Manipulación de la base de datos


class(s100b) # Te indica que clase de objeto es.
dim(s100b)   # Te indica las dimensiones de objeto



edad<-s100b$Age
mean(edad)   # Calcula el valor medio 
sd(edad)     # Calcula la desviavión stándar
var(edad)    # Calcula la varianza
# Histograma de frecuancias de edades
hist(edad, col=1:5, main='Histograma de edades',
     xlab='Edad (Años)', ylab='Frecuencia',breaks =c(40:70) )
# Puedo crea un boxplot
boxplot(edad, ylab='Edad (Años)', main='Boxplot Edad', col='red')
boxplot.stats(s100b$Weight)
min(edad)
max(edad)
mp<-round(mean(edad), 2) # Redondeamos al punto decimal deseado
dep<-round(sd(edad), 2)  # Redondeamos al punto decimal deseado
vp<-round(var(edad), 2)  # Redondeamos al punto decimal deseado

print(paste('El valor medio de la edad es:',mp, 'Años',
            ',la varianza es de',vp, 'y la desviasión estándar es de', dep))


db<-na.omit(s100b[,3])
# Glucosa
glu<-s100b$Glucose
st<-s100b$Stress


peso<-na.omit(s100b$Weight)
mean(peso)
plot(edad, peso, xlab='Edad (Años)', ylab='Peso (Kg)',
     main='Edad vs Peso', col=1:176, pch=0, lwd=3)

plot(edad, peso, xlab='Edad (Años)', ylab='Peso (Kg)',
     main='Edad vs Peso', col='blue', pch=0, lwd=3)





c1<-1:21 # Crea una secuencia de números igualmente espaciados
c2<- -10:10

seq(1,10, by=0.5) # Crea secuencia de números con espacio definido
plot(c1, c2)

#Cargar nuestra base de datos
#Vamos a crear una nueva base de datos separando en esta caso variables
# numericas 
matrix
m1<-matrix(c(1,2,3,4,
             5,6,7,8,
             9,10,11,12,
             13,14,15,16), ncol=4, nrow=4, byrow = FALSE );m1

length(s100b$Age)
nbd<-matrix(c(s100b$Name, s100b$Age, s100b$Weight, s100b$Height),nrow = 176, 
            ncol=4, byrow =FALSE  )

#Otra manera es usando el comando lenght para saber el número de filas y columnas

nbd<-matrix(c(s100b$Name, s100b$Age, s100b$Weight, s100b$Height),
            nrow = length(s100b$Age), ncol=4, byrow =FALSE  )
colnames(nbd)<-c('Nombre','Edad','Peso','Altura')
class(nbd)
#Como accesar a la matriz de datos 
# nbd[Fila,Columna]
nbd[1,] #Muestra toda la fila 
nbd[, 1] #Muestra la columna deseada

#Vamos comprobar si hay valores faltantes o celdas vacias ####

is.na(nbd)      #Valores faltante

#which(is.na(s100b)) #Encontar la posición del dato faltante
sum(is.na(nbd)) # cuantos valores tiene la base de datos   
complete.cases(nbd) #Saber cuantos casos tenemos completo
sum(complete.cases(s100b))   # Cuantos casos completos hay
is.na(nbd[,3])
mean(as.numeric(nbd[,2]))
mean(as.numeric(nbd[,3]))
mean(as.numeric(nbd[,3]), na.rm=TRUE) 
var(as.numeric(nbd[,3]), na.rm=TRUE) # Valor medio sin valores faltante
sd(as.numeric(nbd[,3]), na.rm=TRUE) 




mean(as.numeric(nbd[,4]))
mean(as.numeric(nbd[,4]), na.rm=TRUE) 
var(as.numeric(nbd[,4]), na.rm=TRUE) # Valor medio sin valores faltante
sd(as.numeric(nbd[,4]), na.rm=TRUE) 


bn<-na.omit(nbd) #Nueva base de datos sin valores faltante
cor(as.numeric(bn[,2]), as.numeric(bn[,3])) #Correlación edad vs peso
cor(as.numeric(bn[,2]), as.numeric(bn[,4])) #Correlación edad vs altura
cor(as.numeric(bn[,3]), as.numeric(bn[,4])) #correlación peso vs altura

#Trabajando valor faltantes sobre un vector 
is.na(s100b$Waist)
sum(is.na(s100b$Waist))
which(is.na(s100b$Waist)) 


# Imputación de datos 
#Sustituir los valores faltantes por el valor promedio
s100b$Waist[is.na(s100b$Waist)]<-mean(s100b$Waist, na.rm=TRUE) 

mean(s100b$Waist)
hist(s100b$Waist)
boxplot(s100b$Waist)

library(ggplot2)
library(gridExtra)
library(plotly)
# Hacemos graficos simples
g1 <- ggplot(s100b, aes(x=complete.cases(Weight))) + geom_density(fill="slateblue")
g2 <- ggplot(s100b, aes(x=Weight, y=Waist, color=Age)) + geom_point(size=5)+geom_smooth(method = lm, se = FALSE) 
g2<-ggplotly(g2);g2
g3 <- ggplot(s100b, aes(x=factor(CLASBMI), y=Weight, fill=CLASBMI)) + geom_boxplot() + theme(legend.position="none")
g4 <- ggplot(s100b , aes(x=factor(CLASBMI), fill=factor(CLASBMI))) +  geom_bar()
mtcars
# Plots
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 2)
grid.arrange(g1, g2, g3, nrow = 3)
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 1)
grid.arrange(g2, arrangeGrob(g3, g4, nrow=2), nrow = 1)

# Plots
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 2)
grid.arrange(g1, g2, g3, nrow = 3)
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 1)
grid.arrange(g2, arrangeGrob(g3, g4, nrow=2), nrow = 1)



fig <- plot_ly(
  type = 'scatter',
  x = mtcars$hp,
  y = mtcars$qsec,
  text = rownames(mtcars),
  hoverinfo = 'text',
  mode = 'markers',
  transforms = list(
    list(
      type = 'filter',
      target = 'y',
      operation = '>',
      value = mean(mtcars$qsec)
    )
  )
)

fig

fig <- plot_ly() 
# fig <- fig %>% add_trace( ... )
# fig <- fig %>% layout( ... ) 

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new()
app$layout(
  htmlDiv(
    list(
      dccGraph(figure=fig) 
    )
  )
)

app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)


# Amputación de datos
complete.cases(s100b)
nd<-s100b[complete.cases(s100b),]

#Saber cuantos valores faltantes existen en la base de datos por columna
apply(X = is.na(s100b), MARGIN = 2, FUN = sum)

library(heatmaply)
heatmaply_na(s100b)
library(DMwR2)
# Sustituir los valores mediante K-Vecinos más próximos
knnImputation(s100b,k=1) 

#Calcular la correlación usando los valores faltantes
cor(na.omit(edad), na.omit(st)

# Construcción de un data frame

# Construcción de una matriz de datos

# Variables categóricas
# Las variables categóricas también se denominan variables cualitativas
# o variables de atributos. Los valores de una variable categórica son 
# categorías o grupos mutuamente excluyentes. Los datos categóricos pueden
# tener o no tener un orden lógico.

# Variables cuantitativas
# Los valores de una variable cuantitativa son números que suelen representar
# un contro o una medición.

# Ejemplos de variables cuantitativas
# Tipo de datos	Ejemplos
# Numérico
# 
# Número de quejas de clientes
# Proporción de clientes elegibles para un reembolso
# Peso de llenado de una caja de cereales
# Fecha/hora
# 
# Fecha y hora en que se recibió el pago
# Fecha y hora del incidente de soporte técnico


# Ejemplos de variables categóricas
# Tipo de datos	Ejemplos
# Numérico
# 
# Sexo (1 = Mujer, 2 = Hombre)
# Resultados de una encuesta (1 = De acuerdo, 2 = Neutral, 3 = En desacuerdo)
# Texto
# 
# Formas de pago (Efectivo o Crédito)
# Configuraciones de una máquina (Bajo, Medio, Alto)
# Tipos de producto (Madera, Plástico, Metal)
# Fecha/hora
# 
# Días de la semana (lunes, martes, miércoles)
# Meses del año (enero, febrero, marzo)


# Vamos a usar el paquete ggplot2 para la visualización de datos


# Vamos a realizar gráficos interactivos usando el paquete plotly

glu<-s100b$Glucose
# Verificamos si existen NA
is.na.data.frame(s100b$Glucose)#Valores Faltantes
which(is.na.data.frame(s100b$Glucose)) #Encontar la posición del dato faltante
sum(is.na.data.frame(s100b$Glucose)) # cuantos valores tiene la base de datos   
complete.cases(s100b$Glucose) #Saber cuantos casos tenemos completo
sum(complete.cases(s100b$Glucose))   # Cuantos casos completos hay
is.na(s100b$Glucose)
mean(as.numeric(s100b$Glucose), na.rm=T)  # Valor medio sin valores faltante
bn<-na.omit(s100b$Glucose) #Nueva base de datos sin valores faltante

# Sustitución por la media
s100b$Glucose[is.na(s100b$Glucose)]<-mean(s100b$Weight, na.rm=TRUE) 



# Agregar columna nueva a la base de datos
cut(glu, breaks = 3) #Corta en tres intervalos
cut(glu, breaks = c(0,60, 90, 120)) #Corta en un  intervalo especifico
s100b$n_glu<-cut(s100b$Glucose, breaks = 3,
                 labels = c('Bajo', 'Normal', 'Alto'))

s100b$n_glu

hist(s100b$Glucose)
boxplot(glu)
boxplot.stats(glu)
min(s100b$Glucose)
max(glu)


c <- cut(glu, breaks = c(0,80, 90, 130))
 c<-as.numeric(c)
levels(c) <- c("Bajo", "Normal", "Alto")
hist(c)



ba<-s100b %>%
  filter(Glucose >= 110 & Glucose <=130 & Weight >= 77.0 & Weight<= 90  ) #
barplot(table(c), cex.names = 0.8, main="Personas por etapas de vida",
        col = terrain.colors(4, alpha = 0.8), font=2)
hh<-s100b[s100b$Age <= 60 & s100b$Age >= 50,]




BD2 <- read_csv("C:/Users/Admin/Desktop/Semestre_Enero_Julio_2021/Verano/BD2.csv")

edad<-BD2$Age
mean(edad)





