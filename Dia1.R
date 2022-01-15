# Verano de Investigación ####

# Vamos crear el primer objeto 

A<-8   # Asignación de variables 
B<-9   # Asignación de variables

C<-A+B   # Suma o resta
D<-A*B   # Multiplicación 
E<-A/B   # División
G<-A**3  # Potencia
H<-sqrt(A) # Raíz cuadrada


#Imprimir resultados
A<-8   # Asignación de variables 
B<-9   # Asignación de variables

C<-A+B   # Suma o resta
D<-A*B   # Multiplicación 
E<-A/B   # División
G<-A**3  # Potencia
H<-sqrt(A) # Raíz cuadrada

# Colección 

J<-c(1,2,3,4)   # Cadena de valores

mean(J)
var(J)
sd(J)
K<-mean(J)+3*C+D**2
J[1]


x<-c(1,2,3,4,5,6,7,8,9,10)
y<-c(1,2,3,4,5,6,7,8,9,10)
z<-c(1,3,5,7,8,9,11,12,13,14)
plot(x,y,main = 'Tiempo vs Distancia', xlab='Tiempo (s)'
     , ylab='distancia (m)', type='o', col='brown', lwd=2)
lines(x,z, col='red', type='o', lwd=2)



# libraries:
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)



# Primer Ejercicio ####