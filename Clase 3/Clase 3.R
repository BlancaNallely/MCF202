#Blanca Hernandez
#07/08/2018
#Clase 3

#EJERCICIO 1
#Procedimiento general para la prueba de t independientes
grupo <- gl(2,12, labels = c("Fotografia", "Araña"))
Ansiedad <- c(30, 35, 45, 40, 50, 35, 55, 25, 30, 45, 40, 50, 40, 35, 50, 55,
              65, 55, 50, 35, 30, 50, 60, 39)
summary(grupo)

Datos <- data.frame(grupo,Ansiedad)
head(Datos)

#Diferencias que existen entre los grupos
#Analisis de muestras independientes
boxplot(Datos$Ansiedad ~ Datos$grupo, col="lightgreen", ylab="Nivel de ansiedad")

#La media de grupos
tapply(Datos$Ansiedad, Datos$grupo, mean)

mean(Datos$Ansiedad)

#Describa la hipotesis nula y la alternativa
#H0= No existe una diferencia significativa entre los niveles de ansiedad
#de los dos grupos.

#H1= si existen diferencias significativas entre los grupos.

shapiro.test(Datos$Ansiedad)

bartlett.test(Datos$Ansiedad, Datos$grupo)

#Tienen una distribucion normal ya que p (0.4977)es mayor al alfa (0.05)

library(pastecs)

by(Datos$Ansiedad, Datos$grupo, stat.desc, basic= FALSE, norm= TRUE)

#Aplicar la prueba de t

gr.t <-t.test(Datos$Ansiedad ~ Datos$grupo, var.equal= TRUE)

n<- length(grupo)


# Ejercicio 2 -------------------------------------------------------------

#Hipotesis nula (H0): la media que tiene el consumidor es igual al que dice la empresa (la media es igual a 80)

#La hipotesis alternativa (H1):que la media del consumidor es menor a 80 que dice el vendedor


costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)

summary(costal)

#Determinar el numero de observaciones
n <- length(costal)
n

#Determinar la media

costa.media <- mean(costal)
costa.media

#Sacar la desviacion estandar

costa.sd <- sd(costal)
costa.sd

#Formula para obtner el valor de t
#Sacar el valor de t
costa.se <- costa.sd/ sqrt(n)
costa.se

#Valor de T
costa.T <- (costa.media - 80)/costa.se
costa.T

#Calcular el valor de P
pt(costa.T, df = n-1)

#Procedimiento en R
t.test(costal, mu=80, alternative = "less") 
