#Blanca Hernandez
#07/08/2018
#Clase 3

grupo <- gl(2,12, labels = c("Fotografia", "Araña"))
Ansiedad <- c(30, 35, 45, 40, 50, 35, 55, 25, 30, 45, 40, 50, 40, 35, 50, 55,
              65, 55, 50, 35, 30, 50, 60, 39)
summary(grupo)

Datos <- data.frame(grupo,Ansiedad)
head(Datos)
#Analisis de muestras independientes
boxplot(Datos$Ansiedad ~ Datos$grupo, col="lightgreen", ylab="Nivel de ansiedad")


#La media de grupos
tapply(Datos$Ansiedad, Datos$grupo, mean)

mean(Datos$Ansiedad)


#Describir la hipotesis nula y alternativa
shapiro.test(Datos$Ansiedad)

#Tienen una distribucion normal ya que p (0.4977)es mayor al alfa (0.05)

library(pastecs)

by(Datos$Ansiedad, Datos$grupo, stat.desc, basic= FALSE, norm= TRUE)

#Aplicar la prueba de t

gr.t


# Ejercicio 2 -------------------------------------------------------------

#Hipotesis nula: la media que tiene el consumidor es igual al que dice la empresa (la media es igual a 80)
#La hipotesis alternativa:que la media del consumidor es menor a 80 que dice el vendedor


costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)
summary(costal)

#Determinar el numero de observaciones
n <- length(costal)

#Determinar la media
mean(costal)

costal.media <- mean(costal)
costal.media
#Sacar la desviacion estandar

costa.sd <- sd(costal)
costa.sd

#Formula para obtner el valor de t
#Sacar el valor de t
costa.se <- costa.sd/sqrt(n)

#Valor de T
costa.T <- (costa.media - 80)/costa.se

#Calcular el valor de P
pt(costa.T, df = n-1)

t.test(costal, mu=80, alternative = "less")


