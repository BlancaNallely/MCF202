#Blanca Hernandez
#09/08/2019
#Clase 4

# Correlación -------------------------------------------------------------

library(repmis)
erupciones <- source_data("https://dl.dropboxusercontent.com/s/liir6sil7hkqlxs/erupciones.csv")

plot(erupciones$waiting, erupciones$eruptions, pch= 19, col= "red",
     xlab = "Tiempo de espera (min)",
     ylab = "Duración en (min")

library(pastecs)
stat.desc(erupciones$eruptions, basic = FALSE, norm = TRUE)

shapiro.test(erupciones$eruptions)

#Segun la prueba de shipiro los datos no son de distribucion normal ya que se encuentran 
#por debajo de el alfa establecido 0.05. Además de que involucran la variable tiempo la cual 
#normalmente los datos no son normales.

shapiro.test(log(erupciones$eruptions))
shapiro.test(erupciones$waiting)

cor.test(erupciones$eruptions, erupciones$waiting)
#Si hay una correlacion 
#La correlacion es significativa porque esta por debajo de 0.05 por lo cual se acepta H1


# Regresión Lineal  -------------------------------------------------------

#Hipotesis general: Que el tiempo de espera nos ayudara a predecir la duracion de la
#proxima erupcion del geyser Old Faithfull.

#H0= no es significativa para la predicción.
#H1= si es significativa para predecir.

#Comando "ml" para realizar la regresión
lm.erup <- lm(erupciones$eruptions ~ erupciones$waiting)

#Grafica
plot(erupciones$waiting, erupciones$eruptions, pch= 19, col= "blue",
     xlab = "Tiempo de espera (min)",
     ylab = "Duración en (min")

abline(lm.erup, col= "green")

text(52, 4.5, "Y = -1.87 + 0.07*x")
text(52, 4, "r^2 = 0.81")

lm.erup
summary(lm.erup)

length(erupciones$eruptions)

sqrt(0.90)
(0.90)^2

#Para saber la duración en tiempo de espera de 60 min
y.60 <- -1.87 + 0.07*60
y.60


# Datos de regresión ------------------------------------------------------

espera <-erupciones$waiting
duracion <- erupciones$eruptions

res <- resid(lm.erup)
res
sum(res)

pre <- fitted(lm.erup)
res.2 <- res^2

cuadro <- round(data.frame(espera, duracion, pre, res,
                           res.2),4)
SSE <- sum(cuadro$res.2)
SSE

SSE <- sum((duracion - pre)^2)                
SSE

vari <- SSE/(length(erupciones$waiting)-2)
vari                               
 
# Prueba de hipotesis de la regresión -------------------------------------

an.erup <- anova(lm.erup)
an.erup

#aceptamos la hiotesis alternativa que el modelo de regresion aplicado son 
#significativos, entonces podemos decir que la regresión se puede aplicar.

# Ejercicio 2 -------------------------------------------------------------

#Importa datos de Ebanos (altura y diametro)
ebanos <- read.csv("C:/MCF202-2019/MCF202/Datos/ebanos.csv", header= T)

#Establecer la hipotesis
#hipotesis Nula (H0): No existen diferencias significativas entre las variables
#diametro y altura

#Hipotesis alternativa (H1): Si existen diferencias significativas entre las variables
#diametro y altura.
#Grafica

plot(ebanos$diametro, ebanos$altura, pch=19, col= "blue",
     xlab = "Diametro",
     ylab = "Altura")

#Se realizo la prueba de stat.desc solo con altura ya que es la variable dependiente.
library(pastecs)
stat.desc(ebanos$altura, basic = FALSE, norm= TRUE)

#Prueba de normalidad de datos
shapiro.test(log(ebanos$altura))
shapiro.test(ebanos$altura)
#Si existen diferencias significativas en la normalidad de los datos, 
#y de distribucion anormal ya que el valor obtenido de p-value (0.008242) es menor a
#el alfa establecido de 0.05.

shapiro.test(ebanos$diametro)
#La distribucion es anormal, entonces si existen diferencias significativas 
#segun la prueba de shapiro que nos da un valor de p-value de 1.215e-05.

cor.test(ebanos$diametro, ebanos$altura)

#Deacuerdo a la prueba de correlacion aceptamos la hipotesis alternativa H1
#la cual nos indica que si hay diferencias significativas en los datos de diametro
#y altura ya que el p-value nos da un valor de 2.2e-16 que es menor al alfa de 0.05.



