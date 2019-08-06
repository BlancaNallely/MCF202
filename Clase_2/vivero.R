#Blanca Hernández
#05/08/2019
#clase 2

#BASE DE DATOS VIVEROS


# Importar Datos Vivero ---------------------------------------------------

vivero <- read.csv("C:/MCF202-2019/MCF202/Datos/Tvivero.csv", header = T)
summary(vivero)


# Prueba de t de una muestra ----------------------------------------------

par(mfrow=c(1,1))
boxplot(vivero$IE)
t.test(vivero$IE, mu = 0.85)
#Hipotesis Nula

#La media observada no es diferente estadisticamente ya que el valor
#de P es mayor que el alfa establecido (0.05). Además la media teorética se 
#encuentra dentro del rango de los valores de intervalos de confianza.

t.test(vivero$IE, mu = 0.9)
#Hipotesis alternativa 

#La media observada es diferente a la medida teorética, por lo cual aceptamos
#la H1. 1 valor de p(0.01) es menor que el valor de alfa establecido (0.05)


# Pruebas de t muestras independientes ------------------------------------

boxplot(vivero$IE ~ vivero$Tratamiento, col= "green", xlab = "Tratamiento",
        ylab = "IE")

shapiro.test(vivero$IE)

var.test(vivero$IE ~ vivero$Tratamiento)

#Las varianzas de ambos tratamientos son iguales asi lo prueba el valor de p
#obtenido mediante la prueba de varianzas (var.test)
t.test(vivero$IE ~ vivero$Tratamiento,var.equal = T)

#Existencia un diferencia significativa entre el IE de las plantas fertilizadas,
#El valor de p (0.004) comprueba nuestra hipotesis de que el fertilizante
#"power" mejora el IE.

t.test(vivero$IE ~ vivero$Tratamiento)


# Pruebas de t muestras dependientes --------------------------------------

t.test(vivero$IE ~ vivero$Tratamiento, paired = T)


# Ejercicio Producción ----------------------------------------------------
inventario <- read.csv("C:/MCF202-2019/MCF202/Datos/Produccion.csv", header = T)
summary(inventario)

boxplot(inventario$Kgsem ~ inventario$Tiempo, col= "blue", xlab = "kgsem",
ylab = "Kgsem")

t.test(inventario$Kgsem ~ inventario$Tiempo, paired = T)



boxplot(inventario$Germ ~ inventario$Tiempo, col= "green", xlab = "Germ",
        ylab = "Germ")

t.test(inventario$Germ ~ inventario$Tiempo, paired = T)

#Para sacar las medias de la germinacion en el tiempo

tapply(inventario$Germ, inventario$Tiempo, mean)
