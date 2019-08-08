#Blanca Hernández
#05/08/2019
#clase 0

# #pasos básicos ----------------------------------------------------------

2+2
a <- 2

a + a
a + 5


# Importar datos ----------------------------------------------------------

diametro <- c(12, 8.6, 9.2, 7.7, 12.9, 11.7, 9.7, 14.2,
              11.8, 14.3, 12.5)
diametro

#Medidas de tendencia central

mean(diametro)
median(diametro)

#Medidas de dispersión
sd(diametro)
var(diametro)


# Graficas ----------------------------------------------------------------

boxplot(diametro, horizontal = TRUE, col = "lightblue", main="Diametro",
        xlab="D (cm)")


# Importar datos ----------------------------------------------------------

BD_alturas <-read.csv("c:/MCF202-2019/MCF202/Datos/Alturas.csv")

#Graficas
boxplot(BD_alturas$crecimiento)
boxplot(BD_alturas$crecimiento ~ BD_alturas$tratamiento,
        col= "lightgreen",
        xlab= "tratamiento",
        ylab= "crecimiento",
        main= "Efectos de fertilizante" )

mean (BD_alturas$crecimiento)

#Restricciones ----------------------------------------------------------
sum(BD_alturas$crecimiento < mean(BD_alturas$crecimiento))

#Excluir el Tratamiento A

TratA <- BD_alturas[!(BD_alturas$tratamiento == "TA"),]

mean(TratA$crecimiento)


# Submuestra --------------------------------------------------------------

T.mean <- subset(BD_alturas,crecimiento >= mean(BD_alturas$crecimiento))
boxplot(T.mean$crecimiento ~ T.mean$tratamiento)
                 

