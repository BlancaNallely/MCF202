#Blanca Hernández
#05/08/2019
#clase 0

# #pasos básicos ----------------------------------------------------------

2+2
a <- 2

a + a
a + 5

diametro <- c(12, 8.6, 9.2, 7.7, 12.9, 11.7, 9.7, 14.2,
              11.8, 14.3, 12.5)
diametro
#Medidas de tendencia central

mean(diametro)
median(diametro)

#Medidas de dispersión
sd(diametro)
var(diametro)

# Gráficas ----------------------------------------------------------------

boxplot(diametro, horizontal = TRUE, col = "lightblue", main="Diametro",
        xlab="D (cm)")


# Importado excel actvividad 1---------------------------------------------------------

DB_alturas <- read.csv("c:/MCF202-2019/Datos/alturas.csv", header = T)
head(DB_alturas)
boxplot(DB_alturas$crecimiento)
boxplot(DB_alturas$crecimiento ~ DB_alturas$tratamiento,
        col= "ligthgreen",
        xlab= "Tratamientos",
        ylab= "crecimiento (cm)",
        main= "Efectos del fertilizante")

mean(DB_alturas$crecimiento)


#Restricciones ----------------------------------------------------------

sum(DB_alturas$crecimiento < mean(DB_alturas$crecimiento))

#Excluir el Tratamiento A
Trat_A <- DB_alturas[!(DB_altura$tratamiento == "TA"),]

mean(Trat_A$crecimiento)


# Submuestra --------------------------------------------------------------

T.mean <- subset(DB_alturas, crecimiento >= mean(DB_alturas$crecimiento)
boxplot(T.mean$crecimiento ~ T.mean$tratamiento)                 

