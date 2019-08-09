#Blanca Hernández
#09/08/2019
#Clase 5


# Establecer hipotesis ----------------------------------------------------

#H0= No existen diferencias significativas entre las medias de los 
#diferentes tipos de suelo
#H1= Si existen diferencias significativas entre las medias de los 
#diferentes tipos de suelo

# Establecer datos --------------------------------------------------------
arena <- c(6, 10, 8, 6, 14, 17, 9, 11, 7, 11)
arcilla <- c(17, 15, 3, 11, 14, 12, 12, 8, 10, 13)
limo <- c(13, 16, 9, 12, 15, 16, 17, 13, 18, 14)

y.ton <-c(arena, arcilla, limo)
suelo <-gl(3, 10, 30, labels=c("arena", "arcilla", "limo"))

prod <-data.frame(suelo, y.ton)
head(prod)

# Sacar medias ------------------------------------------------------------
tapply(prod$y.ton, prod$suelo, mean)

#Sacar las varianzas
tapply(prod$y.ton, prod$suelo, var)

# Normalidad de datos -----------------------------------------------------
shapiro.test(prod$y.ton)

#Los datos son de distribucion normal

# Para determinar homogeniedad de varianza --------------------------------

#Más robusta
bartlett.test(prod$y.ton, prod$suelo)
#Más ligera
fligner.test(prod$y.ton, prod$suelo)

#Graficas
boxplot(prod$y.ton ~ prod$suelo, xlab = "Tipo de suelo",
        ylab = "Ton/ha", col= "yellow")

aov.suelo <- aov(prod$y.ton ~ prod$suelo)
aov.suelo
summary(aov.suelo)

#Inspección visual

par(mfrow=c(2,2))
plot(aov(prod$y.ton ~ prod$suelo))

summary.lm(aov.suelo)

par(mfrow=c(1,1))   

TukeyHSD(aov.suelo, conf.level = 0.95)

#Graficar Tukey

plot(TukeyHSD(aov.suelo))


     