library("readxl")
library(ggpubr) 
library(dplyr)

#Limpiar values.
rm(list = ls())

#Ingresar datos
datos <- read_excel(file.choose())

raza_negra <- datos %>% filter(Raza=="Negra")
raza_negra <- raza_negra %>% select(Previo,Posterior)
diferencia <- raza_negra$Previo - raza_negra$Posterior

#¿Sugieren los datos que la mejor marca de los atletas de raza negra se reduce en promedio más de 1,5 segundos tras el entrenamiento?

  #Hipotesis:
                #Hipotesis nula: La mejor marca de la raza negra de atletas no se reduce en promedio a mas 1,5 segundos tras el entrenamiento. mu <= 1.5 segundos
                #Hipotesis alternativa: La mejor marca de la raza negra de atletas se reduce en promedio a mas de 1.5 segundos tras el entrenamiento. mu>1.5 segundos



#Verificar si la distribucion se acerca a la normal
g <- ggqqplot(data = data.frame(diferencia),
              x = "diferencia",
              color = "steelblue",
              xlab = "Teorico",
              ylab = "Muestra",
              title = "Grafico Q-Q vs distr. normal")

print(g)      #Conclusion: La muestra sigue una distribucion normal, sin valores atipicos.

normalidad <- shapiro.test(diferencia)

print(normalidad) #Conclusion: el test presenta un w = 0.98 aproximadamente, lo que es cercano a 1, por tanto indica que los datos tienen una distribucion normal.

#Establecer un alfa
alpha <- 0.05

#Aplicar la prueba t a la diferencia de medias
valor_nulo <- 1.5
pruebat <- t.test(diferencia,
                  alternative = "greater",
                  mu = valor_nulo,
                  conf.level = 1-alpha)

print(pruebat)


#Conclusion: 
#El valor p = 0.2059 es mayor que alpha = 0.05, es decir p>alpha, por tanto no existe evidencia suficiente para rechazar la hipotesis nula.
#Por tanto la hipotesis nula se acepta.



