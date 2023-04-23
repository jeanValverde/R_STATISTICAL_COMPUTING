library(ggpubr)
library(tidyverse)

rm(list = ls())

n <- 100                      # Tamaño de la muestra
sigma <- 1                    # Desviación estándar conocida de la población
mu0 <- 10                     # Media de la hipótesis nula
alfa <- 0.05                  # Nivel de significancia

#genero la muestra con las diferentes medias
mu <- seq(10, 10.5, by = 0.1)

#Calculo power.t.test para cada media
poder <- sapply(mu, function(m) power.t.test(n = n, 
                                             delta = m - mu0, 
                                             sd = sigma, 
                                             sig.level = alfa,
                                             power = NULL, 
                                             type = "one.sample",
                                             alternative = "two.sided")$power)

#Creo una data frame que contenga los poderes y las medias respectivas
datos <- data.frame(mu = mu, poder = poder)
    
# Gráfico del poder estadístico
plot(mu, poder, type = "l", xlab = "Media poblacional", ylab = "Potencia",   main = "Gráfico del poder estadístico") 






