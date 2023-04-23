library(ggpubr)

rm(list = ls())

#ACTIVIDAD:
#Se sabe que una máquina que envasa detergentes industriales llena bidones con un volumen de producto que sigue una distribución normal 
#con desviación estándar de 1 litro. Usando una muestra aleatoria de 100 envases, el ingeniero a cargo de la planta requiere determinar 
#si la máquina está llenando los bidones con una media de 10 litros.

#Pregunta 1.
#Si el ingeniero está seguro de que el verdadero volumen medio no puede ser inferior a 10 litros y piensa rechazar la hipótesis nula 
#cuando la muestra presente una media mayor a 10,3 litros, ¿cuál es la probabilidad de que cometa un error de tipo I?

#Defino mis hipotesis:

#     H0: La maquina llena los bidones con una media de 10 litros. mu0 = 10.
#     HA: La maquina esta llenando los vidoes con una media mayor a 10 litros.


# Definir los parámetros
n <- 100                      #numero de observaciones
sigma <- 1                    #Desviacion estandar conocida de la poblacion
mu0 <- 10                     #Media de la hipotesis nula   
media <- 10.3                 #Media de la muestra.
alfa <- 0.05
SE <- sigma/sqrt(n)

#Grafica

x <- seq(97*SE,103*SE,0.01)
y <- dnorm(x, mean=mu0, sd=SE)
g <- ggplot(data = data.frame(x,y), aes(x))
g <- g + stat_function(fun = dnorm,
                       args = list(mean = mu0, sd = SE),
                       colour = "red",
                       size = 1)
g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name = "Hola")

g <- g + theme_pubr()
print(g)



# Calculo pnorm
p <- pnorm(media,
           mean = mu0, 
           sd = sigma/sqrt(n),
           lower.tail = FALSE
           )

cat("La probabilidad de alfa es: ",p*100,"%\n")

#Por lo tanto la probabilidad de cometer un error de tipo 1, es de 0.135% aproximadamente.

#Pregunta 2
#Si el verdadero volumen medio de los bidones fuera de 10,2 litros,
#¿cuál sería la probabilidad de que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?

#Formulo Hipotesis:
    #H0: El verdadero volumen medio de los bidones es de 10,2 litros.
    #HA: El verdadero volumen medio de los bidones es distinto de 10,2.
    

media <- 10.2

#aplicamos la power.t.test, con valor a buscar power.
cat("Calculo el poder:")
power <- power.t.test(n = n,
                      delta = media-mu0,
                      sd = sigma,
                      sig.level = alfa,
                      power = NULL,
                      type = "two.sample",
                      alternative = "two.sided")$power

print(power)

beta <- 1-power
print(beta)

cat("El porcentaje de cometer un error de tipo 2 es:",beta*100,"%")


#Pregunta 4.
#Considerando un volumen medio de 10 litros, 
#¿cuántos bidones deberían revisarse para conseguir un poder estadístico de 0,75 y un nivel de significación de 0,05?

media <- 10.2

bidones <- power.t.test(n = NULL,
                        delta = media-mu0,
                        sd = 1,
                        sig.level = 0.05,
                        power = 0.75,
                        type = "two.sample",
                        alternative = "two.sided"
                        )$n

print(bidones)

#Pregunta 5
#¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I a un 1% solamente?

bidones <- power.t.test(n = NULL,
                        delta = media-mu0,
                        sd = 1,
                        sig.level = 0.01,    #Modificamos alfa a 1%
                        power = 0.75,
                        type = "two.sample",
                        alternative = "two.sided")$n

print(bidones)






                    
                    














              









