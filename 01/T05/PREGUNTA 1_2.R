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
 
# Calculo pnorm
p <- pnorm(media,
           mean = mu0, 
           sd = sigma/sqrt(n),
           lower.tail = FALSE
)

cat("La probabilidad de alfa es: ",p*100,"%\n")

#Por lo tanto la probabilidad de cometer un error de tipo 1, es de 0.135% aproximadamente.
