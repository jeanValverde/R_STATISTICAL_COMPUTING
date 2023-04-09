library("readxl")
library(ggpubr) 
library(dplyr)

#Limpiar values.
rm(list = ls())

#Ingresar datos
datos <- read_excel(file.choose())

#Filtrar y selecionar datos segun lo solicitado
oriental <- datos %>% filter(Raza=="Oriental")
oriental <- oriental %>% select(Previo)

#Formulo hipotesis

    #Hipotesis nula (H0): EL tiempo medio de los atletas de raza oriental previo es superior o igual a 19,8. mu >= 19.8
   #Hipotesis alternativa (Ha): El tiempo medio de los atletas de raza oriental previo es inferior a 19,8 segundos. mu < 19.8

#Establecer datos
n <- length(oriental$Previo)
media_muestra <- mean(oriental$Previo)
sd <- sd(oriental$Previo)
valor_nulo = 19.8
grados_libertad <- n -1
alpha = 0.05  #Nivel de significacion
error <- sd/sqrt(n)

#Primero verifico que la distribucion se acerca a la normal (para verificar si es posible usar la prueba t)
g <- ggqqplot(data = oriental,
              x = "Previo",
              color = "red",
              xlab = "Teorico",
              ylab = "Muestra",
              tittle = "Q-Q VS DIST. NORMAL")
print(g)

              #Conclusion: El grafico g, presenta una tendencia cercana a la normal y no se ven valores atipicos.


#Calculo el estadistico de prueba 
cat("\tPrueba t para la muestra \n\n")
t <- (media_muestra - valor_nulo) / error
cat("t =", t, "\n")

#calculo valor p
p <- pt(t,                        #valor t
        df = grados_libertad,     #grados de libertad
        lower.tail = TRUE)        #cola inferior TRUE; cola superior = FALSE

cat("p = ", p, "\n")

#Calculo del intervalo de confianza.
t_critico <- qt(alpha, df = grados_libertad, lower.tail = FALSE)
superior <- media_muestra + t_critico*error
cat("Intervalo de confianza = (-Inf, ", superior, "]\n", sep = "")

prueba <- t.test(oriental,              #Muestra
                 alternative = "less",  #Tipo de prueba: "two.sided" (bilateral), "greater" (unilateral, media mayor al valor nulo), "less" (unilateral, media manor al valor nulo) 
                 mu = valor_nulo,       #mu = valor nulo
                 conf.leve = 0.95)   #nivel de confianza

print(prueba)


cat("Conclusiones \n")
cat("El valor p al ser mayor que el nivel de significacion alpha ", p,">", alpha, ". Por tanto no hay suficiente evidencia para rechazar la hipotesis nula.")
cat("Sin embargo, la prueba t, nos proporciona un error de tipo I, donde se rechaza la hipotesis nula, cuando en realidad es verdadera")

















