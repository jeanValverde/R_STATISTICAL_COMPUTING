library("readxl")
library(ggpubr) 
library(dplyr)

#Limpiar values.
rm(list = ls())

#¿Es posible afirmar que, en promedio, 
#los atletas de raza negra superan a los de raza oriental por 3 segundos después del entrenamiento?

#Formulo hipotesis

 #Hipotesis nula (H0):  No hay diferencia en el tiempo promedio de los atletas de raza negra y los de raza oriental 
 #Hipotesis alternativa (HA): El tiempo promedio de los atletas de raza negra superan a los de raza oriental en 3 segundos 

#Establecer datos



#Ingresar datos
datos <- read_excel(file.choose())

#Filtrar y selecionar datos segun lo solicitado
oriental <- datos %>% filter(Raza=="Oriental")
oriental <- oriental %>% select(Posterior)

negra <- datos %>% filter(Raza=="Negra")
negra <- negra %>% select(Posterior)

valor_nulo  <- 3 #segundos 

#distribucion 
oriental_normalidad <- shapiro.test(oriental$Posterior)
oriental_g <- ggqqplot(data = oriental,
              x = "Posterior",
              color = "red",
              xlab = "Teorico",
              ylab = "Muestra",
              tittle = "Q-Q VS DIST. NORMAL")

negra_normalidad <- shapiro.test(negra$Posterior)
negra_g <- ggqqplot(data = negra,
              x = "Posterior",
              color = "blue",
              xlab = "Teorico",
              ylab = "Muestra",
              tittle = "Q-Q VS DIST. NORMAL")
print(negra_g)

cat("Normaldiad raza oriental: " , oriental_normalidad$p.value, "\n")
cat("Normalidad raza negra: " , negra_normalidad$p.value, "\n")
cat("Debido a que la normalidad de la raza negra en el valor p es 
bajo podemos bajar el nivel de significacion en la prueba \n")

alfa <- 0.05  #Nivel de significacion
cat("Nivel de significacion: ", alfa, "\n" )


#prueba
prueba <- t.test( x = oriental$Posterior , y = negra$Posterior , 
                paired = FALSE , 
                alternative = "greater" ,
                mu = valor_nulo, 
                conf.level = 1 - alfa )

cat("Prueba t para dos muestras independientes: ", prueba$p.value)


media_oriental <- mean (oriental$Posterior)
media_negra <- mean(negra$Posterior)
diferencia <-  media_oriental - media_negra
cat("Diferencia de las medias = " , diferencia , "s\n ")


cat("Conclusiones \n")
cat("Al aplicar la prueba t, obtenemos que la diferencia entre las medias es, ", diferencia, "[s] y que elintervalo de confianza es ", 
prueba$conf.int , " . Además, el valor p: ", prueba$p.value )


cat(prueba$p.value, ">", alfa, "? H0 es correcto : H1 es correcto")















