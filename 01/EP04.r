#El Comité Olímpico cree que el mejor tiempo medio de los atletas de raza oriental antes de 
#ingresar al programa de entrenamiento es inferior a 19,8 segundos. ¿Soportan los datos esta afirmación?

#¿Sugieren los datos que la mejor marca de los atletas de raza negra se reduce en promedio más de 1,5 
#segundos tras el entrenamiento?

#¿Es posible afirmar que, en promedio, los atletas de raza negra superan a los de raza oriental por 3 
#segundos después del entrenamiento?



if(!require(ggpubr)){
  install.packages("ggpubr", dependencies=TRUE )
  require(ggpubr)
}
 
if(!require(ggplot2)){
  install.packages("ggplot2", dependencies=TRUE )
  require(ggplot2)
}

if(!require(dplyr)){
  install.packages("dplyr", dependencies=TRUE )
  require(dplyr)
}

if(!require(tidyverse)){
  install.packages("tidyverse", dependencies=TRUE )
  require(tidyverse)
}



install.packages("readxl")
install.packages("TeachingDemos")


library("readxl")
library(ggpubr) 
require(ggplot2)  
library(tidyverse)
require(dplyr)

library(TeachingDemos)

rm(list = ls()) #clear  
data <-  read_excel("../Downloads/EP03-datos.xls")   
data <- data %>% filter( Raza == "Oriental") 
data <- data %>% select(Previo)  
arr <- as.matrix(data)

################

#solo 1 muestra 

media <- mean(arr)  

desviacion_estandar <- sd(arr) 

tamaño_muestra <- length(arr)

normalidad = shapiro.test(arr)

alfha <- 2.0 #no se que es eso :( muero    desviacion_estandar? 

valor_nulo <- 19.8

Z <- (media - valor_nulo) / (desviacion_estandar / sqrt(tamaño_muestra))

P <- 2 * pnorm(Z, lower.tail = FALSE)


#comparar dos muestras 


#dos notas por lectura 

#6 notas por trabajos practivos 

#POR OPCIONAL avisar al profesor sobre un 3 y assitencia 75% 
#2 preguntas practicas y 2 escritas  

