#Descargar el archivo (EP01-Datos.csv) obtenido desde el repositorio del Ministerio de Ciencia, Tecnología, Conocimiento, e Innovación que da cuenta del número de casos nuevos de COVID-19 confirmados por día según resultado del diagnóstico y que han presentado síntomas, por región de residencia, reportados por el Ministerio de Salud. Esta serie de tiempo incluye los casos nuevos denominados "con síntomas" por la autoridad sanitaria.

#Filtrar los datos considerando la pregunta asignada al equipo, de manera de quedarse con los datos relevantes.

#Obtener la media y desviación estándar del conjunto de datos relevantes.

#Seleccionar una muestra aleatoria (pero reproducible) de 16 valores. Obtenga la media de estos valores.



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

library(ggpubr) 
require(ggplot2)  
library(tidyverse)


data <- read.csv("./Datos-EP01.csv")

dt <- data %>% filter(Region == "O’Higgins") 
dt <- dt %>% select( X2022.12.21 :   X2023.03.20  )  

arr <- as.matrix(dt)
 
#MEDIA   
media_pob <- mean(arr) # media de un arreglo
media_pob

#DESVIACION ESTANDAR 
desviacion_estandar <- sd(arr)
desviacion_estandar

#MUESTRA
tamaño_muestra <- 16
muestra <- sample(dt, tamaño_muestra) # obtencion de muestra
muestra

