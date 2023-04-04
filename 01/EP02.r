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


rm(list = ls()) #clear  
data <- read.csv2("C:/Users/jgonzalez/Downloads/EP02-Datos.csv")


#¿Tienen hombres y mujeres ingresos similares en la RM?
dt <- data %>% filter( region == "Región Metropolitana de Santiago") 
dt <- dt %>% select( sexo, ytot  )  

resultado <- dt %>%  group_by(sexo) %>%  summarize(ytot = sum(ytot))
ggplot(data = dt) +  geom_bar(aes(x = dt$sexo , y = dt$ytot), stat = "identity", color = "red" )  


#¿Se distribuye de igual manera la situación ocupacional de los hombres que viven en áreas rurales y quienes viven en áreas urbanas de la RM?
ocupacional <- data %>% filter( region  == "Región Metropolitana de Santiago", sexo == "Hombre"  )  
ocupacional <- ocupacional %>% select(zona, ch1) 
ocupacional <- ocupacional %>%   group_by(zona) %>% summarise(cantidad = n()) 
ggplot(data = ocupacional, aes(x = zona, y = cantidad ) ) + geom_bar(stat = "identity", color = "red")


#¿Van los ingresos de las mujeres de la RM incrementándose con la edad?
incremento <- data %>% filter(region == "Región Metropolitana de Santiago", sexo == "Mujer" )
incremento <- incremento %>% select( edad, ytot ) %>% arrange( edad ) 
ggplot(data = incremento, aes(x = edad , y = ytot ) ) + geom_bar(stat = "identity", color = "red")

#Cómo diría que es el ingreso de los hombres de la RM? (simétrico/asimétrico, concentrado/disperso, unimodal/multimodal, etc.)
ingreso <- data %>% filter( region  == "Región Metropolitana de Santiago", sexo == "Hombre"  )  
ingreso <- ingreso %>% select( ytot ) %>% arrange(ytot)



