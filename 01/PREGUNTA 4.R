#PREGUNTA 4

# Cálculo del tamaño de muestra necesario

# Especificación de los parámetros
mu0 <- 10     # Hipótesis nula
mu1 <- 10.2   # Hipótesis alternativa
sd <- 1       # Desviación estándar poblacional
alpha <- 0.05 # Nivel de significancia
power <- 0.75 # Potencia deseada


#n = NULL (numero de observaciones) ya que es quien debemos determinar 
#d = Diferencias entre las medidas / por la desviacion estandar agrupada 
#sig.level = probabilidad error TIPO I 
#power= potencia deseada 
#type= dos muestras 
#alternative = dos 

n <- ceiling(pwr.t.test(n = NULL, d = abs(mu1 - mu0) / sd, sig.level = alpha, power = power, type = "two.sample", alternative = "two.sided")$n)

cat("El numero de observaciones necesarios son: ", n)


