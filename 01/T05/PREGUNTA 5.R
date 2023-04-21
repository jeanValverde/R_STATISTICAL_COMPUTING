
#PREGUNTA 5

# Especificación de los parámetros

mu0 <- 10     # Hipótesis nula
mu1 <- 10.2   # Hipótesis alternativa
sd <- 1       # Desviación estándar poblacional
alpha <- 0.01 # Nivel de significancia
power <- 0.75 # Potencia deseada

# Cálculo del tamaño de muestra necesario
n <- ceiling(pwr.t.test(n = NULL, d = abs(mu1 - mu0) / sd, sig.level = alpha, power = power, type = "two.sample", alternative = "two.sided")$n)
cat("El numero de observaciones necesarios son: ", n)
