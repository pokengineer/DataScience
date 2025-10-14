#cargar datas
df <- read.csv( "D:/uba/aprendizajeEstadistico/alturas_n_490.csv" )
df$y <- (df$genero == "M")*1

# Calcular la Media Muestral
mean_altura <- mean(df$altura)
cat("Media de Altura:", mean_altura, "\n")

# Calcular la Varianza muestral
var_altura <- var(df$altura)
cat("Varianza muestral de Altura:", var_altura, "\n")

# Desvío estándar muestral
sd_altura <- sd(df$altura)
cat("Desvío estándar muestral:", sd_altura, "\n")

# Distancia intercuartil
# IQR = Q3 - Q1
iqr_altura <- IQR(df$altura)
cat("IQR de Altura:", iqr_altura, "\n")

# Cuartiles
cuartiles <- quantile(x)
cat("Quartiles:", cuartiles, "\n")

cat("\nSummary:\n")
summary(df$altura)