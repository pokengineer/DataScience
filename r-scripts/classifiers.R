####################### Functiones de clasificacion ############################
clas_knn <- function(x, y, x_nuevo, k)
{
  dist <- abs( x - x_nuevo)
  vecinos <- order( dist )[1:k]
  return( (mean(y[vecinos])>0.5)*1)
}

clas_prop_loc  <- function (x, y, x_nuevo, h)
{
  region <- y[x>=(x_nuevo-h) & x<=(x_nuevo+h)]
  return(  (mean( region )>0.5)*1 )
}

clas_gen <- function(x, y, x_nuevo, h0, h1)
{
  dens0 <- density(x[y == 0], kernel = "gaussian", bw = h0, from = x_nuevo, to = x_nuevo, n = 1)$y
  dens1 <- density(x[y == 1], kernel = "gaussian", bw = h1, from = x_nuevo, to = x_nuevo, n = 1)$y
  propm0 <- mean(y == 0)
  propm1 <- mean(y == 1)
  return( (dens1*propm1 >= dens0*propm0 )*1 )
}

# Read a dataset
df <- read.csv( "D:/uba/aprendizajeEstadistico/alturas_n_490.csv" )
df$y <- (df$genero == "M")*1
par(mar=c(2,2,2,2))

# Using the classifiers
clas_gen( df$altura, df$y, 160, 5,5)
clas_knn( df$altura, df$y, 160, 5)
clas_prop_loc( df$altura, df$y, 160, 5)

####################### Validación Cruzada Leave One Out ############################
loocv_knn <- function(k)
{
  n = nrow(df)
  ypred <- rep(0,n)
  val <- rep(0,n)
  for(i in 1:n){
    ypred[i] <- clas_knn(df$altura[-i], df$y[-i], df$altura[i],k)
    val[i] <- (ypred[i] - df$y[i])**2
  }
  return(mean(val))
}
loocv_v <- rep(1,100)
for(i in 5:100){
  loocv_v[i] <- loocv_knn(i)
}
which(loocv_v == min(loocv_v))

# ---------------------------

loocv_prop_loc <- function(h) {
  n <- nrow(df)
  ypred <- rep(0, n)
  val <- rep(0, n)
  for(i in 1:n){
    ypred[i] <- clas_prop_loc(df$altura[-i], df$y[-i], df$altura[i], h)
    val[i] <- (ypred[i] - df$y[i])^2
  }
  return(mean(val))
}
h_grid <- seq(1.5, 12, by = 0.1)
loocv_vals <- length(h_grid)
for(i in seq_along(h_grid)){
  loocv_vals[i] <- loocv_prop_loc(h_grid[i])
}
h_grid[which(loocv_vals == min(loocv_vals))]
min(loocv_vals)

# ---------------------------

loocv_gen <- function(h0, h1) {
  n <- nrow(df)
  ypred <- rep(0, n)
  val <- rep(0, n)
  for(i in 1:n){
    x_train <- df$altura[-i]
    y_train <- df$y[-i]
    x_test <- df$altura[i]
    y_test <- df$y[i]
    ypred[i] <- clas_gen(x_train, y_train, x_test, h0, h1)
    val[i] <- (ypred[i] - y_test)^2
  }
  return(mean(val))
}
h_vals <- seq(1, 10, by = 0.5)
error_matrix <- matrix(NA, nrow = length(h_vals), ncol = length(h_vals),
                       dimnames = list(paste0("h0=", h_vals), paste0("h1=", h_vals)))

for(i in seq_along(h_vals)){
  for(j in seq_along(h_vals)){
    error_matrix[i, j] <- loocv_gen(h_vals[i], h_vals[j])
  }
}

min_idx <- which(error_matrix == min(error_matrix), arr.ind = TRUE)
best_h0 <- h_vals[min_idx[1]]
best_h1 <- h_vals[min_idx[2]]

####################### Calcular error para un set de testeo ############################

df_test <- read.csv( "D:/uba/aprendizajeEstadistico/alturas_testeo.csv" )
df_test$y <- (df_test$genero == "M")*1

# Valores óptimos hallados anteriormente
best_k <- 15          
best_h <- 9.9
best_h0 <- 2.5
best_h1 <- 3

ypred_knn <- sapply(df_test$altura, function(x_nuevo) {
  clas_knn(df$altura, df$y, x_nuevo, best_k)
})
error_knn <- mean(ypred_knn != df_test$y)

ypred_prop_loc <- sapply(df_test$altura, function(x_nuevo) {
  clas_prop_loc(df$altura, df$y, x_nuevo, best_h)
})
error_prop_loc <- mean(ypred_prop_loc != df_test$y)

ypred_gen <- sapply(df_test$altura, function(x_nuevo) {
  clas_gen(df$altura, df$y, x_nuevo, best_h0, best_h1)
})
error_gen <- mean(ypred_gen != df_test$y)

min(c(knn = error_knn, prop_loc = error_prop_loc, gen = error_gen))


##############################
# Vector xNuevo
xNuevo <- seq(160, 170, by = 0.01)

# Predicciones para cada método
pred_knn <- sapply(xNuevo, function(xn) clas_knn(df$altura, df$y, xn, best_k))
pred_prop_loc <- sapply(xNuevo, function(xn) clas_prop_loc(df$altura, df$y, xn, best_h))
pred_gen <- sapply(xNuevo, function(xn) clas_gen(df$altura, df$y, xn, best_h0, best_h1))

# Encontrar puntos de corte (primera altura en que el clasificador predice 1)
cutoff_knn <- xNuevo[which(pred_knn == 1)[1]]
cutoff_prop <- xNuevo[which(pred_prop_loc == 1)[1]]
cutoff_gen <- xNuevo[which(pred_gen == 1)[1]]

# Título con los puntos de corte
titulo <- paste("Cortes - KNN:", cutoff_knn,
                "| Prop. Local:", cutoff_prop,
                "| Generativo:", cutoff_gen)

# Graficar
plot(xNuevo, pred_knn, type = "l", col = "blue", lwd = 2,
     ylim = c(-0.1, 1.1), xlab = "Altura", ylab = "Clasificación",
     main = titulo)

lines(xNuevo, pred_prop_loc, col = "red", lwd = 2)
lines(xNuevo, pred_gen, col = "darkgreen", lwd = 2)

legend("bottomright", legend = c("KNN", "Promedios Locales", "Generativo"),
       col = c("blue", "red", "darkgreen"), lwd = 2)
