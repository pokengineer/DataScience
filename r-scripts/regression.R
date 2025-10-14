library('ISLR2')
plot(Boston$rm, Boston$medv)
##################### Promedios Locales ##################

local_average_reg <- function(x, y, x_nuevo, h) {
  region <- y[x >= (x_nuevo - h) & x <= (x_nuevo + h)]
  return(mean(region))
}
local_average_reg( Boston$rm,  Boston$medv, 7.5 , 0.5)

####################### KNN ##############################

knn_reg <- function(x, y, x_nueva, k) {
  dist <- abs(x - x_nueva)
  vecinos <- order(dist)[1:k]
  return(mean(y[vecinos]))
}
knn_reg( Boston$rm,  Boston$medv, 7.5 , 7)


####################### Nadaraya-Watson ##############################
gaussian_kernel <- function(u) {
  exp(-0.5 * u^2) / sqrt(2 * pi)
}

nadaraya_watson_reg <- function(x, y, x0, h) {
  u <- (x - x0) / h
  weights <- gaussian_kernel(u)
  estimate <- sum(weights * y) / sum(weights)
  return(estimate)
}

nadaraya_watson_reg( Boston$rm,  Boston$medv, 7.5 , 0.01)

######################## ksmooth ########################################

ksmooth(Boston$rm,  Boston$medv,x.points=7.5, kernel = "normal", bandwidth = 0.1)$y

# dejo un código de loocv para encontrar un h óptimo,
# esta en otro script creo pero porlas lo meto aca
cv_ksmooth <- function(x, y, h_seq) {
  sapply(h_seq, function(h) {
    preds <- sapply(1:length(x), function(i) {
      xi <- x[-i]
      yi <- y[-i]
      ksmooth(xi, yi, x.points = x[i], bandwidth = h, kernel = "normal")$y
    })
    mean((y - preds)^2)
  })
}
h_seq <- seq(0.1, 3, by = 0.1)
mse_seq <- cv_ksmooth(Boston$rm,  Boston$medv, h_seq)
plot(h_seq, mse_seq, type = "b", pch = 19, col = "blue",
     xlab = "Bandwidth (h)", ylab = "CV MSE",
     main = "CV Error vs. Bandwidth for Nadaraya-Watson")