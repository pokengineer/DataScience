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
