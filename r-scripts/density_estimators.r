df <- read.csv( "D:/uba/aprendizajeEstadistico/datos_sim_ninos.csv" )

proba_est <- function(x, x_0, h){
  sum( x>=x_0-h & x<= x_0+h ) / length(x)
}
densidad_est_parzen <- function(x, x_0, h){
  proba_est(x, x_0,h ) / (2*h)
}
silverman <- function( x ){
  return( 1.06 * min( sd( x), IQR( x)/1.34 ) * length(x)**(-0.2) )
}

silverman(df$x)
bw.nrd( df$x ) #silverman es igual al de arriba
bw.ucv( df$x ) #cv

x_grid <- seq(min(df$x), max(df$x), length.out = 200)
h_nrd <- bw.nrd(df$x)
h_ucv <- bw.ucv(df$x)

dens_nrd       <- sapply(x_grid, function(x0) densidad_est_parzen(df$x, x0, h_nrd))
dens_ucv       <- sapply(x_grid, function(x0) densidad_est_parzen(df$x, x0, h_ucv))
dens_low       <- sapply(x_grid, function(x0) densidad_est_parzen(df$x, x0, 0.3))
dens_hig       <- sapply(x_grid, function(x0) densidad_est_parzen(df$x, x0, 1))


plot(x_grid, dens_nrd, type = "l", lwd = 2, col = "blue",
     ylab = "Density", xlab = "x", main = "Parzen Density Estimates with Different h")
lines(x_grid, dens_ucv, col = "red", lwd = 2)
lines(x_grid, dens_low, col = "green", lwd = 2)
lines(x_grid, dens_hig, col = "yellow", lwd = 2)
legend("topright", legend = c("bw.nrd", "bw.ucv","0.01","1"),
       col = c("blue", "red","green","yellow"), lwd = 0.5)