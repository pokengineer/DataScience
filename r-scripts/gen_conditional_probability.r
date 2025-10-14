# X ~ sqrt( Unif(0,1) )
# Y ~ Exp( 1/ X^2 )
gen_dat <- function( n ){
  salida <- matrix( NA , n, 2)
  for( i in 0:n){
    x <- sqrt( runif(1,0,1))
    y <- rexp( 1, 1/(x**2))
    salida[i,] <- c(x,y) 
  }
  return(salida)
}
datos <- gen_dat(1000)
plot(datos, col="grey")
curve(x**2, from=0, to=1, lwd=2, col="red", add=TRUE)
##################################################################
# X ~ Unif(-1,1)
# X ~ Unif(-1,1)
# Y ~ Z * X
#
# En este caso Cov(X,Y) = 0 (constante)
# E( Y|X ) = 0
# pero no son independientes 
gen_dat2 <- function( n ){
  salida <- matrix( NA , n, 2)
  for( i in 0:n){
    x <- runif(1,-1,1)
    y <- runif(1,-1,1) * x
    salida[i,] <- c(x,y) 
  }
  return(salida)
}
datos <- gen_dat2(1000)

plot(datos, col="grey")
curve(0*x, from=-1, to=1, lwd=2, col="red", add=TRUE)
##################################################################
## Las aves parásitas de cría ponen huevos en nidos de otras especies (hospedador), 
# las cuales incuban los huevos y crían al pichón parásito. 
# En un bosque de talas de la provincia de Buenos Aires hay dos especies hospederas que son indistinguibles a simple vista. 
# Una de las principales diferencias entre estas especies radica en el grado de discriminación y
# remoción de huevos parásitos de sus nidos. Una de las especies es “aceptadora“ de huevos parásitos (Y = 0),
# ya que remueve del nido sólo el 30% de los huevos parásitos, mientras que la otra especie es “rechazadora“ (Y = 1)
# ya que remueve el 80% de los huevos parásitos presentes en su nido. 
# Además, se sabe que el 90% de los nidos del bosque corresponden a la especie “aceptadora“ , 
# mientras que apenas el 10% restante son nidos de la especie “rechazadora“. 
# Se elige al azar un nido del bosque y se colocan k = 8 huevos parásitos. 
# Denotemos con X a la variable aleatoria que indica el número de huevos removidos del nido. 
# Asuma que, en cada nido, la remoción (o no) de los diferentes huevos se realiza de manera independiente.
gen_dat <- function( n ){
  salida <- matrix( NA , n, 2)
  for( i in 0:n){
    y <- rbinom(1, 1, 0.1)
    x <- rbinom(1, 8, 0.3 + 0.5*y)
    
    salida[i,] <- c(x,y) 
  }
  return(salida)
}
datos <- gen_dat(1000)
hist(datos,breaks = 9)

