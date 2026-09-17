rm(list = ls())

library(CCA)
library(GGally)
library(ggplot2)
library(aplpack)
library(corrplot)
library(MASS)
##############  Visualizaciones ############## 

pairs( USArrests )
corrplot( cor(USArrests))
faces( USArrests )
stars( USArrests )
ggpairs( USArrests )

############## Proyecciones #####################

data <- read.table('F:/Materias/00-ingreso/trvd/t8-5.dat')
data_s <- scale( cbind( data[,4], data[,2]) )
plot(data_s, asp=1, pch=20)
abline( h=0, v=0, col='gray')

# defino el vector sobre el cual voy a proyectar
a <- c(1,0.2)
a_s <- a / sqrt(sum(a^2)) # lo normalizo, que es hacer que sqrt(sum(a_s^2)) = 1

proy <- (data_s)
proy <- data_s %*% a_s %*% t(a_s)

dist <- data_s %*% a_s

proy <- dist %*% t(a_s)
# x <- dist*a_s[1]
# y <- dist*a_s[2]
points( proy , col='red')
#points( x,y , col='red' )
abline(a=0,b=0.2, col='red')

############## PCA ###########################

df <- scale(USArrests)
pca <- prcomp( df )

pca$rotation

# scree plot
variance = pca$sdev^2 / sum(pca$sdev^2)
plot(variance, type = 'l')

# ploteo los datos en PC1 y PC2
pca$rotation
k <- 2
matriz_eigenvectores <- pca$rotation[, 1:k] 
espacio_pca <- df  %*%  matriz_eigenvectores
plot(espacio_pca)
# que es lo mismo que:
plot(pca$x[,1:2])
abline(h = 0, v = 0, col = "grey60", lty = 2, lwd = 1)

# puedo reconstruir la matriz original 
datos_reconstruidos <- pca$x %*% t(pca$rotation)
# puedo reconstruir la matriz original con menos componentes
datos_reconstruidos <- pca$x[,1:3] %*% t(pca$rotation[,1:3])

############## CCA ###########################

data <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")

X <- scale(data[1:3],scale=TRUE)
Y <- scale(data[4:8],scale=TRUE)

cca <- cc(X,Y)
# visualizaciones que pueden ser utiles:
plot(cca$scores$xscores[,1],cca$scores$xscores[,2])
plot(cca$scores$yscores[,1],cca$scores$yscores[,2])
plot(cca$scores$xscores[,1],cca$scores$yscores[,1])

# lo que esta en cca:
cca$cor  # correlaciones canonicas
c(cca$names$Xnames,cca$names$Ynames) # los nombres de las variables
cca$xcoef  # los "pesos" de cada variable original en cada componente de X
cca$ycoef

# los scores, osea el valor de cada fila de x/y proyectado con la componente
cca$scores$xscores
cca$scores$yscores
# los loadings, medio parecido a los coef 
cca$scores$corr.X.xscores
cca$scores$corr.Y.yscores
# cross-loadings, puede ser util chequear que no haya nada muy alto aca
cca$scores$corr.Y.xscores
cca$scores$corr.X.yscores

# laburito a mano
S11 <- cov(X)
S22 <- cov(Y)
S12 <- cov(X, Y)
S21 <- t(S12)
choclazo <- solve(S11) %*% S12 %*% solve(S22) %*% S21
alfa <- eigen( choclazo )$vectors
rho <- sqrt(eigen( choclazo )$values)

alfa1 <- alfa[,1] / sqrt( as.numeric( t(alfa[,1]) %*% S11 %*% alfa[,1] ))
alfa2 <- alfa[,2] / sqrt( as.numeric( t(alfa[,2]) %*% S11 %*% alfa[,2] ))
alfa3 <- alfa[,3] / sqrt( as.numeric( t(alfa[,3]) %*% S11 %*% alfa[,3] ))
xcoef <- cbind( alfa1, alfa2, alfa3)
plot( X%*%alfa1,(-1)*X%*%alfa2)

############## Clasificacion #########################

data <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
data$Y <- (data$science > 50 ) * 1  # me invente un target

# LogReg 
a.glm <- glm( Y  ~ read + math, data=data, family=binomial)

read_seq  <- seq(min(data$read)  - 1, max(data$read)  + 1, length.out = 200)
math_seq <- seq(min(data$math) - 1, max(data$math) + 1, length.out = 200)
grid <- expand.grid(read = read_seq, math = math_seq)

y_pred <- predict(a.glm, newdata = grid, type="response")
grid$Y <- y_pred 
grid$Y_bin <- y_pred > 0.5

ggplot()+
  geom_raster( data = grid, aes(read, math, fill=Y)) +
  #scale_fill_continuous(palette = "viridis") +
  geom_point(data = data, aes(x = read, y = math, color = factor(Y)), size = 2) +
  geom_contour(data = grid, aes(x = read, y = math, z = Y),breaks = 0.5, color = "white", size = 0.6) + 
  scale_color_manual(name = "Y (observado)", values = c("0" = "blue", "1" = "red")) +
  labs(x = "read", y = "math", title = "Mapa de probabilidades (logit) y puntos") +
  theme_minimal()

# LDA 
a.lda <- lda( Y  ~ read + math, data=data, family=binomial)

y_pred <- predict(a.lda, newdata = grid, type="response")
grid$Y <- y_pred$posterior[,1]
grid$Y_bin <- y_pred$class

ggplot()+
  geom_raster( data = grid, aes(read, math, fill=Y)) +
  #scale_fill_continuous(palette = "viridis") +
  geom_point(data = data, aes(x = read, y = math, color = factor(Y)), size = 2) +
  geom_contour(data = grid, aes(x = read, y = math, z = Y),breaks = 0.5, color = "white", size = 0.6) + 
  scale_color_manual(name = "Y (observado)", values = c("0" = "blue", "1" = "red")) +
  labs(x = "read", y = "math", title = "Mapa de probabilidades (logit) y puntos") +
  theme_minimal()

### QDA
a.qda <- qda(Y  ~ read + math, data=data)
y_pred <- predict(a.qda, newdata = grid)
grid$Y_bin <- y_pred$class
grid$Y <- y_pred$posterior[,1]

ggplot()+
  geom_raster( data = grid, aes(read, math, fill=Y)) +
  #scale_fill_continuous(palette = "viridis") +
  geom_point(data = data, aes(x = read, y = math, color = factor(Y)), size = 2) +
  geom_contour(data = grid, aes(x = read, y = math, z = Y),breaks = 0.5, color = "white", size = 0.6) + 
  scale_color_manual(name = "Y (observado)", values = c("0" = "blue", "1" = "red")) +
  labs(x = "read", y = "math", title = "Mapa de probabilidades (logit) y puntos") +
  theme_minimal()

### SVM
library(e1071)
a.svm <- svm(Y  ~ read + math, data = data, kernel = "linear")
y_pred <- predict(a.svm, newdata = grid)
grid$Y <- y_pred 
grid$Y_bin <- y_pred > 0.5

ggplot()+
  geom_raster( data = grid, aes(read, math, fill=Y)) +
  #scale_fill_continuous(palette = "viridis") +
  geom_point(data = data, aes(x = read, y = math, color = factor(Y)), size = 2) +
  geom_contour(data = grid, aes(x = read, y = math, z = Y),breaks = 0.5, color = "white", size = 0.6) + 
  scale_color_manual(name = "Y (observado)", values = c("0" = "blue", "1" = "red")) +
  labs(x = "read", y = "math", title = "Mapa de probabilidades (logit) y puntos") +
  theme_minimal()


### bayes
a.nb <- naiveBayes(Y  ~ read + math, data = data)
y_pred <- predict(a.nb, newdata=grid[1:2],type = 'raw')
grid$Y <- y_pred[,1]
grid$Y_bin <- y_pred[,1] > 0.5

ggplot()+
  geom_raster( data = grid, aes(read, math, fill=Y)) +
  #scale_fill_continuous(palette = "viridis") +
  geom_point(data = data, aes(x = read, y = math, color = factor(Y)), size = 2) +
  geom_contour(data = grid, aes(x = read, y = math, z = Y),breaks = 0.5, color = "white", size = 0.6) + 
  scale_color_manual(name = "Y (observado)", values = c("0" = "blue", "1" = "red")) +
  labs(x = "read", y = "math", title = "Mapa de probabilidades (logit) y puntos") +
  theme_minimal()

############## Clustering #########################

df <- data.frame(scale(USArrests))

pca <- prcomp( df )
plot(pca$x[,1:2])

df$pc1 <- pca$x[,1]
df$pc2 <- pca$x[,2]

# kmeans
clase <- kmeans( pca$x[,1:2], centers = 3)
df$clase <- as.factor(clase$cluster)

ggplot(data = df, aes(pc1, pc2, color=clase,size =UrbanPop)) +
  geom_point() +
  scale_color_manual(values = c('darkgreen','red','orange')) +
  theme_minimal()

# jerarquico
dist_mat <- dist(df[, c("pc1", "pc2")], method = "euclidean")
hc <- hclust(dist_mat)

plot(hc, labels = rownames(df), main = "Hierarchical clustering (ward.D2)",
     xlab = "", sub = "", cex = 0.7)
