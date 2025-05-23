#install.packages('ISLR2')
#install.packages('tree')

library('ISLR2')
library('tree')

set.seed(1)
plot(Boston$age, Boston$medv)
boxplot(Boston$medv ~ Boston$chas)


sample <- sample.int(n = nrow(Boston), size = floor(.5*nrow(Boston)), replace = F)
train <- Boston[sample, ]
test  <- Boston[-sample, ]

tree_boston <- tree(medv~. , train)

summary(tree_boston)

y_pred <- predict(tree_boston, train)
sum( (train$medv - y_pred)**2 )
summary(tree_boston)$dev

plot(tree_boston)
text(tree_boston, cex=0.5)

dato_clase <- data.frame(
  crim = 1.19294,
  zn = 0,
  indus = 21.89,
  chas = 0,
  nox = 0.624,
  rm = 6.326,
  age = 97.7,
  dis = 2.271,
  rad = 4,
  tax = 437,
  ptratio = 21.2,
  lstat = 12.26
)
y_dato <- predict(tree_boston, dato_clase)
y_dato

y_pred_test <- predict(tree_boston, test)
mean( (test$medv - y_pred_test)**2 )

set.seed(3)
cv_boston <- cv.tree(tree_boston, K = 10)
cv_boston
plot(cv_boston$size, cv_boston$dev, type = 'b')

pruned_tree <- prune.tree( tree_boston, best= 5 )
plot(pruned_tree)
text(pruned_tree)

set.seed(3)
max_tree <- tree(medv~. , Boston, mincut=1, minsize=2, mindev=0.0005)
cv_boston <- cv.tree(max_tree, K = 10)
cv_boston
plot(cv_boston$size, cv_boston$dev, type = 'b')

pruned_tree <- prune.tree( max_tree, best= 9 )
plot(pruned_tree)
text(pruned_tree, cex=0.5)


plot(max_tree)
text(max_tree, cex=0.5)

#############################################

library('tree')
set.seed(2)
airquality
sample <- sample.int(n = nrow(airquality), size = floor(.5*nrow(airquality)), replace = F)
train <- airquality[sample, ]
test  <- airquality[-sample, ]
tree_aq <- tree(Ozone ~. , train)
summary(tree_aq)
y_pred_test <- predict(tree_aq, test)
mean( (test$Ozone - y_pred_test)**2 )
#hay que remover los nulos
mean((test$Ozone - y_pred_test)^2, na.rm=TRUE)

cv_aq <- cv.tree(tree_aq, FUN = prune.tree, K = 10)
plot(cv_aq$size, cv_aq$dev, type = "b", pch = 19,
     xlab = "Número de nodos terminales",
     ylab = "Deviance (CV 10-fold)",
     main = "Error de CV vs. tamaño del árbol")


optimal_size <- cv_aq$size[which.min(cv_aq$dev)]
cat("Tamaño óptimo:", optimal_size, "\n")
tree_aq2 <- prune.tree(tree_aq, best = optimal_size)
y_pred_test2 <- predict(tree_aq2, test)
mean((test$Ozone - y_pred_test2)^2, na.rm = TRUE)

par(mfrow = c(1, 2))
plot(tree_aq)
text(tree_aq, cex=0.5)
plot(tree_aq2)
text(tree_aq2, cex=0.5)

# Métricas comparativas
cat("== Comparación de Árboles ==\n")
cat("Árbol original:\n")
cat("  Nodos terminales:", length(unique(tree_aq$where)), "\n")
cat("  Deviance (entrenamiento):", summary(tree_aq)$dev, "\n")
cat("  MSE test:", mean((test$Ozone - y_pred_test)^2, na.rm = TRUE), "\n\n")

cat("Árbol podado:\n")
cat("  Nodos terminales:", length(unique(tree_aq2$where)), "\n")
cat("  Deviance (entrenamiento):", summary(tree_aq2)$dev, "\n")
cat("  MSE test:", mean((test$Ozone - y_pred_test2)^2, na.rm = TRUE), "\n")