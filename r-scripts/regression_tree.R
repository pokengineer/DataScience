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


max_tree <- tree(medv~. , Boston, mincut=1, minsize=2, mindev=0.005)
fgl <- cv.tree(max_tree,, prune.tree)
plot(fgl)

