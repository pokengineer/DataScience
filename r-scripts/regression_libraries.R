library(ggplot2)
library(splines)        # Splines
library(class)          # KNN
library(FNN)            # KNN para Reg
library(leaps)          # regsubsets
library(rpart)          # CART
library(randomForest)   # RandomForest
library(glmnet)         # LASSO & Ridge
library(mgcv)           # GAM

############ Generate some sample data
set.seed(123)
x <- seq(0, 10, length.out = 100)
y <- sin(x) + rnorm(100, sd = 0.2)
df <- data.frame(x, y)
ggplot(df, aes(x, y)) + geom_point() + theme_minimal()

set.seed(123)
train_idx <- sample(1:nrow(df), 80)
train <- df[train_idx, ]
test  <- df[-train_idx, ]


############ Regresion lineal
lm_fit <- lm(y ~ x, data=df)
summary(lm_fit)


############ GLM
glm_fit <- glm(y ~ x, family = gaussian(), data=df)
summary(glm_fit)

############ KNN para Regresion
library('FNN')
m_knn <- knn.reg(train =train$x , y = train$y, test = as.matrix(test$x) , k = 5)
pred_knn <- m_knn$pred

############ rpart
m_rpart <- rpart(y ~ x, data=train)
pred_rpart <- predict(m_rpart, newdata = test)

############ RANDOM FOREST 
m_rf <- randomForest(y ~ x, data=train, ntree=300)
pred_rf <- predict(m_rf, newdata = test)


############ Subset
df <- data.frame( x1 = x, 
                  x2 = x**2,
                  x3 = x**3, 
                  x4 = x**4,
                  x5 = x**5, 
                  x6 = x**6,
                  x7 = x**7, 
                  x8 = x**8,
                  x9 = x**9,
                  x10 = x**10,
                  y = y)
m_rsubset <- regsubsets( y ~ . , data=df )


############ Ridge
cv_ridge <- cv.glmnet(
  as.matrix(df[1:10]), y,
  alpha = 0,        # RIDGE
  nfolds = 10
)
plot(cv_ridge)
cv_ridge$lambda.1se 
round(coef(cv_ridge, s = "lambda.min"), 5) #s = "lambda.1se"


df_test <- data.frame( x1 = test$x, 
                  x2 = test$x**2,
                  x3 = test$x**3, 
                  x4 = test$x**4,
                  x5 = test$x**5, 
                  x6 = test$x**6,
                  x7 = test$x**7, 
                  x8 = test$x**8,
                  x9 = test$x**9,
                  x10 = test$x**10)
pred_ridge <- predict( cv_ridge, as.matrix(df_test))

############ Lasso
cv_lasso <- cv.glmnet(
  as.matrix(df[1:10]), y,
  alpha = 0,        # lasso
  nfolds = 10
)
plot(cv_lasso)
cv_lasso$lambda.1se 
round(coef(cv_lasso, s = "lambda.min"), 5) #s = "lambda.1se"

pred_lasso <- predict( cv_lasso, as.matrix(df_test))

############ Fit Spline
m_spline <- lm( y~ bs(x, knots = c( 4,8 )), data=df)
summary(m_spline)
pred_spline <- predict(m_spline, newdata = test)



############ GAM
m_gam <- gam(y ~ s(x), data=train)
pred_gam <- predict(m_gam, test)


############ PLOT
ggplot()+
  geom_line(aes(x=test$x, test$y, color='real'))+
  geom_line(aes(x=test$x, pred_knn, color='knn'))+
  geom_line(aes(x=test$x, pred_rpart, color='rpart'))+
  geom_line(aes(x=test$x, pred_rf, color='rf'))+
  geom_line(aes(x=test$x, pred_gam, color='gam'))+ 
  geom_line(aes(x=test$x, pred_lasso, color='lasso'))+ 
  geom_line(aes(x=test$x, pred_ridge, color='ridge'))+ 
  geom_line(aes(x=test$x, pred_spline, color='spline'))+ 
  scale_color_manual(values = c(
    "real"  = "red",
    "knn"   = "blue",
    "rpart" = "darkgreen",
    "rf"    = "purple",
    "gam"   = "darkred",
    "lasso" = "yellow2",
    "ridge" = "orange3",
    "spline"= "darkcyan"
  )) +
  theme_minimal()