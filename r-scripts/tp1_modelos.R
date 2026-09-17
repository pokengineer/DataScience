install.packages("MLmetrics")
install.packages("fastDummies")
install.packages("rpart")
install.packages("rpart.plot")
library(caret)   # for data splitting and evaluation
library(MLmetrics)  # for F1_Scoreinstall.packages("class")
library(class)    
library(fastDummies)
library(rpart)
library(rpart.plot)
library(caret)

df <- read.csv('F:/Materias/00-ingreso/taller/clasificacion/bank_marketing.csv')
colnames(df)

for( c in colnames(df)){
  print( paste(c,'---', typeof(df[,c])))
}

#df <- read.csv("~/Escritorio/facucapo/bank_marketing.csv")
df$y_bool <- df$y == 'yes'

num <- c( 'age', 'campaign','pdays','previous',
          'emp.var.rate','cons.price.idx','cons.conf.idx',
          'euribor3m','nr.employed')

cat <- c('job', 'marital', 'education', 'default', 'housing', 
         'loan', 'contact', 'month', 'day_of_week', 'poutcome')
df[cat] <- lapply(df[cat], factor)



for( c in cat){
  print( paste(c,'-----', length(unique(df[,c]))))
}

nrow(df[!df$y_bool,])
######### FEATURE ENGINEERING ############
df$day_mf <- (df$day_of_week == 'mon' | df$day_of_week == 'fri')*1
df$job_st <- (df$job == 'student')*1
df$job_re <- (df$job == 'retired')*1
df$job_bc <- (df$job == 'blue-collar')*1

df$pdays_n <- (df$pdays == 999)*1
df$campa_d <- (df$campaign < 10)*1
df[,num] <- scale(df[,num])

library(fastDummies)
df_d <- dummy_cols(df, select_columns = c('marital','default','housing','loan','contact','poutcome'))
colnames(df_d)[ (ncol(df)-5) : ncol(df_d)]

low_dim <- c('marital','default','housing','loan','contact','poutcome',
             'day_mf','job_st','job_re','job_bc','pdays_n')
features <- c(num, low_dim)

num_features <- c(num, colnames(df_d)[ (ncol(df)-4) : ncol(df_d)] )
################ MODELS ################
library(caret) 
set.seed(123)  
trainIndex <- createDataPartition(df_d$y_bool, p = 0.8, list = FALSE)
train <- df_d[trainIndex, ]
test <- df_d[-trainIndex, ]

weights_vector <- ifelse(train$y_bool, 10, 1)

######## GLM ########
formula <- paste("y_bool ~", paste(num, collapse = " + "))
model <- glm(formula, data = train, family = binomial)
probs <- predict(model, newdata = test, type = "response")
preds1_1 <- ifelse(probs > 0.5, TRUE, FALSE)

formula2 <- paste("y_bool ~", paste(num_features, collapse = " + "))
model2 <- glm(formula2, data = train, family = binomial,control = glm.control(maxit = 1000))
probs <- predict(model2, newdata = test, type = "response")
preds1_2 <- ifelse(probs > 0.5, TRUE, FALSE)

model <- glm(formula, data = train, family = binomial, weights=weights_vector)
probs <- predict(model, newdata = test, type = "response")
preds1_3 <- ifelse(probs > 0.5, TRUE, FALSE)

model2 <- glm(formula2, data = train, family = binomial, weights=weights_vector,control = glm.control(maxit = 1000))
probs <- predict(model2, newdata = test, type = "response")
preds1_4 <- ifelse(probs > 0.5, TRUE, FALSE)

formula2
######## KNN ########
library(class)  
library(kknn)
for( k in c(11,13,15)){
  preds <- knn(train = train[,num], 
      test = test[,num], 
      cl = train$y_bool, 
      k = k)
  f1 <- F1_Score(y_pred = preds, y_true = test$y_bool,positive='TRUE')
  print( paste(k,'---',f1))
}

preds3 <- knn(train = train[,num], 
              test = test[,num], 
              cl = train$y_bool, 
              k = 3)

preds4 <- knn(train = train[,num_features], 
              test = test[,num_features], 
              cl = train$y_bool, 
              k = 9)
f1 <- F1_Score(y_pred = preds4, y_true = test$y_bool,positive='TRUE')
print( paste(k,'---',f1))

######## tree ########
tree1 <- rpart(formula2, data = train, method = "class", maxdepth = 5)
preds7 <- predict(tree1, newdata = test, type = "class")

tree2 <- rpart( formula2 , data = train, method = "class", weights=weights_vector, maxdepth = 5)
preds8 <- predict(tree2, newdata = test, type = "class")
library(rpart.plot)
rpart.plot(tree2)

################ F1 SCORES ################
library(MLmetrics)
f1_11 <- F1_Score(y_pred = preds1_1 , y_true = test$y_bool,positive='TRUE')
f1_12 <- F1_Score(y_pred = preds1_2, y_true = test$y_bool,positive='TRUE')
f1_13 <- F1_Score(y_pred = preds1_3 , y_true = test$y_bool,positive='TRUE')
f1_14 <- F1_Score(y_pred = preds1_4, y_true = test$y_bool,positive='TRUE')
print(paste("RLog num:", round(f1_11, 5)))
print(paste("RLog all", round(f1_12, 5)))
print(paste("RLogw num:", round(f1_13, 5)))
print(paste("RLogw all", round(f1_14, 5)))


f1_3 <- F1_Score(y_pred = preds3, y_true = test$y_bool,positive='TRUE')
f1_4 <- F1_Score(y_pred = preds4, y_true = test$y_bool,positive='TRUE')
f1_5 <- F1_Score(y_pred = preds5, y_true = test$y_bool,positive='TRUE')
f1_6 <- F1_Score(y_pred = preds6, y_true = test$y_bool,positive='TRUE')
f1_7 <- F1_Score(y_pred = preds7, y_true = test$y_bool,positive='TRUE')
f1_8 <- F1_Score(y_pred = preds8, y_true = test$y_bool,positive='TRUE')
print(paste("knn num 3:", round(f1_3, 5)))
print(paste("knn all 3:", round(f1_4, 5)))
print(paste("knn num 3:", round(f1_5, 5)))
print(paste("knn all 3:", round(f1_6, 5)))
print(paste("tree1 all:", round(f1_7, 5)))
print(paste("tree2 all:", round(f1_8, 5)))


################ Cross-validation ################
library(caret)
library(MLmetrics)

f1_summary <- function(data, lev = NULL, model = NULL) {
  f1 <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = "yes")
  c(F1 = f1)
}
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                              summaryFunction = f1_summary)
train$y <- as.factor(train$y)
test$y  <- as.factor(test$y)
set.seed(123)
formula2 <- paste("y ~", paste(num_features, collapse = " + "))
tree_cv <- train(
  as.formula(formula2),
  data = train,
  method = "rpart",
  trControl = train_control,
  metric = "F1",
  weights = weights_vector
)

print(tree_cv)
plot(tree_cv)

preds <- predict(tree_cv, newdata = test)
f1_test <- MLmetrics::F1_Score(y_pred = preds, y_true = test$y_bool, positive = "yes")
f1_test