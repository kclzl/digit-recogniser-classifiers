# read data

data = read.csv("./train.csv")
xtrain = read.csv("./xtrain.csv")
ytrain = read.csv("./ytrain.csv")
xtest = read.csv("./xtest.csv")
ytest = read.csv("./ytest.csv")
df_encoded_train = read.csv("./df_encoded_train.csv")
df_encoded_test = read.csv("./df_encoded_test.csv")

# mvn test
library(MVN)
# mvn_raw = mvn(xtrain)$multivariateNormality
# mvn test on encoded data
mvn(df_encoded_train[1:3360, ])$multivariateNormality

# BoxM test
library(biotools)
train = cbind(ytrain, df_encoded_train)
boxM(data = train, grouping = train$label)

# LDA
library(MASS)
lda.model = lda(label~., data = encoded_train)
# predict using lda
train_pred = predict(lda.model, newdata = df_encoded_train)
test_pred = predict(lda.model, newdata = df_encoded_test)
# evaluate using confusion matrix
train_conf_mat = table(train_pred$class, t(ytrain))
test_conf_mat = table(test_pred$class, t(ytest))
 # yaxis of mat is predicted, xaxis is actual
train_conf_mat
test_conf_mat

# define a scoring function
score  = function(conf_mat){
  hitRate <- sum(diag(conf_mat)) / sum(conf_mat)
  maxChance <- 0.1
  propChance <- 10*(0.1^2)
  cat(sprintf("Hit Rate : %3f \n", hitRate))
  cat(sprintf("Maximum Chance Criterion : %3f \n", maxChance))
  cat(sprintf("Proportional Chance Criterion : %3f \n", propChance))
}

score(train_conf_mat)
score(test_conf_mat)
