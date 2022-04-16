#library(buildmodels)
library(e1071)
library(xgboost)
library(randomForest)
source("R/xgbStandardize.r")
source("R/buildmodels.r")
source("R/appends.r")
source("R/gridCV.r")

attach(iris)
sample.iris <- sample(1:nrow(iris), nrow(iris)*0.7)
iris.train <- iris[sample.iris,]
iris.test <- iris[-sample.iris,]

# Extending the buildmodels functions
models <- list(
    list(name = "SVM Linear", method = svm, 
        method.parameters = list(kernel="linear", probability = TRUE), 
        tune.parameters = list(cost = 10^(-1:3))),
    list(name = "Random Forest", method = randomForest, 
        method.parameters = list(ntree = 1000), 
        tune.parameters = list(mtry = 1:4)),
    list(name = "Gradient Boosted", method = xgbStandardize, 
        method.parameters = list(objective="multi:softmax", verbose=0, 
            num_class=3, eval_metric='mlogloss', nrounds=100), 
        tune.parameters = list(max.depth = 1:4, eta = 10^(-4:-1)))
)
models <- buildmodels("Species", iris.train, iris.test, models)

table(iris.test$Species, models[[3]]$predict)