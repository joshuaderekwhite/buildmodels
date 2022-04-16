library(buildmodels)
library(e1071)
library(xgboost)
library(randomForest)
source("R/xgbStandardize.r")

attach(iris)
sample.iris <- sample(1:nrow(iris), nrow(iris)*0.7)
iris.train <- iris[sample.iris,]
iris.test <- iris[-sample.iris,]

svm.linear.hyper <- gridCV(
    data = iris.train, predictor = "Species", 
    method = svm, method.parameters = list(kernel="linear"), folds = 4,
    tune.parameters = list("cost" = 10^(-1:3))
    )

rf.hyper <- gridCV(
    data = iris.train, predictor = "Species",
    method = randomForest, method.parameters = list(ntree = 1000), folds = 4,
    tune.parameters = list(mtry = 1:4)
)

xgb.hyper <- gridCV(
    data = iris.train, predictor = "Species",
    method = xgbStandardize, folds = 4,
    method.parameters = list(objective="multi:softmax", verbose=0, 
        num_class=3, eval_metric='mlogloss', nrounds=100),
    tune.parameters = list(max.depth = 1:4, eta = 10^(-4:-1))
)
xgbStandardize(Species~., iris.train, objective="multi:softmax", verbose=0, 
        num_class=3, eval_metric='mlogloss', nrounds=100, max.depth = 4, eta = 0.01)

# Find the parameters
tune.grid <- xgb.hyper$tune.grid
if (ncol(tune.grid) <= 2) {
    param <- colnames(tune.grid)[1]
    output <- list(
        parameters = as.list(setNames(tune.grid[1,1:(ncol(tune.grid)-1)], colnames(tune.grid)[1])), 
        accuracy = tune.grid[1,ncol(tune.grid)], 
        tune.grid = tune.grid)
} else {
    output <- list(
        parameters = as.list(tune.grid[1,1:(ncol(tune.grid)-1)]), 
        accuracy = tune.grid[1,ncol(tune.grid)], 
        tune.grid = tune.grid)
}


# New function for learning the models
models <- list(
    list(name = "SVM Linear", method = svm, method.parameters = append(list(kernel="linear", probability = TRUE), svm.linear.hyper$parameters)),
    list(name = "Random Forest", method = randomForest, method.parameters = append(list(ntree = 1000), rf.hyper$parameters)),
    list(name = "Gradient Boosted", method = xgbStandardize, method.parameters = append(list(objective="multi:softmax", verbose=0, 
        num_class=3, eval_metric='mlogloss', nrounds=100), xgb.hyper$parameters))
)

i <- 1
for (model in models){
    models[[i]]$model <- do.call(model$method, append(list(formula = Species~., data = iris.train), model$method.parameters))
    i <- i + 1
}

# Predictions with Decision Values - Trees
pred.linear <- predict(models[[1]]$model, newdata=iris.test)
pred.rf <- predict(models[[2]]$model, newdata=iris.test)
pred.xgb <- predict(models[[3]]$model, newdata=iris.test)


svm.linear.hyper <- gridCV(
    data = iris.train, predictor = "Species", 
    method = svm, method.parameters = list(kernel="linear"), folds = 4,
    tune.parameters = list("cost" = 10^(-1:3))
    )

rf.hyper <- gridCV(
    data = iris.train, predictor = "Species",
    method = randomForest, method.parameters = list(ntree = 1000), folds = 4,
    tune.parameters = list(mtry = 1:4)
)

xgb.hyper <- gridCV(
    data = iris.train, predictor = "Species",
    method = xgbStandardize, folds = 4,
    method.parameters = list(objective="multi:softmax", verbose=0, 
        num_class=3, eval_metric='mlogloss', nrounds=100),
    tune.parameters = list(max.depth = 1:4, eta = 10^(-4:-1))
)
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

testit <- function(x = sort(runif(20)), ...)
{
    cat("Testing print out...\n")
    Sys.sleep(1)
    pb <- txtProgressBar(...)
    for(i in c(0, x, 1)) {Sys.sleep(0.1); setTxtProgressBar(pb, i)}
    Sys.sleep(1)
    close(pb)
}
testit(style = 3)

table(iris.test$Species, models[[1]]$predict)
table(iris.test$Species, pred.rf)
table(iris.test$Species, pred.xgb)