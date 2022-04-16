library(buildmodels)
library(e1071)
library(xgboost)
library(randomForest)

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


table(iris.test$Species, pred.linear)
table(iris.test$Species, pred.rf)
table(iris.test$Species, pred.xgb)