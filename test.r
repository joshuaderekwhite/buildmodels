library(e1071)
library(xgboost)
library(randomForest)

loadPackage <- function(base){
    switch(
        base,
        "library" = {library(buildmodels)},
        "local" = {
            source("R/xgbStandardize.r")
            source("R/buildmodels.r")
            source("R/appends.r")
            source("R/gridCV.r")
        }
    )
}

binaryClassModel <- function(...){
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
return(models)
}

regressionModel <- function(...){
# Regression Models
models <- list(
    list(name = "SVM Linear", method = svm, 
        method.parameters = list(kernel="linear", probability = TRUE), 
        tune.parameters = list(cost = 10^(-1:3))),
    list(name = "Random Forest", method = randomForest, 
        method.parameters = list(ntree = 1000), 
        tune.parameters = list(mtry = 1:4)),
    list(name = "Gradient Boosted", method = xgbStandardize, 
        method.parameters = list(verbose=0, nrounds=100), 
        tune.parameters = list(max.depth = 1:4, eta = 10^(-4:-1)))
)
models <- buildmodels("Sepal.Length", iris.train, iris.test, models, bin = list(round, digits = 0))
return(models)
}

regressionModel2 <- function(...){
models <- list(list(name = "Gradient Boosted", method = xgbStandardize, 
        method.parameters = list(objective="reg:squarederror", verbose=0, 
            nrounds=50), 
        tune.parameters = list(max.depth = 1:4, eta = 10^(-4:-1))))
models <- buildmodels("numberOfPassRushers", plays.train, plays.test, models, bin = list(round, digits = 0))
return(models)
}

# loadPackage("local")
# attach(iris)
# sample.iris <- sample(1:nrow(iris), nrow(iris)*0.7)
# iris.train <- iris[sample.iris,]
# iris.test <- iris[-sample.iris,]
# models <- regressionModel(iris.train, iris.test)
# models[[3]]$results

# Partition the data for train and test
loadPackage("local")
library(dplyr)
load("c:/Users/joshu/OneDrive/Documents/Course Work/UCF/STA6704/Assignments/Project/allPlays.RData")
plays.regression <- plays.regression %>%
    mutate(
        numberOfPassRushers = as.factor(numberOfPassRushers),
        defendersInTheBox = as.factor(defendersInTheBox),
        quarter = as.factor(quarter),
        down = as.factor(down),
        possessionInd = as.factor(possessionInd)
    ) %>% mutate_if(is.numeric, scale) %>%
    mutate(numberOfPassRushers = as.numeric(levels(numberOfPassRushers)[as.numeric(numberOfPassRushers)]))
plays.regression <- plays.regression %>% sample_n(1000) # This is for testing purposes only. Need to revert in final run
plays.regression$Partition <- sample(
    x = c("Train", "Test"),
    size = nrow(plays.regression),
    replace = TRUE,
    prob = c(0.7, 0.3)
) 
plays.regression %>% group_by(Partition, numberOfPassRushers) %>% count()
plays.train <- plays.regression %>% ungroup() %>% 
    filter(Partition == 'Train') %>% select(-Partition)
plays.test <- plays.regression %>% ungroup() %>% 
    filter(Partition == 'Test') %>% select(-Partition)
test <- regressionModel2(plays.train, plays.test)

data.ready <- xgb.DMatrix(
        data = model.matrix(~., data=plays.train %>% select(-numberOfPassRushers))[,-1] %>%
            as.matrix(),
        label = plays.train$numberOfPassRushers
    )
test2 <- xgboost(data.ready,nrounds = 50)
data.ready <- xgb.DMatrix(
        data = model.matrix(~., data=plays.test %>% select(-numberOfPassRushers))[,-1] %>%
            as.matrix(),
        label = plays.test$numberOfPassRushers
    )
predict(test2, newdata=data.ready)