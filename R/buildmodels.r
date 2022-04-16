#' Predict the optimize model of each of the listed models
#'
#' Iterate through each of the models to optimize the hyperparameters
#' learn the models with the hyperparameters, and predict the 
#' corresponding model.
#' 
#' @param predictor Prediction variable used to learn or predict the model
#' @param train.data The training data set for the model
#' @param test.data The testing data set for the model
#' @param models A list of all of the models with the name, method, method.parameters,
#'  tuning.parameters, and pred.parameters
#' @return A list of models with the optimized parameters, the learned model, and the
#'  prediction of the predictor
#' @export
#' @examples
#' models <- list(
#'     list(name = "SVM Linear", method = svm, 
#'         method.parameters = list(kernel="linear", probability = TRUE), 
#'         tune.parameters = list(cost = 10^(-1:3))),
#'     list(name = "Random Forest", method = randomForest, 
#'         method.parameters = list(ntree = 1000), 
#'         tune.parameters = list(mtry = 1:4)),
#'     list(name = "Gradient Boosted", method = xgbStandardize, 
#'         method.parameters = list(objective="multi:softmax", verbose=0, 
#'             num_class=3, eval_metric='mlogloss', nrounds=100), 
#'         tune.parameters = list(max.depth = 1:4, eta = 10^(-4:-1)))
#' )
#' models <- buildmodels("Species", iris.train, iris.test, models)

buildmodels <- function(predictor, train.data, test.data, models, folds=4){
    i <- 1
    for (model in models){
        model$tuned <- gridCV(train.data, predictor, model, folds)
        
        model$model <- do.call(model$method, append(list(formula = Species~., data = iris.train), 
            append(model$method.parameters, model$tuned$parameters)))

        model$predict <- predict(model$model, newdata = test.data)

        models[[i]] <- model
        i <- i + 1
    }
    return(models)
}