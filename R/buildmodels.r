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

buildmodels <- function(predictor, train.data, test.data, models, bin=list(), folds=4){
    i <- 1
    for (model in models){
        # Optimize Hyper-Parameters for each model from training data
        model$tuned <- gridCV(train.data, predictor, model, folds)
        
        # Learn the provided models
        model$model <- do.call(model$method, appends(
            formula = formula(paste(predictor,"~.")), 
            data = train.data, 
            model$method.parameters, 
            model$tuned$parameters,
            models$pred.parameters))

        # Predict the test data from each learned model
        model$predict <- predict(model$model, newdata = test.data)
        if (!is.null(model$pred.parameters$round)) {
            model$predict
        }

        # Capture the results of the prediction
        # classification (accuracy, confusion)
        # binned classification (accuracy (binned), confussion (binned))
        # regression (MSE)
        # binned regression (accuracy (binned), MSE (unbinned), confusion (binned))
        actual <- unlist(test.data[,predictor])
        predicted <- model$predict
        if (!is.numeric(actual)) { # classification
            if (length(bin) > 0) { # binned
                predicted <- do.call(bin[[1]], appends(predicted, bin[-1]))
                actual <- do.call(bin[[1]], appends(actual, bin[-1])) 
            }
            model$results <- list(
            accuracy = mean(actual == predicted),
            confusion = table(actual, predicted)
        )} else { # regression
            if (class(predicted) == "factor") { # handle models that require factors
                predicted <- as.numeric(levels(predicted))[as.numeric(predicted)]
            }
            model$results <- list(MSE = mean((actual - predicted)^2))
            if (length(bin) > 0) { # binned
                predicted <- do.call(bin[[1]], appends(predicted, bin[-1]))
                actual <- do.call(bin[[1]], appends(actual, bin[-1])) 
                model$results$accuracy <- mean(actual == predicted)
                model$results$confusion <- table(actual, predicted)
            }
        }

        models[[i]] <- model
        i <- i + 1
    }
    return(models)
}