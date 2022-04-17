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
        if (!is.numeric(unlist(test.data[,predictor]))) { # classification
            if (length(bin) > 0) { # binned
                pred <- do.call(bin[[1]], appends(pred, bin[-1]))
                actual <- do.call(bin[[1]], appends(test.data[,predictor], bin[-1])) 
            } else { #unbinned
                predicted <- model$predict
                actual <- test.data[,predictor]
            }
            model$results <- list(
            accuracy = mean(test.data[,predictor] == model$predict),
            confusion = table(test.data[,predictor], model$predict)
        )} else { # regression
            if (class(model$predict) == "factor") { # handle models that require factors
                pred <- as.numeric(levels(model$predict))[as.numeric(model$predict)]
            } else {
                pred <- model$predict
            }
            model$results <- list(MSE = mean((test.data[,predictor] - pred)^2))
            if (length(bin) > 0) { # binned
                pred <- do.call(bin[[1]], appends(pred, bin[-1]))
                actual <- do.call(bin[[1]], appends(test.data[,predictor], bin[-1])) 
                model$results$accuracy <- mean(actual == pred)
                model$results$confusion <- table(actual, pred)
            }
        }

        models[[i]] <- model
        i <- i + 1
    }
    return(models)
}