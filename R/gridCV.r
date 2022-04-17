#' Perform a grid search for the provided tuning parameters using k-fold
#' cross validation
#'
#' For a given method, the values will be tuned systematically by iterating
#' through each of the values provided in the model$tune.parameters and compared to
#' all of the other model$tune.parameters. The hyper parameters are the tuning
#' parameter values with the highest error value.
#' 
#' @param data Input to the model in a data frame format
#' @param predictor A string of the vector of the validation set that contains
#'  the true values of the dependent variable.
#' @param method The function call used to learn the model
#' @param model$method.parameters Additional parameters passed through to the method
#'  to learn the model. This should not include parameters that are going to be
#'  predicted.
#' @param folds The number of folds in the cross validation method.
#'  Default is 10.
#' @param model$tune.parameters Parameters that will be predicted based on the cross-
#'  validation error. This should be passed through as a list with each of
#'  the objects equal to an array of values to test.
#' @param model$pred.parameters Parameters required to use the predict function on
#'  the method type. There is no need to pass values for the model or the data,
#'  they are already provided.
#' @return A list with the values of the hyper parameters stored as the parameter
#'  object. Also the overall cross-validated error is reported as the error
#'  object. And all of the grid values and their errors are reported as
#'  the tune.grid object.
#' @keywords cross-validation
#' @seealso [caret::train()]
#' @export
#' @examples
#' svm.radial.hyper <- gridCV(
#'    method = svm, model$method.parameters = list(kernel="radial"), folds = 4,
#'    data = weather.train, predictor = "RainTomorrow", 
#'    tune = list("gamma" = 10^(1:3), "cost" = 10^(-1:0))
#'    )
#' 
#' ## [1] "Tuning..."
#' ##     gamma      cost  error 
#' ## 0.1000000 0.1000000 0.8309324 
#' ##    gamma     cost error 
#' ##     1.00     0.10     0.78 
#' ##    gamma     cost error 
#' ##    10.00     0.10     0.78 
#' ##    gamma     cost error 
#' ##   100.00     0.10     0.78 
#' ##    gamma     cost error 
#' ##  1000.00     0.10     0.78 
#' ##    gamma     cost error 
#' ## 0.100000 1.000000 0.852933 
#' ##    gamma     cost error 
#' ##     1.00     1.00     0.78 
#' ##    gamma     cost error 
#' ##    10.00     1.00     0.78 
#' ##    gamma     cost error 
#' ##   100.00     1.00     0.78 
#' ##    gamma     cost error 
#' ##  1000.00     1.00     0.78 

gridCV <- function(data, predictor, model, folds=10){
    require(caret)
    require(dplyr)

    # Create folds and tuning grid
    folds <- createFolds(data[[predictor]], k = folds)
    tune.grid <- expand.grid(model$tune.parameters) 
    if (nrow(tune.grid) > 0) tune.grid <- tune.grid %>% mutate(error = NA)
    cat(paste("Tuning", model$name, "for", 
        paste(names(model$tune.parameters), collapse = ", "), "\n"))
    progress.bar <- txtProgressBar(style = 3)

    # Average the value of each fold
    for (i in 1:nrow(tune.grid)) {
        err <- 0
        for (fold in folds){
            if (length(model$method.parameters) + length(model$tune.parameters) > 0){
                cv.model <- do.call(model$method, appends(
                formula(paste(predictor,"~.")),
                data = data[-fold,], 
                model$method.parameters,
                as.list(tune.grid[i,])[1:length(model$tune.parameters)]
                ))
            }
            else {
                cv.model <- do.call(model$method, appends(
                formula(paste(predictor,"~.")),
                data = data[-fold,]))
            }
            if (exists("pred.parameters", model)){
                pred <- do.call("predict", appends(model, newdata = data[fold,], 
                    eval(model$pred.parameters)))
                if (exists(model$pred.parameters$post)){
                    pred <- pred[quote(model$pred.parameters$post)]
                }
            }
            else{
                pred <- predict(cv.model, newdata = data[fold,])
            }
            # MSE for regression; (FPR+FNR) for classification
            if (is.numeric(data[,predictor])) {
                err <- err + mean((data[fold,predictor] - pred)^2)
            } else {
                err <- err + mean(data[fold,predictor] != pred)
            }
        }
        tune.grid[i,"error"] <- err / length(folds)
        # Report progress of tuning here: setTxtProgressBar
        setTxtProgressBar(progress.bar, i/nrow(tune.grid))
    }
    tune.grid <- tune.grid %>% arrange(error)

    if (ncol(tune.grid) <= 2){
        output <- list(
            parameters = as.list(setNames(tune.grid[1,1:(ncol(tune.grid)-1)], colnames(tune.grid)[1])), 
            error = tune.grid[1,ncol(tune.grid)], 
            tune.grid = tune.grid)
    }
    else {
        output <- list(
            parameters = as.list(tune.grid[1,1:(ncol(tune.grid)-1)]), 
            error = tune.grid[1,ncol(tune.grid)], 
            tune.grid = tune.grid)
    }

    # Clean and close the progress bar
    close(progress.bar)
    cat(paste0("Optimal Parameters (",
        paste(paste(names(output$parameters),output$parameters, sep=": "), collapse = ", "), 
        ")\n\n"))

    return(output)
}