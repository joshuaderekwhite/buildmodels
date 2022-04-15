# Grid CV Function
#' Perform a grid search for the provided tuning parameters using k-fold
#' cross validation
#'
#' For a given method, the values will be tuned systematically by iterating
#' through each of the values provided in the tune.parameters and compared to
#' all of the other tune.parameters. The hyper parameters are the tuning
#' parameter values with the highest accuracy value.
#' 
#' @param data Input to the model in a data frame format
#' @param predictor A string of the vector of the validation set that contains
#'  the true values of the dependent variable.
#' @param method The function call used to learn the model
#' @param method.parameters Additional parameters passed through to the method
#'  to learn the model. This should not include parameters that are going to be
#'  predicted.
#' @param folds The number of folds in the cross validation method.
#'  Default is 10.
#' @param tune.parameters Parameters that will be predicted based on the cross-
#'  validation accuracy. This should be passed through as a list with each of
#'  the objects equal to an array of values to test.
#' @param pred.parameters Parameters required to use the predict function on
#'  the method type. There is no need to pass values for the model or the data,
#'  they are already provided.
#' @return A list with the values of the hyper parameters stored as the parameter
#'  object. Also the overall cross-validated accuracy is reported as the accuracy
#'  object. And all of the grid values and their accuracies are reported as
#'  the tune.grid object.
#' @keywords cross-validation
#' @seealso [caret::train()]
#' @export
#' @examples
#' svm.radial.hyper <- gridCV(
#'    method = svm, method.parameters = list(kernel="radial"), folds = 4,
#'    data = weather.train, predictor = "RainTomorrow", 
#'    tune = list("gamma" = 10^(1:3), "cost" = 10^(-1:0))
#'    )
#' 
#' ## [1] "Tuning..."
#' ##     gamma      cost  accuracy 
#' ## 0.1000000 0.1000000 0.8309324 
#' ##    gamma     cost accuracy 
#' ##     1.00     0.10     0.78 
#' ##    gamma     cost accuracy 
#' ##    10.00     0.10     0.78 
#' ##    gamma     cost accuracy 
#' ##   100.00     0.10     0.78 
#' ##    gamma     cost accuracy 
#' ##  1000.00     0.10     0.78 
#' ##    gamma     cost accuracy 
#' ## 0.100000 1.000000 0.852933 
#' ##    gamma     cost accuracy 
#' ##     1.00     1.00     0.78 
#' ##    gamma     cost accuracy 
#' ##    10.00     1.00     0.78 
#' ##    gamma     cost accuracy 
#' ##   100.00     1.00     0.78 
#' ##    gamma     cost accuracy 
#' ##  1000.00     1.00     0.78 

gridCV <- function(data, predictor, method, method.parameters=list(), folds=10, 
    tune.parameters=list(), 
    pred.parameters=list()){
    require(caret)
    require(dplyr)

    # Create folds and tuning grid
    folds <- createFolds(data[[predictor]], k = folds)
    tune.grid <- expand.grid(tune) 
    if (nrow(tune.grid) > 0) tune.grid <- tune.grid %>% mutate(accuracy = NA)
    print("Tuning...")

    # Average the value of each fold
    for (i in 1:nrow(tune.grid)) {
        acc <- 0
        for (fold in folds){
            if (length(method.parameters) + length(tune) > 0){
                model <- do.call(method, append(list(
                formula(paste(predictor,"~.")),
                data = data[-fold,]), 
                append(method.parameters,
                as.list(tune.grid[i,])[1:length(tune)])
                ))
            }
            else {
                model <- do.call(method, append(list(
                formula(paste(predictor,"~.")),
                data = data[-fold,])))
            }
            if (length(pred.parameters) > 0){
                pred <- do.call("predict", append(list(model, newdata = data[fold,]), 
                    eval(pred.parameters)))
                if(exists(pred.parameters$post)){
                    pred <- pred[quote(pred.parameters$post)]
                }
            }
            else{
                pred <- predict(model, newdata = data[fold,])
            }
            acc <- acc + mean(data[fold,predictor] == pred)
        }
        tune.grid[i,"accuracy"] <- acc / length(folds)
        print(unlist(tune.grid[i,]))
    }
    tune.grid <- tune.grid %>% arrange(-accuracy)
    print(tune.grid)

    output <- append(tune.grid[1,], list(tune.grid = tune.grid))
    return(output)
}