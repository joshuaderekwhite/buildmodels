#' Standardize the format of XGBoost
#'
#' Call the xgbStandardize function in lieu of the xgboost function
#' to be able to use the xgboost function by using a formula and data
#' frame to learn a model, to use the gridCV function in the buildmodels
#' package and to make predictions.
#' 
#' The xgbStandardize also overrides the behavior of the prediction function
#' for an xgb.standard model. This allows for the pass through of data frame
#' as the data object, and provides the prediction probabilities if
#' decision.value == TRUE.
#' 
#' @param data Input to the model in a data frame format
#' @param formula Model formula used to learn or predict the model
#' @inheritParams xgboost
#' @return A model with the list values of xgboost with new values for
#'  predictor and predictor.factors
#' @keywords xgboost
#' @seealso [xgboost::xgboost()]
#' @export
#' @examples
#' model.xgb <- xgbStardize(status~., data, nrounds = 100, eta = 0.01)
#' pred.xgb <- predict(model.xgb, newdata=data)
#' prob.xgb <- predict(model.xgb, newdata=data, decision.values=TRUE)

xgbStandardize <- function(formula, data, ...) {
    require(xgboost)
    require(dplyr)
    require(methods)
    require(rlang)
    setClass("xgb.standard")

    data.ready <- xgb.DMatrix(
        data = model.matrix(~., data=data %>% select(-!!as.character(formula[[2]])))[,-1] %>%
            as.matrix(),
        label = as.numeric(data[,as.character(formula[[2]])]) - 1 %>% as.vector()
    )
    model <- do.call(xgboost, list(data = data.ready, ...))
    model$predictor <- as.character(formula[[2]])
    model$predictor.factors <- levels(data[,as.character(formula[[2]])])
    class(model) <- "xgb.standard"
    model$call <- call2("xgbStandardize", data = quote(data.ready), !!!list(...))
    return(model)

    setMethod("predict", signature(object = "xgb.standard"),
        function(object, newdata, decision.values=FALSE, ...){
            require(xgboost)
            data.ready <- xgb.DMatrix(
                data = model.matrix(~., data=newdata %>% select(-!!object$predictor))[,-1] %>%
                    as.matrix(),
                label = as.numeric(newdata[,object$predictor]) - 1 %>% as.vector()
            )
            class(object) <- "xgb.Booster"
            pred <- predict(object, data.ready, ...)
            if(decision.values){
                return(pred)
            }
            else{
                return(as.factor(object$predictor.factors[round(pred)+1]))
            }
        }
    )

    setMethod("xgb.importance", signature(model="xgb.standard"),
        function(model){
            class(model) <- "xgb.Booster"
            return(callGeneric(model=model))
        }
    )
}