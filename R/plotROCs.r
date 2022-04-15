#' Plot ROC Curve of all listed models
#'
#' Takes in a list of models and returns the ROC curves associated
#' with each. All of the ROC curves are plotted on a single plot.
#' 
#' @param predictor The vector of the validation set that contains
#'  the true values of the dependent variable.
#' @param models A list of all models that the ROC curves should be
#'  plotted for. Each model needs to contain at a minimum a value for
#'  name which is what will be included in the legend, as well as a
#'  value for prob, in which the probabilities of the model
#'  classifications should be passed through.
#' @param title The title that should be displayed for the plot.
#' @keywords ROC
#' @seealso [pROC::roc()]
#' @export
#' @examples
#' plotROCs(predictor = weather.test$RainTomorrow, models = list(
#'    list(name = "Linear SVM", prob = prob.linear),
#'    list(name = "Gaussian SVM", prob = prob.radial),
#'    list(name = "Random Forest", prob = prob.rf),
#'    list(name = "Gradient Boosted Tree", prob = prob.xgb)
#'    ),
#'    title = "ROC Curves for Predicition Models of Rain Tomorrow"
#' )

plotROCs <- function(predictor, models, title){
    require(pROC)
    add <- list(main = title)
    legend <- c()
    i <- 1
    for (model in models){
        params <- append(list(plot=TRUE, print.auc=TRUE, col=i, 
            lwd =4, legacy.axes=TRUE, print.auc.y=0.3+(0.05*i)), add)
        print(model$name)
        do.call("roc", append(list(predictor, model$prob), params))
        print(i)
        legend[i] <- model$name
        add <- list(add = TRUE)
        i <- i + 1
    }
    legend("bottomright", 
    legend=legend, col=1:length(models), lwd=4)
}