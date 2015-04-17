# modify the \code{caret::createModel} to pass un-evalued modelControl to the funciton (i.e., let expression be evaluated inside function
#
#' @title fit a model
#' @description 
#' almost identical to \code{caret::createModel}
#' @param x          Design matrix
#' @param y          Response vector
#' @param wts        Weights of samples
#' @param method     Model information. See \code{\link{getModelInfo}}
#' @param tuneValue  A one row data frame containing tuning parameter values
#' @param obsLevels  Class labels for classification
#' @param pp         PreProcess object
#' @param last       A logical for whether the current fit is the final fit
#' @param classProbs Should classProbs be computed
#' @param ...        Additional paramters other than tuning parameters passed to model fitting methods
#' @return A list of model fitting object and a preProcess object
#' @export
createModel <- function (x, y, wts, method, tuneValue, obsLevels, pp = NULL, 
    last = FALSE, classProbs, ...) 
{
    if (!is.null(pp$options)) {
        pp$method <- pp$options
        pp$options <- NULL
        if ("ica" %in% pp$method) 
            pp$n.comp <- pp$ICAcomp
        pp$ICAcomp <- NULL
        pp$x <- x
        ppObj <- do.call("preProcess", pp)
        ppObj$call <- "scrubed"
        x <- predict(ppObj, x)
        rm(pp)
    }
    else ppObj <- NULL
    modelFit <- do.call(
		what = method$fit, 
		args = c(
			list(
				x = x, y = y, wts = wts, param = tuneValue, lev = obsLevels, 
				last = FALSE, classProbs = classProbs
				), 
        	list(...)
			)
		);
    if (!isS4(modelFit)) {
        modelFit$xNames <- colnames(x)
        modelFit$problemType <- if (is.factor(y)) 
            "Classification"
        else if ('Surv' == class(y)) "Survival" else "Regression";
        modelFit$tuneValue <- tuneValue
        modelFit$obsLevels <- obsLevels
    }
    list(fit = modelFit, preProc = ppObj)
}
