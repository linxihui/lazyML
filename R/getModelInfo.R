#' @title get model information 
#' @description
#' search built-in model informations, such as fit, predict methods. It is also identical to \code{caret::getModelInfo}
#' @param model Name of model
#' @param regex Should regular expression be used for matching
#' @param ... Other argument passed to \code{\link{grepl}}
#' @return A list of matched models, each of which is a list of fit, predict methods
#' @seealso \code{caret::getModelInfo}
#' @export
getModelInfo <- function (model = NULL, regex = TRUE, ...) {
    load(system.file('models', 'models.RData', package='lazyML'));
    if (!is.null(model)) {
        keepers <- if (regex) grepl(model, names(models), ...) else which(model == names(models))[1]; 
        models <- models[keepers];
    	}
    if (0 == length(models)) {
        stop("That model is not in lazyML's built-in library");
		}
   return(models);
}
