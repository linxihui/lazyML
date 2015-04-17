#' @title list of list of models
#' @param n    number of models to list
#' @param y    response vector, used to determine problem type if \code{type} is missing
#' @param type type of problem. Default to be determined by \code{y}
#' @return A list of model names
#' @export
getDefaultModel <- function(n = Inf,  y, type = c(NA, 'classification', 'regression', 'survival')) {
    load(system.file('models', 'default_models.RData', package='lazyML'));
	type <- match.arg(type);
	if (is.na(type)) {
		type <- ifelse('Surv' == class(y), 'survival',
					   ifelse(is.numeric(y), 'regression', 'classification'));
		}
	n <- min(n, length(default.models[[type]]));
	return(default.models[[type]][1:n])
	}
