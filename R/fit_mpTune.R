#' @title fit the best model according to \code{\link{mpTune}}
#'
#' @param obj     A \code{\link{mpTune}} object returned from a call to \code{\link{mpTune}}
#' @param x       Design matrix if not in \code{obj}
#' @param y       Response vector if not in \code{obj}
#' @param weights Sample weights if not in \code{obj}
#' @param formula Model formula if not in \code{obj}
#' @param data    Ata if not in \code{obj}
#' @param model   An interger or model name indicating which model to fit. Default to 1, the best model
#' @param ...     Further arguments to be passed to \code{\link{summary.mpTune}}
#' @return A 'bestMpTuneModel' or 'bestMpTuneModel.formula' object with entries:
#'     \itemize{
#'         \item{fit}{model fit}
#'         \item{preProc}{}
#'         \item{modelName}{}
#'         \item{modelInfo}{a list of models fitting, predict information, as from caret}
#'         \item{selectionMetric}{the performace matric used to select the best model}
#'         \item{mpTunePerformance}{performance of best model in the mpTune object}
#'     }
#' @seealso See \code{\link{mpTune}} for examples
#' @export
fit <- function(obj, ...) UseMethod('fit');

#' @rdname fit
#' @export fit.mpTune.formula
fit.mpTune.formula <- function(obj, formula = obj$data$formula, data = obj$data$data, ...) {
	mf <- model.frame(formula, data);
	x <- model.matrix(formula, data);
	y <- model.response(mf);
	xint <- match("(Intercept)", colnames(x), nomatch = 0);
	if (xint > 0) x <- x[, -xint, drop = FALSE];
	rm(mf);

	out <- fit.mpTune(obj, x, y, ...);
	out$formula <- obj$data$formula;
	class(out) <- c('bestMpTuneModel.formula', 'bestMpTuneModel');
	return(out);
	}


#' @rdname fit
#' @export fit.mpTune
fit.mpTune <- function(obj, x = obj$data$x, y = obj$data$y, weights = obj$data$weights, model = 1, ...) {
	sumObj <- summary(obj, ...);

	if ('metric' != attr(sumObj, 'by')) {
		stop('Summarized object must be summarized by metric!')
		}
	#  best model
	modelName <- if(is.numeric(model)) names(sumObj[[1]])[[model]] else model;
	selectionMetric <- names(sumObj)[[1]]; 

	if (all(is.na(sumObj[[1]][[modelName]][obj$performanceMetric]))) {
		stop('All performance are NAs. No best model can be found.');
		}
	
	if(is.list(obj$config$models[[modelName]])) {
		modelInfo <- obj$config$models[[modelNames]];
	} else {
		modelInfo <- getModelInfo(obj$config$models[[modelName]], FALSE)[[1]];
		}

	if (interactive()) checkInstall(modelInfo$library);
	for (pkg in c(modelInfo$library)) {
		suppressPackageStartupMessages(
				do.call(require, list(pkg, quietly = TRUE, warn.conflicts = FALSE))
				);
		}

	paramValue <- sumObj[[1]][[modelName]][, as.character(modelInfo$parameters$parameter), drop = FALSE];
		
	mod <- do.call(createModel,
		args = c(
			list(
				x          = x, #obj$data$x,
				y          = y, #obj$data$y,
				wts        = weights, #obj$data$weights,
				method     = modelInfo,
				tuneValue  = paramValue,
				obsLevels  = if(is.factor(y)) levels(y) else NULL,
				pp         = obj$config$preProcess,
				classProbs = obj$config$mpTnControl$classProbs
				),
			obj$config$modelControl[[modelName]]
			),
		quote = TRUE
		);

	return(
		   structure(
					 c(mod, list(
								 modelName = modelName, 
								 modelInfo = modelInfo,
								 selectionMetric = selectionMetric,
								 mpTunePerformance = sumObj[[1]][[modelName]]
								 )),
					 class = 'bestMpTuneModel'
					 )
		   );
	}
