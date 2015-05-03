#' @title methods for 'bestMpTuneModel', 'resampleMpTune'
#'
#' @description
#' predict, print and resample consistency checking methods for 'bestMpTuneModel', 'resampledMpTune'
#'
#' @param object     A 'bestMpTuneModel' or 'resampleMpTune'
#' @param x          A 'bestMpTuneModel' or 'resampleMpTune'
#' @param newdata    A new design matrix if object is 'bestMpTuneModel' or a new data frame if object is 'bestMpTuneModel.formula'
#' @param digits     Number of effective digits to keep for numeric variables
#' @param type       A string indicated type of prediction, only for classification and possibly survival.
#' @param row.names  Should row names / numbers be printed
#' @param ...        Just for campatibility
#' 
#' @return  prediction vector or matrix, performance summary, consistency indicators 
#' @details
#' For predict, it depends on problem types and models,
#' \itemize{
#' 	\item{'classifiction'} {
#' 		a vector of predictions if type = 'raw' and a matrix where each coloumn is predicted 
#' 		probabilities for an each class named by its class label if type = 'prob}
#' 	\item{'regression'} {it is always a vector of prediction}
#' 	\item{'survival'} {
#' 		if type = 'raw', return a vector of 'risk' or predicted survival time (depending on models). 
#' 		If type = 'prob', for some model (e.g., randomForestSRC::rfsrc), it returns a matrix of survival probabilities 
#' 		with each columns named by survival times}
#' 	}.
#' Function \code{checkConsistency} checks on the consistency of resampling in 'resampledMpTune
#'
#' @seealso See \code{\link{mpTune}} for examples
#' @details 
#' print information of the fited best model for 'bestMpTuneModel' objects;
#' print the resampled performance;
#' apply the fitted best model on new data for prediction
#' @name bestMpTuneModel
NULL

#' @rdname bestMpTuneModel
#' @export
predict.bestMpTuneModel.formula <- function(object, newdata, type = c('raw', 'prob'), ...) {
	type <- match.arg(type);
	stopifnot(is.data.frame(newdata));
	x <- model.matrix(object$formula[-2], newdata);
	xint <- match("(Intercept)", colnames(x), nomatch = 0);
	if (xint > 0) x <- x[, -xint, drop = FALSE];
	return(predict.bestMpTuneModel(object, x, type = type, ...));
	}


#' @rdname bestMpTuneModel
#' @export predict.bestMpTuneModel
predict.bestMpTuneModel <- function(object, newdata, type = c('raw', 'prob'), ...) {
	type <- match.arg(type);
	switch(type,
		raw =  caret::predictionFunction(
			method   = object$modelInfo,
			modelFit = object$fit,
			newdata  = newdata,
			preProc  = object$preProcess,
			param    = NULL
			),
		prob <- caret::probFunction(
			method   = object$modelInfo,
			modelFit = object$fit,
			newdata  = newdata,
			preProc  = object$preProc,
			param    = NULL
			)
		)
	}


#' @rdname bestMpTuneModel
#' @export print.bestMpTuneModel
print.bestMpTuneModel <- function(x, digits = 4, row.names = FALSE, ...) {
	cat('\nThe best model based on', x$selectionMetric, 'is', x$modelName);
	cat(', with parameter(s) and mpTune performance:\n\n');
	print(x$mpTunePerformance, digits = digits, row.names = row.names, ...);
	cat('\n');
	invisible(NULL);
	}



#' @rdname bestMpTuneModel
#' @export print.resampledMpTune
print.resampledMpTune = function(x, digits = 4, ...) {
	cat('Resampled performance:\n');
	print(x$meanPerformance, digits = digits, ...);
	cat('Mean Spearson correlation of model ranks between resamples: \n');
	print(checkConsistency(x));
	invisible(x$meanPerformance);
	}


#' @rdname bestMpTuneModel
#' @export checkConsistency
checkConsistency <- function(object) {
	if (is.null(object$allPerformanceSummaries)) {
		stop('No summary information in object.');
		}
	metric <- names(object$allPerformanceSummaries[[1]]);
	out <- sapply(
		X = metric, 
		FUN = function(mtr) {
			orders <- sapply(object$allPerformanceSummaries, function(x) order(names(x[[mtr]]))); 
			corMat <- cor(orders, method = 'spearman'); 
			mean(corMat[upper.tri(corMat)])
			}
		);
	out <- t(out);
	rownames(out) <- 'resample consistency';
	out
	}
