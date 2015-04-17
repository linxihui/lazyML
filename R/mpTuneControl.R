#' @title Generate mpTnControl for mpTune
#' @param samplingFunction A function to takes x, y as input and generate a list of training indices of cases (rows in x)
#' @param ...              Other argument to be passed to \code{samplingFunction}
#' @param sampleIndex      A list of training indices. If given, will be used directly and \code{samplingFunction} will be ignored
#' @param problemType      'classification', 'regression' or 'survival'. Default to be automatically determined. Currently not effective any way
#' @param classProbs       Should class probabilities be returned in prediction
#' @param preProcess       A caret preProcess function
#' @param summaryFunction  summary function for model performance. See caret;;defaultSummary for detail of how to customize. Default to defaultSummary which
#'                         automatically guess problemType from input and determine what to return
#' @param returnData       Should data be saved in the mpTune output
#' @param allowParallel    Should models be tuned in parallel
#' @return A list ready for \code{mpTnControl} in \code{\link{mpTune}}.
#' @export
mpTuneControl <- function(
	samplingFunction = createCVFolds,
	...,
	sampleIndex      = NULL,
	problemType        = NA,
	classProbs       = TRUE,
	preProcess       = NULL,
	summaryFunction  = defaultSummary,
	returnData       = TRUE,
	allowParallel    = TRUE
	) {
	# the following code looks nice, but seems to have scoping issue. Leveal for future examination
	# mc <- as.list(match.call())[-1];
	# return( lapply(modifyList(formals(mpTuneControl), mc), eval) );

	if (!is.null(sampleIndex)) { number <- repeats <- NULL; }
	list(
		samplingFunction = modifyFunction(samplingFunction, ...), 
		sampleIndex = sampleIndex, problemType = problemType,
		classProbs = classProbs, preProcess = preProcess, summaryFunction = summaryFunction,
		returnData = returnData, allowParallel = allowParallel
		)
	}
