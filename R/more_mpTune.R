# Add morer resample to get a stable performance estimate
# more.resampledMpTune 

# should also add a `combine` method for both 'mpTune' and 'resampledMpTune' object
# for manual parallellization

#' @title tune more models or do more resampling
#' @description
#' tune more models using the same resampling setting and performance metric for fair comparison
#'
#' @param obj A 'mpTune' or 'resampledMpTune' 
#' @param ... Other arguments passed to \code{\link{mpTune}}
#' @return same object as \code{x}
#' @export
more <- function(obj, ...) UseMethod('more');


#' @title tune more models
#'
#' @description
#' tune more models using the same resampling setting and performance metric for fair comparison
#'
#' @param obj               A 'mpTune' object
#' @param x                 A design matrix or a formula
#' @param y                 A response vector or a data.frame, which must be consistent with \code{x}
#' @param weights           Sample weigth
#' @param models            A vector of names from \code{\link{models}} in models.RData
#' @param ...               Other arguments (expect 'mpTnControl') passed to 'mpTune'
#' @param gridLength        Grid length if using grid search for hyper-parameters (tuning parameters)
#' @param randomizedLength  Number of hyper-parameter configuration if randomizedSearch is available for models
#'
#' @return A 'mpTune' object with more models
#' @seealso See \code{\link{mpTune}} for examples
#' @method more mpTune
#' @export more.mpTune
more.mpTune <- function(
	obj, 
	x = obj$data[[1]], y = obj$data[[2]], weights = obj$data$weights, 
	models, 
	...,
	gridLength = obj$config$gridLength[seq_along(models)], 
	randomizedLength = obj$config$randomizedLength[seq_along(models)]
	) {

	tuneObj2 <- mpTune(
		x, y, weights = weights,
		models = models, 
		preProcess = obj$config$preProcess,
		gridLength = gridLength, 
		randomizedLength = randomizedLength, 
		mpTnControl = modifyList(obj$config$mpTnControl, list(sampleIndex = obj$config$sampleIndex, returnData = FALSE))
		);

	return(structure(
		list(
			allModelsPerformance = c(obj$allModelsPerformance, tuneObj2$allModelsPerformance),
			allCVs               = c(obj$allCVs, tuneObj2$allCVs),
			data                 = obj$data,
			performanceMetric    = obj$performanceMetric,
			config = list(
				sampleIndex      = obj$config$sampleIndex,
				models           = c(obj$config$models, tuneObj2$config$models),
				modelControl     = c(obj$config$modelControl, tuneObj2$config$modelControl),
				mpTnControl      = obj$config$mpTnControl,
				preProcess       = obj$config$preProcess,
				gridLength       = c(obj$config$gridLength, tuneObj2$config$gridLength),
				randomizedLength = c(obj$config$randomizedLength, tuneObj2$config$randomizedLength)
				)
			), 
		class = class(obj)
		));
	}
