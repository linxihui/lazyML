# combine two mpTune objects
.combineTwoMpTune <- function(obj1, obj2) {
	if(!identical(obj1$config$sampleIndex, obj2$config$sampleIndex)) 
		stop('Resample must be identical.');

	if(!all(obj2$performanceMetric %in% obj1$performanceMetric))
		stop('Performance used must be identical.');

	return(structure(
		list(
			allModelsPerformance = c(obj1$allModelsPerformance, obj2$allModelsPerformance),
			allCVs               = c(obj1$allCVs, obj2$allCVs),
			data                 = obj1$data,
			performanceMetric    = obj1$performanceMetric,
			config = list(
				sampleIndex      = obj1$config$sampleIndex,
				models           = c(obj1$config$models, obj2$config$models),
				modelControl     = c(obj1$config$modelControl, obj2$config$modelControl),
				mpTnControl      = obj1$config$mpTnControl,
				preProcess       = obj1$config$preProcess,
				gridLength       = c(obj1$config$gridLength, obj2$config$gridLength),
				randomizedLength = c(obj1$config$randomizedLength, obj2$config$randomizedLength)
				)
			), 
		class = class(obj1)
		))
	}

#' @title combine mpTune objects
#' @param ... mpTune objects
#' @return A combined mpTune objects
combine.mpTune <- function(...) {
	Reduce(.combineTwoMpTune, list(...));
	}
