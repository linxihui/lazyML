#' @title Resample to evaluate performance of \code{\link{mpTune}} chosen model
#'
#' @description
#' Resample (e.g., cross validation, cross validation) to estimate the true performance 
#' of the whole \code{\link{mpTune}} model choosing precedure.
#'
#' @param obj                A \code{\link{mpTune}} object returned from a call to \code{\link{mpTune}}
#' @param x                  Design matrix if not in \code{obj}
#' @param y                  Response vector if not in \code{obj}
#' @param weights            Sample weights if not in \code{obj}
#' @param formula            A model formula. Default to obj$data$formula if any
#' @param data               Data to fit. Default to obj$data$data if any
#' @param returnAllSummaries Should all \code{summarizedMpTune} be returned
#' @param summaryArgs        Additional argument passed to \code{\link{summary.mpTune}}
#' @param returnAllMpTunes   Should all \code{mpTune} objects be returned
#' @param printresampleInfo  If to print resampling fitting progress (not implemented yet)
#' @param parallelResampling Should resampling fitting be parallelled (not implemented yet)
#' @param samplingFunction   Any function that returns a list of indices for training
#' @param ...                Further arguments to be passed to \code{\link{resampling}}
#' @return A 'resampledMpTune' object with entries: \cr
#' 		$ aggregatePerformance: a data.frame gives an aggregated (over resamples) performance \cr
#' 		$ allResamplePerformance: a data.frame gives detail performance on earch resample \cr
#' 		$ sampleIndex: a list of resample indices for training \cr
#'
#' @seealso See \code{\link{mpTune}} for examples
#' @export
resample <- function(obj, ...) UseMethod('resample');


#' @rdname resample
#' @export
resample.mpTune.formula <- function(obj, formula = obj$data$formula, data = obj$data$data, ...) {

	mf <- model.frame(formula, data);
	x <- model.matrix(formula, data);
	y <- model.response(mf);
	xint <- match("(Intercept)", colnames(x), nomatch = 0);
	if (xint > 0) x <- x[, -xint, drop = FALSE];
	rm(mf);

	out <- resample.mpTune(obj, x, y, ...);
	out$formula <- formula;
	class(out) <- c('resampledMpTune.formula', 'resampledMpTune');
	return(out);
	}
	

#' @rdname resample
#' @export resample.mpTune
resample.mpTune <- function(
	obj, x = obj$data$x, y = obj$data$y, weights = obj$data$weights,
	samplingFunction = createCVFolds, ..., 
	returnAllSummaries = TRUE, summaryArgs = NULL, 
	returnAllMpTunes = FALSE,
	printresampleInfo = TRUE,
	parallelResampling = FALSE) {

	if (is.character(y)) y <- as.factor(y);
	lev <- if (is.factor(y)) levels(y) else NULL;

	samples <- samplingFunction(x, y, ...);

	if(!is.null(obj$config$mpTuneControl$sampleIndex)) {
		stop('sampleIndex in obj$config$mpTnControl must be NULL.');
		}

	`%op%` <- if (parallelResampling) `%dopar%` else `%do%`;

	resampleMpTune <- foreach(this.sample = samples, .errorhandling = 'pass') %op% {
		tuneObj <- mpTune(
			x = x[this.sample, , drop = FALSE],
			y = y[this.sample],
			weights = weights[this.sample],
			models = obj$config$models, 
			modelControl = obj$config$modelControl,
			preProcess = obj$config$preProcess,
			gridLength = obj$config$gridLength, 
			randomizedLength = obj$config$randomizedLength, 
			mpTnControl = modifyList(obj$config$mpTnControl, list(returnData = FALSE, sampleIndex = NULL))
			);

		bestFit <- do.call(
			what = fit, 
			args = c(
				list(
					obj = tuneObj, x = x[this.sample, , drop = FALSE], 
					y = y[this.sample], weights = weights[this.sample]
					), 
				summaryArgs
				)
			);

		pred <- predictionFunction(
			method   = bestFit$modelInfo,
			modelFit = bestFit$fit,
			newdata  = x[-this.sample, , drop = FALSE],
			preProc  = bestFit$preProc,
			param    = NULL
			);

		prob <- NULL;
		if (tuneObj$config$mpTnControl$classProbs && !is.null(bestFit$modelInfo$prob) && !is.null(lev)) {
			prob <- probFunction(
				method   = bestFit$modelInfo,
				modelFit = bestFit$fit,
				newdata  = x[-this.sample, , drop = FALSE],
				preProc  = bestFit$preProc,
				param    = NULL
				);
			}

		perf <- .getPerformanceSingle(
					summaryFunction = tuneObj$config$mpTnControl$summaryFunction, 
					obs = y[-this.sample], pred = pred, prob = prob, 
					lev = lev, model = bestFit$modelName);

		this.out <- list(perf = perf);
		if (returnAllSummaries) {
			this.out <- c(this.out, list(
					perfSummary = do.call(summary, c(list(obj = tuneObj), summaryArgs))
					));
			}
		if (returnAllMpTunes) {
			this.out <- c(this.out, list(mpTuneObj = tuneObj));
			}
		return(this.out);
		}

	# check error
	allResamplePerf <- foreach(this.result = resampleMpTune, this.resample = names(samples)) %op% {
		if (inherits(this.result, 'condition')) {
			cat("Fail at resample:", this.resample, '\n');
			print(this.result);
			list(perf = structure(rep(NA_real_, length(obj$performanceMetric)), .Names = obj$performanceMetric))
		} else {
			this.result
			}
		}
	names(resampleMpTune) <- names(samples);

	allResamplePerf <- NULL;
	meanPerf <- NULL;
	tryCatch(
		expr = {
			allResamplePerf <- t(sapply(resampleMpTune, '[[', 'perf'));
			
			meanPerf <- apply(
					X = allResamplePerf, 
					MARGIN = 2, 
					FUN = function(x) c(
										Mean = mean(x, na.rm = TRUE), 
										SD = sd(x, na.rm = TRUE), 
										resampleSize = length(na.omit(x))
										)
					);
			}, 
		error = function(e) {print(e);}
		);

	out <- list(
				meanPerformance = meanPerf, 
				allResamplePerformance = allResamplePerf,
				sampleIndex = samples
				);
	if (returnAllSummaries) {
		out <- c(out, list(
			allPerformanceSummaries = lapply(resampleMpTune, `[[`, 'perfSummary')
			));
		}
	if (returnAllMpTunes) {
		out <- c(out, list(
			allMpTuneObjects = lapply(resampleMpTune, `[[`, 'mpTuneObj')
			));
		}
	class(out) <- 'resampledMpTune';
	return(out);
	}
