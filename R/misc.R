makeloops <- function(modelInfo, modelControl, models.tuneGrids, sampleIndex) {
	models <- names(modelInfo);
	expand.model <- foreach(
		this.model     = models,
		this.info      = modelInfo,
		this.modelControl = modelControl[models],
		this.tuneGrid  = models.tuneGrids,
		.combine       = c
	) %:% foreach(
		this.foldName  = names(sampleIndex),
		this.train     = sampleIndex,
		.combine       = c
	) %:% foreach(
		i.row = 1:nrow(this.tuneGrid$loop),
		.combine = c
	) %do% {
		out <- list(list(this.model = this.model, 
			 this.info = this.info,
			 this.modelControl = this.modelControl,
			 this.tuneGrid = this.tuneGrid,
			 this.train = this.train,
			 this.foldName = this.foldName,
			 i.row = i.row
			 ));
		names(out) <- paste(this.model, this.foldName,  i.row, sep = '_._.');
		out
		}
	}


makeTuneGrid <- function(x, y, modelInfo, gridLength, randomizedLength) {
	mapply(
		FUN = function(this.model, this.glen, this.rlen) {
			if (!is.null(this.model$randomizedSearch)) {
				tuneGrid <- list(loop = this.model$randomizedSearch(x, y, this.rlen))
			} else {
				tuneGrid <- list(loop = this.model$grid(x, y, this.glen));
				}
			if (is.null(this.model$loop)) {
				return(tuneGrid);
			} else {
				return(this.model$loop(tuneGrid$loop));
				}
			},
		this.model = modelInfo,
		this.glen = gridLength,
		this.rlen = randomizedLength,
		SIMPLIFY = FALSE
		)
	}


.getPerformanceSingle <- function(summaryFunction, obs, pred, prob = NULL, lev = NULL, model = NULL) {
	if (!is.null(lev)) pred <- factor(pred, levels = lev);
	if (is.null(prob)) {
		pred <- data.frame('obs' = obs, 'pred' = pred, check.names = FALSE);
	} else { 
		pred <- data.frame('obs' = obs, 'pred' = pred, prob, check.names = FALSE); 
		}
	perf <- summaryFunction(
		pred, #data.frame(obs = obs, pred, check.names = FALSE), 
		lev = lev, model = model
		);
	return(t(perf));
	}


.getPerformanceList <- function(summaryFunction, obs, pred, prob = NULL, lev = NULL, model = NULL) {
	if(!is.null(lev)) pred <- lapply(pred, factor, levels = lev);
	if (is.null(prob)) {
		pred <- lapply(pred, function(z) data.frame(pred = z, stringsAsFactors = FALSE));
	} else {
		pred <- mapply(
			FUN = data.frame, 
			pred = pred, prob, 
			check.names = FALSE, 
			stringsAsFactors = FALSE,
			SIMPLIFY = FALSE
			);
		}
	perf <- foreach(this.pred = pred, .combine = rbind) %do% {
		summaryFunction(data.frame(obs = obs, this.pred, check.names = FALSE), 
			lev = lev, model = model, is.prob = !is.null(prob))
		}
	rownames(perf) <- names(pred);
	return(perf);
	}


reducePerformance <- function(performanceList, tuneGridList, modelInfo, parallel = FALSE) {
	`%op%` <- if (parallel) `%dopar%` else `%do%`;
	performanceListReduced <- foreach(
			thisModelPerf = performanceList, 
			thisTuneGrid = tuneGridList, 
			thisInfo = modelInfo, 
			.errorhandling = 'remove'
			) %op% {
		perf <- plyr::ddply(
			.data = thisModelPerf[, 
					setdiff(
							names(thisModelPerf), 
							c('node', 'size', 'status', 'message', 'resample')
							), 
					drop = FALSE
					],
			.variables = names(thisTuneGrid$loop),
			.fun       = meanSD,
			exclude    = names(thisTuneGrid$loop)
			);
		if (!is.null(thisInfo$sort)) {
			perf <- thisInfo$sort(perf); 
			} # if multiple 'best model', choose the first one after this sorting.
		return(perf); # list(perf = perf);
		}
	names(performanceListReduced) <- names(performanceList);
	return(performanceListReduced);
	}


#' @title modify default arguments of a function
#' @param f A function to be modified
#' @param ... Arguments an values key pairs
#' @return A function with modified default values
#' @examples
#' # function twoClassSummary has an argument 'metric' which defaults to long vector of metrics,
#' # to make a numer twoClassSummary function that returns only 'Kappa' and 'AUC', we can do
#' newRegressionSummary <- modifyFunction(twoClassSummary, metric = c('Kappa', 'AUC'));
#' @export
modifyFunction <- function(f, ...) {
	dots <- list(...);
	function(...) {
		#innerDots <- list(...);
		#innerDots[names(dots)] <- dots;
		do.call(f, modifyList(list(...), dots))
		}
	}


# the following 2 functions are taken from packages 'caret'
expandParameters <- function(fixed, seq) {
	if (is.null(seq)) { return(fixed); }
	isSeq <- names(fixed) %in% names(seq);
	out <- fixed[rep(1, nrow(seq)+1),, drop=FALSE];
	out[-1, isSeq] <- seq;
	return(out);
	}

meanSD <- function(x, exclude=NULL) {
	if(!is.null(exclude)) { x <- x[, !(colnames(x) %in% exclude), drop=FALSE]; }
	mean.x <- colMeans(x, na.rm = TRUE);
	sd.x <- sapply(x, sd, na.rm = TRUE);
	names(sd.x) <- paste(names(mean.x), 'SD');
	return(c(mean.x, sd.x));
	}

# TODO: rename the following function (thus, need to change all model information having using this function
# This function was taken from package 'kernlab' and modified by Eric, to generate REASONABLE random guess of
# the Gaussian kernel scale parameter
sigest.random <- function (x, frac = 0.5, scaled = TRUE, na.action = na.omit, len = 3) {
	x <- na.action(x);
	if (1 == length(scaled)) { scaled <- rep(scaled, ncol(x)); }
	if (any(scaled)) {
		co <- !apply(x[, scaled, drop = FALSE], 2, var);
		if (any(co)) {
			scaled <- rep(FALSE, ncol(x));
			warning(paste("Variable(s)", paste("`", colnames(x[,
			  scaled, drop = FALSE])[co], "'", sep = "",
			  collapse = " and "), "constant. Cannot scale data."));
		} else {
			xtmp <- scale(x[, scaled]);
			x[, scaled] <- xtmp;
			}
		}
	m      <- dim(x)[1];
	n      <- floor(frac * m);
	index  <- sample(1:m, n, replace = TRUE);
	index2 <- sample(1:m, n, replace = TRUE);
	temp   <- x[index, , drop = FALSE] - x[index2, , drop = FALSE];
	dist   <- rowSums(temp^2);
	srange <- 1/quantile(dist[dist != 0], probs = runif(len));
	return(srange);
	}
