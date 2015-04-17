#' @title summarize result from \code{\link{mpTune}}
#' 
#' @description
#' summarize a \code{\link{mpTune}} result and print summary information
#'
#' @param object    A 'mpTune' object or a 'summarizedMpTune' object
#' @param x         A 'mpTune' object or a 'summarizedMpTune' object
#' @param metric    A vector a metric for sumary. Default to metrics used in \code{obj}
#' @param maximize  A logical vector inddicating if metric values the larger the better
#' @param by        Which should be used for the first level of the returned list. See the following for detail
#' @param indent    A string used to indent he next level of a list
#' @param prefix    A vector of characters put in front of each entry
#' @param digits    Number of effective digits to keep for double numeric value
#' @param justify   Either 'left' (default), 'right', 'center', 'none' for alignment of keys (list names)
#' @param ...       Just for compatibility
#' 
#' @return summary returns a 'summarizedMpTune' object, which is a list of two levels, either
#' performance metrics -> models (by = 'metric') or models -> performance metrics, where models are always ranked by
#' the performance
#'
#' @seealso See \code{\link{mpTune}} for examples
#' @export
summary.mpTune <- function(
	object,
	metric = object$performanceMetric,
	maximize = sapply(metric, function(x) ifelse(x %in% c('RMSE', 'MAE'), FALSE, TRUE)),
	by = c('metric', 'model'), ...
	) {
	by <- match.arg(by);

	perfColNames <- c(object$performanceMetric, paste(object$performanceMetric, 'SD'));

	# library(plyr);
	ranges <- llply(
		.data = object$allModelsPerformance, 
		.fun = function(modelPerf) {
			perfCols <- modelPerf[, !(names(modelPerf) %in% perfColNames), drop = FALSE];
			llply(perfCols, function(x) unique(sort(x)))
			}
		);

	out <- switch(by,
		'metric' = structure( mlply(
				.data = data.frame(mtr = metric, mx = 2*maximize - 1, stringsAsFactors = FALSE),  
				.fun = function(mtr, mx) {
					out <- llply(
						.data = object$allModelsPerformance,
						.fun = function(modelPerf) {
							i.best <- which.max(modelPerf[, mtr]*mx);
							if (length(i.best) == 0) if (length(object$performanceMetric) <= 1) i.best <- 1 else {
									i.best <- which.max(rowMeans(modelPerf[, setdiff(object$performanceMetric, mtr), drop = FALSE], na.rm = TRUE));
									i.best <- ifelse(length(i.best) > 0, i.best, 1);
									};
							modelPerf[i.best, , drop = FALSE]
							}
						);
					attributes(out) <- NULL;
					names(out) <- names(object$allModelsPerformance);
					out[order(laply(out, '[[', mtr)*mx, decreasing = TRUE)]
					}
				), .Names = metric),
		'model' = llply(
				.data = object$allModelsPerformance, 
				.fun = function(modelPerf) {
					out <- mlply(
						.data = data.frame(mtr = metric, mx = 2*maximize - 1, stringsAsFactors = FALSE), 
						.fun = function(mtr, mx) {
							i.best <- which.max(modelPerf[, mtr]*mx);
							if (length(i.best) == 0) if (length(object$performanceMetric) <= 1) i.best <- 1 else {
									i.best <- which.max(rowMeans(modelPerf[, setdiff(object$performanceMetric, mtr), drop = FALSE], na.rm = TRUE));
									i.best <- ifelse(length(i.best) > 0, i.best, 1);
									};
							modelPerf[i.best, , drop = FALSE]
							}
						)
					attributes(out) <- NULL;
					structure(out, .Names = metric)
					}
				)
		);
	
	attr(out, 'split_labels') <- NULL;
	attr(out, 'split_type') <- NULL;

	structure(
		out, 
		class = c('summarizedMpTune', class(out)), 
		parameterRange = ranges, 
		by = by
		)
	}


#' @rdname summary.mpTune
#' @export
print.summarizedMpTune <- function(
	x, 
	indent = rep(' ', 4), 
	prefix = c('-', '-', '+'), 
	digits = 3, 
	justify = c('left', 'right', 'center', 'none'), 
	...
	) {

	justify <- match.arg(justify);
	prefix <- paste0(rep(prefix, length = 3), ' ');

	for (v1 in names(x)) {
		cat(prefix[1], v1, ' :\n', sep = '');
		for (v2 in names(x[[v1]])) {
			cat(indent, prefix[2], v2, ' :\n', sep = '');
			vModel <- switch(attr(x, 'by'), metric = v2, model = v1);
			whichSD <- grep(' SD$', names(x[[v1]][[v2]]));
			whichPerf <- c(whichSD - length(whichSD), whichSD);
			param <- x[[v1]][[v2]][, -whichPerf, drop = FALSE];
			param <- sapply(param, function(z) if(is.numeric(z))  as.character(round(z, digits) ) else as.character(z));	
			paramRanges <- laply(
				.data = attr(x, 'parameterRange')[[vModel]],
				.fun = function(z) {
					if (length(z) < 2) return('');
					if (is.numeric(z) && all(z %% 1 == 0)) mode(z) <- 'integer';
					switch(class(z),
						   integer = paste0('[', paste0(range(z), collapse = ', '), ']'),
						   numeric = paste0(
								'[', 
								paste0(sprintf(paste0('%.', digits, 'f'), range(z)), collapse = ', '), 
								']'),
						   paste0('{', paste0(z, collapse = ', '), '}')
						   )
					}
				);
			param <- paste(param, paramRanges);
			paramNames <- names(x[[v1]][[v2]])[-whichPerf];
			perf <- matrix(unlist(x[[v1]][[v2]][, whichPerf]), nrow = 2, byrow = TRUE);
			perf <- apply(perf, 2, function(z) sprintf(paste0('%.', digits, 'f (%.', digits, 'f)'), z[1], z[2]))
			perfNames <- names(x[[v1]][[v2]])[whichSD - length(whichSD)]
			width <- max(nchar(paramNames), nchar(perfNames));

			for (i in seq_along(paramNames)) 
				cat(indent, indent, prefix[3], format(paramNames[i], width = width, justify = justify), ' : ', param[i], '\n', sep = '');

			for (i in seq_along(perfNames)) 
				cat(indent, indent, prefix[3], format(perfNames[i], width = width, justify = justify), ' : ', perf[i], '\n', sep = '');
			}
		}

	invisible(x);
	}
