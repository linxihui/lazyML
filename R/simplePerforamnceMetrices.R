#' @title performance metrics
#' @description
#' built-in performance metrics for classification, regression and survival
#' @param data            Data frame with columns 'obs', 'pred' and possible columns with class labels as names if class probabilities needed
#' @param metric          Performance metrics to be computed
#' @param type            Problem type, either 'twoClass', 'multiClass', 'regression' or 'survival'
#' @param is.prob         TRUE or FALSE indicated if class probability is availabed in \code{data}.
#' @param lev             Class labels for a classification problem. NULL for others
#' @param model           Model names (optional)
#' @param surv.percentile Percentile used to compute quantile survival time as predition if data has time to survival information.
#' @param max.surv.time   Maximum follow-up time
#' @param na.rm           Should NA rows in data be removed before metric calculation
#' @param ...             Other argument to pass to internal methods
#' @details
#' \code{availableMetric} gives a list of metrics currently supported. \code{defaultSummary} is a wrap over \code{twoClassSummary}, 
#' \code{multiClass}, \code{regressionSummary} and \code{survivalSummary}. It automatically determines which one to use by response
#' in \code{data$obs}. \code{requireSummary} a variation of \code{\link{modifyFunction}} to modify \code{defaultSummary},
#' particularly the \code{metric} argument to request a vector of non-default metric. See \code{\link{modifyFunction}} for example
#' @name metric
NULL

### summary functions for classification, regression and survival analysis. ###

## two-class classification

.twoClassSummaryResponse <- function(response, prediction, 
	metric = availableMetric('twoClass', FALSE)) {
	# prediction <- factor(prediction, lev = levels(response));
	
	tab              <- table(response, prediction) 
	sens             <- recall <- tab[2, 2] / sum(tab[2, ]);
	spec             <- tab[1, 1] / sum(tab[1, ]);
	precision        <- tab[2, 2] / sum(tab[, 2]);
	npv              <- tab[1, 1] / sum(tab[, 1]);
	acc              <- sum(diag(tab)) / sum(tab);
	random.agreement <- sum(rowSums(tab) * colSums(tab) / sum(tab)^2);

	all.metric.outcome <- c(
		'BAC' = (sens + spec) / 2,
		'Accuracy' = acc,
		'Kappa' = (acc - random.agreement) / (1 - random.agreement),
		'F1' = 2*(precision*recall) / (precision + recall),
		'Recall' = sens,
		'Sensitivity' = sens,
		'Specifity' = spec,
		'Precision' = precision
		);

	return(all.metric.outcome[metric]);
	}

.twoClassSummaryAUC = function(response, prediction) {
		return(roc.default(response, prediction, direction = '<')$auc[1])
	}

#' @rdname metric
#' @export
twoClassSummary <- function(
	data, 
	lev = levels(as.factor(data[, 'obs'])) ,
	model = NULL, 
	metric = availableMetric('twoClass'), 
	...) {
	stopifnot(length(lev) == 2);
	stopifnot(all(c('obs', 'pred') %in% colnames(data)));
	response <- factor(data[, 'obs'], levels = lev);
	prediction <- factor(data[, 'pred'], levels = lev);
	out <- NULL;

	out <- structure(rep(NA_real_, length(metric)), .Names = metric);
	noAUC <- metric[metric != 'AUC'];
	out[noAUC] <- .twoClassSummaryResponse(response, prediction, metric = noAUC);
	
	if (all(lev %in% colnames(data)) & ('AUC' %in% toupper(metric))) {
		out['AUC'] <- .twoClassSummaryAUC(response, data[, lev[2]]);
		}
	return(out)
	}
	

### mulinomial

.multiClassSummary <- function(response, prediction, metric = availableMetric('multiClass'), na.rm = TRUE, ...) {
	if ((is.matrix(prediction) | is.data.frame(prediction))) {
		if (length(unique(response)) != ncol(prediction)) {
			stop('Numbers of classes in response and prediction do not match.');
			}
		prediction <- apply(prediction, 1, which.max);
	} else if (length(unique(response)) != length(unique(prediction))) {
			stop('Numbers of classes in response and prediction do not match.');
		}

	tab <- table(response, prediction);
	acc <- sum(diag(tab)) / sum(tab);
	random.agreement <- sum(rowSums(tab)*colSums(tab)/sum(tab)^2);

	all.metric.outcome <- c(
		'BAC' = mean(diag(prop.table(tab, margin = 1))),
		'Accuracy' = acc,
		'Kappa' = (acc - random.agreement) / (1 - random.agreement)
		);

	return(structure(all.metric.outcome[metric], .Names = metric));
	}

#' @rdname metric
#' @export
multiClassSummary <- function(
	data, 
	lev = levels(as.factor(data[, 'obs'])), # setdiff(colnames(data), c('pred', 'obs')), 
	model = NULL, 
	metric = availableMetric('multiClass'), 
	...) {
	stopifnot( all(c('obs', 'pred') %in% colnames(data)));
	return(.multiClassSummary(data[, 'obs'], data[, 'pred'], metric = metric, ...));
	}

## regression

.regressionSummary <- function(response, prediction, metric = availableMetric('regression'), na.rm = TRUE, ...) {
	if (na.rm) {
		which.na <- is.na(response) | is.na(prediction); 
		if (any(which.na))  {
			response <- response[!which.na];
			prediction <- prediction[!which.na];
			}
		}

	is.zeroVariance <- sd(response) == 0 || sd(prediction) == 0;

	all.metric.outcome <- list(
		'Spearman' = if (is.zeroVariance) 0 else expression(cor(response, prediction, method = 'spearman')),
		'Pearson'  = if (is.zeroVariance) 0 else expression(cor(response, prediction, method = 'pearson')),
		'Kendall'  = if (is.zeroVariance) 0 else expression(cor(response, prediction, method = 'kendall')),
		'RMSE'     = expression(sqrt(mean((response - prediction)^2))),
		'MAE'      = expression(mean(abs(response - prediction))),
		'R2'       = expression(summary(lm(response ~ prediction))$r.squared)
		);
	env <- environment();	
	return(structure(sapply(all.metric.outcome[metric], eval, envir = env), .Names = metric));
	}

#' @rdname metric
#' @export
regressionSummary <- function(data, lev = NULL, model, metric = availableMetric('regression')[1:4], ...){
	stopifnot( all(c('obs', 'pred') %in% colnames(data)));
	return(.regressionSummary(data[, 'obs'], data[, 'pred'], metric = metric, ...))
	}

### survival 

.survivalSummary <- function(
	response, prediction, times, is.prob = NA, 
	surv.percentile = 0.5, max.surv.time = max(response[, 1], na.rm = TRUE), 
	metric = availableMetric('survival'), 
	na.rm = TRUE, ...) {
	#suppressMessages(require(survival, quietly = TRUE)); if (!is.Surv(response)) stop('response must be a Surv object.');
	if (is.data.frame(prediction)) { prediction <- as.matrix(prediction); }
	if (!is.matrix(prediction)) { prediction <- as.matrix(prediction); }
	if (!is.numeric(prediction)) { stop('prediction must be numeric.'); }
	if (is.na(is.prob)) { is.prob <- (1 != ncol(prediction)); }
	if (na.rm) {
		which.na <- is.na(response[,1]) | is.na(response[, 2]) | apply(prediction, 1, anyNA);
		response <- response[!which.na];
		prediction <- prediction[!which.na, , drop = FALSE];
		} 
	if (is.prob) { prediction <- survival.quantiles(surv.percentile, prediction, times, max.surv.time); }
	prediction <- as.matrix(prediction);
	#
	all.supported.metrics <- c('C-index', 'CI', 'AUC', "Somers' Dxy", 'Dxy', 'Pearson', 'Spearman', 'RMSE', 'MAE'); # the last two are errors
	names(metric) <- metric;
	matched <- pmatch(toupper(metric), toupper(all.supported.metrics));
	if (all(is.na(matched))) { stop('No valid metric!'); }
	if (anyNA(matched)) {
		warning(paste0(
					   paste(metric[is.na(matched)], collapse = ', '), 
					   ifelse(sum(is.na(matched)) == 1, ' is', ' are'), 
					   ' not supported and ignored'
					   ));
		}
	metric <- structure(all.supported.metrics[matched[!is.na(matched)]], .Names = names(metric)[!is.na(matched)]);
	is.observed <- as.logical(response[, 2]);
	all.metric.outcome <- NULL;
	if (sum(is.observed) <= 2) {
		return(structure(rep(NA, length(metric)), .Names = metric)); 
	} else {
		isInfNA <- all(!is.finite(prediction[, 1])) || all(is.na(prediction[, 1]));
		if (isInfNA) {
			all.metric.outcome <- matrix(NaN, nrow = 4, ncol = ncol(prediction));
			rownames(all.metric.outcome) <- c('Pearson', 'Spearman', 'RMSE', 'MAE');
		} else {
			anyConstant <- sd(prediction[is.observed, 1]) == 0 || 
					sd(response[is.observed, 1]) == 0;
			all.metric.outcome <- rbind(
				'Pearson'  = if(anyConstant) 0 else abs(c(cor(prediction[is.observed, ], response[is.observed, 1]))),
				'Spearman' = if(anyConstant) 0 else abs(c(cor(prediction[is.observed, ], response[is.observed, 1], method = 'spearman'))),
				'RMSE'     = sqrt(colMeans((prediction[is.observed, , drop = FALSE] - response[is.observed, 1])^2)),
				'MAE'      = colMeans(abs(prediction[is.observed, , drop = FALSE] - response[is.observed, 1]))
				);
			}
		if (any(c('CI', 'C-index','AUC', "Somers' Dxy", 'Dxy') %in% metric)) {
			if (isInfNA) {
				CI <- rep(NA, ncol(prediction));
			} else {
				CI <- apply(-prediction, 2, survConcordance.fit, y = response);
				CI <- (CI[1, ] + 0.5*CI[3, ]) / colSums(CI[1:3, , drop = FALSE]);
				CI <- 0.5 + abs(0.5-CI);   ## this is a temporary fix to the situation that prediction can be survival time or risk score.
				}
			all.metric.outcome <- rbind(
				'C-index' = CI, 'CI' = CI, 'AUC' = CI, 
				"Somers' Dxy" = 2*CI - 1, 'Dxy' = 2*CI - 1,
				all.metric.outcome);
			}
		}
	return(all.metric.outcome[metric, ]);
	}

# For caret (data must be a data.frame with first columns a vector of survival object)!
# required colnames of prediction to be a vector of time strings (can be corced to 
# numeric times using as.numeric).

# currently ALWAYS use median survival time
# unlike above, data much be a data frame

#' @rdname metric
#' @export
survivalSummary <- function(
	data, lev, model, is.prob = NA, 
	surv.percentile = 0.5, max.surv.time = max(data$obs[, 1], na.rm = TRUE), 
	metric = availableMetric('survival')[1:3],
	na.rm = TRUE, ...) {

	stopifnot( 'obs' %in% names(data));
	if ( 'pred' %in% names(data) ) {
		return(.survivalSummary(
			data$obs, data$pred, 
			metric = metric, na.rm = na.rm, ...
			));
	} else {
		times.string <- setdiff(names(data), c('obs', 'pred'))
		times <- as.numeric(sub('^X', '', times.string));
		stopifnot(all(!is.na(times)));
		times.order <- order(times);

		return(.survivalSummary(
			data$obs, data[times.string[times.order]], 
			times = times[times.order], is.prob = TRUE, surv.percentile = surv.percentile, 
			max.surv.time = max.surv.time, metric = metric, na.rm = na.rm, ...
			));
		}
	}

# a wrapper to auto decide which summary function to use
performanceSummary <- function(response, prediction, type = c(NA, 'twoClass', 'multiClass', 'regression', 'survival'), ...) {
	type <- match.arg(type); 
	if (is.na(type)) {
		type <- switch(class(response), 
					   'factor' = ifelse(nlevels(response) == 2, 'twoClass', 'multiClass'),
					   'character' = ifelse(length(unique(response)) == 2, 'twoClass', 'multiClass'),
					   'Surv' = 'Surv',
					   'numeric' = 'regression',
					   stop('Not a valid "type". '))
		}
	# if ('class' == type) { type <- ifelse(length(unique(response)) == 2, 'twoClass', 'multiClass'); }
	switch(
		   type, 
		   'twoClass'   = .twoClassSummary(response, prediction, ...),
		   'multiClass' = .multiClassSummary(response, prediction, ...),
		   'Surv'       = .survivalSummary(response, prediction, ...),
		   'regression' = .regressionSummary(response, prediction, ...)
		   )
	}


#' @rdname metric
#' @export
defaultSummary <- function(data, ..., type = c(NA, 'twoClass', 'multiClass', 'regression', 'survival')) {
	type <- match.arg(type); 
	if (is.na(type)) {
		type <- switch(class(data[, 'obs']), 
					   'factor' = ifelse(nlevels(data[, 'obs']) == 2, 'twoClass', 'multiClass'),
					   'character' = ifelse(length(unique(data[, 'obs'])) == 2, 'twoClass', 'multiClass'),
					   'Surv' = 'Surv',
					   'numeric' = 'regression',
					   stop('Not a valid "type". '))
		}
	switch(
		   type, 
		   'twoClass'   = twoClassSummary(data, ...),
		   'multiClass' = multiClassSummary(data, ...),
		   'Surv'       = survivalSummary(data, ...),
		   'regression' = regressionSummary(data, ...)
		   )
	}

#' @rdname metric
#' @export
requireSummary <- function(...) modifyFunction(defaultSummary, ...);


#' @rdname metric
#' @export
availableMetric <- function(type = c('twoClass', 'multiClass', 'regression', 'survival'), is.prob = TRUE) {
	type <- match.arg(type);
	twoClass <- c('BAC', 'Accuracy', 'Kappa', 'F1', 'Recall', 'Sensitivity', 'Specifity', 'Precision');
	if (is.prob) twoClass <- c('AUC', twoClass);
	multiClass <- c('Kappa', 'Accuracy', 'BAC' ); 
	regression <- c('Spearman', 'Pearson', 'RMSE', 'MAE', 'Kendall');
	survival <- c('C-index', 'Spearman', 'Pearson', "Somers' Dxy", 'RMSE', 'MAE');
	switch(type,
		   'twoClass' = twoClass,
		   'multiClass' = multiClass,
		   'regression' = regression,
		   'survival' = survival
		   );
	}
