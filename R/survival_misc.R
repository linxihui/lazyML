# smooth concordance index (SCI) loss for mboost
#' @title A Smooth concordance loss function for mboost algorithms
#' @param alpha the scale parater in 1/(1+alpha*z) to approach the function with value = {0, 1}. Default to 1
#' @return a \code{mboost::Family} object for mboost algorithms
#' @references http://www.hindawi.com/journals/cmmm/2013/873595/
#' @export
SCI <- function(alpha = 1) {
	stopifnot(requireNamespace('mboost'));
	mboost::Family(
		ngradient = function(y, f, w = 1) {
			time <- y[, 1];
			event <- y[, 2];
			n <- nrow(y);
			P <- expand.grid(i = 1:n, j = 1:n);
			P <- P[event[P$i] == 1 & time[P$i] < time[P$j], ];
			if (nrow(P) == 0) return(rep(0, n));
			if (length(w) == 1) w <- rep(w, n);
			delta <- exp(alpha*(P$i - P$j));
			delta <- w[P$i] * w[P$j] * delta / (1 + delta)^2;
			sapply(1:n, function(k) sum(delta[P$j == k]) - sum(delta[P$i == k])) * alpha / nrow(P)
			},
		risk = function(y, f, w = 1) {
			time <- y[, 1];
			event <- y[, 2];
			n <- nrow(y);
			P <- expand.grid(i = 1:n, j = 1:n);
			P <- P[event[P$i] == 1 & time[P$i] < time[P$j], ];
			if (nrow(P) == 0) return(1e9);
			if (length(w) == 1) w <- rep(w, n);
			- mean(w[P$i] * w[P$j] / (1 + exp(alpha* (f[P$i]  - f[P$j]))))
			},
		offset = function(y, w = 1) 0, 
		check_y = function(y) {
			if (!inherits(y, "Surv")) 
				stop("response is not an object of class ", sQuote("Surv"), 
					" but ", sQuote("family = CoxPH()"))
			y
			},
		name = 'Smooth Concordance Index'
		)
	}


## extend the survival prediction function
#' @title predict coxph model
#' @description 
#' extend survival::predict.coxph to give 'median', 'mean' and a vector a survial quantile
#' @param object        Coxph object
#' @param newdata       Newdata to predict
#' @param type          Type of prediction
#' @param max.surv.time Usually maximum follow-up time
#' @param ...           Other argument passed to survival :  : predict.coxph
#' @return a vector of predicted value or a matrix of survival probability if type = 'prob', with colnames = times
#' @export
predict.coxph <- function(
	object, newdata,
	type = c('lp', 'risk', 'expected', 'terms', 'median', 'mean', 'prob'), # more prediction choices
	max.surv.time = max(object$y[, 1]), ...
	) {
	type <- match.arg(type);
	if (!(type %in% c('median', 'mean', 'prob'))) {
		return(survival:::predict.coxph(object, newdata, type, ...));
	}
	#
	train.link <- survival:::predict.coxph(object);
	fit <- survfit(coxph(object$y ~ offset(train.link - mean(train.link)), method = 'breslow'));
	test.link <- survival:::predict.coxph(object, newdata);
	probs <- exp(exp(test.link) %*% t(log(fit$surv)));
	#
	if ('prob' == type) return(list(survival = probs, time = fit$time));
	surv.percentile <- switch(type, 'median' = 0.5, 'mean' = -1);
	return(survival.quantiles(surv.percentile, probs, fit$time, max.surv.time));
	}


#' @title get survival quantiles
#' @description
#' Get survival quantiles from survival curves for each sample
#' @param surv.percentile  A vector of survival probabilities.
#' @param prediction       A matrix of survival probilites, with row = sample, col = time
#' @param times            A vector of times of interest
#' @param max.surv.time    Usually maximum follow-up time
#' @return A matrix of survival quantitles (times), with row = sample, col = surv.percentile
#' @export
survival.quantiles <- function(surv.percentile = 0.5, prediction, times, max.surv.time = max(times)) {
	times.order <- order(times);
	if (!identical(times.order, 1:length(times.order))) {
		times <- times[times.order];
		prediction <- prediction[, times.order];
		}
	times <- c(0, times, max.surv.time);
	prediction <- cbind(1, prediction, 0);
	is.percentile <- surv.percentile > 0 & surv.percentile < 1;
	percentile <- surv.percentile[is.percentile];
	mean.time <- NULL;
	if (any(!is.percentile)) {
		mean.time <- c(prediction[, -ncol(prediction)] %*% diff(times));
		}
	if (length(percentile) == 0) {
		prediction <- NULL;
	} else {
		names(percentile) <- paste0(100*percentile, '%');
		prediction <- sapply(  X = percentile, 
							 FUN = function(p) { 
								apply(prediction, 1, function(s) times[s <= p][1])
							 });
		}
	out <- cbind(prediction, 'mean' = mean.time);
	if (1 == ncol(out)) out <- out[,1];
	return(out);
	}


# plot.surv.predict <- function(times, surv) {
# 	xyplot(
# 		   as.formula(
# 					  paste(paste(names(surv), collapse = '+'), '~ times')
# 					  ), data = data.frame(times = times, surv)
# 		   )
# 	}
