### sampling methods
# 1.
### create repeated cross validation folds ########################################################

#' @title create (repeated) cross validation folds
#' @param x        Design matrix
#' @param y        Response vector
#' @param nfold    Number of folds
#' @param repeats  Number of repeats
#' @param stratify Should folds be stratified on response classes or censoring indicators (if survival)
#' @param seed     Seed to set before sampling
#' @param replace  If to sample with replacement
#' @return A list of repeated cross validation training samples (as row numbers of x) or a list of Bootstrap samples (as row numbers of x as well).
#' @name resampling
NULL


#' @rdname resampling
#' @export
createCVFolds <- function(x, y, nfold = 5, repeats = 1, stratify = FALSE, seed = NULL) { 
	N <- ifelse(is.data.frame(y) || is.matrix(y), nrow(y), length(y));
 
	if (!is.null(seed)) { set.seed(seed); }

	folds <- 1:nfold;
	names(folds) <- paste0('Fold', folds);

	stratify <- stratify && (is.factor(y) || 'Surv' == class(y));
	if (!stratify) {
		resp <- replicate(repeats, sample(rep(1:nfold, length = N), size= N, replace = FALSE), simplify = FALSE);
	} else {
		stratify.vector <- if ('Surv' == class(y)) as.character(y[, 'status']) else as.character(y);
		resp <- replicate(
			repeats, 
			expr = {
				class.distr <- table(stratify.vector);
				out <- vector(mode = 'numeric', length = N);
				for(lev in names(class.distr)) {
					out[stratify.vector == lev] <- sample(rep(1:nfold, length = class.distr[lev]))
					}
				out
				}, 
			simplify = FALSE
			);
		}
	names(resp) <- paste0('Rep', 1:repeats);

	cvFolds <- lapply(
		X = resp, 
		FUN = function(this.col) {
			lapply(
				X = folds, 
				FUN = function(this.fold) {
					(1:N)[this.col != this.fold]
					}
				)
			}
		); 

	cvFolds <- unlist(cvFolds, recursive = FALSE);
	}



# 2. Bootstrap

#' @rdname resampling
#' @param n Bootstrapped sample size (if integer) or proportional
#' @export
createBootstrap <- function(x, y, n, repeats = 25, replace = TRUE, stratify = FALSE, seed = NULL) { 
	N <- ifelse(is.data.frame(y) || is.matrix(y), nrow(y), length(y));
	if (missing(n)) n <- ifelse(replace, N, round(N*.632));
	if (n < 1) n <- N * n;
	stratify <- stratify && (is.factor(y) || 'Surv' == class(y));
	if (!stratify) {
		out <- replicate(repeats, expr = {sample(1:N, n, replace)}, simplify = FALSE);
	} else if (is.factor(y)) {
		stratify.vector <- if ('Surv' == class(y)) as.character(y[, 'status']) else as.character(y);
		uniqueValues <- na.omit(unique(stratify.vector));
		out <- replicate(
			repeats, 
			expr = {
				out1 <- lapply(
					X = uniqueValues, 
					FUN = function(v) {
						y1 <- which(stratify.vector == v);
						n1 <- ifelse(replace, length(y1), round(length(y1)*.632));
						sample(y1, n1, replace)
						}
					);
				unlist(out1)
				}, 
			simplify = FALSE
			);
		}
	names(out) <- paste0('Boot', 1:repeats);
	return(out);
	}

bootstrapResample <- createBootstrap;
