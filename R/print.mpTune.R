#' @title print mpTune result
#' @description 
#' Print information of the best model, failure and message if any
#' @param x         mpTune object
#' @param metric    Performance metric to determine the best model. Default to the 1st one
#' @param digits    Digits to keep for numeric result
#' @param row.names Should row name / number be printed
#' @param ...       Other arguments pass to the default base print function
#' @return NULL
#' @export print.mpTune
print.mpTune <- function(
	x, 
	metric = x$performanceMetric[1], 
	digits = 4, 
	row.names = FALSE, 
	...) {
	sumX <- summary(x);
	cat('\nThe best model based on', names(sumX)[1], 'is', names(sumX[[1]])[1], '\b,', 'with parameter(s) and mpTune performance:\n\n');
	print(sumX[[1]][[1]], digits = digits, row.names = row.names, ...);
	cat('\n');

	## check failure
	anyError <- t(sapply(x$allCVs, 
		   function(z) table(factor(
					z$status, 
					levels = 0:1, 
					labels = c('Success', 'Error')))
		   ));
	goodOrBad <- t(sapply(x$allCVs, 
		   function(z) {
			   table(factor(
					is.na(z[, metric]),
					levels = c(FALSE, TRUE), 
					labels = c('Good', 'Bad')
					))
		   	 	}
		   ));
	goodOrBad <- data.frame(goodOrBad, anyError[, 2, drop = FALSE])[goodOrBad[, 2] > 0, ];
	
	if (nrow(goodOrBad) > 0) {
		cat("Models do not give proper result:\n");
		print(goodOrBad);
		if (any(anyError[, 2] > 0)) {	
			errorMessage <- sapply(
				X = x$allCVs[anyError[, 2] > 0], 
				FUN = function(z) unique(z[, 'message'])[1]
				);
			cat("\nModels fail to fit or predict:\n");
			cat(rep('=', 30), '\n', sep = '');
			for( model in names(errorMessage)) {
				cat(model, ':\n', rep('-', 20), '\n', sep = '');
				cat('\t', errorMessage[[model]], '\n', sep = '');
				}
			} else cat("\nNo failure.\n");
	} else cat("No failure.\n");

	invisible(NULL);
	}
