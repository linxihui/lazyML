# @title Execute model fitting and prediction
# @description Execute fitting and prediction for one model with one parameter config set on one resample for mpTune
#
executeTask <- function(
	x, y, weights, lev, 
	mpTnControl, preProcess, perf.proto, 
	this.modelControl, this.model, this.info, 
	this.tuneGrid, this.train, this.foldName, i.row
	) {
	if (interactive() && !mpTnControl$allowParallel) checkInstall(this.info$library);
	for (pkg in c(this.info$library)) {
		suppressPackageStartupMessages(
			do.call(require, list(pkg, quietly = TRUE, warn.conflicts = FALSE))
			)
		}
	thisRow.tuneGrid <- this.tuneGrid$loop[i.row, , drop = FALSE];
	this.submodels <- if(
		is.null(this.tuneGrid$submodels) || !nrow(this.tuneGrid$submodels[[i.row]])
		) NULL else this.tuneGrid$submodels[[i.row]];

	tryCatch(
		expr = {
			mod <- do.call(createModel,
				args = c(
					list(
						x          = x[this.train, , drop = FALSE],
						y          = y[this.train],
						wts        = if(is.null(weights)) NULL else weights[this.train],
						method     = this.info,
						tuneValue  = thisRow.tuneGrid,
						obsLevels  = lev,
						pp         = preProcess,
						classProbs = mpTnControl$classProbs
						),
					this.modelControl
					),
				quote = TRUE);

			fittingObjectSize <- object.size(mod);

			pred <- predictionFunction(
				method   = this.info,
				modelFit = mod$fit,
				newdata  = x[-this.train, ,drop = FALSE],
				preProc  = mod$preProc,
				param    = this.submodels
				);

			prob <- NULL;
			if (mpTnControl$classProbs && !is.null(this.info$prob) && !is.null(lev)) {
				prob <- probFunction(
					method   = this.info,
					modelFit = mod$fit,
					newdata  = x[-this.train, ,drop = FALSE],
					preProc  = mod$preProc,
					param    = this.submodels
					);
				}
			rm(mod); # it can be big
			if (is.null(this.submodels)) {
				perf <- .getPerformanceSingle(
					summaryFunction = mpTnControl$summaryFunction, obs = y[-this.train], 
					pred = pred, prob = prob, lev = lev, model = this.model);
			} else {
				perf <- .getPerformanceList(
					summaryFunction = mpTnControl$summaryFunction, obs = y[-this.train], 
					pred = pred, prob = prob, lev = lev, model = this.model);
				}

			fitting.status <- 0;
			fitting.message <- '';

			runningNode <- Sys.info()['nodename']; # Sys.getpid();
			totalUsedMemory <- paste(round(sum(fittingObjectSize, 
						sapply(ls(), function(x) object.size(get(x)))) / 1024^2, 3), 'Mb'); 
			out <- data.frame(
				node = unname(runningNode), 
				size = totalUsedMemory, 
				status = fitting.status,
				message = fitting.message, 
				resample = this.foldName, 
				expandParameters(thisRow.tuneGrid, this.submodels), 
				perf, 
				stringsAsFactors = FALSE,
				check.names = FALSE);
			return(out);
			},
		 error = function(e) {
			 perf <- as.data.frame(matrix(NA_real_, nrow = 1, ncol = length(perf.proto)));
			 names(perf) <- c(names(perf.proto));
			 fitting.status <- 1;
			 fitting.message <- paste(e, collapse = '\n'); #e$message;
			 runningNode <- Sys.info()['nodename'];
			 totalUsedMemory <- paste(round(sum(sapply(ls(), function(x) object.size(get(x)))) / 1024^2, 3), 'Mb'); 
			 out <- data.frame(
				 node = unname(runningNode), 
				 size = totalUsedMemory, 
				 status = fitting.status,
				 message = fitting.message, 
				 resample = this.foldName, 
				 expandParameters(thisRow.tuneGrid, this.submodels), 
				 perf, 
				 stringsAsFactors = FALSE,
				 check.names = FALSE);
			 return(out);
			 }
		);
	}
