# @title Execute model fitting and prediction
# @description Execute fitting and prediction for one model with one parameter config set on one resample for mpTune
#
executeTask <- function(
	task, x, y, weights, lev, 
	mpTnControl, preProcess, perf.proto
	) {

	if (interactive() && !mpTnControl$allowParallel) caret::checkInstall(task$this.info$library);
	for (pkg in c('lazyML', task$this.info$library)) {
		suppressPackageStartupMessages(
			do.call(require, list(pkg, quietly = TRUE, warn.conflicts = FALSE))
			)
		}
	thisRow.tuneGrid <- task$this.tuneGrid$loop[task$i.row, , drop = FALSE];
	task$this.submodels <- if(
		is.null(task$this.tuneGrid$submodels) || !nrow(task$this.tuneGrid$submodels[[task$i.row]])
		) NULL else task$this.tuneGrid$submodels[[task$i.row]];

	tryCatch(
		expr = {
			mod <- do.call(createModel,
				args = c(
					list(
						x          = x[task$this.train, , drop = FALSE],
						y          = y[task$this.train],
						wts        = if(is.null(weights)) NULL else weights[task$this.train],
						method     = task$this.info,
						tuneValue  = thisRow.tuneGrid,
						obsLevels  = lev,
						pp         = preProcess,
						classProbs = mpTnControl$classProbs
						),
					task$this.modelControl
					),
				quote = TRUE);

			fittingObjectSize <- object.size(mod);

			pred <- caret::predictionFunction(
				method   = task$this.info,
				modelFit = mod$fit,
				newdata  = x[-task$this.train, ,drop = FALSE],
				preProc  = mod$preProc,
				param    = task$this.submodels
				);

			prob <- NULL;
			if (mpTnControl$classProbs && !is.null(task$this.info$prob) && !is.null(lev)) {
				prob <- caret::probFunction(
					method   = task$this.info,
					modelFit = mod$fit,
					newdata  = x[-task$this.train, ,drop = FALSE],
					preProc  = mod$preProc,
					param    = task$this.submodels
					);
				}
			rm(mod); # it can be big
			if (is.null(task$this.submodels)) {
				perf <- .getPerformanceSingle(
					summaryFunction = mpTnControl$summaryFunction, obs = y[-task$this.train], 
					pred = pred, prob = prob, lev = lev, model = task$this.model);
			} else {
				perf <- .getPerformanceList(
					summaryFunction = mpTnControl$summaryFunction, obs = y[-task$this.train], 
					pred = pred, prob = prob, lev = lev, model = task$this.model);
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
				resample = task$this.foldName, 
				expandParameters(thisRow.tuneGrid, task$this.submodels), 
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
				 resample = task$this.foldName, 
				 expandParameters(thisRow.tuneGrid, task$this.submodels), 
				 perf, 
				 stringsAsFactors = FALSE,
				 check.names = FALSE);
			 return(out);
			 }
		);
	}
