#' @title Model and parameter simultaneous tuning
#'
#' @description 
#' Tuning multiple models and hyper-parameter using grid search or randomized search, in a optional parallel fashion. 
#
#' @param x                 Design matrix, usually derived from model.matrix.
#' @param y                 Response vector, numeric (regression), factor (classification) or 'Surv' object
#' @param formula           A model formula simliar to formula in \code{\link{lm}}
#' @param data              A data frame contains for the training data
#' @param weights           Sample weigth
#' @param models            A vector of names from \code{models} in models.RData
#' @param modelControl      A list of named (same as in \code{models}) lists contains additional parameters (cannot be tuning paramters)
#' 							If one needs the arguments to be evaluated when inside the inner tuning loop, it should be quoted.
#' 							For example,  to use a balanced randomForest, one should would specify it like \code{sampsize = quote(rep(min(table(y)), 2))},
#' 							where the y will be evaluated in the resampling loop, instead of being evaluated in the environment where mpTune is called
#' @param preProcess        A preProcess object from \code{caret}
#' @param gridLength        Grid length if using grid search for hyper-parameters (tuning parameters)
#' @param randomizedLength  Number of hyper-parameter configuration if randomizedSearch is available for models
#' @param mpTnControl       A list generated by function \code{\link{mpTuneControl}} 
#' @param loopingRule       A function that actually does the iteration, looked like function(executeFunciton, loopList, ...), similar to base::Map.
#'                          One can use it to pass a customized parallel method. Built-in choice are foreachLoop (default), mclapplyLoop, mclapplyBatchLoop,
#'                          parLapplyLoop and parLapplyLBLoop (balanced load parallel::paraLapply), parLapplyBLBatchLoop, and the non-paralled lapplyLoop.
#'                          See ?loopingRule for detail on these functions
#' @param verbose           Should fitting message be displayed if any
#' @param ...               For mpTune.formula, this is the arguments passed to muTune.matrix
#'                          For mpTune.mpTune, it is 'modelControl', 'verbose', 'test'
#' 
#' @return a list with class 'mpTune' or 'mpTune.formula' (inherits of 'mpTune') containing the following entries
#' 		\itemize{
#'         \item{allModelsPerformance}{a list of lists (length = number of models) of all tried model-parameter combinations, not ranked}
#'         \item{allCVs}{a list of lists of all tried model-parameter-fold combinations, not ranked}
#'         \item{sampleIndex}{a list of cross validation folds. Each fold is a validation or test set, and its complementary set is the training}
#'         \item{data}{list(x, y, weights) or list(formula, data, weights) mpTnControl$returnData if TRUE (default)}
#'         \item{performanceMetric}{performance matrix used, which is specified in mpTune$summaryFunction}
#'         \item{config}{a list of
#'             \itemize{
#'                 \item{sampleIndex}{a list of resample used for tuning}
#'                 \item{models}{a list of models as specified}
#'                 \item{modelControl}{a list of further arguments as specifed}
#'                 \item{mpTnControl}{a 'mpTuneControl' object as specifed}
#'                 \item{preProcess}{a 'preProcess' object as specified}
#'                 \item{gridLength}{a vector of gridLength as specifed}
#'                 \item{randomizedLength}{a vector of randomized research as specifed}
#'                 }
#'             }
#'         }
#'
#' @examples
#' \dontrun{
#'
#' if (require(doMC) && detectCores() > 2) {
#'     registerDoMC(cores = detectCores());
#'     }
#' 
#' if (require(mlbench)) {
#'     data(Sonar, package = 'mlbench');
#'     inTraining <- sample(1:nrow(Sonar), floor(nrow(Sonar)*0.6), replace = TRUE); 
#'     training   <- Sonar[inTraining, ];
#'     testing    <- Sonar[-inTraining, ];
#' 
#'     sonarTuned <- mpTune(
#'         formula = Class ~. ,
#'         data = training,
#'         models =  list(balancedRF = 'rf', rf = 'rf', 'gbm'), 
#'         mpTnControl = mpTuneControl(
#'             samplingFunction = createCVFolds, nfold = 3, repeats = 1, 
#'             stratify = TRUE, classProbs = TRUE,
#'             summaryFunction = requireSummary(metric = c('AUC', 'BAC', 'Kappa'))),
#'         gridLength = 3, 
#'         randomizedLength = 3, 
#'         modelControl = list(
#'             gbm = list(verbose = FALSE),
#'             balancedRF = list(ntree = 100, sampsize = quote(rep(min(table(y)), 2)))
#'             )
#'         );
#' 
#'     print(sonarTuned);
#'     print(summary(sonarTuned));
#' 
#'     # tune more model
#'     sonarTuned <- more(sonarTuned, models = 'glmnet');
#'      
#'     # Now sonarTuned contains tuning information of four models: balancedRF, rf, gbm and glmnet
#'     # fit the model giving the best 'AUC' 
#'     bestModel <- fit(sonarTuned, metric = 'AUC')
#'     print(bestModel);
#' 
#'     # predict on hold out sample
#'     # sonarTestPred <- predict(bestModel, newdata = testing);
#' 
#'     # perform a cross validation for a fair performance estimate cosidering multiple model tunings and selections
#'     sonarTunedPerf <- resample(sonarTuned, nfold = 3, repeats = 1, stratify = TRUE);
#'     print(sonarTunedPerf);
#'     }
#' 
#' ## 
#' ## Survival analysis
#' ##
#' 
#' # check what models are avaible for right censored survival data
#' print(getDefaultModel(type = 'survival'))
#' 
#' if (require(randomForestSRC)) {
#'     data(pbc, package = 'randomForestSRC');
#'     pbc <- na.omit(pbc);
#'     pbc <- pbc[sample(nrow(pbc), 100), ];
#' 
#'     survTune <- mpTune(
#'         Surv(days, status) ~., 
#'         data = pbc,
#'         models = list(
#'             Cox = 'coxph',
#'             elasticnet = 'glmnet',
#'             gbm = 'gbm',
#'             survivalForest = 'rfsrc',
#'             boostedSCI = 'glmboost'
#'             ),
#'         mpTnControl = mpTuneControl(
#'             samplingFunction = createCVFolds,nfold = 3, repeats = 1, 
#'             stratify = TRUE, summaryFunction = survivalSummary),
#'         modelControl = list(
#'             boostedSCI = list(family = SCI()),
#'             gbm = list(verbose = FALSE)
#'             ),
#'         gridLength = 2, 
#'         randomizedLength = 3
#'         );
#'
#'     print(survTune); 
#'     summary(survTune, metric = 'C-index');
#'     }
#' 
#' }
#'
#' @export
mpTune <- function(x, ...) UseMethod('mpTune');


#' @rdname mpTune
#' @export mpTune.formula
mpTune.formula <- function(formula, data, weights = NULL, ...) {
	formula <- stats::formula(terms(formula, data = data));
	mf <- model.frame(formula, data);
	x <- model.matrix(formula, data);
	y <- model.response(mf);
	xint <- match("(Intercept)", colnames(x), nomatch = 0);
	if (xint > 0) x <- x[, -xint, drop = FALSE];
	rm(mf);
	out <- mpTune.default(x, y, weights, ...);
	if(out$config$mpTnControl$returnData) {
		out$data <- list(formula = formula, data = data)
		}
	out$formula <- formula;
	class(out) <- c('mpTune.formula', class(out));
	return(out);
	}


#' @rdname mpTune
#' @export mpTune.default
mpTune.default <- function(
	x, y, 
	weights = NULL,
	models = list('rf', 'gbm'), 
	modelControl = list(),
	preProcess = NULL,
	gridLength = 5, 
	randomizedLength = 20, 
	mpTnControl = mpTuneControl(), 
	loopingRule = foreachLoop,
	verbose = TRUE, ...
	) {

	if(is.numeric(models)) {
		models <- getDefaultModel(models, y);
		}

	N.samples <- if(is.matrix(y) || is.data.frame(y)) nrow(y) else length(y);
	N.models <- length(models);
	gridLength <- rep(gridLength, length = N.models);
	randomizedLength <- rep(randomizedLength, length = N.models);
	lev <- if (is.factor(y)) levels(y) else NULL;

	perf.proto <- mpTnControl$summaryFunction(
		data.frame(obs = y, pred = if(is.Surv(y)) y[, 1] else y)
		);
	performance.names <- names(perf.proto); 

	internalModels <- !sapply(models, is.list, USE.NAMES = FALSE);
	if (is.null(names(models))) {
		namedModels <- rep(FALSE, length(models));
	} else {
		namedModels <- !sapply(names(models), `==`, '', USE.NAMES = FALSE);
		}
	if (!all(namedModels[!internalModels])) {
		stop("Customized models musted be named in 'models'!");
		}
	names(models)[!namedModels] <- models[!namedModels];

	modelInfo <- models;
	modelInfo[internalModels] <- sapply(models[internalModels], getModelInfo, regex = FALSE);
	models.input <- models;
	models <- names(models);

	models.tuneGrids <- makeTuneGrid(x, y, modelInfo, gridLength, randomizedLength);

	# create cv folds if not exist and check names
	if (!is.null(mpTnControl$sampleIndex)) {
		sampleIndex <- mpTnControl$sampleIndex;
	} else {
		sampleIndex <- mpTnControl$samplingFunction(x, y);
		}

	`%op%` <- if (mpTnControl$allowParallel) `%dopar%` else `%do%`;

	loopList <- makeLoops(
		modelInfo = modelInfo, modelControl = modelControl, 
		models.tuneGrids = models.tuneGrids, sampleIndex = sampleIndex
		);
	models.perf <- loopingRule(
		loopList, executeTask,
		x = x, y = y, weights = weights, 
		lev = lev, mpTnControl = mpTnControl, 
		preProcess = preProcess, perf.proto = perf.proto
		);
	modelLoopList <- sub('^(.*?)_\\._\\..*$', '\\1', names(models.perf));
	modelLoopList <- factor(modelLoopList, models);
	models.perf <- lapply(
		X = split(models.perf, modelLoopList), 
		FUN = function(z) {
			o <- do.call(rbind, z);
			rownames(o) <- NULL;
			return(o);
			}
		);
	# average over resamples
	models.perf.reduced <- tryCatch(
		expr = {
			models.perf.reduced <- reducePerformance(
				models.perf, models.tuneGrids, modelInfo
				);
			}, 
		error = function(e) {print(e); NULL}
		);

	return(structure(
		list(
			allModelsPerformance = models.perf.reduced,
			allCVs               = models.perf,
			data                 = if(mpTnControl$returnData) list(x = x, y = y, weights = weights) else NULL,
			performanceMetric    = names(perf.proto),
			config = list(
				sampleIndex      = sampleIndex,
				models           = models.input,
				modelControl     = modelControl,
				mpTnControl      = mpTnControl,
				preProcess       = preProcess,
				gridLength       = gridLength,
				randomizedLength = randomizedLength
				)
			), 
		class = 'mpTune'
		));
	}
