#' @title parallel looping functions
#' @description
#' parallel looping functions, usually called by \code{\link{modifyFunction}} to pass parameter, for example \code{cl} in parLapplyLoop
#' @param loopList        An internal model list to execute over
#' @param executeFunction An internal task execution function
#' @param cl              A SNOW cluster as decribed in \code{parallel::parLapplyLoop}
#' @param shuffle         Should loopList be shuffle before depatch to different compute nodes. Shuffle the loopList
#'                        gives a more balanced batch load
#' @param batchSize       Number of models to send to a cluster or core at a time
#' @param mc.cores        Number of cores or CPUs to use in each compute node
#' @param ...             Other parameter to pass to \code{executeFunction}. Must be left in a loopingRue
#' @details
#' A looping rule is a \code{lapply} like function, whose first two arguments are a list and a function respectively, 
#' with \code{...} available for further argument passed to the function. The order of the first two arguments matters while names do not. 
#' Its return value must be a list. Therefore, \code{bas::lapply}, \code{parallel::mclapply}, \code{parallel::parLapply}, 
#' \code{parallel::parLapplyLB} are all valide looping rule. \code{parLapplyBatchLoop}, \code{foreachBatchLoop} here are usually used with openMPI, 
#' and send its jobs by batches, with batchSize = 3 as default. One can also execute jobs (<=batchSize) in a node with multiple cores, 
#' by setting \code{mc.cores} > 1. But since only at more \code{batchSize} jobs running on a node at a time, \code{mc.cores} must be less 
#' or equal to \code{batchSize}. It is usually disencouraged to use many cores in a single node in openMPI jobs, as it may crash a node (heavy load).  
#' To use 2 cores on one node, one can use the \code{\link{modifyFunction}}, like \code{\modifyFunction(foreachLoop, mc.cores = 2L)}.
#'
#' The default looping rule is the foreachLoop, which sends one job to one node or core at a time and execute the next one if one registered node
#' or core is available. If no backend registered, \code{foreachBatchLoop} and \code{foreachLoop} execute jobs sequentially.
#' @name loopingRule
NULL

#' @rdname loopingRule
#' @export
parLapplyBatchLoop <- function(
	loopList, executeFunction, ...,
	cl,
	batchSize = 3,
	mc.cores = getOption('mc.cores', 1L),
	shuffle = TRUE
	) {

	loopListNames <- names(loopList);
	if(shuffle) loopList <- loopList[sample(loopListNames)];
	batchNumber <- ceiling(length(loopList) / batchSize);
	loopBatchList <- split(
		loopList, 
		rep(1:batchNumber, batchSize, length = length(loopList))
		);
	names(loopBatchList) <- NULL;

	out <- parLapplyLB(
		cl = cl,
		X = loopBatchList, 
		fun = function(modelBatch) {
			mclapply(
				X = modelBatch, 
				FUN = executeFunction,
				...,
				mc.preschedule = FALSE,
				mc.cores = mc.cores
				)
			}
		);
	unlist(out, FALSE)[names(loopList)]
	}


#' @rdname loopingRule
#' @export
parLapplyLoop <- function(
	loopList, executeFunction, 
	..., 
	cl, shuffle = TRUE) {
	loopListNames <- names(loopList);
	loopList <- loopList[sample(loopListNames)];
	parLapply(
		cl = cl,
		X = loopList, 
		fun = function(model) {
	 		do.call(executeFunction, c(model, list(...)), quote = TRUE)
			}
		)[loopListNames]
	}


#' @rdname loopingRule
#' @export
parLapplyLBLoop <- function(
	loopList, executeFunction, 
	..., cl) {
	parLapplyLB(
		cl = cl,
		X = loopList, 
		fun = function(model) {
	 		do.call(executeFunction, c(model, list(...)), quote = TRUE)
			}
		)
	}


#' @rdname loopingRule
#' @export
foreachBatchLoop <- function(
	loopList, executeFunction, ...,
	batchSize = 3, 
	mc.cores = getOption('mc.cores', 1L),
	shuffle = TRUE
	) {
	loopListNames <- names(loopList);
	if (shuffle) loopList <- loopList[sample(loopListNames)];
	batchNumber <- ceiling(length(loopList) / batchSize);
	loopBatchList <- split(
		loopList, 
		rep(1:batchNumber, batchSize, length = length(loopList))
		);
	names(loopBatchList) <- NULL;
	`%op%` <- if (getDoParRegistered()) `%dopar%` else `%do%`;
	out <- foreach(modelBatch = loopBatchList, .combine = c) %op% {
		mclapply(
			X = modelBatch, 
			FUN = executeFunction,
			...,
			mc.preschedule = FALSE,
			mc.cores = mc.cores
			)
		}
	return(out[loopListNames]);
	}


#' @rdname loopingRule
#' @export
foreachLoop <- function(loopList, executeFunction, ...) {
	`%op%` <- if (getDoParRegistered()) `%dopar%` else `%do%`;
	out <- foreach(model = loopList ) %dopar% {
		executeFunction(model, ...)
		}
	structure(out, .Names = names(loopList))
	}
