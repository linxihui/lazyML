#' @title parallel looping functions
#' @description
#' parallel looping functions, usually called by \code{\link{modifyFunction}} to pass parameter, for example \code{cl} in parLapplyLoop
#' @param executeFunciton An internal task execution function
#' @param loopList        An internal model list to execute over
#' @param cl              A SNOW cluster as decribed in \code{parallel::parLapplyLoop}
#' @param shuffle         Should loopList be shuffle before depatch to different compute nodes. Shuffle the loopList
#'                        gives a more balanced batch load
#' @param batchSize       Number of models to send to a cluster or core at a time
#' @param mc.cores        Number of cores / CPUs to use in each compute node
#' @param parNumber       Number of cores / CPUs to use for parallel. See \code{parallel::mclapply} for detail
#' @param preschedule     Should models depatch be done before sent to nodes or cores. See \code{parallel::mclapply} for detail
#' @param ...             Other parameter to pass to \code{executeFunciton}. Must be left in a loopingRue
#' @name loopingRule
NULL

#' @rdname loopingRule
#' @export
parLapplyBatchLoop <- function(
	executeFunciton, loopList, ...,
	cl,
	batchSize = 5,
	mc.cores = 1,
	shuffle = TRUE
	) {
	loopBatchList <- split(
		loopList, 
		sample(rep(
				   seq_along(loopList), 
				   each = batchSize, 
				   length = length(loopList)
				   ))
		);
	names(loopBatchList) <- NULL;
	out <- parLapplyLB(
		cl = cl,
		X = loopBatchList, 
		fun = function(modelBatch) {
			mclapply(
				X = modelBatch, 
				FUN = executeFunciton,
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
	executeFunciton, loopList, 
	..., 
	cl, shuffle = TRUE) {
	loopListNames <- names(loopList);
	loopList <- loopList[sample(loopListNames)];
	parLapply(
		cl = cl,
		X = loopList, 
		fun = function(model) {
	 		do.call(executeFunciton, c(model, list(...)), quote = TRUE)
			}
		)[loopListNames]
	}


#' @rdname loopingRule
#' @export
parLapplyLBLoop <- function(
	executeFunciton, loopList, 
	..., cl) {
	parLapplyLB(
		cl = cl,
		X = loopList, 
		fun = function(model) {
	 		do.call(executeFunciton, c(model, list(...)), quote = TRUE)
			}
		)
	}

#' @rdname loopingRule
#' @export
mclapplyBatchLoop <- function(
	executeFunciton, loopList, ...,
	parNumber = getOption('mc.cores'), 
	batchSize = 2,  
	preschedule = FALSE) {
	loopBatchList <- split(
		loopList, 
		sample(rep(
				   seq_along(loopList), 
				   each = batchSize, 
				   length = length(loopList)
				   ))
		);
	names(loopBatchList) <- NULL;
	out <- mclapply(
		X = loopBatchList, 
		FUN = function(modelBatch) {
			lapply(modelBatch, function(model) {
				do.call(executeFunciton, c(model, list(...)), quote = TRUE)
				})
			},
		mc.preschedule = preschedule,
		mc.cores = if(is.null(parNumber)) detectCores() - 1 else parNumber
		);
	unlist(out, FALSE)[names(loopList)]
	}

#' @rdname loopingRule
#' @export
mclapplyLoop <- function(
	executeFunciton, loopList, ...,
	parNumber = getOption('mc.cores'), 
	preschedule = FALSE) {
	mclapply(
		X = loopList, 
		FUN = function(model) {
	 		do.call(executeFunciton, c(model, list(...)), quote = TRUE)
			},
		mc.preschedule = FALSE,
		mc.cores = if(is.null(parNumber)) detectCores() - 1 else parNumber
		)
	}


#' @rdname loopingRule
#' @export
lapplyLoop <- function(
	executeFunciton, loopList, ...,
	parNumber = getOption('mc.cores'), 
	preschedule = FALSE) {
	lapply(
		X = loopList, 
		FUN = function(model) {
	 		do.call(executeFunciton, c(model, list(...)), quote = TRUE)
			}
		)
	}

#' @rdname loopingRule
#' @export
foreachLoop <- function( executeFunciton, loopList, ...) {
	# if (!getOdParRegistered()) 
	out <- foreach::foreach(model = loopList ) %dopar% {
	 	do.call(executeFunciton, c(model, list(...)), quote = TRUE)
		}
	structure(out, .Names = names(loopList))
	}
