#' Automatic machine learning algorithms selection and hyper-parameters optimization.
#'
#' This package is built upon the caret package my Max Kuhn, with auto algorithm selection and hyper-parameter optimization.
#' 
#' The package is designed to use a single call to \code{\link{mpTune}} to get model selection and hyper-parameter optimization done at once,
#' with optional parallel choice. Other possible usage is to first use \code{\link{createCVFolds}} to create cross validation folds, and use
#' multiple calls to \code{\link{mpTune}} with different models using 'qsub', and use \code{\link{combine.mpTune}} to combine all the results.
#'
#' @docType package
#' @name lazyML
NULL



# @importFrom survival Surv is.Surv coxph survfit survreg survConcordance.fit
#' @importFrom caret checkInstall probFunction predictionFunction
#' @importFrom pROC roc.default
# @importFrom plyr ddply llply mlply laply daply maply
#' @importFrom parallel mclapply parLapply parLapplyLB
#' @import foreach
NULL
