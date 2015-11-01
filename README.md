# lazyML

[![Build Status](https://api.travis-ci.org/linxihui/lazyML.png?branch=master)](https://travis-ci.org/linxihui/lazyML)
[![Coverage Status](http://codecov.io/github/linxihui/lazyML/coverage.svg?branch=master)](http://codecov.io/github/linxihui/lazyML?branch=master)

An R package aims to automatically select models and tune parameters, built upon the popular package caret.

The main function `mpTune` can tune hyper-parameters of a list of models simultaneously with parallel support.
It also has functionality to give an unbiased performance estimate of the `mpTune` procedure. 

Currently, classification, regression and survival models are supported.

# Install



```r
library(devtools);
install_github('linxihui/lazyML');
```


# Short Tutorial

## Classification


```r
library(lazyML);
set.seed(123);

data(Sonar, package = 'mlbench');
inTraining <- sample(1:nrow(Sonar), floor(nrow(Sonar)*0.6), replace = TRUE);
training   <- Sonar[inTraining, ];
testing    <- Sonar[-inTraining, ];
```
Internal models are ranked to their empirical performance and model simplicity, function
`getDefaultModel` shows the list.


```r
print(getDefaultModel(4, type = 'classification'))
```

```
## [1] "rf"        "gbm"       "svmRadial" "nb"
```

To fit the first 4 models (random forest, stochastic gradient boosting, svm with RBF kernal, naive Bayes), we simply do:


```r
library(doMC);
registerDoMC(cores = detectCores() - 1);

sonarTuned <- mpTune(
	formula = Class ~. ,
	data = training,
	models =  4, 
	mpTnControl = mpTuneControl(
		samplingFunction = createCVFolds, nfold = 3, repeats = 1,
		stratify = TRUE, classProbs = TRUE,
		summaryFunction = requireSummary(metric = c('AUC', 'BAC', 'Kappa'))),
	gridLength = 3,
	randomizedLength = 3,
	modelControl = list(
		gbm = list(verbose = FALSE),
		balancedRF = list(ntree = 100, sampsize = quote(rep(min(table(y)), 2)))
		)
	);

print(sonarTuned)
```

```
## 
## The best model based on AUC is **svmRadial**, with parameter(s) and mpTune performance:
## 
##    sigma     C    AUC    BAC  Kappa  AUC SD  BAC SD Kappa SD
##  0.02245 10.39 0.9783 0.8678 0.7403 0.01081 0.03708  0.07202
## 
## No failure.
```

Note the above tuning only use one 1 level of cross validation, to simultaneously select model and its hyper-parameters.


To see ranks of all models by metric,


```r
summary(sonarTuned)
```

```
## - AUC :
##     - svmRadial :
##         + sigma : 0.022 [0.007, 0.022]
##         + C     : 10.387 [2.328, 10.387]
##         + AUC   : 0.978 (0.011)
##         + BAC   : 0.868 (0.037)
##         + Kappa : 0.740 (0.072)
##     - rf :
##         + mtry  : 2 [2, 60]
##         + AUC   : 0.972 (0.016)
##         + BAC   : 0.902 (0.024)
##         + Kappa : 0.806 (0.049)
##     - gbm :
##         + shrinkage         : 0.002 [0.002, 0.170]
##         + interaction.depth : 5 [1, 5]
##         + n.trees           : 5000 [50, 5000]
##         + AUC               : 0.961 (0.023)
##         + BAC               : 0.877 (0.000)
##         + Kappa             : 0.757 (0.002)
##     - nb :
##         + usekernel : TRUE {FALSE, TRUE}
##         + fL        : 0 
##         + AUC       : 0.921 (0.044)
##         + BAC       : 0.839 (0.038)
##         + Kappa     : 0.677 (0.074)
## - BAC :
##     - rf :
##         + mtry  : 2 [2, 60]
##         + AUC   : 0.972 (0.016)
##         + BAC   : 0.902 (0.024)
##         + Kappa : 0.806 (0.049)
##     - gbm :
##         + shrinkage         : 0.002 [0.002, 0.170]
##         + interaction.depth : 5 [1, 5]
##         + n.trees           : 5000 [50, 5000]
##         + AUC               : 0.961 (0.023)
##         + BAC               : 0.877 (0.000)
##         + Kappa             : 0.757 (0.002)
##     - svmRadial :
##         + sigma : 0.01 [0.007, 0.022]
##         + C     : 2.328 [2.328, 10.387]
##         + AUC   : 0.957 (0.019)
##         + BAC   : 0.870 (0.050)
##         + Kappa : 0.742 (0.099)
##     - nb :
##         + usekernel : TRUE {FALSE, TRUE}
##         + fL        : 0 
##         + AUC       : 0.921 (0.044)
##         + BAC       : 0.839 (0.038)
##         + Kappa     : 0.677 (0.074)
## - Kappa :
##     - rf :
##         + mtry  : 2 [2, 60]
##         + AUC   : 0.972 (0.016)
##         + BAC   : 0.902 (0.024)
##         + Kappa : 0.806 (0.049)
##     - gbm :
##         + shrinkage         : 0.002 [0.002, 0.170]
##         + interaction.depth : 5 [1, 5]
##         + n.trees           : 5000 [50, 5000]
##         + AUC               : 0.961 (0.023)
##         + BAC               : 0.877 (0.000)
##         + Kappa             : 0.757 (0.002)
##     - svmRadial :
##         + sigma : 0.01 [0.007, 0.022]
##         + C     : 2.328 [2.328, 10.387]
##         + AUC   : 0.957 (0.019)
##         + BAC   : 0.870 (0.050)
##         + Kappa : 0.742 (0.099)
##     - nb :
##         + usekernel : TRUE {FALSE, TRUE}
##         + fL        : 0 
##         + AUC       : 0.921 (0.044)
##         + BAC       : 0.839 (0.038)
##         + Kappa     : 0.677 (0.074)
```

Meanings of bracket symbols above: [minimum, maximum], {value 1, value 2, ..., value k}, (standard deviation).


To add more models using the same resamples and performance metric


```r
sonarTuned <- more(sonarTuned, models = 'glmnet');
```

Fit the best model:


```r
bestModel <- fit(sonarTuned, metric = 'AUC')
bestModel
```

```
## 
## Model **svmRadial** is chosen, with parameter(s) tuned based on AUC:
## 
##    sigma     C    AUC    BAC  Kappa  AUC SD  BAC SD Kappa SD
##  0.02245 10.39 0.9783 0.8678 0.7403 0.01081 0.03708  0.07202
```

Predict on new sample


```r
sonarTestPred <- predict(bestModel, newdata = testing);
```

Since we have tune on a list of models, each of which has a list of hyper-parameter configurations, the performance of the best
model from the output of `mpTune` is biased (selection bias).  To account for this selection bias, we will either evaluate our
selected model on new data (usually not available), or use an outer resampling. 


```r
sonarTunedPerf <- resample(sonarTuned, nfold = 3, repeats = 1, stratify = TRUE);
sonarTunedPerf
```

```
## Resampled performance:
##                  AUC     BAC  Kappa
## Mean         0.93644 0.83038 0.6616
## SD           0.02853 0.06083 0.1227
## resampleSize 3.00000 3.00000 3.0000
## Mean Spearson correlation of model ranks between resamples: 
##                            AUC       BAC     Kappa
## resample consistency 0.7333333 0.7333333 0.8333333
```

You can also check the mean correlation of model ranking among the outer resamples.


```r
checkConsistency(sonarTunedPerf);
```

```
##                            AUC       BAC     Kappa
## resample consistency 0.7333333 0.7333333 0.8333333
```


## Regression

Similar to classification.


## Survival 


```r
data(pbc, package = 'randomForestSRC');
pbc <- na.omit(pbc);
pbc <- pbc[sample(nrow(pbc), 100), ];
```


```r
survTune <- mpTune(
	Surv(days, status) ~.,
	data = pbc,
	models = list(
		Cox = 'coxph',
		elasticnet = 'glmnet',
		gbm = 'gbm',
		survivalForest = 'rfsrc',
		boostedSCI = 'glmboost'
		),
	mpTnControl = mpTuneControl(
		samplingFunction = createCVFolds,nfold = 3, repeats = 1,
		stratify = TRUE, summaryFunction = survivalSummary),
	modelControl = list(
		boostedSCI = list(family = SCI()),
		gbm = list(verbose = FALSE)
		),
	gridLength = 2,
	randomizedLength = 3
	);

print(survTune);
```

```
## 
## The best model based on C-index is **survivalForest**, with parameter(s) and mpTune performance:
## 
##  mtry C-index Spearman Pearson C-index SD Spearman SD Pearson SD
##     2  0.8191   0.5585  0.5616    0.05234      0.2932     0.3287
## 
## No failure.
```

Check model ranks by concordance index:


```r
summary(survTune, metric = 'C-index');
```

```
## - C-index :
##     - survivalForest :
##         + mtry     : 2 [2, 17]
##         + C-index  : 0.819 (0.052)
##         + Spearman : 0.558 (0.293)
##         + Pearson  : 0.562 (0.329)
##     - elasticnet :
##         + alpha    : 0 [0.000, 1.000]
##         + lambda   : 0.334 [0.005, 2.495]
##         + C-index  : 0.788 (0.064)
##         + Spearman : 0.527 (0.274)
##         + Pearson  : 0.485 (0.238)
##     - gbm :
##         + shrinkage         : 0.026 [0.001, 0.105]
##         + interaction.depth : 2 [2, 5]
##         + n.trees           : 50 [50, 5000]
##         + C-index           : 0.787 (0.063)
##         + Spearman          : 0.359 (0.345)
##         + Pearson           : 0.367 (0.346)
##     - Cox :
##         + parameter : none 
##         + C-index   : 0.739 (0.067)
##         + Spearman  : 0.406 (0.153)
##         + Pearson   : 0.282 (0.100)
##     - boostedSCI :
##         + nu       : 0.1 [0.030, 0.100]
##         + prune    : no 
##         + mstop    : 50 [50, 500]
##         + C-index  : 0.720 (0.018)
##         + Spearman : 0.513 (0.319)
##         + Pearson  : 0.469 (0.235)
```
