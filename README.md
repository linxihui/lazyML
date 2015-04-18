# lazyML

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
## The best model based on AUC is svmRadial , with parameter(s) and mpTune performance:
## 
##    sigma     C    AUC    BAC  Kappa  AUC SD  BAC SD Kappa SD
##  0.02245 10.39 0.9783 0.8849 0.7732 0.01081 0.06241   0.1212
## 
## No failure.
```

Note the above tuning only use one 1 level of cross validation, to simultaneously select model and its hyper-parameters.

<br>

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
##         + BAC   : 0.885 (0.062)
##         + Kappa : 0.773 (0.121)
##     - rf :
##         + mtry  : 2 [2, 60]
##         + AUC   : 0.962 (0.014)
##         + BAC   : 0.877 (0.025)
##         + Kappa : 0.757 (0.047)
##     - gbm :
##         + shrinkage         : 0.005 [0.002, 0.170]
##         + interaction.depth : 1 [1, 5]
##         + n.trees           : 5000 [50, 5000]
##         + AUC               : 0.960 (0.022)
##         + BAC               : 0.854 (0.041)
##         + Kappa             : 0.709 (0.085)
##     - nb :
##         + usekernel : TRUE {FALSE, TRUE}
##         + fL        : 0 
##         + AUC       : 0.921 (0.044)
##         + BAC       : 0.839 (0.038)
##         + Kappa     : 0.677 (0.074)
## - BAC :
##     - rf :
##         + mtry  : 7 [2, 60]
##         + AUC   : 0.961 (0.019)
##         + BAC   : 0.886 (0.036)
##         + Kappa : 0.773 (0.076)
##     - svmRadial :
##         + sigma : 0.022 [0.007, 0.022]
##         + C     : 10.387 [2.328, 10.387]
##         + AUC   : 0.978 (0.011)
##         + BAC   : 0.885 (0.062)
##         + Kappa : 0.773 (0.121)
##     - gbm :
##         + shrinkage         : 0.17 [0.002, 0.170]
##         + interaction.depth : 1 [1, 5]
##         + n.trees           : 2525 [50, 5000]
##         + AUC               : 0.953 (0.036)
##         + BAC               : 0.879 (0.048)
##         + Kappa             : 0.758 (0.098)
##     - nb :
##         + usekernel : TRUE {FALSE, TRUE}
##         + fL        : 0 
##         + AUC       : 0.921 (0.044)
##         + BAC       : 0.839 (0.038)
##         + Kappa     : 0.677 (0.074)
## - Kappa :
##     - svmRadial :
##         + sigma : 0.022 [0.007, 0.022]
##         + C     : 10.387 [2.328, 10.387]
##         + AUC   : 0.978 (0.011)
##         + BAC   : 0.885 (0.062)
##         + Kappa : 0.773 (0.121)
##     - rf :
##         + mtry  : 7 [2, 60]
##         + AUC   : 0.961 (0.019)
##         + BAC   : 0.886 (0.036)
##         + Kappa : 0.773 (0.076)
##     - gbm :
##         + shrinkage         : 0.17 [0.002, 0.170]
##         + interaction.depth : 1 [1, 5]
##         + n.trees           : 2525 [50, 5000]
##         + AUC               : 0.953 (0.036)
##         + BAC               : 0.879 (0.048)
##         + Kappa             : 0.758 (0.098)
##     - nb :
##         + usekernel : TRUE {FALSE, TRUE}
##         + fL        : 0 
##         + AUC       : 0.921 (0.044)
##         + BAC       : 0.839 (0.038)
##         + Kappa     : 0.677 (0.074)
```

Meanings of bracket symbols above: [minimum, maximum], {value 1, value 2, ..., value k}, (standard deviation).

<br>

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
## The best model based on AUC is svmRadial , with parameter(s) and mpTune performance:
## 
##    sigma     C    AUC    BAC  Kappa  AUC SD  BAC SD Kappa SD
##  0.02245 10.39 0.9783 0.8849 0.7732 0.01081 0.06241   0.1212
```

Predict on new sample


```r
sonarTestPred <- predict(bestModel, newdata = testing);
```

```
## Warning in method$predict(modelFit = modelFit, newdata = newdata,
## submodels = param): kernlab class prediction calculations failed;
## returning NAs
```
<br>

Since we have tune on a list of models, each of which has a list of hyper-parameter configurations, the performance of the best
model from the output of `mpTune` is biased (selection bias).  To account for this selection bias, we will either evaluate our
selected model on new data (usually not available), or use an outer resampling. 


```r
sonarTunedPerf <- resample(sonarTuned, nfold = 3, repeats = 1, stratify = TRUE);
sonarTunedPerf
```

```
## Resampled performance:
##                 [,1]    [,2]    [,3]
## Mean         0.93719 0.83871 0.67788
## SD           0.02726 0.04672 0.09526
## resampleSize 3.00000 3.00000 3.00000
## Mean Spearson correlation of model ranks between resamples: 
##                      AUC       BAC     Kappa
## resample consistency 0.6 0.7333333 0.7333333
```

You can also check the mean correlation of model ranking among the outer resamples.


```r
checkConsistency(sonarTunedPerf);
```

```
##                      AUC       BAC     Kappa
## resample consistency 0.6 0.7333333 0.7333333
```

<br>

## Regression

Similar to classification.

<br>

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
```

```
## Loading required namespace: mboost
```

```r
print(survTune);
```

```
## 
## The best model based on C-index is survivalForest , with parameter(s) and mpTune performance:
## 
##  mtry C-index Spearman Pearson C-index SD Spearman SD Pearson SD
##     2  0.8196   0.5631  0.5465    0.05747      0.3048     0.3333
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
##         + C-index  : 0.820 (0.057)
##         + Spearman : 0.563 (0.305)
##         + Pearson  : 0.547 (0.333)
##     - gbm :
##         + shrinkage         : 0.001 [0.001, 0.105]
##         + interaction.depth : 5 [2, 5]
##         + n.trees           : 50 [50, 5000]
##         + C-index           : 0.796 (0.080)
##         + Spearman          : 0.420 (0.380)
##         + Pearson           : 0.353 (0.311)
##     - elasticnet :
##         + alpha    : 0 [0.000, 1.000]
##         + lambda   : 0.334 [0.005, 2.495]
##         + C-index  : 0.788 (0.064)
##         + Spearman : 0.527 (0.274)
##         + Pearson  : 0.485 (0.238)
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
