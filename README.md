---
title: "OpenCancer package: colon cancer incidence"
author: "Lino Galiana"
date: "30 novembre 2017"
output:
  html_document:
    keep_md: true
---



`OpenCancer` package installation might fail when `CARET` is not already installed in the computer. As it is stated in (CARET documentation)[https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf], to install it, you should run


```r
install.packages("caret", dependencies = c("Depends", "Suggests"))
```


```r
install.packages("devtools")
devtools::install_github("linogaliana/OpenCancer")
```

`OpenCancer` package has been designed to help anyone wanting to work on cancer data to build a dataset. As an example, we use colon cancer data. However, functions are general enough to be applied to any similar data. One of the main challenges of the `Epidemium` dataset is that it requires high-dimensional statistical techniques. `OpenCancer` package allows to

* Easily import and merge [Epidemium](http://qa.epidemium.cc/data/epidemiology_dataset/) datasets
* Build a clean training table
* Perform feature selection
* Apply statistical models on selected features

Vignettes have been written to help any users working with `OpenCancer`:

* [Import Epidemium data and build training table](/vignettes/import_data.Rmd)
* [Use pointers to build statistical models](/vignettes/estimation_pointers.Rmd)

It might be hard to work with Epidemium data because they require lots of RAM when working with R. It is a challenge to take advantage of the statistical power of R packages without being limited by the R memory handling system. Many functions of the `OpenCancer`, relying on the `bigmemory` package, implement memory efficient techniques based on C++ pointer.

`OpenCancer` package has been designed such that it is possible to work with pointers (`big.*` functions) or apply equivalent functions when working with standard dataframes (same functions names without `big.*` prefix). In this tutorial, we will use pointers since it is less standard and might require some explanations.

# Importing Epidemium data using pointers

You can find [here](/vignettes/import_data.Rmd) a `Vignette` describing how to create the clean training table stored as a `csv`. After installing `OpenCancer` package, we import training data using pointers


```r
library(OpenCancer)
datadir <- paste0(getwd(),"/vignettes/inst")

X <- bigmemory::read.big.matrix(paste0(datadir,"/exampledf.csv"), header = TRUE)
```

This `markdown` presents a standard methodology :

* Feature selection using LASSO
* Linear regression on selected features

`Epidemium` data are multi-level panel data. An individual unit is defined by a series of variables that are overlapping levels (country, region, sex, age levels and sometimes ethnicity). `OpenCancer` functions allow to apply this methodology on groups that are defined independently by a series of variables. Some function executions can be parallelized. The main interest of using pointers rather than dataframes is that data are never imported in memory, avoiding to sature computer's RAM. 


# Feature selection

Feature selection is performed using LASSO. Given a penalization parameter $\lambda$ and a set of $p$ explanatory variables, we want to solve the following program 
$$\widehat{\beta}\in\arg\min_{\beta\ \in\mathbb{R}^p}\ \frac{1}{2}\ \left|\right|y-X\beta\left|\right|_2^2 + \lambda ||\beta||_1 $$
using standard matrix notations. The $\lambda$ parameter is of particular importance. Its value determines the sparsity of the model: the higher $\lambda$ is, the stronger the $\ell_1$ constraint is and the more $\beta$ coefficients will be zero.

The optimal set of parameters can be selected using cross validation (`OpenCancer` package also allows not to perform cross validation but is not recommended).

`big.simplelasso` has been designed to perform LASSO using `biglasso` package (for the non-pointers version, `simplelasso` the `glmnetUtils` is used). Assume we want to use a pooled model, i.e. we do not define independent groups. In that case, the following command can be used


```r
lassomodel <- big.simplelasso(X,yvar = 'incidence', labelvar = c("cancer", "age", "sex",
  "Country_Transco", "year"),
  crossvalidation = T,
  nfolds = 10, returnplot = F)
```

where we excluded a few variables - that are labelling variables, not explanatory - from the set of covariates. The `returnplot` option, when it is set to `TRUE` will produce the following plot


```r
plot(lassomodel$model)
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The LASSO performance is the following


```r
summary(lassomodel$model)
```

```
## lasso-penalized linear regression with n=5928, p=8185
## At minimum cross-validation error (lambda=1.3028):
## -------------------------------------------------
##   Nonzero coefficients: 14
##   Cross-validation error (deviance): 7765.60
##   R-squared: 0.01
##   Signal-to-noise ratio: 0.01
##   Scale estimate (sigma): 88.123
```

In that case, we see that from 1420 variables, LASSO selects 15 variables (15 variables and a constant). If parallelization is wanted, assuming one core is let aside of computations,


```r
big.simplelasso(X,yvar = 'incidence', labelvar = c("cancer", "age", "sex",
  "Country_Transco", "year", "area.x", "area.y"), crossvalidation = T,
  nfolds = 10, returnplot = F,
  ncores = parallel::detectCores() - 1)
```

# Linear regression after feature selection

`big.simplelasso` is useful to select features. To go further, we can perform linear regression on selected variables. The `big.model.FElasso` allows to launch a `big.simplelasso` routine, extract non-zero coefficients and use, afterwards, linear regression. 


```r
pooledOLS <- big.model.FElasso(X,yvar = "incidence",
                           labelvar = c("cancer", "age",
  "Country_Transco", "year", "area.x", "area.y"),
  returnplot = F, groupingvar = NULL)

summary(pooledOLS)
```

```
## Large data regression model: biglm(formula = formula, data = data, ...)
## Sample size =  5928 
##                    Coef       (95%       CI)        SE      p
## (Intercept)     43.1951 -4217.8538 4304.2439 2130.5244 0.9838
## sex              4.7179     0.1425    9.2934    2.2877 0.0392
## `6741..72040` -103.7412 -8142.3828 7934.9003 4019.3208 0.9794
## `6690..5110`    -0.0090    -0.7658    0.7477    0.3784 0.9810
## `1057..5112`     0.0001    -0.0111    0.0114    0.0056 0.9811
## `372..5510`      0.0000    -0.0007    0.0007    0.0003 0.9698
## `2600..5510`     0.0000    -0.0002    0.0002    0.0001 0.9730
## `1062..5510`     0.0001    -0.0058    0.0060    0.0029 0.9797
## `1780..5510`     0.0001    -0.0083    0.0085    0.0042 0.9779
## `51..5510`       0.0000    -0.0010    0.0010    0.0005 0.9803
## `882..5510`     -0.0001    -0.0083    0.0080    0.0041 0.9779
## `1765..5510`     0.0000    -0.0003    0.0003    0.0001 0.9805
## `358..5312`          NA         NA        NA        NA     NA
## `1770..5510`         NA         NA        NA        NA     NA
## `2738..5300`         NA         NA        NA        NA     NA
## `2738..5510`         NA         NA        NA        NA     NA
## `2848..5510`         NA         NA        NA        NA     NA
## `2948..5510`         NA         NA        NA        NA     NA
```


```r
DTsummary.biglm(pooledOLS)$coefftab
```

<!--html_preserve--><div id="htmlwidget-3d2d100447ed9681bb6b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3d2d100447ed9681bb6b">{"x":{"filter":"none","data":[["(Intercept)","sex","`6741..72040`","`6690..5110`","`1057..5112`","`372..5510`","`2600..5510`","`1062..5510`","`1780..5510`","`51..5510`","`882..5510`","`1765..5510`"],[43.1950530418599,4.71794871794853,-103.741246304305,-0.00901459894442328,0.000133257844912611,1.25653079791143e-005,4.04846387212368e-006,7.49757199621026e-005,0.000115743392962076,-1.24182467443275e-005,-0.000112719885064692,-3.54678620533849e-006],[2130.5244226746,2.28774331833179,4019.32075476665,0.378379544507429,0.00561232512482455,0.000331789623559271,0.000119797977064646,0.00294351935590655,0.0041864946270997,0.000502811819248692,0.00407176838759786,0.000145396019477949],[0.020274375915219,2.06227188170255,-0.0258106413083042,-0.0238242237860887,0.0237437856768459,0.0378713108756087,0.0337940921150867,0.0254714547100409,0.0276468509508777,-0.0246976030970851,-0.0276832752589818,-0.024393970468197],[0.983825181058499,0.039225466861619,0.979409245559338,0.98099362205773,0.981057781515869,0.969791566338748,0.973042488292237,0.979679776898339,0.977944747733813,0.980297000766796,0.977915697690905,0.980539181325082],["","*","","","","","","","","","",""]],"container":"<table class=\"hover\">\n  <thead>\n    <tr>\n      <th>Coefficient<\/th>\n      <th>Estimate<\/th>\n      <th>Std. Error<\/th>\n      <th>Student t-stat<\/th>\n      <th>Pr(&gt;|t|)<\/th>\n      <th><\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":20,"crosstalkOptions":{"key":null,"group":null},"columnDefs":[{"className":"dt-right","targets":[1,2,3,4]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[10,20,25,50,100],"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 4, 3, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 3, 1, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 2, 2, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 1, 4, 3, ',', '.');\n}"},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->

```r
DTsummary.biglm(pooledOLS)$modeltab
```

<!--html_preserve--><div id="htmlwidget-9a9d81f42692d140ffc3" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9a9d81f42692d140ffc3">{"x":{"filter":"none","data":[["Nombre Observations","Nombre Paramètres","R2","R2 ajusté","F-stat","Degré Liberté"],[5928,18,0.112179746619817,0.109475267932925,43.926638104355,5910]],"container":"<table class=\"hover\">\n  <thead>\n    <tr>\n      <th>Statistique<\/th>\n      <th>Valeur<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":20,"crosstalkOptions":{"key":null,"group":null},"columnDefs":[{"className":"dt-right","targets":1}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[10,20,25,50,100],"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 1, 3, 3, ',', '.');\n}"},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->

# Apply same methodology with independent groups

`big.model.FElasso` allows to apply the same methodology with dataframes splitted by groups. 
For instance, defining independent groups by sex and age class


```r
panelOLS <- big.model.FElasso(X,yvar = "incidence",
                              groupingvar = c('sex','age'),
                              labelvar = c('year','Country_Transco')
)

DTsummary.biglm(panelOLS[[2]])
```

# Importing data after feature selection

Once features have been selected, using pointers is no longer so relevant since the dataset with a few columns not being so large. It is thus possible, once features have been selected, to import data with selected features back in the memory. `recover_data` has been designed for that. Taking a pointer as input, it performs LASSO to select feature and imports relevant variables back in the memory as a `tibble`. 


```r
df <- unique(recover_data(X))
DT::datatable(df[sample.int(nrow(df),10),1:7])
```

<!--html_preserve--><div id="htmlwidget-62ea05f34d5098eb1865" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-62ea05f34d5098eb1865">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10"],[1,1,2,2,2,1,2,2,2,2],[6,16,13,17,8,17,17,7,10,11],[19,19,19,19,19,19,6,19,19,19],[1975,1977,1977,1976,1979,1980,1982,1978,1982,1981],[3,526,2,86,3,11,15,15,1,13],[886800,954000,954000,926000,1060000,1053000,187000,1001000,1072000,1078000],[3812900,3811500,3811500,3806000,4083300,4126100,950000,3962300,4123000,4139200]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sex<\/th>\n      <th>age<\/th>\n      <th>Country_Transco<\/th>\n      <th>year<\/th>\n      <th>incidence<\/th>\n      <th>1057..5112<\/th>\n      <th>1062..5510<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"crosstalkOptions":{"key":null,"group":null},"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

It is henceforth possible to use standard statistical and visualization tools. For instance, assume we want to perform random forest using `CARET`


```r
train.index <- sample.int(n = nrow(df), size = 0.8*nrow(df))
trainData <- df[train.index,-which(colnames(df) == "year")]
testData  <- df[-train.index,-which(colnames(df) == "year")]

rfctrl <- trainControl(method = "cv", number = 5)
randomforest <- caret::train(incidence ~ ., data = trainData,
                             trControl = rfctrl, method = "rf")

knitr::kable(data.frame(yhat = predict(randomforest,testData),
                        y = testData$incidence)[1:10,])
```
