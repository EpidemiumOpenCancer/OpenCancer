## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadpackage, message=FALSE,warning=FALSE----------------------------

library(OpenCancer)
datadir <- if (stringr::str_detect(getwd(),"/vignettes")) paste0(getwd(),"/inst") else paste0(getwd(),"/vignettes/inst")


## ---- include = TRUE,message = F, warning=F, eval = F--------------------
#  
#  url <- "https://github.com/EpidemiumOpenCancer/OpenCancer/raw/master/vignettes/inst/exampledf.csv"
#  download.file(url,destfile = paste0(datadir,"/exampledf.csv"))

## ----readbigmatrix, include = TRUE,message = F, warning=F, eval = T------
X <- bigmemory::read.big.matrix(paste0(datadir,"/exampledf.csv"), header = TRUE)

## ---- include = FALSE, eval=FALSE----------------------------------------
#  
#  pryr::mem_used(X)
#  

## ----bigsimplelasso, message = F, warning=F------------------------------

pooledLASSO <- big.simplelasso(X,yvar = 'incidence', labelvar = c("cancer", "age",
  "Country_Transco", "year", "area.x", "area.y"), crossvalidation = T,
  nfolds = 5, returnplot = F)
summary(pooledLASSO$model)



## ---- message = F, warning=F, fig.width = 8, fig.height=5----------------

plot(pooledLASSO$model)


## ---- message = F, warning=F---------------------------------------------

groupingvar <- c('age')
indices <- bigtabulate::bigsplit(X,groupingvar, splitcol=NA_real_)
indices <- indices[5:8]

# ESTIMATE MODEL WITH PARALLELIZED GROUPS
model <- foreach(i = indices, .combine='list',
                 .multicombine = TRUE,
                 .maxcombine = nrow(X),
                 .errorhandling = 'pass',
                       .packages = c("bigmemory","biglasso","biganalytics",
                                     'OpenCancer')) %do% {
                         return(
                           list(results = big.simplelasso(bigmemory::deepcopy(X, rows = i),
                                           yvar = 'incidence',
                                           labelvar = c("cancer", 'sex',
                                                        "Country_Transco", "year", "area.x", "area.y"),
                                           crossvalidation = T, nfolds = 5, returnplot = F),
                                indices = i
                           )
                         )
                       }

## ---- message = F, warning=F---------------------------------------------

summary(model[[1]]$results$model)
summary(model[[2]]$results$model)
summary(model[[3]]$results$model)
    

## ---- message = F, warning=F---------------------------------------------
# POOLED OLS

pooledOLS <- big.model.FElasso(X,yvar = "incidence",returnplot = F,
                               relabel = T)

DTsummary.biglm(pooledOLS)$coefftab
DTsummary.biglm(pooledOLS)$modeltab

## ---- message = F, warning=F, eval = F-----------------------------------
#  
#  model <- big.model.FElasso(X,yvar = "incidence",
#                                groupingvar = c('sex','age'),
#                                returnplot = F,
#                             relabel = T)
#  
#  DTsummary.biglm(model[[38]]$results)$coefftab
#  DTsummary.biglm(model[[38]]$results)$modeltab

