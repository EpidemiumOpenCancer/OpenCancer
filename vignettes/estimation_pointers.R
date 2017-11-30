## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadpackage, message=FALSE,warning=FALSE----------------------------

library(OpenCancer)
datadir <- paste0(getwd(),"/inst")


## ----readbigmatrix, include = TRUE,message = F, warning=F----------------

url <- "https://github.com/linogaliana/OpenCancer/raw/master/vignettes/inst/exampledf.csv"
download.file(url,destfile = paste0(datadir,"/exampledf.csv"),mode = "wb")

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



## ---- message = F, warning=F, fig.width = 10, fig.height=7---------------

plot(pooledLASSO$model)


## ---- message = F, warning=F---------------------------------------------

groupingvar <- c('age')
indices <- bigtabulate::bigsplit(X,groupingvar, splitcol=NA_real_)
indices <- indices[5:8]

# ESTIMATE MODEL WITH PARALLELIZED GROUPS
model <- foreach(i = indices, .combine='list', .errorhandling = 'pass',
                       .packages = c("bigmemory","biglasso","biganalytics",
                                     'OpenCancer')) %do% {
                         return(
                           big.simplelasso(bigmemory::deepcopy(X, rows = i),
                                           yvar = 'incidence',
                                           labelvar = c("cancer", 'sex',
                                                        "Country_Transco", "year", "area.x", "area.y"),
                                           crossvalidation = T, nfolds = 5, returnplot = F)
                           
                         )
                       }

## ---- message = F, warning=F---------------------------------------------

x <- list()
x[[1]] <- model[[2]]
for (i in 2:length(indices)){
    eval(parse(text = paste0("x[[",i,"]] <- ",
                             "model",paste(rep("[[1]]",i-1), collapse = ""),"[[2]]")))
}
model <- x


## ---- message = F, warning=F---------------------------------------------

summary(model[[1]]$model)
summary(model[[2]]$model)
summary(model[[3]]$model)
    

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
#  DTsummary.biglm(model[[2]])$coefftab
#  DTsummary.biglm(model[[2]])$modeltab

