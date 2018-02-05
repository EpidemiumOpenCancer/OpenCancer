#' Perform LASSO for feature selection and create linear
#' regression using selected features for big.matrix object
#'
#' @param X A big.matrix object
#' @param yvar Name of the explained variable in \code{X}
#' @param labelvar Names of variables that should be excluded from
#' the set of covariates
#' @param groupingvar Variables that should be used to define independent
#' groups. Default to \code{NULL} means there is no groups used
#' @param crossvalidation Should we perform cross-validation
#' @param nfolds Number of folds for cross validation.
#' Ignored if \code{crossvalidation = F}. If \code{crossvalidation}
#' equals the number of observations, a leave-one-out cross validation
#' is performed
#' @param ncores The number of OpenMP threads used for parallel computing.
#' @param returnplot \code{TRUE} or \code{FALSE}. Should we return a plot of the
#' LASSO performance depending of the $\lambda$ value
#' @param relabel \code{TRUE} or \code{FALSE}. Should we present variables codes
#' (\code{FALSE}) or variables labels (\code{TRUE}) in the regressions. If \code{TRUE},
#' \code{import_label()} is used to upload data from the internet. It will fail if no
#' internet connection is available.
#' @return Linear model fitted with selected features. If \code{groupingvar} is
#' not \code{NULL}, a nested dataframe is returned with linear regressions stored by
#' groups

big.model.FElasso <- function(X, yvar = "incidence",
                              labelvar = c("cancer","age", "Country_Transco", "year", "area.x","area.y"),
                              groupingvar = NULL, crossvalidation = T,
                              nfolds = 10, ncores = 1,
                              returnplot = T,
                              relabel = FALSE){
  # CONTROL PARALLELIZATION
  if (ncores > parallel::detectCores()) warning(paste0(
    "Number of cores given in argument is superior to available cores (",
    parallel::detectCores(), " cores). This might slow down computations"
  ))

  if (is.null(groupingvar)){

    # NON-GROUPED MODEL
    model <- big.FE.lasso(X, yvar, labelvar, crossvalidation,
                          nfolds, ncores, returnplot = returnplot)

  } else{

    # DIVIDE DATAFRAME FOR PARALLELIZATION
    indices <- bigtabulate::bigsplit(X,groupingvar, splitcol=NA_real_)

    if (ncores>1){ # IF PARALLELIZATION IS REQUESTED

      # CREATE CLUSTERS
      cl <- makeCluster(ncores,outfile="")
      registerDoParallel(cl)

      # ESTIMATE MODEL WITH PARALLELIZED GROUPS
      if (relabel){
        model <- foreach(i = indices, .combine='list',
                         .multicombine = TRUE,
                         .maxcombine = nrow(X),
                         .errorhandling = 'pass',
                         .packages = c("bigmemory","biglasso","biganalytics")) %dopar% {
                           return(
                             label_variables(big.FE.lasso(bigmemory::deepcopy(X, rows = i), yvar = "incidence",
                                                          labelvar = c(labelvar, groupingvar),crossvalidation,
                                                          nfolds, ncores, returnplot = returnplot))
                           )
                         }
      } else{

        model <- foreach(i = indices, .combine='list',
                         .multicombine = TRUE,
                         .maxcombine = nrow(X),
                         .errorhandling = 'pass',
                         .packages = c("bigmemory","biglasso","biganalytics")) %dopar% {
                           return(
                             big.FE.lasso(bigmemory::deepcopy(X, rows = i), yvar = "incidence",
                                          labelvar = c(labelvar, groupingvar),crossvalidation,
                                          nfolds, ncores, returnplot = returnplot)
                           )
                         }

      }
      # STOP CLUSTERIZATION
      stopCluster(cl)

    } else{ # IF NO PARALLELIZATION

      # SEQUENTIALLY RUN BY GROUP
      if (relabel){
      model <- foreach(i = indices, .combine='list',
                       .multicombine = TRUE,
                       .maxcombine = nrow(X),
                       .errorhandling = 'pass',
                       .packages = c("bigmemory","biglasso","biganalytics")) %do% {
                         return(
                           label_variables(big.FE.lasso(bigmemory::deepcopy(X, rows = i), yvar = "incidence",
                                        labelvar = c(labelvar, groupingvar),crossvalidation,
                                        nfolds, ncores = 1, returnplot = returnplot))
                         )
                       }
      } else{

        model <- foreach(i = indices, .combine='list',
                         .multicombine = TRUE,
                         .maxcombine = nrow(X),
                         .errorhandling = 'pass',
                         .packages = c("bigmemory","biglasso","biganalytics")) %do% {
                           return(
                             big.FE.lasso(bigmemory::deepcopy(X, rows = i), yvar = "incidence",
                                          labelvar = c(labelvar, groupingvar),crossvalidation,
                                          nfolds, ncores = 1, returnplot = returnplot)
                           )
                         }

      }

    }

    # x <- list()
    # x[[1]] <- model[[2]]
    # for (i in 2:length(indices)){
    #   eval(parse(text = paste0("x[[",i,"]] <- ",
    #                            "model",paste(rep("[[1]]",i-1), collapse = ""),"[[2]]")))
    # }
    # model <- x

  }
  return(model)
}
