#' Perform LASSO for feature selection and create linear
#' regression using selected features using a big.matrix
#'
#' @param X A \code{big.matrix} object
#' @param yvar Column name for the explained variable
#' @param labelvar Column name for additional variables
#' @param crossvalidation Should we perform cross-validation
#' @param nfolds Number of folds for cross validation.
#' Ignored if \code{crossvalidation = F}. If \code{crossvalidation}
#' equals the number of observations, a leave-one-out cross validation
#' is performed
#' @param ncores Number of cores used for computations. If \code{ncores}>1,
#' parallel processing is used
#' @return Linear model fitted with selected features


big.FE.lasso <- function(X, yvar = "incidence",
                         labelvar = c("cancer","age", "Country_Transco", "year", "area.x","area.y"),
                         crossvalidation = T,
                         returnplot = T,
                         nfolds = 10, ncores = 1){


  # LASSO FEATURE SELECTION
  lassomodel <- big.simplelasso(X, yvar, labelvar = labelvar, crossvalidation = crossvalidation, nfolds = nfolds,
                                ncores = ncores, returnplot = returnplot)

  # EXTRACT FEATURES
  FExvar <- as.character(lasso.coeff(lassomodel$coeff)$xname)
  if (sum(stringr::str_detect(FExvar,"Intercept")>0)) FExvar <- FExvar[
    !stringr::str_detect(FExvar,"Intercept")]
  #FExvar <- stringr::str_replace_all(FExvar,"^X","")
  rm(lassomodel)
  # ESTIMATE MODEL WITH SELECTED FEATURE
  if (length(FExvar) != 0){
    lmmodel <- biganalytics::biglm.big.matrix(as.formula(paste0(yvar, " ~ ",paste(addq(FExvar),collapse = "+"))),
                                              data = X)
  } else{
    lmmodel <- "Empty regression"
  }

  return(lmmodel)
}
