
#' Perform feature selection on pointers and
#' import data back in R memory
#'
#' \code{recover_data} has been designed to
#' easily perform feature selection by LASSO on
#' pointers and import selected features as well as
#' additional relevant variables back in the memory
#'
#' @param X A \code{big.matrix} object
#' @param yvar Name of the variable we want to
#' explain in \code{X}
#' @param labelvar Variable names of columns excluded from
#' the set of covariates in LASSO. All variables
#' included in \code{labelvar} will be added to the set of
#' selected features imported back in memory
#' @param crossvalidation Should we perform a cross-validated
#' LASSO ? \code{TRUE} or \code{FALSE}
#' @param nfolds Number of folds for cross-validation. Ignored if
#' \code{crossvalidation = FALSE}. If \code{nfolds} equals the number
#' of observations, leave-one-out cross validation is performed
#' @param returnplot Should we return a plot of the relationship
#' between quadratic risk and $\lambda$ parameter? \code{TRUE} or
#' \code{FALSE}
#' @param ncores Number of cores used for computations.
#' If \code{ncores>1}, parallel processing is used

recover_data <- function(X, yvar = "incidence",
                         labelvar = c("cancer","age", "Country_Transco", "year", "area.x","area.y"),
                         crossvalidation = T, nfolds = 10,
                         returnplot = F,
                         ncores = 1){

  # LASSO FEATURE SELECTION
  lassomodel <- big.simplelasso(X, yvar, labelvar = labelvar,
                                crossvalidation = crossvalidation, nfolds = nfolds,
                                ncores = ncores, returnplot = returnplot)

  # EXTRACT FEATURES
  FExvar <- as.character(lasso.coeff(lassomodel$coeff)$xname)
  if (sum(stringr::str_detect(FExvar,"Intercept")>0)) FExvar <- FExvar[
    !stringr::str_detect(FExvar,"Intercept")]
  #FExvar <- stringr::str_replace_all(FExvar,"^X","")
  rm(lassomodel)

  df <- tbl_df(X[,which(colnames(X) %in% c(yvar,labelvar,FExvar))])

  return(df)
}
