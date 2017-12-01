#' Perform LASSO for feature selection and create linear
#' regression using selected features
#'
#' @param df Dataframe
#' @param crossvalidation Should we perform cross-validation
#' @param nfolds Number of folds for cross validation.
#' Ignored if \code{crossvalidation = F}. If \code{crossvalidation}
#' equals the number of observations, a leave-one-out cross validation
#' is performed
#' @param include.intercept Should LASSO model include a constant?
#' @return Linear model fitted with selected features

FE.lasso <- function(df, crossvalidation = T,
                     nfolds = 10, include.intercept = TRUE,
                     relabel = F){

  # LASSO FEATURE SELECTION
  lassomodel <- simplelasso(df,crossvalidation = crossvalidation, nfolds = nfolds,
                            include.intercept = include.intercept, simplify = simplify)

  # EXTRACT FEATURES
  FExvar <- as.character(lasso.coeff(lassomodel$coeff)$xname)
  if (sum(stringr::str_detect(FExvar,"Intercept")>0)) FExvar <- FExvar[
    !stringr::str_detect(FExvar,"Intercept")]
  FExvar <- stringr::str_replace_all(FExvar,"^X","")

  # ESTIMATE LINEAR MODEL WITH SELECTED FEATURES
  tempdf <- df[,c("incidence",FExvar)]
  lmmodel <- lm("incidence ~ .", data = tempdf)
  if (relabel) lmmodel <- label_variables(lmmodel)

  return(lmmodel)
}
