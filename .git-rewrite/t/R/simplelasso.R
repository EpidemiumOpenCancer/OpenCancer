#' Perform LASSO to select best features
#'
#' \code{simplelasso} estimates a model using LASSO and
#' returns a sparse structure.
#' Cross-validation can be used to select best covariates
#' combination
#' @param df Dataframe
#' @param crossvalidation Should cross-validation be performed? TRUE or FALSE
#' @param nfolds Number of folds for cross-validation. Ignored if
#' \code{crossvalidation = T}
#' @param simplify Boolean indicating whether some factor variables should
#' be dropped
#' @return A list of three elements.
#'
#' \code{output$model} returns the model. \cr
#'
#' \code{output$plot} returns a plot. If \code{crossvalidation = F},
#' coefficients values when $\lambda$ penalization term
#' evolves is represented.
#' If \code{crossvalidation = T}, the RMSE is represented with respect to the
#' number of variables with non-zero weight \cr
#'
#' \code{output$coeff} returns the coefficient values returned by the LASSO
#' (or the coefficients of the RMSE-minimizing model if \code{crossvalidation = T})


simplelasso <- function(df, yvar = 'incidence', crossvalidation = T, nfolds = 10,
                        include.intercept = TRUE, lag.order = NULL){

  # REMOVE FACTOR OR CHARACTER THAT MAKE GLMNET MODEL CRASH
  tempdf <- df[,c(which(colnames(df) == yvar),7:ncol(df))]
  tempdf <- tempdf[, !sapply(tempdf, is.character)]
  tempdf <- tempdf[, !sapply(tempdf, is.factor)]

  # REMOVE CONSTANT VARIABLES
  tempdf <- tempdf[,apply(tempdf, 2, var, na.rm=TRUE) != 0]


  if (!crossvalidation){

    # NON CROSS VALIDATED MODEL
    lassomodel <- glmnetUtils::glmnet(incidence ~.,data = data.frame(tempdf), alpha = 1,
                                      intercept = include.intercept)
    return(list(model = lassomodel, plot = plot(lassomodel),coeff = coef(lassomodel)))

  } else{

    # CROSS VALIDATE LASSO
    lassomodel <- glmnetUtils::cv.glmnet(incidence ~.,data = data.frame(tempdf),
                                         alpha = 1, nfolds = nfolds,
                                         intercept = include.intercept)
    return(list(model = lassomodel,plot = plot(lassomodel),coeff = coef(lassomodel,s = "lambda.min")))

  }
}
