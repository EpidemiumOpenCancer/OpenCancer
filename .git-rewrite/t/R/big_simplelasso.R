#' Perform LASSO to select best features for big.matrix objects
#'
#' \code{big.simplelasso} estimates a model using LASSO and
#' returns a sparse structure.
#' Cross-validation can be used to select best covariates
#' combination. The dataframe must be a C++ pointer
#' @param X A \code{big.matrix} object
#' @param yvar Column name for the explained variable
#' @param labelvar Additional variables providing information but
#' that should not be used as explanatory variables
#' @param crossvalidation Should cross-validation be performed? TRUE or FALSE
#' @param nfolds Number of folds for cross-validation. Ignored if
#' \code{crossvalidation = T}
#' @param ncores Number of cores to used for computation. If \code{ncores}>1,
#' parallel processing is used
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


big.simplelasso <- function(X, yvar = "incidence",
                            labelvar = c("cancer","age", "Country_Transco", "year", "area.x","area.y"),
                            crossvalidation = T, nfolds = 10,
                            returnplot = T,
                            ncores = 1){

  if (class(X) != "big.matrix") stop("X must be a 'big.matrix' object")
  if (ncores > parallel::detectCores()) warning(paste0(
    "Number of cores given in argument is superior to available cores (",
    parallel::detectCores(), " cores). This might slow down computations"
  ))

  # NECESSARY TO TRANSFER COLUMN NAMES
  options(bigmemory.allow.dimnames=TRUE)

  # SUBSETING THE MATRIX FOR LASSO
  Xvar <- deepcopy(X, cols = !(colnames(X) %in% c(yvar,labelvar)))
  y    <- deepcopy(X, cols = colnames(X) == yvar)
  colnames(Xvar) <- colnames(X)[!(colnames(X) %in% c(yvar,labelvar))]

  if (!crossvalidation){

    # NON CROSS VALIDATED MODEL
    lassomodel <- biglasso::biglasso(Xvar,y,penalty = 'lasso',ncores = ncores)
    # RETURN: MODEL, PLOT, COEFFICIENTS IN A LIST
    if (returnplot){
      return(list(model = lassomodel, plot = plot(lassomodel),coeff = coef(lassomodel)))
    } else{
      return(list(model = lassomodel, coeff = coef(lassomodel)))
    }

  } else{

    # CROSS VALIDATE LASSO
    lassomodel <- biglasso::cv.biglasso(Xvar, y[1:nrow(y)], nfolds = nfolds, ncores = ncores)
    if (returnplot){
      return(list(model = lassomodel, plot = plot(lassomodel),
                  coeff = coef(lassomodel,s = "lambda.min")))
    } else{
      return(list(model = lassomodel,
                  coeff = coef(lassomodel,s = "lambda.min")))
    }

  }
}
