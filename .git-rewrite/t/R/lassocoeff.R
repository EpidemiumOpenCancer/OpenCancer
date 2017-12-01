#' Recover coefficients from a LASSO model
#'
#' This function arranges the output of glmnet
#' models to easily access coefficient values
#'
#' @param model A \code{glmnet} model
#' @return A dataframe with coefficients values and
#' associated variable name by descending order
#' @section Warnings:
#' Coefficients are ranked by descending order in absolute
#' value. Confidence intervals are not returned since there
#' is no obvious distribution for parameter values in LASSO

lasso.coeff <- function(model){

  resultslasso <- data.frame(xname = rownames(model)[model@i+1], beta = model@x) %>%
    arrange(desc(abs(beta)))

  return(resultslasso)
}
