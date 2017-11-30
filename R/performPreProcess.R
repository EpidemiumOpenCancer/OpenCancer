#' Preprocess data to harmonize scales
#'
#' @param X A \code{big.matrix} object
#' @param groupingvar Variables used for grouping observations
#' @param labelvar Additional variables that should not be
#' pre-processed
#' @return A \code{preProcess} object

performPreProcess <- function(X,groupingvar = c("sex","age","Country_Transco"),
                              labelvar = c("year", "Zonier")){
  options(bigmemory.allow.dimnames=TRUE)
  if (length(c(groupingvar,labelvar))>0){
    data <- deepcopy(X, cols = !(colnames(X) %in% c(groupingvar,labelvar)))
    colnames(data) <- colnames(X)[!(colnames(X) %in% c(groupingvar,labelvar))]
    preProc <- caret::preProcess(data[,])
  } else{
    preProc <- caret::preProcess(X[,])
  }

  return(preProc)
}
