#' LASSO feature selection and linear regression by group
#'
#' @param df Dataframe
#' @param groupingvar Column names of variables used for group. If
#' \code{groupingvar = NULL}, observations are ungrouped() and pooled
#' estimation is performed.
#' @param nfolds Number of folds for cross validation.
#' Ignored if \code{crossvalidation = F}. If \code{crossvalidation}
#' equals the number of observations, a leave-one-out cross validation
#' is performed
#' @param include.intercept Should LASSO model include a constant?

model.FElasso <- function(df, groupingvar = NULL, crossvalidation = T,
                          nfolds = 10, include.intercept = TRUE){

  if (is.null(groupingvar)){

    # NON GROUPED-MODEL
    model <- FE.lasso(df,crossvalidation,nfolds)

  } else{

    # EXECUTE FUNCTION BY GROUP
    model <- df %>% group_by_(groupingvar) %>%
      do(modelgroup = FE.lasso(.,crossvalidation = crossvalidation,nfolds = nfolds, simplify = F,
                               include.intercept = include.intercept))

  }

  return(model)
}
