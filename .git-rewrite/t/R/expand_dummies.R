#' Transfor factor variables with a set of dummies
#'
#' \code{expand.dummies} applies, for all factor
#' variables, an expansion of the variable in wide format
#' by setting several dummy columns for all possible values of
#' the variable
#' @param df Dataframe
#' @param labelvar Factor column name that must be excluded from
#' the transformation
#' @return Dataframe with factor variables converted as a set of
#' dummy variables

expand.dummies <- function(df,labelvar = 'sex'){

  nam <- which(sapply(df, is.factor))
  if (length(which(sapply(df, is.character))) >0)
    nam <- c(nam,which(sapply(df, is.character)))
  nam <- names(nam)[!(names(nam) %in% labelvar)]
  levelsfactor <- check.factor(df,check.levels = T)
  nam <- nam[nam %in% names(levelsfactor)[as.numeric(levelsfactor)>1]
             ]

  tempX <- caret::dummyVars(as.formula(paste0("~",paste(addq(nam),collapse = "+"),
                                              '-1')), df, sep = "_"
  )
  predict(tempX,df)


}
