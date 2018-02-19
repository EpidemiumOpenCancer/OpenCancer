#' Convert all character variables and factor variables to numeric
#'
#' \code{convert.character} can be used to detect
#' and convert all character and factor vectors within a
#' dataframe to numeric. A correspondance table of all
#' performed transmutation is also returned
#'
#' @param df Dataframe
#' @return A list of two elements. The first one (\code{data}),
#' is the modified dataframe. The second one (\code{correspondance})
#' is a list of all transformation that have been applied

convert.character2 <- function(df){

  nam <- which(sapply(df, is.character)|sapply(df,is.factor))
  if (length(nam) ==0) stop("Nothing to modify")
  codesvar <- lapply(names(nam), function(i) encode.character(df,char.var = i))
  names(codesvar) <- names(nam)
  df[,nam] <- sapply(names(nam), function(i) encode.character(df,char.var = i,
                                                              codelist = F))
  return(list(data = df,correspondance = codesvar))
}
