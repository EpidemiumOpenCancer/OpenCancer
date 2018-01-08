#' Determine if a variable should be of factor class
#'
#' @param data A dataframe
#' @param check.levels Boolean indicating whether we
#' want to transform dataset (\code{FALSE}) or only
#' return number of levels factor variables would have
#' if converted (\code{TRUE})
#' @return A dataframe where some columns have been converted
#' to factor class or a named vector where potential factor
#' variables are associated with number of levels they would
#' have if converted
#' @examples \dontrun{
#' check.factor(df_FAO,check.levels = T)
#' df <- check.factor(df_FAO)}

check.factor <- function(data,check.levels = F,
                         threshold = 10){

  # WHICH VARIABLES MOGHT BE FACTOR
  # factorcheck <- sapply(1:ncol(data), function(i) nrow(unique(data[,i])))
  #CAN INDUCE ERROR : Error in check.factor(data) : (list) object 
  #cannot be coerced to type #'double'
  factorcheck <- sapply(1:ncol(data), function(i) length(unlist(unique(data[,i]))))
  factorcheck <- factorcheck < threshold
  factorcheck[factorcheck < threshold & sapply(data,class) == "character"] <-
    rep(FALSE,sum(sapply(data,class) == "character"))

  # FACTOR THEM
  data[which(factorcheck)] <- lapply(data[which(factorcheck)], factor)
  if (!check.levels) return(data)

  # IF WE ONLY WANT TO CHECK LEVELS OF POTENTIAL FACTORS
  if (check.levels) return(sapply(data[which(factorcheck)], function(x) length(levels(x))))
}
