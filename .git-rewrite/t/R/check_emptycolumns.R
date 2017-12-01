#' Count share of missing values in a variable
#'
#' @param data Dataframe to inspect
#' @param proportion Boolean indicating whether we
#' should return proportion of missing values or a
#' boolean vector indicating whether all values are
#' missing or not
#' @return A named vector (from \code{data} colnames)
#' of missing values proportion or TRUE/FALSE vector
#' @examples
#' df <- data.frame(x1 = rnorm(10),x2 = rnorm(10))
#' df$x3 <- rep(NA,10)
#' df$x1[df$x1<0] <- NA
#' check.emptycolumn(df)
#' check.emptycolumn(df,proportion = T)

check.emptycolumn <- function(data, proportion = F){

  n <- nrow(data)
  emptycheck <- sapply(1:ncol(data), function(i) sum(is.na(data[,i])))
  emptycheck <- if (proportion) emptycheck/n else emptycheck==n

  return(setNames(emptycheck,colnames(data)))
}


check.emptyline <- function(data, proportion = F){

  n <- nrow(data)
  emptycheck <- sapply(1:ncol(data), function(i) sum(is.na(data[i,])))
  emptycheck <- if (proportion) emptycheck/n else emptycheck==n

  return(setNames(emptycheck,colnames(data)))
}

