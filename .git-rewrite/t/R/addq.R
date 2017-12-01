#' Wrapper to quote name
#'
#' @param x A string
#' @return \code{x} between quotes, as it must be to be supported by regex
addq <- function(x) paste0("`", x, "`")
