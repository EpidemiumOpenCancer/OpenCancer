# == INTERMEDIATE FUNCTION

#' Given a vector, applies LOCF transform to impute missing data and impute first or
#' last potential missing observations with resp. the first obs. available or the last 
#' obs. available depending on the fromLast argument chosen in na.locf. 
#'
#' @details Intermediate function, do not use
#' @param x a vector (with NAs)
#' @param fromLast if True, complete from last observation, see na.locf function
#' @return A vector with the same length and without NAs

completed_LOCF <- function(x,fromLast=F){
  if (!fromLast){
    x[1:pastecs::first(which(x==pastecs::first(x,na.rm=T)))] <- pastecs::first(x,na.rm=T)
    x <- na.locf(x,fromLast=fromLast)
    return(x)
  } else {
    x[(pastecs::last(which(x==pastecs::last(x,na.rm=T)))):length(x)] <- pastecs::last(x,na.rm=T)
    x <- na.locf(x,fromLast=fromLast)
    return(x)
  }
}