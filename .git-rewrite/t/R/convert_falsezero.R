
#' Detect potential miscoding NAs
#'
#' \code{convert_fakezero} is designed to inspect
#' a dataframe and detect potential miscoded NAs as 0.
#' The suspected miscoded columns
#' are returned if \code{convert = F} while suspects are
#' converted if \code{convert = T}
#' @examples \dontrun{
#' # ASSUMING YOU HAVE A df_ILO DATAFRAME IN ENVIRONMENT
#' convert_fakezero(df)
#' convert_fakezero(df,convert=T)}

convert_fakezero <- function(df, convert = F){

  # INTERMEDIATE FUNCTION
  fake_zero <- function(y, convert = convert){

    # IF ONE OF THE FOLLOWING IS TRUE, WE WILL LOOK AT ZEROS
    crit1 <- mean(floor(y) == y,na.rm=T) # CRIT1: VARIABLE PRECISION
    crit2 <- max(y,na.rm = T)>100        # CRIT2: NUMERIC VECTOR WITH LARGE VALUE: UNLIKELY TO GET ZERO
    crit3 <- sum(y==0,na.rm = T)>1       # CRIT3: WE HAVE AT LEAST ONE ZERO
    if (TRUE %in% (crit1>0.5 | crit2) & crit3){
      if (convert){
        y <- as.numeric(unlist(y))
        y[y==0 & !is.na(y)] <- NA
        return(y)
      } else{
        return(TRUE)
      }
    } else{
      if (convert) return(y) else return(FALSE)
    }
  }
  ### END OF INTERMEDIATE FUNCTION

  # COLUMNS WHERE 0s HAVE SENSE
  colsnum <- which(sapply(df, is.numeric))

  if (convert){ #CONVERT DATAFRAME
    df[,colsnum] <- do.call(cbind,lapply(colsnum, function(i) fake_zero(df[,i],
                                                                        convert = T)))
    return(df)
  } else{ # RETURN POTENTIAL PROBLEMATIC COLUMNS
    return(
      sapply(colsnum, function(i) fake_zero(df[,i],
                                         convert = F))
    )
  }

}
