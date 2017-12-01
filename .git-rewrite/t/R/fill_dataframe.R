# INTERMEDIATE FUNCTION
rep.row <- function(r, n){
  plyr::colwise(function(x) rep(x, n))(r)
}

#' Filling missing intermediate year vector
#'
#' @param df Dataframe with unbalanced year vector
#' @param year.var Column name of the year variable
#' @param label.var Columns that should be filled by
#' constant values and not by NAs
#' @return Dataframe with intermediate years added filled
#' by NAs

fill_dataframe <- function(df, year.var = 'year',
                           label.var = c('ref_area','area',
                                         'Country_Transco','Zonier')){

  years <- seq(min(df[,year.var], na.rm=T), max(df[,year.var], na.rm=T))
  years <- years[!(years %in% as.numeric(unlist(df[,year.var])))]

  if (length(years) != 0){

    # CREATE DATAFRAME TO APPEND
    dfappend <- data.frame(matrix(ncol = ncol(df),nrow = length(years)))
    colnames(dfappend)   <- colnames(df)
    dfappend[,year.var]  <- years

    # LABEL VARIABLES SHOULD BE FILLED
    if (!is.null(label.var)) dfappend[,label.var] <- rep.row(df[,label.var][1,],nrow(dfappend))

    # APPENDING DATAFRAMES
    df <- rbind(df,dfappend) %>% arrange_(year.var)

  }

  return(df)
}
