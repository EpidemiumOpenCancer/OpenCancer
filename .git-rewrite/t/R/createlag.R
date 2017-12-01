#' Transform dataframe by creating multiple lagged variables
#'
#' Given a dataframe, \code{createlag} returns a dataframe with lagged
#' variables. Lag function is applied by groups. Additional variables
#' that should not be lagged can be provided
#' @param df Dataframe whose variables must be lagged
#' @param groupingvar Variables used to define group
#' @param k Maximum lag that must be computed. If k>1, lags 1:k are
#' computed
#' @param labelvar Variables that should not be lagged
#' @return A dataframe with lagged observations by groups

createlag <- function(df,groupingvar = c("sex","age","Country_Transco"), k = 1,
                      labelvar = c("year", "Zonier")
){

  applylag <- function(df,xvar = "1012..5322", k){

    tempdf <- lapply(1:k, function(h) lag(as.numeric(unlist(df[,xvar])),h)
    )
    tempdf <- dplyr::tbl_df(data.frame(do.call(cbind,tempdf)))
    colnames(tempdf) <- paste0(xvar,"_lag",1:k)
    return(tempdf)
  }

  applylag.all <- function(df,k){
    tempdf <- lapply(colnames(df), function(y) applylag(df,y,k))
    tempdf <- dplyr::tbl_df(do.call(cbind,tempdf))
    return(tempdf)
  }

  tempdf <- df %>% group_by_(.dots = groupingvar) %>%
    tidyr::nest() %>%
    dplyr::mutate(x = purrr::map2(data,k,applylag.all))

  tempdf <- tempdf %>% tidyr::unnest()

  if (sum(paste0(labelvar,"_lag") %in% colnames(tempdf))){

    tempdf <- tempdf %>% select_(.dots = paste0("-",
                                                as.character(sapply(1:k, function(h) paste0(labelvar,"_lag",h)))
    ))
  }
  return(tempdf)

}
