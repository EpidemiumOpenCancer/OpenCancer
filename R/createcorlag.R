#' Transform dataframe by creating multiple lagged variables
#'
#' Given a dataframe, \code{createcorlag} returns a dataframe with lagged
#' variables whose absolute pairwise correlation with a reference variable -cancer incidence e.g.-
#' exceeds a threshold. Lag function is applied by groups. Additional variables
#' that should not be lagged can be provided.
#' @param df Dataframe whose variables must be lagged
#' @param refvar Reference variable from which to compute the cross correlation with other lagged variables
#' @param groupingvar Variables used to define group
#' @cross_cor_threshold Correlation threshold required to keep a lag
#' @param k Maximum lag that must be computed. If k>1, lags 1:k are
#' computed
#' @param labelvar Variables that should not be lagged
#' @return A dataframe with lagged observations by groups

createcorlag <- function(df, refvar="incidence", groupingvar = c("sex","age","Country_Transco"), k = 1, cross_cor_threshold=0.2,
                      labelvar = c("year", "Zonier")
){

  applylag <- function(df,xvar = "1012..5322", k){
#between orders 1 and k, we keep the lags exceeding an absolute cross-corr. level with the target
    tempdf <- lapply(1:k, function(h) if (abs(cor(lag(as.numeric(unlist(df[,xvar])),h),as.numeric(unlist(df[,refvar])),use="pairwise.complete.obs"))>cross_cor_threshold){lag(as.numeric(unlist(df[,xvar])),h)} else {c()}
    )
#we concatenate the columns and convert the data frame into a tibble
    tempdf <- dplyr::tbl_df(data.frame(do.call(cbind,tempdf)))
#we build a list with appropriate names for the lags columns exceeding an absolute cross-corr. level with the target
    lags_kept <- lapply(1:k, function(h) if (abs(cor(lag(as.numeric(unlist(df[,xvar])),h),as.numeric(unlist(df[,refvar])),use="pairwise.complete.obs"))>cross_cor_threshold){h} else {c()}
    )
#we concatenate the elements and convert the data frame into a tibble
    lags_kept <- dplyr::tbl_df(data.frame(do.call(cbind,lags_kept)))
#we give appropriate names to the corresponding columns
    colnames(tempdf) <- paste0(xvar,"_lag",lags_kept)
    return(tempdf)
  }

  applylag.all <- function(df,k){
    #we run applylag on every variable except those contained in labelvar
    tempdf <- lapply(setdiff(colnames(df),labelvar), function(y) applylag(df,y,k))
    #we convert the dataframe into a tibble
    tempdf <- dplyr::tbl_df(do.call(cbind,tempdf))
    return(tempdf)
  }

  tempdf <- df %>% group_by_(.dots = groupingvar) %>%
  #we group by groupingvar and use the function nest -which creates a tibble for each group resulting from groupingvar- in order to have isolated tibbles -whose names are data by default after nest operation- on which we can perform applylag.all easily (quicker than with a loop)
    tidyr::nest() %>%
    dplyr::mutate(x = purrr::map2(data,k,applylag.all))
 #we unnest to insert the groupingvar features again and recover the initial structure, but with new columns: one for each significant lagged variable in terms of cross corr. with the target
  tempdf <- tempdf %>% tidyr::unnest()


  return(tempdf)

}
