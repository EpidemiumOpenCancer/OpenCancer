#' Count share of missing values for all variables by group
#'
#' @param df Dataframe
#' @param groupingvar Character vector indicating variable
#' names used for grouping (e.g. country level variable)
#' @seealso \code{check.emptycolumn}
#' @return A dataframe where, for each group, the proportion
#' of missing values is returned for each column. With a
#' \code{n x p} dataframe, \code{count_NAs} returns an
#' \code{np x 3} dataframe
#' @examples \dontrun{
#' # ASSUMING YOU HAVE A df_ILO DATAFRAME IN ENVIRONMENT
#' count_NAs(df)
#' count_NAs(df, "year")}

count_NAs <- function(df, groupingvar = "Country_Transco"){

  # COMPUTING SHARE OF MISSING VALUES
  missingval <- df %>% group_by_(.dots = groupingvar) %>%
    tidyr::nest() %>%
    dplyr::mutate(p = purrr::map2(data,TRUE, check.emptycolumn)
    ) %>% arrange_(groupingvar)

  # RECOVERING VARIABLES NAMES
  missingnames <- missingval %>% group_by_(.dots = groupingvar) %>%
    do(varia = names(.$p[[1]]))

  dfNAs <- dplyr::bind_cols(missingval %>% tidyr::unnest(p),missingnames %>% tidyr::unnest(varia))
  dfNAs <- dfNAs[,-which(colnames(dfNAs) %in% paste0(groupingvar,"1"))]

  return(dfNAs)
}
