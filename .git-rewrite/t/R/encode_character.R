#' Encode character vector with numeric values
#'
#' \code{encode.character} takes a character column of a dataframe
#' and returns it converted with numeric values. It is also possible
#' to return the correspondance between the numeric values and the
#' initial characters
#'
#' @param df Dataframe
#' @param char.var Character variable name that should be converted
#' @param codelist If \code{FALSE}, a numeric vector is returned
#' with numeric codes. If \code{TRUE}, a dataframe is returned indicating
#' the correspondance between the numeric values and the initial character
#' vector
#' @return A dataframe of two columns or a numeric vector

encode.character <- function(df,char.var = "Country_Transco",codelist = T){

  df$char.var <- as.character(unlist(df[,char.var]))
  #df <-  df %>% dplyr::mutate_(.dots = setNames(char.var,"char.var"))
  if (class(df$char.var) != "character") stop("Variable must be numeric")

  if (codelist){
    correspondance <- data.frame(value = df$char.var,
                                 code = as.numeric(as.factor(df$char.var))) %>%
      group_by(code) %>% dplyr::mutate(n = row_number()) %>%
      filter(n==1) %>% select(-n)
    return(correspondance)
  } else{
    return(as.numeric(as.factor(df$char.var)))
  }

}
