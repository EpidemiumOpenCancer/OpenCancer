#' Match country name with its unique key
#'
#' Associate to a country its unique identifier necessary
#' to merge dataframes
#'
#' @param df_data Dataframe with a country vector
#' @param df_keys Dataframe where countries are associated to
#' a unique key
#' @param origin In \code{df_keys}, which data source should
#' we look at?
#' @param countrycol Column name of the variable where countries
#' can be found in \code{df_data}
#'
#' @return A dataframe with unique id keys
#' @examples \dontrun{
#' codes <- readxl::read_excel(path = paste0(datadir,"/creation zonier.xls"),
#' sheet = "Transco_Country")
#' # MAPPING IDs TO THEIR VALUES
#' df_FAO = matchkeys(df_FAO,codes)
#' df_WB = matchkeys(df_WB,codes,origin = "World Bank")
#' df_ILO = matchkeys(df_ILO,codes, origin = "IloStat")}

matchkeys <- function(df_data,df_keys, origin = "FaoStat",countrycol = "area"){

  # FILTER KEYS WITH THE DATABASE WE ARE INTERESTED IN
  df_keys <- df_keys %>% filter(Source == origin)

  # HARMONIZE COLUMN NAME
  if (countrycol != "area") df_data <-
      df_data %>% dplyr::mutate_(.dots = setNames(paste0("area =",countrycol),"area"))

  # PRINT A MESSAGE
  message(paste0("Number of matching countries: ",
                 length(intersect(df_data$area,df_keys$Country_Source)
                 )))

  if (nrow(df_keys %>% filter(!(Country_Source %in% intersect(df_data$area,df_keys$Country_Source)))) == 0){
    message("Country identifiers in df_keys are all matched")
  } else{
    mismatch <- df_keys %>% filter(!(Country_Source %in% intersect(df_data$area,df_keys$Country_Source))) %>%
      unique(.$Country_Transco)
    message(paste0("Remaining keys from df_keys: ", paste(mismatch, collapse = ", ")))
  }

  # FILTER COUNTRIES: ABANDON THOSE NOT MATCHING
  tempdf_data <- df_data %>% filter((df_data$area %in% intersect(df_data$area,df_keys$Country_Source)))

  # FILTER CODES NOT MATCHING AND UNIFY COLNAMES
  tempdf_keys <- df_keys %>% filter((df_keys$Country_Source %in% intersect(df_data$area,df_keys$Country_Source))) %>%
    dplyr::mutate_(.dots = setNames(paste0(countrycol,"= Country_Source"),countrycol)) %>%
    select(-Country_Source, -Source)

  joined_data <- dplyr::left_join(tempdf_data,tempdf_keys, by = countrycol)

  return(joined_data)
}
