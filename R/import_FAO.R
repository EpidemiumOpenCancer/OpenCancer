#' Importing data from Epidemium
#'
#' These functions allow you to import FAO, World Bank, ILO data
#' and training data
#' from your computer
#' or directly from the internet
#' @param path The path where the file can be found (ignored if file
#' is loaded from the web)
#' @param filename Name of the file with extension (csv required)
#' @param fromURL Boolean indicating whether the file should be
#' loaded from computer (\code{fromURL = FALSE}) or internet
#' ((\code{fromURL = TRUE}))
#' @param url The url where the file can be found (ignored if \code{fromURL = FALSE})
#' @param colstokeep A sequence indicating which columns to keep. If \code{NULL}
#' the whole dataframe is returned
#' @param fill.Ethiopia Specific to \code{import_FAO}. Boolean indicating whether we
#' should fill Ethiopia data with Ethiopia PDR data
#' @return A tibble with requested data
#' @section Warning:
#' Downloading from URL can be very long, it is recommended to load from computer
#' @examples \dontrun{
#' df_FAO <- import_FAO(getwd(), colstokeep = 1:10)
#' df_WB  <- import_WB(fromURL = T, colstokeep = 1:6)
#' df_ILO <- import_ILO(paste0(getwd(),"/other"))
#' df_training <- import_training(path = datadir, colstokeep = NULL)}


import_FAO <- function(path = getwd(),filename = "Faostat_Data.csv",
                       fromURL = FALSE,
                       url = "http://qa.epidemium.cc/data/epidemiology_dataset/fao_data/Faostat_Data.csv", colstokeep = 1:10,
                       fill.Ethiopia = T
){
  if (!fromURL){

    df_FAO <- readr::read_csv(paste0(path,"/",filename))
    if (!is.null(colstokeep)) df_FAO <- df_FAO[,colstokeep]

  } else{

    df_FAO <- readr::read_csv(url)
    if (!is.null(colstokeep)) df_FAO <- df_FAO[,colstokeep]

  }

  df_FAO <- tibble::add_column(df_FAO, sourceFAO = "FAO")

  if (fill.Ethiopia){

    # WE KEEP ONLY ONE POINT FOR ETHIOPIA: WHEN TWO DATA CONFLICT, WE KEEP "Ethiopia"
    ethiopia <- df_FAO %>% filter(area %in% c("Ethiopia PDR","Ethiopia")) %>%
      arrange(year,area) %>% group_by(year) %>%
      dplyr::mutate(n = row_number(),N=n()) %>%
      filter(N==1 | (N==2 & n==1)) %>% ungroup() %>%
      dplyr::mutate(area = "Ethiopia") %>% select(-n,-N)

    # WE SUBSTITUTE IN THE INITIAL DATASET
    df_FAO <- dplyr::bind_rows(df_FAO[!(df_FAO$area %in% c("Ethiopia PDR","Ethiopia")),],
                               ethiopia)

  }


  return(df_FAO)
}

#' @rdname import_FAO

import_WB <- function(path = getwd(),filename = "WorldBank_Data.csv",
                      fromURL = FALSE,
                      url = "http://qa.epidemium.cc/data/epidemiology_dataset/world_bank_data/WorldBank_Data.csv", colstokeep = 1:10
){
  if (!fromURL){

    df_WB <- readr::read_csv(paste0(path,"/",filename))
    if (!is.null(colstokeep)) df_WB <- df_WB[,colstokeep]

  } else{

    df_WB <- readr::read_csv(url)
    if (!is.null(colstokeep)) df_WB <- df_WB[,colstokeep]

  }

  df_WB <- tibble::add_column(df_WB, sourceWB = "WB")

  return(df_WB)
}

#' @rdname import_FAO

import_ILO <- function(path = getwd(),filename = "Ilostat_Data.csv",
                       fromURL = FALSE,
                       url = "http://qa.epidemium.cc/data/epidemiology_dataset/ilo_data/Ilostat_Data.csv",
                       colstokeep = 1:10
){
  if (!fromURL){

    df_ILO <- readr::read_csv(paste0(path,"/",filename))
    if (!is.null(colstokeep)) df_ILO <- df_ILO[,colstokeep]

  } else{

    df_ILO <- readr::read_csv(url)
    if (!is.null(colstokeep)) df_ILO <- df_ILO[,colstokeep]

  }

  df_ILO <- tibble::add_column(df_ILO, sourceILO = "ILO")

  return(df_ILO)
}

#' @rdname import_FAO
#' @param cancercode Code for cancer. Default is 'C18' that is colon cancer

import_training <- function(path = getwd(),filename = "training.csv",
                            fromURL = FALSE,
                            url = "http://qa.epidemium.cc/data/incidence_dataset/2017-10-10_dataset_core/training.txt",
                            colstokeep = 1:10,
                            cancercode = "C18"
){

  if (!fromURL){

    df_training <- readr::read_csv2(paste0(path,"/",filename), col_names = F)
    if (!is.null(colstokeep)) df_training <- df_training[,colstokeep]

  } else{
    if (stringr::str_detect(url,"\\.csv$")){
      df_training <- readr::read_csv2(url)
    } else{
      df_training <- readr::read_txt(url, col_names = F)
    }
    if (!is.null(colstokeep)) df_training <- df_training[,colstokeep]
  }

  colnames(df_training) <- c("cancer","sex","age","country","region","ethnicity",
                             "year","incidence")
  df_training <- df_training %>%
    filter(stringr::str_detect(cancer, paste0("^",cancercode,"$")))

}
