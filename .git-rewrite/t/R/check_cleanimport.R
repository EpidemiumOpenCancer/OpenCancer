#' Check if dataframe import is clean
#'
#' \code{check.clean.import} imports a file
#' using \code{readr::read_csv} and \code{utils::read.csv}
#' routines and compare whether they are equal or
#' not. If they differ, a more precise look is made by
#' testing near equality (\code{base::all.equal}) and a
#' diagnosis is returned
#' @param path Directory where the csv file can be found
#' @param filename Filename with extension
#' @param training Boolean indicating whether we import
#' training dataset or other datasets (formatting differ)
#' @examples \dontrun{
#' # To run following commands, you should
#' # define a datadir path where all files are located
#' diag1 <- check.clean.import(path = datadir)
#' diag2 <- check.clean.import(path = datadir, filename = "WorldBank_Data.csv",training = F)
#' diag3 <- check.clean.import(path = datadir, filename = "Ilostat_Data.csv",training = F)
#' diag4 <- check.clean.import(path = datadir, filename = "Faostat_Data.csv",training = F)}


check.clean.import <- function(path = getwd(),filename = "training.csv",
                               training = T){

  dfreadr <- if (training)
    readr::read_csv2(paste0(path,"/",filename), col_names = F) else
      readr::read_csv(paste0(path,"/",filename))

  dfbase <- if (training) read.csv(paste0(path,"/",filename), stringsAsFactors = F,
                                   header = F, sep = ";") else
                                     read.csv(paste0(path,"/",filename), stringsAsFactors = F ,
                                              header = T, sep = ",")
  #pryr::object_size(dfreadr)
  #pryr::object_size(dfbase)
  #str(dfbase)
  #str(dfreadr)


  comparison <- compare::compareEqual(dfreadr,dfbase)$detailedResult
  if (sum(!comparison)==0) message("Comparison test succeeded: all columns are equal")

  if (sum(!comparison)>0){

    message(paste0("Number of equal columns: ",sum(comparison),
                   ". Inspecting more precisely unequal columns (",
                   sum(!comparison)," columns)"))

    # STORE IN A LIST COLUMNS TWO BY TWO
    X <- lapply(which(!comparison), function(i)
      data.frame(from.readr = dfreadr[,i], from.base = dfbase[,i]))

    # DIAGNOSE POTENTIAL MISMATCH
    diagno <- unlist(lapply(1:length(X), function(i) all.equal(X[[i]][,1],X[[i]][,2])))

    # RETURN DIAGNOSTIC
    diagno <- setNames(diagno,colnames(dfreadr)[which(!comparison)])
    return(list(comparison,diagno))
  }
}
