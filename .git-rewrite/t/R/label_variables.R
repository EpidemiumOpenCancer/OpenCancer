#' Use variables labels in a regression
#'
#' @param model An object of class \code{lm}
#' or \code{big.lm}
#' @return Same object with modified variable names

label_variables <- function(model){

  codes <- import_coding()
  if (class(model) %in% c("biglm","bigglm")){
    model$names[model$names %in%
                  addq(codes$code)] <- codes$label[addq(codes$code) %in% model$names]

  } else if (class(model) %in% c('lm','glm')){
    #names(model$coefficients) <- stringr::str_replace_all(names(model$coefficients),"X.","")
    colsub <- stringr::str_split(gsub("`","",names(model$coefficients)),"_",simplify = T)
    colsub[,1] <- plyr::mapvalues(colsub[,1],
                    from = colsub[colsub[,1] %in% codes$code,1],
                    to = codes$label[codes$code %in% colsub[,1]])
    colsub[!colsub[,2] == "",1] <-
      paste0(colsub[!colsub[,2] == "",1]," (",colsub[!colsub[,2] == "",2],
           ")")
    colsub <- as.character(colsub[,1])
    names(model$coefficients) <- colsub

  }
  return(model)
}
