big.predict.lm <- function(object, newdata, row.idx = 1:nrow(newdata),
                           type = c("link", "response", "class",
                                    "coefficients"),
                           ...) {

  type <- match.arg(type)
  beta <- coef(object, drop = FALSE)
  if (type=="coefficients") return(beta)
  #  if (type %in% c("class","response")) message("For the moment, only predict.lm implemented")

  if (!inherits(object, c('biglm','bigglm'))) {
    stop("Model must be a biglm or bigglm object.")
  }

  if (inherits(newdata, 'big.matrix')) {
    message("Importing in RAM necessary vectors")
  } else{
    #stop("newdata must be a big.matrix object.")
  }

  # EXTRACT COVARIATES
  xvar <- stringr::str_replace_all(names(beta[!is.na(beta)])[-1],"`","")
  Xmatrix <- newdata[row.idx,xvar]

  # PREDICT
  pred <- if (!is.null(nrow(Xmatrix)))
    cbind(rep(1,nrow(Xmatrix)),Xmatrix) %*% beta[!is.na(beta)] else
      cbind(rep(1,length(Xmatrix)),Xmatrix) %*% beta[!is.na(beta)]

  return(pred)
}
