big.predict.glm <- function(object, newdata, row.idx = 1:nrow(newdata),
                            type = c("link", "response",
                                     "coefficients"),
                            ...){
  type <- match.arg(type)

  if (inherits(object,'bigglm')){
    pred <- big.predict.lm(object, newdata, row.idx,
                           type = ifelse(type == "link", "response", type)
    )
    switch(type, response = {
      pred <- family(object)$linkinv(pred)
    }, link = , terms = )

  }

  return(pred)
}
