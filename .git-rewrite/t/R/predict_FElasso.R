#' Prediction after a model.FElasso routine
#'
#' @param model A nested dataframe or and lm model
#' @param newdata Dataframe
#' @param grouped Boolean indicating if newdata should be
#'  grouped

predict.FElasso <- function(model,newdata, grouped = FALSE){

  if (!grouped){
    newdata <- tibble::add_column(newdata, prediction = predict(model,newdata))
  } else{
    prediction <- model %>% broom::augment(modelgroup)
    prediction <- prediction[,1:which(colnames(prediction) == '.fitted')]
    newdata <- left_join(newdata,prediction)
  }

  return(newdata)
}
