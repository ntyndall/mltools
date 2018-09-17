#' @title Create Folds
#'
#' @export


create_folds <- function(vec, folds, percentage, proportion = TRUE) {
  
  # How to select the data
  foldData <- if (proportion) {
    vec %>% mltools::proportion_folds(
      folds = folds
    )
  } else {
    caret::createFolds(
      y = vec,
      k = folds,
      list = TRUE,
      returnTrain = FALSE
    )
  }
  
  # Create fold data information
  return(
    list(
      FOLDS = foldData,
      NUM = folds,
      PER = percentage
    )
  )
}
