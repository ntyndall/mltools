#' @title Create Folds
#'
#' @export


create_folds <- function(vec, folds, percentage) {
  # Create fold data information
  return(
    list(
      FOLDS = caret::createFolds(
        y = vec,
        k = folds,
        list = TRUE,
        returnTrain = FALSE
      ),
      NUM = folds,
      PER = percentage
    )
  )
}
