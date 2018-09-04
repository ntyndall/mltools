#' @title Confusion Stats
#'
#' @export


confusion_stats <- function(Actual.score, Predicted.score) {
  # Build a table of results
  resultTable <- table(Actual.score, Predicted.score)
  rt <- caret::confusionMatrix(data = resultTable)

  # Refine stats here from the confusion matrix
  oStats <- rt$overall[c('Accuracy', 'AccuracyLower', 'AccuracyUpper')] %>% as.double
  oSens <- if (resultTable %>% nrow %>% `>`(2)) {
    rt$byClass[1:3, 'Sensitivity'] %>% as.double
  } else {
    rt$byClass['Sensitivity'] %>% as.double %>% rep(3)
  }

  # Return the statistics
  return(c(oStats, oSens))
}
