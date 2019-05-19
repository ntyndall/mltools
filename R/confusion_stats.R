#' @title Confusion Stats
#'
#' @export


confusion_stats <- function(Actual.score, Predicted.score, positiveC = "W") {

  # Build a table of results
  resultTable <- table(Actual.score, Predicted.score)
  
  rt <- if (resultTable %>% nrow %>% `==`(2)) {
    resultTable %>% caret::confusionMatrix(positive = positiveC)
  } else {
    resultTable %>% caret::confusionMatrix()
  }

  # Refine stats here from the confusion matrix
  oSens <- if (resultTable %>% nrow %>% `>`(2)) {
    
    oSens <- rt$byClass[1:(resultTable %>% nrow), 'Sensitivity'] %>%
      as.double
  
    # Get individual class statistics
    labelStats <- oSens %>% as.list
    names(labelStats) <- "tot" %>% 
      paste0(
        rt$byClass %>% 
          row.names %>% 
          strsplit("Class: ") %>%
          purrr::map(2) %>% 
          purrr::flatten_chr()
      )
  } else {
    posSens <- rt$byClass['Sensitivity'] %>% as.double
    labelStats <- c(posSens, 1 - posSens)
    names(labelStats) <- "tot" %>%
      paste0(
        c(
          rt$positive,
          rt$table[1, ] %>% names %>% setdiff(rt$positive)
        )
      )
  }

  # Create totalStats object
  totalStats <- rt$overall[c('Accuracy', 'AccuracyLower', 'AccuracyUpper')] %>% 
    as.double %>%
    as.list
  names(totalStats) <- paste0("totAcc", c("", "L", "U"))

  # Return the statistics
  return(
    list(
      totalStats = totalStats %>% c(labelStats),
      cm = rt
    )
  )
}
