#' @title Check Folds
#' 
#' @export


check_folds <- function(data.set, cName, fold.info) {
  # Calculate folds
  fold.info %<>% as.list
  if (fold.info %>% length %>% `!=`(2)) {
    stop(" Fold info must contain 2 numeric elements.")
  } else {
    names(fold.info) <- c("num", "per")
  }

  # Make sure condition is met for fold numbers
  if (fold.info$num %>% `<`(fold.info$per)) {
    stop(" Total folds must be more than the subset amount.")
  }
  
  # Actually generate the logical vectors within each fold
  return(
    data.set %>% 
      `[[`(cName) %>%
      mltools::create_folds(
        folds = fold.info$num,
        percentage = fold.info$per
      )
  )
}