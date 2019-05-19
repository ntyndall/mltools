#' @title Proportion Folds
#'
#' @export


proportion_folds <- function(vec, folds) {
  # Proportion the split by counting class type
  classCounts <- vec %>% table

  # Get names
  cNames <- classCounts %>% names

  # Convert to numeric
  classCounts %<>% as.numeric

  # Get remainder modulo folds
  remainders <- classCounts %% folds

  # Get integer counts
  intCounts <- classCounts %/% folds

  myCounts <- lapply(
    X = 1:(cNames %>% length),
    FUN = function(x) {
      c(intCounts[x] %>% rep(folds - remainders[x]), intCounts[x] %>% `+`(1) %>% rep(remainders[x]))
    }
  )

  cTypes <- lapply(
    X = cNames,
    FUN = function(x) vec %>% `==`(x) %>% which
  )

  names(cTypes) <- cNames
  
  allFolds <- c()

  for (i in 1:folds) {
    singleCounts <- myCounts %>%
      purrr::map(i) %>%
      as.integer

    # Initialise allIndexes
    allIndexes <- c()

    # Loop over each type
    for (j in 1:(cNames %>% length)) {
      # Sample it
      indexes <- cTypes[[j]] %>% sample(size = singleCounts[j])

      # Then take the set difference
      cTypes[[j]] %<>% setdiff(indexes)

      # Combine all indexes together
      allIndexes %<>% c(indexes)
    }

    allFolds %<>% c(allIndexes %>% list)
  }

  # Append names
  names(allFolds) <- paste0("Fold", append_zeros(folds))

  # Return list back
  return(allFolds %>% lapply(sort))
}
