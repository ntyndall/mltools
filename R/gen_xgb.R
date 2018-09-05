#' @title Method XGBoost
#'
#' @export


gen_xgb <- function(data.set, ..., cName = "res", logs = FALSE, fold.info = c(10, 7)) {
  
  # Initialise additional user input
  addInput <- list(...)
  XGB <- if (addInput$XGB %>% is.null %>% `!`()) addInput$XGB else list()
  iNames <- if (XGB %>% length %>% `>`(0)) XGB %>% names else c()
  if ("DEPTH" %in% iNames %>% `!`()) XGB$DEPTH <- 0.5
  if ("ETA" %in% iNames %>% `!`()) XGB$ETA <- 0.2
  if ("GAMMA" %in% iNames %>% `!`()) XGB$GAMMA <- 1
  if ("ROUNDS" %in% iNames %>% `!`()) XGB$ROUNDS <- 20000
  
  # Calculate folds
  FOLD_DATA <- data.set %>%
    mltools::check_folds(
      cName = cName,
      fold.info = fold.info
    )
  
  # Initialise bestAcc
  bestAcc <- 0
  
  # Convert to integer values
  new.results <- data.set
  
  # Convert classes to factors
  new.results[[cName]] %<>% 
    as.factor
  
  # Get the actual label names
  newLabels <- new.results %>% 
    `[[`(cName) %>%
    levels
  
  # Convert factor to a numeric value
  new.results[[cName]] %<>%
    as.integer %>%
    `-`(1)
  
  # Convert new results to integer values
  boundLen <- 4
  new.results %<>% 
    mltools::scaled_to_discrete(
      boundLen = boundLen
    )
  
  # How many folds per test set
  foldGroupLen <- FOLD_DATA$NUM - FOLD_DATA$PER
  
  # Loop through all the folds
  foldInd <- 1:(FOLD_DATA$NUM)
  
  # Initialise results vectors
  results <- c()
  totalStats <- list()
  bestResult <- 0
  
  # Start logging
  cat("\n ## XG CV :")
  
  # Build the model
  for (i in 1:(FOLD_DATA$PER + 1)) {
    
    # Print out to see the progress
    cat("", i, "/")
    if (i == (FOLD_DATA$PER + 1)) cat("\n")
    
    # Which indexes of the folds to include
    filterTest <- seq(
      from = i,
      by = 1,
      length.out = foldGroupLen
    )
    
    # Set up train and test data
    train.data <- new.results[FOLD_DATA$FOLDS[-filterTest] %>% purrr::flatten_int(), ]
    test.data <- new.results[FOLD_DATA$FOLDS[filterTest] %>% purrr::flatten_int(), ]
    foldint <- FOLD_DATA$FOLDS[filterTest] %>% purrr::flatten_int()
    
    # Create labels
    trainLabels <- train.data$res
    testLabels <- test.data$res
    
    # Create sparse matrix of training data
    sparse.train <- train.data %>%
      mltools::create_sparse(
        boundLen = boundLen
      )
    
    # Create sparse test matrix
    sparse.test <- test.data %>%
      mltools::create_sparse(
        boundLen = boundLen
      )
    
    # Build xgboost model
    xgb <- xgboost::xgboost(
      data = sparse.train,
      label = trainLabels,
      max_depth = XGB$DEPTH,
      eta = XGB$ETA,
      gamma = XGB$GAMMA,
      nthread = 2,
      nrounds = XGB$ROUNDS,
      objective = "multi:softmax",
      num_class = newLabels %>% length,
      verbose = 0
    )
    
    # Make predictions
    p <- predict(xgb, sparse.test)
    realVec <- newLabels[testLabels %>% `+`(1)] %>% factor(levels = newLabels)
    predVec <- newLabels[p %>% `+`(1)] %>% factor(levels = newLabels)
    
    # Need to calculate the best score
    confResults <- realVec %>% 
      mltools::confusion_stats(
        Predicted.score = predVec
      )
    
    # Append list results on
    totalStats$totAcc %<>% c(confResults[1])
    totalStats$totAccL %<>% c(confResults[2])
    totalStats$totAccU %<>% c(confResults[3])
    totalStats$totD %<>% c(confResults[4])
    totalStats$totL %<>% c(confResults[5])
    totalStats$totW %<>% c(confResults[6])
    
    # Store the best result
    if (confResults[1] > bestAcc) {
      bestAcc <- confResults[1]
      bestXGB <- xgb
    }
    
    # Append the results on
    results %<>% c(
      list(
        actual = realVec,
        predicted = predVec,
        foldint = foldint
      ) 
      %>% list
    )
    
    # Remove large data matricies
    rm(xgb, train.data, test.data)
  }
  
  # Return XGB plus results
  return(
    list(
      model = bestXGB,
      results = results,
      totalStats = totalStats
    )
  )
}
