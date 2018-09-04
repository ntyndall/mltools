#' @title Generate Artifical Neural Network
#'
#' @description A function that takes a scaled data set
#'  builds a neural network and reports on the accuracy
#'  of the built model.
#'
#' @param data.set A data set that contains scaled data
#'  and a vector of results as a column in the data frame.
#'
#' @return A neural network model built from \code{totalData}
#'  which is of class 'nn'.
#'
#' @export


gen_nn <- function(data.set, ..., cName = "res", logs = FALSE, fold.info = c(10, 7)) {
  
  # Initialise additional user input
  addInput <- list(...)
  NN <- if (addInput$NN %>% is.null %>% `!`()) addInput$NN else list()
  iNames <- if (NN %>% length %>% `>`(0)) NN %>% names else c()
  if ("THRESH" %in% iNames %>% `!`()) NN$THRESH <- 0.5
  if ("REP" %in% iNames %>% `!`()) NN$REP <- 1
  
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
  FOLD_DATA <- data.set %>% 
    `[[`(cName) %>%
    mltools::create_folds(
      folds = fold.info$num,
      percentage = fold.info$per
    )
  
  # Check what labels are available
  myClasses <- data.set %>% 
    `[[`(cName) %>% 
    as.character %>% 
    as.factor
  
  # Remove the class column (to rebuild later)
  data.set[[cName]] <- NULL
  
  # Get all feature names
  features <- data.set %>% 
    names
  
  # Get label names
  labelNames <- myClasses %>% 
    levels
  
  # Convert classes to binary
  class.type <- myClasses %>%
    nnet::class.ind()
  
  # Bind the classes and features together
  d.set <- class.type %>% 
    cbind(data.set)
  
  # Concat strings, create the formula by adding up for symbolic formula
  f <- paste0(
    labelNames %>% paste(collapse = " + "),
    " ~",
    paste(features, collapse = " + ")
  ) %>%
    stats::as.formula()
  
  # Calculate number of neurons
  neurons <- features %>%
    length %>%
    `+`(labelNames %>% length) %>%
    `/`(2) %>%
    round %>%
    `+`(1)
  
  # How many folds per test set
  foldGroupLen <- FOLD_DATA$NUM - FOLD_DATA$PER
  
  # Start logging
  if (logs) cat(" ## NN CV :")
  
  # Initialise results vector
  results <- c()
  totalStats <- list()
  bestResult <- 0
  
  # Build the neural network
  for (i in 1:(FOLD_DATA$PER + 1)) {
    
    # Print out to see the progress
    if (logs) cat("", i, "/")
    if (i == (FOLD_DATA$PER + 1) && logs) cat("\n")
    
    # Which indexes of the folds to include
    filterInds <- seq(
      from = i,
      by = 1,
      length.out = foldGroupLen
    )
    
    # Set up train and test data
    train.data <- d.set[FOLD_DATA$FOLDS[-filterInds] %>% purrr::flatten_int(), ]
    test.data <- d.set[FOLD_DATA$FOLDS[filterInds] %>% purrr::flatten_int(), ]
    
    foldint <- FOLD_DATA$FOLDS[filterInds] %>% purrr::flatten_int()
    
    # Build the neural network with split data
    if (logs) cat(' ## Building neural network ## \n')
    
    # Calculate the NN here
    nn <- tryCatch(
      expr = {
        neuralnet::neuralnet(
          formula = f,
          data = train.data,
          hidden = neurons %>% rep(2),
          rep = NN$REP,
          threshold = NN$THRESH,
          act.fct = "logistic",
          linear.output = FALSE,
          lifesign = if (logs) "full" else "none",
          stepmax = 1000000
        )
      }, 
      warning = function(w) return(NULL)
    )
    
    # If the NN couldn't converge in time then move on
    if (nn %>% is.null) next
    
    # Compute Predictions off Test Set
    predictions <- neuralnet::compute(
      x = nn,
      covariate = test.data[features]
    )
    
    # Create vectors to measure accuracy
    realVec <- predVec <- 0 %>% rep(test.data %>% nrow)
    tot <- 0
    for (j in 1:(labelNames %>% length)) {
      current <- test.data[[labelNames[j]]]
      realVec[current %>% `==`(1) %>% which] <- labelNames[j]
    }
    
    # Check the max values per row for the predictions
    netRes <- predictions$net.result
    for (j in 1:(netRes %>% nrow)) predVec[j] <- labelNames[netRes[j, ] %>% which.max]
    
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
    if (confResults[1] > bestResult) {
      bestModel <- nn
      bestResult <- confResults[1]
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
  }
  
  # Return neural network plus results
  return(
    list(
      model = bestModel,
      results = results,
      totalStats = totalStats
    )
  )
}
