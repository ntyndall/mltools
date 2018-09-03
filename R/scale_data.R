#' @title Scale Data
#'
#' @description We subtract 1 from the data frame as the data set
#'  MUST always have a trailing column that contains labelled
#'  match data with 'W / D / L' etc. Also, one feature is not
#'  permitted, so total.results must therefore have 3 or more columns.
#'
#' @param total.results A data frame with multiple columns, each one
#'  containing a feature, and a final row with labelled results
#'  i.e. 'W / D / L'.
#' @param cLabel A character vector that defines what row the classes 
#'  belong to, usually called 'res'.
#'
#' @return A list object with the following names :
#' \itemize{
#'   \item{data : The scaled form of total.results}
#'   \item{scaler : Another list object containing the scales used to create `data`}
#'  }
#'
#' @export


scale_data <- function(total.results, cLabel = "res") {
  
  # Only take complete rows of data
  total.results %<>% 
    subset(total.results %>% stats::complete.cases())
  
  # Save the data classes and remove them
  dataClasses <- total.results %>% `[[`(cLabel)
  total.results[[cLabel]] <- NULL
  
  # Create the data scales for scaling the data
  dataScales <- list(
    sMax = apply(total.results, 2, max), 
    sMin = apply(total.results, 2, min), 
    cols = total.results %>% ncol
  )

  # Actually scale the full data set
  scaled.data <- total.results %>% scale(
    center = dataScales$sMin,
    scale = dataScales$sMax - dataScales$sMin
  ) %>% 
    as.data.frame
  
  # Bind the results onto the scaled data 
  scaled.data$res <- dataClasses
  
  # Return scaled data and the scaler back
  return(
    list(
      data = scaled.data,
      scaler = dataScales
    )
  )
}
