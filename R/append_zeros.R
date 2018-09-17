#' @title Append Zeros
#'
#' @export


append_zeros <- function(num) {

  # Convert straight to character
  toChar <- seq(1:num) %>% as.character

  # Get character lengths
  cLens <- toChar %>% nchar

  # See if the lengths are unique
  zerod <- if (cLens %>% unique %>% length %>%  `>`(1)) {
    sapply(
      X = cLens,
      FUN = function(x) "0" %>% rep(cLens %>% max %>% `-`(x)) %>% paste(collapse = "")
    )
  } else {
    "" %>% rep(num)
  }

  return(zerod %>% paste0(toChar))
}
