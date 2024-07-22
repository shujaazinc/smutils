#' Normalize a numeric vector
#'
#' This function normalizes a numeric vector to the range [0, 1].
#'
#' @param x A numeric vector to be normalized.
#' @return A numeric vector normalized to the range [0, 1].
#' @examples
#' normalize(c(1, 2, 3, 4, 5))
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
