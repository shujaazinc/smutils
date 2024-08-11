#' Normalize a numeric vector
#'
#' This function normalizes a numeric vector to the range [0, 1].
#'
#' @param x A numeric vector to be normalized.
#' @param na.rm A logical value indicating whether to remove NA values before normalization. Default is FALSE.
#' @return A numeric vector normalized to the range [0, 1], maintaining the original length.
#' @examples
#' normalize(c(1, 2, 3, 4, 5))
#' normalize(c(1, 2, NA, 4, 5), na.rm = TRUE)
normalize <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x_non_na <- x[!is.na(x)]
    normalized <- (x_non_na - min(x_non_na)) / (max(x_non_na) - min(x_non_na))
    result <- rep(NA, length(x))
    result[!is.na(x)] <- normalized
    return(result)
  } else {
    return ((x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm)))
  }
}
