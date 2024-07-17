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

#' Calculate Reach Metrics for a Dataset
#'
#' This function calculates the reach metrics for a given dataset and specified columns.
#' It normalizes the specified columns, performs PCA, calculates the weights,
#' and computes the reach metrics.
#'
#' @param data A data frame containing the dataset.
#' @param columns A character vector specifying the names of the columns to be used.
#' @return A data frame with the original data and added reach metrics columns (`reach_p` and `reach_n`).
#' @import dplyr
#' @importFrom stats prcomp
#' @examples
#' \dontrun{
#' columns_to_use <- c("page_views_total", "page_video_views", "photo_views")
#' facebook_reach_week <- estimate_reach(facebook_reach_week, columns_to_use)
#' }
#' @export
estimate_reach <- function(data, columns) {

  # Create the names for the scaled columns by prepending "scaled_" to the original column names
  scaled_columns <- paste0("scaled_", columns)

  # Normalize the specified columns
  data <- data %>%
    mutate(across(all_of(columns), normalize, .names = "scaled_{col}"))

  # Perform PCA on the scaled columns
  pca_formula <- as.formula(paste("~", paste(scaled_columns, collapse = " + ")))
  pca_result <- prcomp(pca_formula, data = data, scale. = TRUE)

  # Calculate the loadings and weights
  loadings <- pca_result$rotation[, 1]
  weights <- abs(loadings) / sum(abs(loadings))

  # Calculate the reach_p and reach_n metrics
  data$reach_p <- rowSums(sweep(data[scaled_columns], 2, weights, `*`))
  data$reach_n <- rowSums(sweep(data[columns], 2, weights, `*`))

  return(data)
}
