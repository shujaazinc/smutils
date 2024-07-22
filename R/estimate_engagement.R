#' Calculate Reach Metrics for a Dataset
#'
#' This function calculates the reach metrics for a given dataset and specified columns.
#' It normalizes the specified columns, performs PCA, calculates the weights,
#' and computes the reach metrics.
#'
#' @param data A data frame containing the dataset.
#' @param columns A character vector specifying the names of the columns to be used.
#' @return A data frame with the original data and added reach metrics columns (`engage_p` and `engage_n`).
#' @import dplyr
#' @importFrom stats prcomp
#' @examples
#' \dontrun{
#' columns_to_use <- c("comments", "likes", "shares")
#' facebook_engaged <- estimate_engagement(facebook_data, columns_to_use)
#' }
#' @export
estimate_engagement <- function(data, columns) {

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
