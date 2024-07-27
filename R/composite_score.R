#' Calculate Reach or Engagement Metrics for a given Dataset
#'
#' This function calculates the estimated reach or engagement metrics for a given dataset and specified columns.
#' It normalizes the specified columns, performs PCA, calculates the weights,
#' and computes the reach metrics.
#'
#' @param data A data frame containing the dataset.
#' @param columns A character vector specifying the names of the columns to be used.
#' @param metric A string decribing the metric being estimated. Either reach or engagement.
#' @return A data frame with the original data and added reach metrics columns (`p`, `n` and `metric`).
#' @import dplyr
#' @importFrom stats prcomp
#' @examples
#' \dontrun{
#' columns_to_use <- c("page_views_total", "page_video_views", "photo_views")
#' facebook_reach_week <- composite_score(facebook_reach_week, columns_to_use, "reach")
#' }
#' @export
composite_score <- function(data, columns, metric) {
  # Ensure metric is either "reach" or "engage"
  if (!(metric %in% c("reach", "engage"))) {
    stop("Invalid metric. Please specify either 'reach' or 'engage'.")
  }

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
  p_metric <- paste0("p_", metric)
  n_metric <- paste0("n_", metric)

  data[[p_metric]] <- rowSums(sweep(data[scaled_columns], 2, weights, `*`))
  data[[n_metric]] <- rowSums(sweep(data[columns], 2, weights, `*`))
  data$metric <- metric

  return(data)
}

