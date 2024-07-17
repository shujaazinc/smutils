#' Standardize Social Media Reach Metrics
#'
#' This function extracts common reach metrics from a given dataframe and standardizes
#' them across different social media platforms by adding columns to indicate the platform and timeline.
#'
#' @param reach_data A dataframe containing social media reach data with columns:
#' `page_name`, `year`, `week`, `reach_p`, and `reach_n`.
#' @param platform A character string indicating the platform. Default is "Facebook".
#' @param timeline A character string indicating the timeline. Default is "Week".
#'
#' @return A dataframe with selected columns and added `platform` and `timeline` columns.
#' @export
#'
#' @examples
#' \dontrun{
#' reach_data <- data.frame(
#'   page_name = c("Page1", "Page2"),
#'   year = c(2023, 2023),
#'   week = c(1, 1),
#'   reach_p = c(1000, 2000),
#'   reach_n = c(500, 1500)
#' )
#' processed_data <- standardize_reach(reach_data)
#' print(processed_data)
#' }
standardize_reach <- function(reach_data, platform = "Facebook", timeline = "Week") {
  reach_data %>%
    select(page_name, year, week, reach_p, reach_n) %>%
    mutate(
      platform = platform,
      timeline = timeline
    )
}
