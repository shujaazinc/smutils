#' Standardize Social Media Engagement Metrics
#'
#' This function extracts common reach metrics from a given dataframe and standardizes
#' them across different social media platforms by adding columns to indicate the platform and timeline.
#'
#' @param reach_data A dataframe containing social media engagement data with columns:
#' `page_name`, `year`, `week`, `engage_p`, and `engage_n`.
#' @param platform A character string indicating the platform. Default is "Facebook".
#' @param timeline A character string indicating the timeline. Default is "Week".
#'
#' @return A dataframe with selected columns and added `platform` and `timeline` columns.
#' @export
#'
#' @examples
#' \dontrun{
#' engage_data <- data.frame(
#'   page_name = c("Page1", "Page2"),
#'   year = c(2023, 2023),
#'   week = c(1, 1),
#'   engage_p = c(1000, 2000),
#'   engage_n = c(500, 1500)
#' )
#' processed_data <- standardize_reach(reach_data)
#' print(processed_data)
#' }
standardize_engagegement <- function(engage_data, platform = "Facebook", timeline = "Week") {
  engage_data %>%
    select(page_name, year, week, engage_p, engage_n) %>%
    mutate(
      platform = platform,
      timeline = timeline
    )
}
