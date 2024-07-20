#' Read and Combine CSV Files from a S3 Folder
#'
#' This function reads all CSV files from a specified folder in a S3 bucket and combines them into one data.table.
#'
#' @param bucket A character string specifying the name of the S3 bucket.
#' @param folder A character string specifying the folder path within the bucket.
#' @return A data.table combining all the CSV files in the specified folder.
#' @examples
#' \dontrun{
#' combined_data <- fetch_csv("my-bucket", "my-folder")
#' }
#' @export
fetch_csv <- function(bucket, folder) {
  library(paws)
  library(data.table)

  # List objects in the specified folder
  objects <- s3$list_objects_v2(Bucket = bucket, Prefix = folder)

  # Check if there are any contents
  if (length(objects$Contents) == 0) {
    stop("No files found in the specified folder.")
  }

  # Filter CSV files
  csv_files <- sapply(objects$Contents, function(x) x$Key)
  csv_files <- csv_files[grepl("\\.csv$", csv_files)]

  # Initialize an empty list to store data frames
  data_list <- list()

  # Read each CSV file and store it in the list
  for (file_key in csv_files) {
    obj <- s3$get_object(Bucket = bucket, Key = file_key)
    raw_data <- rawToChar(obj$Body)
    data <- fread(raw_data)
    data_list[[file_key]] <- data
  }

  # Combine all data frames into one
  combined_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

  return(combined_data)
}
