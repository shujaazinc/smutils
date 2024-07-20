#' List Files in a Specified Folder within a S3 Bucket
#'
#' This function lists all the files in a specified folder within a S3 bucket.
#'
#' @param bucket_name A character string specifying the name of the S3 bucket.
#' @param folder_path A character string specifying the folder path within the bucket.
#' @return A list of file names in the specified folder.
#' @examples
#' \dontrun{
#' list_objects("my-bucket", "my-folder")
#' }
#' @export
list_objects <- function(bucket_name, folder_path) {
  library(paws)

  # Ensure folder_path ends with a slash
  if (substr(folder_path, nchar(folder_path), nchar(folder_path)) != "/") {
    folder_path <- paste0(folder_path, "/")
  }

  # List objects in the specified folder
  result <- s3$list_objects_v2(
    Bucket = bucket_name,
    Prefix = folder_path
  )

  # Extract file names from the result
  file_names <- lapply(result$Contents, function(x) x$Key)

  return(file_names)
}

