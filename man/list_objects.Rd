% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_objects.R
\name{list_objects}
\alias{list_objects}
\title{List Files in a Specified Folder within a S3 Bucket}
\usage{
list_objects(bucket_name, folder_path)
}
\arguments{
\item{bucket_name}{A character string specifying the name of the S3 bucket.}

\item{folder_path}{A character string specifying the folder path within the bucket.}
}
\value{
A list of file names in the specified folder.
}
\description{
This function lists all the files in a specified folder within a S3 bucket.
}
\examples{
\dontrun{
list_objects("my-bucket", "my-folder")
}
}
