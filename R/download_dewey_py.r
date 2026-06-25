#' Download Files Using Deweypy
#'
#' Downloads all files from a Dewey folder using the deweypy Python package.
#' This function interfaces with the Dewey file management system to batch download
#' files from a specified folder to your local machine.
#'
#' @param api_key Character string with the API key for deweypy authentication
#' @param folder_id Character string with the Dewey folder ID or URL to download from
#' @param download_path Character string specifying where to download files.
#'   If NULL (default), uses the default directory from \code{get_download_dir()}
#' @param python_path Character string specifying the path to Python executable. 
#'   If NULL (default), will automatically search for Python on the system.
#' @param num_workers Integer specifying number of workers for multi-threaded downloads.
#'   Default is NULL (uses deweypy's default of 8). Only modify if you have specific 
#'   performance requirements; most users should leave this unchanged.
#' @param partition_key_before Character string in YYYY-MM-DD format. If specified,
#'   includes all partitions up to and including this date. Only relevant for 
#'   date-partitioned datasets. Leave NULL to download all data.
#' @param partition_key_after Character string in YYYY-MM-DD format. If specified,
#'   includes all partitions from and including this date onward. Only relevant for
#'   date-partitioned datasets. Leave NULL to download all data.
#' @param verify If \code{TRUE} (default), scan the downloaded parquet files after
#'   the download and warn about any that are corrupt (empty or missing the "PAR1"
#'   footer) — see \code{\link{verify_dewey_download}}. Pass \code{FALSE} to skip.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Locates Python executable (auto-detect or use provided path)
#'   \item Validates the folder ID/URL
#'   \item Creates download directory if it doesn't exist
#'   \item Executes deweypy's speedy-download command
#' }
#' 
#' @section Advanced Options:
#' The \code{num_workers}, \code{partition_key_before}, and \code{partition_key_after}
#' parameters are advanced options that most users won't need to modify. The default
#' settings work well for typical use cases.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage - auto-detect Python path, default download location
#' download_dewey_py(api_key = "your-api-key", folder_id = "folder123")
#' 
#' # Specify custom download path
#' download_dewey_py(
#'   api_key = "your-api-key", 
#'   folder_id = "folder123",
#'   download_path = "C:/Downloads/my-files"
#' )
#' 
#' # Advanced: Download only recent data from partitioned dataset
#' download_dewey_py(
#'   api_key = "your-api-key", 
#'   folder_id = "folder123",
#'   partition_key_after = "2024-01-01"
#' )
#' 
#' # Advanced: Adjust workers for specific performance needs
#' download_dewey_py(
#'   api_key = "your-api-key", 
#'   folder_id = "folder123",
#'   num_workers = 4
#' )
#' }
download_dewey_py <- function(api_key, 
                           folder_id, 
                           download_path = NULL, 
                           python_path = NULL,
                           num_workers = NULL,
                           partition_key_before = NULL,
                           partition_key_after = NULL,
                           verify = TRUE) {

  # If python_path is NULL, auto-detect it
  if (is.null(python_path)) {
    python_path <- find_python()
  }
  
  # Validate that python_path exists
  if (!file.exists(python_path)) {
    stop("Python executable not found at: ", python_path)
  }

  # Validate folder_id
  folder_id <- parse_url(folder_id)
  
  # Set default download path if not provided
  if (is.null(download_path)) {
    download_path <- get_download_dir(create = TRUE)
  } else {
    # If custom path provided, ensure it exists
    if (!dir.exists(download_path)) {
      dir.create(download_path, recursive = TRUE)
    }
  }
  
  # Execute deweypy download command
  status <- run_deweypy(
    python_path = python_path,
    api_key = api_key,
    download_path = download_path,
    folder_id = folder_id,
    num_workers = num_workers,
    partition_key_before = partition_key_before,
    partition_key_after = partition_key_after
  )

  # Verify the downloaded parquet files (see download_dewey() for why).
  if (verify) {
    verify_dewey_download(download_path)
  }

  invisible(status)
}