#' Run Dewey Download Using UV
#'
#' Downloads files from a Dewey folder using uvx to run deweypy without requiring
#' a local Python installation or manual package management. This function automatically
#' handles uv installation if needed and executes the download in an isolated environment.
#'
#' @param api_key Character string with the API key for deweypy authentication
#' @param folder_id Character string with the Dewey folder ID or URL to download from
#' @param download_path Character string specifying where to download files.
#'   Default is a "dewey-downloads" folder in the current working directory.
#' @param python_version Character string specifying the Python version to use.
#'   Default is "3.13". Must be a valid Python version supported by uv.
#'   Python 3.14 is not compatible with deweypy as of this writing, so do not specify 3.14 or higher.
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
#' This function uses \href{https://docs.astral.sh/uv/}{uv} (a fast Python package installer)
#' to run deweypy without requiring you to manage Python environments manually.
#' 
#' The function performs the following steps:
#' \itemize{
#'   \item Checks if uv is installed, and installs it if needed
#'   \item Creates the download directory if it doesn't exist
#'   \item Executes deweypy's speedy-download command via uvx in an isolated environment
#' }
#' 
#' The download progress will be displayed in real-time as the function executes.
#' 
#' @section Note on UV Installation:
#' If uv needs to be installed, you may see a message recommending you restart your
#' terminal for optimal performance in future runs. The function will work without
#' restarting, but subsequent runs may be faster after a restart.
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
#' # Basic usage with default settings
#' download_dewey(
#'   api_key = "your-api-key",
#'   folder_id = "folder123"
#' )
#' 
#' # Specify custom download location
#' download_dewey(
#'   api_key = "your-api-key",
#'   folder_id = "folder123",
#'   download_path = "C:/my-data"
#' )
#' 
#' # Use a different Python version
#' download_dewey(
#'   api_key = "your-api-key",
#'   folder_id = "folder123",
#'   python_version = "3.12"
#' )
#' 
#' # Advanced: Multi-threaded download with 16 workers
#' download_dewey(
#'   api_key = "your-api-key",
#'   folder_id = "folder123",
#'   num_workers = 16
#' )
#' 
#' # Advanced: Download only partitions after a specific date
#' download_dewey(
#'   api_key = "your-api-key",
#'   folder_id = "folder123",
#'   partition_key_after = "2024-01-01"
#' )
#' }
download_dewey <- function(api_key,
                           folder_id,
                           download_path = NULL,
                           python_version = "3.13",
                           num_workers = NULL,
                           partition_key_before = NULL,
                           partition_key_after = NULL,
                           verify = TRUE) {

  # Ensure download folder exists
  if (is.null(download_path)) {
    download_path <- get_download_dir(create = TRUE)
  } else {
    # If custom path provided, ensure it exists
    if (!dir.exists(download_path)) {
      dir.create(download_path, recursive = TRUE)
    }
  }
  
  # Step 1: Check for uv
  if (!check_uv()) {
    install_uv()
    message("Restarting the terminal will increase speed of future runs")
  }

   # Validate folder_id
  folder_id <- parse_url(folder_id)
  
  # Step 2: Run Dewey download
  status <- run_deweypy_uv(
    api_key = api_key,
    download_path = download_path,
    folder_id = folder_id,
    python_version = python_version,
    num_workers = num_workers,
    partition_key_before = partition_key_before,
    partition_key_after = partition_key_after
  )

  # Step 3: Verify the downloaded parquet files. A multi-worker download can
  # silently save an HTTP error body as a .parquet when the download API hiccups
  # under load; this surfaces those files instead of letting arrow/duckdb choke
  # on them later.
  if (verify) {
    verify_dewey_download(download_path)
  }

  invisible(status)
}