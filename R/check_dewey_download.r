#' Check whether a Dewey download is complete
#'
#' Compares the files on disk against Dewey's file manifest (the same metadata
#' \code{download_dewey()} uses) and reports any that are \strong{missing} or the
#' \strong{wrong size} — i.e. not yet fully downloaded. \code{download_dewey()}
#' already re-downloads size-mismatched files when you re-run it; this gives you
#' a clean way to decide \emph{when to stop} re-running, instead of scraping the
#' "File size did not match" messages from the console.
#'
#' @param api_key Your Dewey API key. Store in \code{.Renviron} as
#'   \code{DEWEY_API_KEY} and access with \code{Sys.getenv("DEWEY_API_KEY")}.
#' @param folder_id The Dewey folder ID or URL (same value you pass to
#'   \code{download_dewey()}).
#' @param download_path The folder you downloaded into. Searched recursively, so
#'   it works regardless of the subfolder layout. Defaults to the same location
#'   \code{download_dewey()} uses.
#' @param partition_key_after,partition_key_before Optional character strings
#'   (typically YYYY-MM-DD or YYYY-MM), the same values you pass to
#'   \code{download_dewey()}. When given, only that slice of the dataset's
#'   manifest is fetched and checked — so you can verify just the partitions you
#'   downloaded instead of comparing against the whole dataset (also faster).
#'
#' @return A tibble (invisibly) with one row per problem file and columns
#'   \code{file}, \code{expected_bytes}, \code{local_bytes} (\code{NA} if
#'   missing), and \code{status} (\code{"missing"} or \code{"size_mismatch"}).
#'   \strong{Zero rows means the download is complete.}
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("DEWEY_API_KEY")
#' path <- "dewey-downloads/veraset"
#'
#' # Re-run the download until every file is present at its expected size.
#' repeat {
#'   download_dewey(api_key, folder_id, download_path = path)
#'   if (nrow(check_dewey_download(api_key, folder_id, download_path = path)) == 0) break
#' }
#' }
#'
#' @export
check_dewey_download <- function(api_key, folder_id, download_path = get_download_dir(),
                                 partition_key_after = NULL, partition_key_before = NULL) {
  validate_partition_key(partition_key_after, "partition_key_after")
  validate_partition_key(partition_key_before, "partition_key_before")

  result <- get_dewey_urls(
    api_key, folder_id,
    partition_key_after = partition_key_after,
    partition_key_before = partition_key_before
  )
  file_names <- result$file_names
  file_sizes <- suppressWarnings(as.numeric(result$file_sizes))

  if (is.null(file_names) || is.null(result$file_sizes) ||
      length(file_names) != length(file_sizes)) {
    stop("The Dewey manifest did not include per-file sizes, so completeness ",
         "can't be checked. Update deweyr (and make sure deweypy is current).",
         call. = FALSE)
  }

  # Map every local file by its base name so we don't depend on the download's
  # subfolder layout.
  local_files <- list.files(download_path, recursive = TRUE, full.names = TRUE)
  local_by_name <- stats::setNames(local_files, basename(local_files))

  local_path  <- unname(local_by_name[file_names])
  local_bytes <- file.size(local_path)            # NA when the file is absent

  missing  <- is.na(local_bytes)
  mismatch <- !missing & !is.na(file_sizes) & local_bytes != file_sizes
  bad      <- missing | mismatch

  problems <- tibble::tibble(
    file           = file_names[bad],
    expected_bytes = file_sizes[bad],
    local_bytes    = local_bytes[bad],
    status         = ifelse(missing[bad], "missing", "size_mismatch")
  )

  n_total <- length(file_names)
  if (nrow(problems) == 0) {
    message("All ", n_total, " files present at the expected size.")
  } else {
    message(n_total - nrow(problems), "/", n_total, " files OK; ",
            nrow(problems), " missing or incomplete (re-run download_dewey() to fetch them).")
  }

  invisible(problems)
}
