#' Check a single parquet file for a valid footer
#'
#' A valid parquet file ends with the 4-byte "PAR1" magic trailer. A truncated
#' file, an empty file, or a non-parquet body (e.g. an HTTP error page saved
#' under a .parquet name) will be missing it — which is exactly what arrow
#' reports as "Parquet magic bytes not found in footer".
#'
#' @param path Path to a single file.
#' @return \code{TRUE} if the file is non-empty and ends with "PAR1", else \code{FALSE}.
#' @keywords internal
#' @noRd
parquet_footer_ok <- function(path) {
  sz <- file.size(path)
  if (is.na(sz) || sz < 8L) return(FALSE) # 0-byte / too small to be valid parquet
  con <- file(path, "rb")
  on.exit(close(con))
  seek(con, where = sz - 4L, origin = "start")
  identical(readBin(con, "raw", n = 4L), charToRaw("PAR1"))
}

#' Which file names fall within a partition-key date range
#'
#' Dewey names date-partitioned files with a leading date (e.g.
#' "2025-01-15--data_....parquet"), and \code{partition_key_*} bounds are dates
#' too. Comparing the file's leading date, truncated to the bound's length,
#' handles mixed granularity (e.g. a YYYY-MM bound against a YYYY-MM-DD file):
#' "2025-01-15" -> "2025-01" matches a bound of "2025-01". Files without a
#' leading date are excluded when a bound is supplied.
#'
#' @param file_names Character vector of file names (base names).
#' @param after,before Optional YYYY-MM or YYYY-MM-DD lower/upper bounds (inclusive).
#' @return Logical vector, \code{TRUE} for names within range.
#' @keywords internal
#' @noRd
in_partition_range <- function(file_names, after = NULL, before = NULL) {
  base <- basename(file_names)
  date <- sub("^([0-9]{4}-[0-9]{2}(-[0-9]{2})?).*$", "\\1", base)
  has_date <- grepl("^[0-9]{4}-[0-9]{2}", base)
  keep <- rep(TRUE, length(file_names))
  if (!is.null(after)) {
    keep <- keep & has_date & substr(date, 1L, nchar(after)) >= after
  }
  if (!is.null(before)) {
    keep <- keep & has_date & substr(date, 1L, nchar(before)) <= before
  }
  keep
}

#' Verify downloaded Dewey parquet files
#'
#' Scans a download folder for parquet files that are corrupt — empty, or
#' missing the trailing "PAR1" footer. This catches files that were silently
#' saved as error responses when the Dewey download API hiccups under a
#' multi-worker download burst (deweypy's \code{speedy-download} does not
#' currently validate each response before writing it to disk). Such files
#' later fail in arrow/duckdb with "Parquet magic bytes not found in footer".
#'
#' \code{download_dewey()} and \code{download_dewey_py()} run this automatically
#' after a download (pass \code{verify = FALSE} to skip). You can also call it
#' directly on any previously downloaded folder.
#'
#' @param download_path Path to the folder the data was downloaded into. Scanned
#'   recursively for \code{*.parquet} files (includes \code{*.snappy.parquet}).
#' @param delete_corrupt If \code{TRUE}, delete the corrupt files so a re-run of
#'   the download re-fetches only those (deweypy's \code{skip_existing} keeps the
#'   good ones). Defaults to \code{FALSE} (report only).
#' @param partition_key_after,partition_key_before Optional character strings
#'   (typically YYYY-MM-DD or YYYY-MM), the same values you pass to
#'   \code{download_dewey()}. When given, only files whose leading date falls in
#'   that range are verified, so you can check just the partitions you downloaded.
#'   Matching is on the date prefix in the file name (how Dewey names partitioned
#'   files); files without a leading date are skipped when a bound is set.
#'
#' @return A character vector of corrupt file paths, invisibly (empty if all OK).
#'
#' @examples
#' \dontrun{
#' # Re-check a folder and remove any bad files, then re-download with fewer workers
#' bad <- verify_dewey_download("dewey-downloads/fine-arts", delete_corrupt = TRUE)
#' if (length(bad)) {
#'   download_dewey(api_key, folder_id,
#'     download_path = "dewey-downloads/fine-arts", num_workers = 1)
#' }
#' }
#'
#' @export
verify_dewey_download <- function(download_path, delete_corrupt = FALSE,
                                  partition_key_after = NULL,
                                  partition_key_before = NULL) {
  validate_partition_key(partition_key_after, "partition_key_after")
  validate_partition_key(partition_key_before, "partition_key_before")

  files <- list.files(
    download_path, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE
  )
  if (length(files) == 0) {
    return(invisible(character(0)))
  }

  # Scope to a partition-key range by the date prefix in the file name.
  if (!is.null(partition_key_after) || !is.null(partition_key_before)) {
    files <- files[in_partition_range(files, partition_key_after, partition_key_before)]
    if (length(files) == 0) {
      message("No downloaded parquet files fall in the given partition_key range.")
      return(invisible(character(0)))
    }
  }

  ok <- vapply(files, parquet_footer_ok, logical(1))
  bad <- files[!ok]

  if (length(bad) == 0) {
    message("Verified ", length(files), " parquet file(s): all have valid footers.")
    return(invisible(character(0)))
  }

  if (delete_corrupt) {
    unlink(bad)
  }

  warning(
    length(bad), " of ", length(files), " downloaded parquet file(s) are corrupt ",
    "(empty or missing the 'PAR1' footer)", if (delete_corrupt) " and have been deleted" else "", ":\n",
    paste0("  - ", bad, collapse = "\n"),
    "\n\nThese are usually error responses saved during a download burst. To fix, ",
    "re-run the same download with fewer workers so only the missing files are re-fetched:\n",
    "  download_dewey(api_key, folder_id, download_path = \"", download_path, "\", num_workers = 1)\n",
    if (!delete_corrupt) paste0(
      "(Delete the files above first, or call ",
      "verify_dewey_download(\"", download_path, "\", delete_corrupt = TRUE).)"
    ) else "",
    call. = FALSE
  )

  invisible(bad)
}
