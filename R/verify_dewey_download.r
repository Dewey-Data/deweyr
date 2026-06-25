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
verify_dewey_download <- function(download_path, delete_corrupt = FALSE) {
  files <- list.files(
    download_path, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE
  )
  if (length(files) == 0) {
    return(invisible(character(0)))
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
