#' Run an expression, retrying on error with exponential backoff
#'
#' Used to ride out transient download failures (e.g. a dropped connection that
#' DuckDB surfaces as "HTTP 0") without aborting a long-running job on the first
#' blip. \code{sleep} and \code{notify} are injectable so the loop is testable
#' without real waits.
#'
#' @param do A zero-argument function to attempt. Re-run on error.
#' @param max_attempts Maximum number of attempts before giving up. Default 5.
#' @param label Human-readable label for messages/errors (e.g. "Batch 3/100").
#' @param hint Extra text appended to the final error after all retries fail.
#' @param sleep Function called with the wait (seconds) between attempts.
#' @param notify Function called with message parts before each retry.
#'
#' @return Invisibly \code{TRUE} on success; otherwise \code{stop()}s after
#'   \code{max_attempts}.
#' @keywords internal
#' @noRd
retry_with_backoff <- function(do, max_attempts = 5L, label = "operation",
                               hint = "", sleep = Sys.sleep, notify = message) {
  attempt <- 1L
  repeat {
    err <- tryCatch({ do(); NULL }, error = function(e) e)
    if (is.null(err)) {
      return(invisible(TRUE))
    }
    if (attempt >= max_attempts) {
      stop(label, " failed after ", max_attempts, " attempts: ",
           conditionMessage(err), hint, call. = FALSE)
    }
    wait <- min(60, 2^attempt)
    notify(label, " failed (attempt ", attempt, "/", max_attempts, "): ",
           conditionMessage(err), " — retrying in ", wait, "s")
    sleep(wait)
    attempt <- attempt + 1L
  }
}
