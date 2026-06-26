# Tests for retry_with_backoff() — the loop that lets a long download ride out
# transient blips (e.g. a dropped connection -> DuckDB "HTTP 0") instead of
# aborting on the first failure. sleep/notify are injected so no real waiting.

quiet  <- function(...) invisible(NULL)
nosleep <- function(secs) invisible(NULL)

test_that("succeeds on the first try without sleeping or notifying", {
  calls <- 0L
  expect_silent(
    retry_with_backoff(function() calls <<- calls + 1L,
                       sleep = function(s) stop("should not sleep"),
                       notify = function(...) stop("should not notify"))
  )
  expect_equal(calls, 1L)
})

test_that("retries transient failures, then succeeds", {
  calls <- 0L
  do <- function() {
    calls <<- calls + 1L
    if (calls < 3L) stop("transient")
    invisible(TRUE)
  }
  res <- retry_with_backoff(do, max_attempts = 5L, sleep = nosleep, notify = quiet)
  expect_true(res)
  expect_equal(calls, 3L)            # failed twice, succeeded on the third
})

test_that("gives up after max_attempts and appends the hint", {
  calls <- 0L
  do <- function() { calls <<- calls + 1L; stop("always") }
  expect_error(
    retry_with_backoff(do, max_attempts = 3L, label = "Batch 7/100",
                       hint = " RESUME-HINT", sleep = nosleep, notify = quiet),
    "Batch 7/100 failed after 3 attempts.*RESUME-HINT"
  )
  expect_equal(calls, 3L)            # exactly max_attempts tries
})

test_that("notifies before each retry", {
  msgs <- character(0)
  do <- local({ n <- 0L; function() { n <<- n + 1L; if (n < 2L) stop("boom") } })
  retry_with_backoff(do, sleep = nosleep,
                     notify = function(...) msgs <<- c(msgs, paste0(...)))
  expect_length(msgs, 1L)            # one retry notification before the success
  expect_match(msgs[[1]], "retrying in")
})
