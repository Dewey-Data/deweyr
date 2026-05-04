# Tests for download_dewey_duck argument validation and forwarding.
#
# Strategy: download_dewey_duck calls validate_partition_key() FIRST, so we
# can hit the validator gates without any mocking. For "did the partition
# key reach get_dewey_urls?" we use local_mocked_bindings (with explicit
# .package = "deweyr" — without it the mock can silently no-op when run
# outside the package test harness, producing false-positive passes) to
# capture the call and short-circuit before DuckDB tries to download.

# ---- validator gates (no mocking needed) -------------------------------------

# ---- regression: empty api_key / data_id error before shell out --------------
# Empty strings are silently dropped by system2(), shifting Python's positional
# argv. Without this gate the user gets a confusing 401 on a malformed URL
# (data/__DEWEYR_NULL__/files) instead of a clear "set your API key" message.

test_that("preview_dewey_duck rejects empty api_key with a useful hint", {
  err <- tryCatch(
    preview_dewey_duck("", "prj_x__fldr_y", limit = 0),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "api_key", fixed = TRUE)
  expect_match(err, "non-empty", fixed = TRUE)
  expect_match(err, "DEWEY_API_KEY", fixed = TRUE)  # the hint
})

test_that("download_dewey_duck rejects NA data_id", {
  expect_error(
    download_dewey_duck("k", NA_character_),
    "data_id"
  )
})

test_that("download_dewey_duck rejects empty partition_key_after", {
  expect_error(
    download_dewey_duck("k", "prj_x__fldr_y", partition_key_after = ""),
    "partition_key_after"
  )
})

test_that("download_dewey_duck rejects multi-element partition_key_before", {
  expect_error(
    download_dewey_duck(
      "k", "prj_x__fldr_y",
      partition_key_before = c("2024-01-01", "2024-02-01")
    ),
    "partition_key_before"
  )
})

test_that("download_dewey_duck rejects NA partition_key_after", {
  expect_error(
    download_dewey_duck("k", "prj_x__fldr_y", partition_key_after = NA_character_),
    "partition_key_after"
  )
})

test_that("download_dewey_duck rejects newline injection in partition_key", {
  expect_error(
    download_dewey_duck(
      "k", "prj_x__fldr_y",
      partition_key_after = "2024-01-01\nmalicious"
    ),
    "newline"
  )
})

# ---- forwarding: every relevant arg reaches get_dewey_urls -------------------

test_that("download_dewey_duck forwards every relevant arg to get_dewey_urls", {
  captured <- new.env()
  fake_get_dewey_urls <- function(api_key, data_id, file_name = NULL,
                                  preview = FALSE,
                                  partition_key_after = NULL,
                                  partition_key_before = NULL) {
    captured$api_key <- api_key
    captured$data_id <- data_id
    captured$file_name <- file_name
    captured$preview <- preview
    captured$partition_key_after <- partition_key_after
    captured$partition_key_before <- partition_key_before
    stop("__captured__")
  }

  testthat::local_mocked_bindings(
    get_dewey_urls = fake_get_dewey_urls,
    .package = "deweyr"
  )

  expect_error(
    download_dewey_duck(
      api_key = "secret",
      data_id = "prj_x__fldr_y",
      file_name = "my-folder",
      partition_key_after = "2024-01-01",
      partition_key_before = "2024-02-01"
    ),
    "__captured__"
  )

  expect_equal(captured$api_key, "secret")
  expect_equal(captured$data_id, "prj_x__fldr_y")
  expect_equal(captured$file_name, "my-folder")
  expect_equal(captured$partition_key_after, "2024-01-01")
  expect_equal(captured$partition_key_before, "2024-02-01")
  # download_dewey_duck never sets preview=TRUE; default should be FALSE.
  expect_false(isTRUE(captured$preview))
})

test_that("download_dewey_duck forwards NULL partition keys when not supplied", {
  captured <- new.env()
  captured$seen <- FALSE
  fake_get_dewey_urls <- function(api_key, data_id, file_name = NULL,
                                  preview = FALSE,
                                  partition_key_after = NULL,
                                  partition_key_before = NULL) {
    captured$seen <- TRUE
    captured$partition_key_after <- partition_key_after
    captured$partition_key_before <- partition_key_before
    stop("__captured__")
  }

  testthat::local_mocked_bindings(
    get_dewey_urls = fake_get_dewey_urls,
    .package = "deweyr"
  )

  expect_error(download_dewey_duck("k", "prj_x__fldr_y"), "__captured__")

  expect_true(captured$seen)
  expect_null(captured$partition_key_after)
  expect_null(captured$partition_key_before)
})

# ---- regression: the validator runs BEFORE get_dewey_urls --------------------
# If someone moves the validate_partition_key calls below get_dewey_urls,
# the mock would be hit before the validator, and we'd get __captured__
# instead of the validator's error. Lock the order in.

test_that("partition_key validator fires before any subprocess call", {
  fake_get_dewey_urls <- function(...) stop("__captured__")
  testthat::local_mocked_bindings(
    get_dewey_urls = fake_get_dewey_urls,
    .package = "deweyr"
  )

  err <- tryCatch(
    download_dewey_duck("k", "prj_x__fldr_y", partition_key_after = ""),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "partition_key_after", fixed = TRUE)
  expect_false(grepl("__captured__", err, fixed = TRUE))
})
