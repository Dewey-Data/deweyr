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

# ---- batched download: no data lost across batch boundaries ------------------
# download_dewey_duck() processes the file manifest in batches of `batch_size`
# rather than handing DuckDB every URL at once (which floods the download API
# with simultaneous requests and 500s on large, unpartitioned datasets like
# Veraset Visits). These run the REAL download + read against LOCAL parquet,
# mocking only the network call. Each source file tags its rows with a unique
# city, so an overwriting bug would drop the distinct-city count.

# Create `n` local parquet files (cols: city, naics_code, state, caid).
make_source_files <- function(dir, n = 7) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (i in seq_len(n)) {
    f <- file.path(dir, sprintf("file_%02d.parquet", i))
    DBI::dbExecute(con, glue::glue(
      "COPY (SELECT * FROM (VALUES
         ('GA_522_{i}', '522110', 'GA', 'caid'),
         ('NY_522_{i}', '522110', 'NY', 'caid'),
         ('GA_541_{i}', '541110', 'GA', 'caid'),
         ('NY_111_{i}', '111111', 'NY', 'caid')
       ) t(city, naics_code, state, caid))
       TO '{f}' (FORMAT PARQUET)"
    ))
  }
  sort(list.files(dir, pattern = "\\.parquet$", full.names = TRUE))
}

# get_dewey_urls() stand-in pointing the function at local files. Absorbs the
# partition_key_* args via ... so it matches the real call signature.
local_files_result <- function(urls) {
  function(api_key, data_id, file_name = NULL, preview = FALSE, ...) {
    list(
      urls            = if (isTRUE(preview)) urls[[1]] else urls,
      parent_folder   = "visits-duckdb",
      file_extension  = ".snappy.parquet",
      partition_key   = "state",
      file_size_bytes = 0,
      cols            = c("city", "naics_code", "state", "caid")
    )
  }
}

test_that("batched, partitioned download keeps every batch's rows (no overwrite)", {
  skip_on_cran()  # loads the httpfs DuckDB extension; not for CRAN's offline runs

  src <- tempfile("dwsrc"); out <- tempfile("dwout")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  urls <- make_source_files(src, n = 7)

  local_mocked_bindings(get_dewey_urls = local_files_result(urls), .package = "deweyr")

  path <- download_dewey_duck(
    api_key = "k", data_id = "prj_x__fldr_y", output_dir = out,
    partition = "state", where = "naics_code = '522110'",
    select = c("city", "naics_code", "state"),
    batch_size = 3                                   # 7 files -> batches of 3, 3, 1
  )

  expect_true(dir.exists(path))

  ga_files <- list.files(file.path(path, "state=GA"), pattern = "\\.parquet$")
  expect_equal(length(ga_files), 3)                  # one uniquely-named file per batch
  expect_equal(length(unique(ga_files)), 3)          # {uuid} names, no clobber
  expect_true(all(grepl("^batch", ga_files)))

  df <- read_dewey_duck(path)
  expect_equal(nrow(df), 14)                                  # 2 matching rows x 7 files
  expect_equal(length(unique(df$city)), 14)                   # every file's rows survived
  expect_equal(sort(unique(df$naics_code)), "522110")         # WHERE applied
  expect_setequal(names(df), c("city", "naics_code", "state")) # SELECT dropped caid
  expect_equal(as.integer(table(df$state)[c("GA", "NY")]), c(7L, 7L))
})

test_that("a single batch (batch_size >= file count) still works", {
  skip_on_cran()

  src <- tempfile("dwsrc"); out <- tempfile("dwout")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  urls <- make_source_files(src, n = 4)

  local_mocked_bindings(get_dewey_urls = local_files_result(urls), .package = "deweyr")

  path <- download_dewey_duck(
    api_key = "k", data_id = "prj_x__fldr_y", output_dir = out,
    partition = "state", where = "naics_code = '522110'",
    select = c("city", "naics_code", "state"), batch_size = 100
  )

  df <- read_dewey_duck(path)
  expect_equal(nrow(df), 8)
  expect_equal(length(unique(df$city)), 8)
  expect_equal(length(list.files(file.path(path, "state=GA"), pattern = "\\.parquet$")), 1)
})

test_that("batched, unpartitioned download writes one file per batch and loses nothing", {
  skip_on_cran()

  src <- tempfile("dwsrc"); out <- tempfile("dwout")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  urls <- make_source_files(src, n = 7)

  local_mocked_bindings(get_dewey_urls = local_files_result(urls), .package = "deweyr")

  path <- download_dewey_duck(
    api_key = "k", data_id = "prj_x__fldr_y", output_dir = out,
    partition = NULL, where = "naics_code = '522110'",
    select = c("city", "naics_code", "state"), batch_size = 3
  )

  data_files <- list.files(path, pattern = "^data_batch\\d+\\.parquet$")
  expect_equal(length(data_files), 3)                # one parquet per batch

  df <- read_dewey_duck(path)
  expect_equal(nrow(df), 14)
  expect_equal(length(unique(df$city)), 14)
  expect_setequal(names(df), c("city", "naics_code", "state"))
})

# ---- resume: a re-run continues instead of redoing/wiping ---------------------
# A job big enough to outlive its 24h download links must be re-run to finish.
# download_dewey_duck() records completed batches in .deweyr_progress.json so a
# re-run skips them. These tests drive that resume path with local files.

dl_args <- function(out, ...) {
  base <- list(api_key = "k", data_id = "prj_x__fldr_y", output_dir = out,
               partition = "state", where = "naics_code = '522110'",
               select = c("city", "naics_code", "state"), batch_size = 2)
  modifyList(base, list(...))   # later args (e.g. batch_size, overwrite) win
}

test_that("resume continues an existing download, skipping completed batches", {
  skip_on_cran()
  src <- tempfile("dwsrc"); out <- tempfile("dwout")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  urls <- make_source_files(src, n = 6)
  local_mocked_bindings(get_dewey_urls = local_files_result(urls), .package = "deweyr")

  path <- do.call(download_dewey_duck, dl_args(out))      # 6 files / 2 = 3 batches
  prog <- jsonlite::fromJSON(file.path(path, ".deweyr_progress.json"))
  expect_equal(sort(as.integer(prog$completed)), 1:3)

  before <- list.files(path, pattern = "\\.parquet$", recursive = TRUE)
  expect_message(do.call(download_dewey_duck, dl_args(out)), "Resuming.*3/3")
  after <- list.files(path, pattern = "\\.parquet$", recursive = TRUE)
  expect_setequal(after, before)                          # nothing re-written
})

test_that("resume re-runs only the missing batch", {
  skip_on_cran()
  src <- tempfile("dwsrc"); out <- tempfile("dwout")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  urls <- make_source_files(src, n = 6)
  local_mocked_bindings(get_dewey_urls = local_files_result(urls), .package = "deweyr")

  path <- do.call(download_dewey_duck, dl_args(out))

  # Simulate batch 3 never finishing: remove its output and drop it from progress.
  unlink(list.files(path, pattern = "^batch3_", recursive = TRUE, full.names = TRUE))
  prog <- jsonlite::fromJSON(file.path(path, ".deweyr_progress.json"))
  prog$completed <- prog$completed[prog$completed != 3]
  jsonlite::write_json(prog, file.path(path, ".deweyr_progress.json"), auto_unbox = TRUE)

  expect_message(do.call(download_dewey_duck, dl_args(out)), "Batch 3/3 complete")

  df <- read_dewey_duck(path)
  expect_equal(nrow(df), 12)                              # all 6 files' matches restored
  expect_equal(length(unique(df$city)), 12)
})

test_that("resume refuses a folder that holds a different download", {
  skip_on_cran()
  src <- tempfile("dwsrc"); out <- tempfile("dwout")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  urls <- make_source_files(src, n = 6)
  local_mocked_bindings(get_dewey_urls = local_files_result(urls), .package = "deweyr")

  do.call(download_dewey_duck, dl_args(out))               # batch_size 2 -> 3 batches
  expect_error(
    do.call(download_dewey_duck, dl_args(out, batch_size = 3)),  # different fingerprint
    "different download"
  )
})

test_that("overwrite = TRUE wipes and restarts the download", {
  skip_on_cran()
  src <- tempfile("dwsrc"); out <- tempfile("dwout")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  urls <- make_source_files(src, n = 6)
  local_mocked_bindings(get_dewey_urls = local_files_result(urls), .package = "deweyr")

  path <- do.call(download_dewey_duck, dl_args(out))
  writeLines("junk", file.path(path, "stray.txt"))
  do.call(download_dewey_duck, dl_args(out, overwrite = TRUE))
  expect_false(file.exists(file.path(path, "stray.txt")))  # wiped on restart
  expect_equal(nrow(read_dewey_duck(path)), 12)
})

test_that("the manifest is ordered by stable file name so batches are deterministic", {
  skip_on_cran()
  src <- tempfile("dwsrc"); out <- tempfile("dwout")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  urls <- make_source_files(src, n = 4)
  # Return file_names in reverse of the URL order; ordering must still read all files.
  fake <- function(api_key, data_id, file_name = NULL, preview = FALSE, ...) {
    list(urls = if (isTRUE(preview)) urls[[1]] else urls,
         file_names = rev(seq_along(urls)),
         parent_folder = "visits-duckdb", file_extension = ".snappy.parquet",
         partition_key = "state", file_size_bytes = 0,
         cols = c("city", "naics_code", "state", "caid"))
  }
  local_mocked_bindings(get_dewey_urls = fake, .package = "deweyr")

  path <- download_dewey_duck(
    api_key = "k", data_id = "prj_x__fldr_y", output_dir = out, partition = "state",
    where = "naics_code = '522110'", select = c("city", "naics_code", "state"),
    batch_size = 2
  )
  df <- read_dewey_duck(path)
  expect_equal(nrow(df), 8)                                # all 4 files read regardless of order
  expect_equal(length(unique(df$city)), 8)
})
