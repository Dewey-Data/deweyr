# Tests for check_dewey_download(): compares local files against the Dewey
# manifest (name + expected size) and reports the ones still missing or the
# wrong size. The manifest fetch (get_dewey_urls) is mocked; files are local.

# A get_dewey_urls() stand-in returning a manifest of (file_names, file_sizes).
fake_manifest <- function(file_names, file_sizes) {
  function(api_key, data_id, file_name = NULL, preview = FALSE, ...) {
    list(
      file_names     = file_names,
      file_sizes     = file_sizes,
      urls           = paste0("u", seq_along(file_names)),
      parent_folder  = "x",
      file_extension = ".snappy.parquet",
      partition_key  = NA,
      file_size_bytes = sum(file_sizes),
      cols           = character(0)
    )
  }
}

test_that("flags missing and wrong-sized files, passes correct ones", {
  dir <- tempfile("dwchk"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  writeBin(raw(10), file.path(dir, "a.parquet"))   # correct
  writeBin(raw(5),  file.path(dir, "b.parquet"))   # present but wrong size (want 10)
  # c.parquet is absent

  local_mocked_bindings(
    get_dewey_urls = fake_manifest(c("a.parquet", "b.parquet", "c.parquet"), c(10, 10, 7)),
    .package = "deweyr"
  )
  res <- suppressMessages(check_dewey_download("k", "prj_x__fldr_y", download_path = dir))

  expect_equal(nrow(res), 2)
  expect_setequal(res$file, c("b.parquet", "c.parquet"))
  expect_equal(res$status[res$file == "b.parquet"], "size_mismatch")
  expect_equal(res$status[res$file == "c.parquet"], "missing")
  expect_true(is.na(res$local_bytes[res$file == "c.parquet"]))
})

test_that("returns zero rows when every file is present at the expected size", {
  dir <- tempfile("dwchk2"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  writeBin(raw(10), file.path(dir, "a.parquet"))
  writeBin(raw(20), file.path(dir, "b.parquet"))

  local_mocked_bindings(
    get_dewey_urls = fake_manifest(c("a.parquet", "b.parquet"), c(10, 20)),
    .package = "deweyr"
  )
  expect_message(res <- check_dewey_download("k", "prj_x__fldr_y", download_path = dir),
                 "All 2 files present")
  expect_equal(nrow(res), 0)
})

test_that("finds files regardless of subfolder layout", {
  dir <- tempfile("dwchk3"); sub <- file.path(dir, "state=GA"); dir.create(sub, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  writeBin(raw(10), file.path(sub, "a.parquet"))   # nested

  local_mocked_bindings(
    get_dewey_urls = fake_manifest("a.parquet", 10),
    .package = "deweyr"
  )
  res <- suppressMessages(check_dewey_download("k", "prj_x__fldr_y", download_path = dir))
  expect_equal(nrow(res), 0)   # found despite being nested
})

test_that("everything missing when the folder is empty", {
  dir <- tempfile("dwchk4"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    get_dewey_urls = fake_manifest(c("a.parquet", "b.parquet"), c(10, 20)),
    .package = "deweyr"
  )
  res <- suppressMessages(check_dewey_download("k", "prj_x__fldr_y", download_path = dir))
  expect_equal(nrow(res), 2)
  expect_true(all(res$status == "missing"))
})

test_that("errors clearly if the manifest has no per-file sizes", {
  local_mocked_bindings(
    get_dewey_urls = function(...) list(file_names = "a.parquet", file_sizes = NULL,
                                        urls = "u1", parent_folder = "x",
                                        file_extension = ".snappy.parquet",
                                        partition_key = NA, file_size_bytes = 10,
                                        cols = character(0)),
    .package = "deweyr"
  )
  expect_error(
    check_dewey_download("k", "prj_x__fldr_y", download_path = tempdir()),
    "per-file sizes"
  )
})

test_that("partition keys are forwarded to the manifest fetch", {
  dir <- tempfile("dwchkpk"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  writeBin(raw(10), file.path(dir, "2025-01-01--a.parquet"))

  captured <- new.env()
  fake <- function(api_key, data_id, file_name = NULL, preview = FALSE,
                   partition_key_after = NULL, partition_key_before = NULL) {
    captured$after  <- partition_key_after
    captured$before <- partition_key_before
    list(file_names = "2025-01-01--a.parquet", file_sizes = 10, urls = "u1",
         parent_folder = "x", file_extension = ".snappy.parquet",
         partition_key = NA, file_size_bytes = 10, cols = character(0))
  }
  local_mocked_bindings(get_dewey_urls = fake, .package = "deweyr")

  suppressMessages(check_dewey_download(
    "k", "prj_x__fldr_y", download_path = dir,
    partition_key_after = "2025-01", partition_key_before = "2025-01"
  ))
  expect_equal(captured$after, "2025-01")
  expect_equal(captured$before, "2025-01")
})

test_that("check_dewey_download validates partition keys before any API call", {
  expect_error(
    check_dewey_download("k", "prj_x__fldr_y", partition_key_after = ""),
    "partition_key_after"
  )
})
