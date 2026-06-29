# Tests for verify_dewey_download(): the client-side guard that flags parquet
# files which came down corrupt (empty, or missing the "PAR1" footer) — the
# symptom of an HTTP error body being saved as a .parquet during a download.

make_valid_parquet <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  p <- gsub("\\\\", "/", path)
  DBI::dbExecute(con, glue::glue("COPY (SELECT 1 AS a, 'x' AS b) TO '{p}' (FORMAT PARQUET)"))
  invisible(path)
}

test_that("parquet_footer_ok recognizes a valid footer and rejects a non-parquet body", {
  dir <- tempfile("dwftr"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  good <- make_valid_parquet(file.path(dir, "good.parquet"))
  expect_true(parquet_footer_ok(good))

  notparq <- file.path(dir, "x.parquet"); writeBin(charToRaw("not a parquet file"), notparq)
  expect_false(parquet_footer_ok(notparq))

  empty <- file.path(dir, "e.parquet"); file.create(empty)
  expect_false(parquet_footer_ok(empty))     # 0-byte
})

test_that("verify_dewey_download flags corrupt + empty files and keeps valid ones", {
  dir <- tempfile("dwverify"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  good  <- make_valid_parquet(file.path(dir, "good.parquet"))
  errbody <- file.path(dir, "error_body.parquet")
  writeLines("<html><body>500 Internal Server Error</body></html>", errbody)   # error page saved as .parquet
  empty <- file.path(dir, "empty.parquet"); file.create(empty)                 # 0-byte

  expect_warning(bad <- verify_dewey_download(dir), "corrupt")
  expect_setequal(bad, c(errbody, empty))
  expect_false(good %in% bad)
  expect_true(file.exists(errbody))           # report-only: not deleted by default
})

test_that("verify_dewey_download is clean when every file is valid", {
  dir <- tempfile("dwok"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  make_valid_parquet(file.path(dir, "a.parquet"))
  make_valid_parquet(file.path(dir, "b.parquet"))

  expect_message(res <- verify_dewey_download(dir), "valid footers")
  expect_length(res, 0)
})

test_that("verify_dewey_download deletes corrupt files when asked, keeps good ones", {
  dir <- tempfile("dwdel"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  good <- make_valid_parquet(file.path(dir, "good.parquet"))
  bad  <- file.path(dir, "bad.parquet"); writeLines("oops", bad)

  expect_warning(res <- verify_dewey_download(dir, delete_corrupt = TRUE), "deleted")
  expect_setequal(res, bad)
  expect_false(file.exists(bad))              # removed
  expect_true(file.exists(good))              # kept
})

test_that("verify_dewey_download scans recursively and ignores non-parquet files", {
  dir <- tempfile("dwrec"); sub <- file.path(dir, "state=GA"); dir.create(sub, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  make_valid_parquet(file.path(sub, "good.parquet"))
  writeLines("col\n1", file.path(dir, "notes.csv"))         # ignored (not parquet)
  badnested <- file.path(sub, "bad.parquet"); writeLines("x", badnested)

  expect_warning(bad <- verify_dewey_download(dir), "corrupt")
  expect_setequal(bad, badnested)
})

test_that("verify_dewey_download is silent when there are no parquet files", {
  dir <- tempfile("dwnone"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  writeLines("x", file.path(dir, "readme.txt"))

  expect_silent(res <- verify_dewey_download(dir))
  expect_length(res, 0)
})

# ---- partition_key scoping ---------------------------------------------------

test_that("partition_key range scopes verification by the file-name date", {
  dir <- tempfile("dwvpk"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  make_valid_parquet(file.path(dir, "2025-01-15--data_a.parquet"))  # in range, valid
  writeLines("err", file.path(dir, "2025-01-20--data_b.parquet"))   # in range, corrupt
  writeLines("err", file.path(dir, "2025-02-05--data_c.parquet"))   # out of range, corrupt

  bad <- suppressWarnings(verify_dewey_download(
    dir, partition_key_after = "2025-01", partition_key_before = "2025-01"))
  expect_equal(basename(bad), "2025-01-20--data_b.parquet")         # Feb file not checked
})

test_that("partition_key_before excludes later partitions (granularity-aware)", {
  dir <- tempfile("dwvpk2"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  writeLines("err", file.path(dir, "2025-01-15--a.parquet"))
  writeLines("err", file.path(dir, "2025-03-15--b.parquet"))
  bad <- suppressWarnings(verify_dewey_download(dir, partition_key_before = "2025-02"))
  expect_equal(basename(bad), "2025-01-15--a.parquet")              # March excluded
})

test_that("a partition range matching no files is reported cleanly", {
  dir <- tempfile("dwvpk3"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  make_valid_parquet(file.path(dir, "2025-01-15--a.parquet"))
  expect_message(res <- verify_dewey_download(dir, partition_key_after = "2030-01"),
                 "No downloaded parquet files fall")
  expect_length(res, 0)
})

test_that("verify_dewey_download validates partition keys", {
  expect_error(
    verify_dewey_download(tempdir(), partition_key_after = "2025-01\nx"),
    "newline"
  )
})
