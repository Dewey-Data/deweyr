# Tests for the R/Python argv contract used by get_dewey_urls.py.
# Order of positional args MUST stay in lockstep with that script.

NONE <- deweyr:::DEWEYR_NULL_SENTINEL

test_that("argv defaults: NULL file_name and partition keys collapse to the sentinel", {
  args <- deweyr:::build_get_dewey_urls_args(
    script = "/tmp/get_dewey_urls.py",
    api_key = "key",
    data_id = "prj_x__fldr_y",
    file_name = NULL,
    preview = FALSE,
    partition_key_after = NULL,
    partition_key_before = NULL
  )
  expect_equal(args[1:4], c("run", "--python", "3.13", "/tmp/get_dewey_urls.py"))
  expect_equal(args[5], "key")
  expect_equal(args[6], "prj_x__fldr_y")
  expect_equal(args[7], NONE)        # file_name
  expect_equal(args[8], "false")     # preview lowercased
  expect_equal(args[9], NONE)        # partition_key_after
  expect_equal(args[10], NONE)       # partition_key_before
  expect_length(args, 10)
})

test_that("argv: preview = TRUE serializes as lowercase 'true'", {
  args <- deweyr:::build_get_dewey_urls_args(
    script = "s.py", api_key = "k", data_id = "d",
    file_name = NULL, preview = TRUE,
    partition_key_after = NULL, partition_key_before = NULL
  )
  expect_equal(args[8], "true")
})

test_that("argv: partition keys are forwarded verbatim", {
  args <- deweyr:::build_get_dewey_urls_args(
    script = "s.py", api_key = "k", data_id = "d",
    file_name = NULL, preview = FALSE,
    partition_key_after = "2024-01-01",
    partition_key_before = "2024-02-01"
  )
  expect_equal(args[9], "2024-01-01")
  expect_equal(args[10], "2024-02-01")
})

test_that("argv: file_name is forwarded when provided", {
  args <- deweyr:::build_get_dewey_urls_args(
    script = "s.py", api_key = "k", data_id = "d",
    file_name = "custom_folder", preview = FALSE,
    partition_key_after = NULL, partition_key_before = NULL
  )
  expect_equal(args[7], "custom_folder")
})

test_that("argv: python_version is overridable", {
  args <- deweyr:::build_get_dewey_urls_args(
    script = "s.py", api_key = "k", data_id = "d",
    file_name = NULL, preview = FALSE,
    partition_key_after = NULL, partition_key_before = NULL,
    python_version = "3.12"
  )
  expect_equal(args[3], "3.12")
})

test_that("argv: numeric partition_key is coerced to character", {
  # Defensive — users may pass an integer year accidentally.
  args <- deweyr:::build_get_dewey_urls_args(
    script = "s.py", api_key = "k", data_id = "d",
    file_name = NULL, preview = FALSE,
    partition_key_after = 2024L, partition_key_before = NULL
  )
  expect_type(args[9], "character")
  expect_equal(args[9], "2024")
})

test_that("argv: sentinel is unlikely to collide with user input", {
  # The sentinel must contain characters that would never appear in
  # api_keys, partition keys (dates), or filenames.
  expect_match(NONE, "DEWEYR")
  expect_false(grepl("^[A-Za-z0-9-]+$", NONE))  # contains underscores
})

test_that("argv: a user partition_key that looks like the literal 'None' is forwarded as data, not sentinel", {
  # The old sentinel was "None" — a real risk of collision. Confirm new
  # sentinel doesn't collide with the obvious string a user might pass.
  args <- deweyr:::build_get_dewey_urls_args(
    script = "s.py", api_key = "k", data_id = "d",
    file_name = NULL, preview = FALSE,
    partition_key_after = "None", partition_key_before = NULL
  )
  expect_equal(args[9], "None")           # forwarded as a literal string
  expect_false(args[9] == NONE)           # NOT the sentinel
})
