# Tests for validate_partition_key — cheap sanity gate before R/Python boundary.

test_that("validate_partition_key: NULL is allowed (means 'no filter')", {
  expect_invisible(deweyr:::validate_partition_key(NULL, "partition_key_after"))
  expect_silent(deweyr:::validate_partition_key(NULL, "partition_key_after"))
})

test_that("validate_partition_key: valid date string passes", {
  expect_silent(deweyr:::validate_partition_key("2024-01-01", "partition_key_after"))
})

test_that("validate_partition_key: empty string is rejected", {
  expect_error(
    deweyr:::validate_partition_key("", "partition_key_after"),
    "must not be an empty string"
  )
})

test_that("validate_partition_key: NA character is rejected", {
  expect_error(
    deweyr:::validate_partition_key(NA_character_, "partition_key_after"),
    "must be a single character string"
  )
})

test_that("validate_partition_key: numeric is rejected", {
  expect_error(
    deweyr:::validate_partition_key(20240101, "partition_key_after"),
    "must be a single character string"
  )
})

test_that("validate_partition_key: multi-element character is rejected", {
  expect_error(
    deweyr:::validate_partition_key(c("2024-01-01", "2024-02-01"), "partition_key_after"),
    "must be a single character string"
  )
})

test_that("validate_partition_key: zero-length character is rejected", {
  expect_error(
    deweyr:::validate_partition_key(character(0), "partition_key_after"),
    "must be a single character string"
  )
})

test_that("validate_partition_key: embedded newline is rejected", {
  expect_error(
    deweyr:::validate_partition_key("2024-01-01\nrm -rf /", "partition_key_after"),
    "must not contain newline"
  )
})

test_that("validate_partition_key: embedded carriage return is rejected", {
  expect_error(
    deweyr:::validate_partition_key("2024-01-01\r", "partition_key_after"),
    "must not contain newline"
  )
})

test_that("validate_partition_key: arg_name is included in the error message", {
  expect_error(
    deweyr:::validate_partition_key("", "partition_key_before"),
    "partition_key_before"
  )
})
