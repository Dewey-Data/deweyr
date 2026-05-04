#' Build the argv vector for the get_dewey_urls.py subprocess
#'
#' Pure helper extracted so the R/Python boundary contract is testable
#' without spawning a subprocess. Order of positional args here MUST match
#' \code{inst/python/get_dewey_urls.py}.
#'
#' @keywords internal
#' @noRd
# Peek column names for a single URL via DuckDB. Used as a fallback when the
# Python script's schema discovery fails. Stays in lockstep with the URL set
# the caller is about to download.
#
# @keywords internal
# @noRd
peek_cols_from_url <- function(url, file_extension) {
  read_fn <- ifelse(file_extension == ".snappy.parquet", "read_parquet", "read_csv")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  df <- DBI::dbGetQuery(con, glue::glue(
    "SELECT * FROM {read_fn}(['{url}']) LIMIT 0"
  ))
  colnames(df)
}

# Sentinel string for "no value provided". MUST match NONE_SENTINEL in
# inst/python/get_dewey_urls.py. Chosen so it cannot collide with any
# user-supplied value (api_key, partition_key, file_name).
DEWEYR_NULL_SENTINEL <- "__DEWEYR_NULL__"

build_get_dewey_urls_args <- function(script, api_key, data_id, file_name,
                                      preview, partition_key_after,
                                      partition_key_before,
                                      python_version = "3.13") {
  none <- DEWEYR_NULL_SENTINEL
  c(
    "run", "--python", python_version, script,
    api_key,
    data_id,
    if (is.null(file_name)) none else as.character(file_name),
    tolower(as.character(preview)),
    if (is.null(partition_key_after)) none else as.character(partition_key_after),
    if (is.null(partition_key_before)) none else as.character(partition_key_before)
  )
}

# Reject an empty / NA / missing api_key BEFORE shelling out to uv.
# system2() drops empty-string args, which silently shifts every positional
# argv on the Python side and produces a confusing 401-on-the-wrong-URL.
validate_required_string <- function(value, arg_name) {
  if (is.null(value) || !is.character(value) || length(value) != 1 ||
      is.na(value) || !nzchar(value)) {
    stop(arg_name, " must be a non-empty character string. ",
         "Got: ", deparse(value),
         if (identical(arg_name, "api_key"))
           " (hint: Sys.getenv(\"DEWEY_API_KEY\") may be empty if .Renviron didn't load — try readRenviron(\"~/.Renviron\"))"
         else "")
  }
  invisible(NULL)
}

#' Validate a partition_key argument
#'
#' Cheap sanity-check on user-supplied partition_key values before they go
#' across the R/Python boundary. Catches the common mistakes (empty string,
#' multi-element, embedded newline) while leaving date format up to the API.
#'
#' @keywords internal
#' @noRd
validate_partition_key <- function(value, arg_name) {
  if (is.null(value)) return(invisible(NULL))
  if (!is.character(value) || length(value) != 1 || is.na(value)) {
    stop(arg_name, " must be a single character string or NULL.")
  }
  if (!nzchar(value)) {
    stop(arg_name, " must not be an empty string. Pass NULL to skip.")
  }
  if (grepl("[\r\n]", value)) {
    stop(arg_name, " must not contain newline characters.")
  }
  invisible(NULL)
}

#' Get Dewey dataset file metadata
#'
#' Calls the Dewey API via a Python script to retrieve download URLs and
#' metadata for a given dataset. Used internally by \code{preview_dewey()}
#' and \code{download_dewey()}.
#'
#' @param api_key Your Dewey API key. Store in \code{.Renviron} as
#'   \code{DEWEY_API_KEY} and access with \code{Sys.getenv("DEWEY_API_KEY")}.
#' @param data_id The Dewey dataset ID (e.g. \code{"prj_xxx__fldr_yyy"}).
#' @param preview If \code{TRUE}, returns only the first file URL instead of
#'   paginating the full dataset manifest. Used internally by \code{preview_dewey()}.
#'   Defaults to \code{FALSE}.
#' @param partition_key_after Optional partition key lower bound (inclusive).
#'   Forwarded to deweypy. Ignored when \code{preview = TRUE}.
#' @param partition_key_before Optional partition key upper bound (inclusive).
#'   Forwarded to deweypy. Ignored when \code{preview = TRUE}.
#'
#' @return A list with the following fields:
#' \describe{
#'   \item{urls}{Character vector of download URLs for all files in the dataset,
#'     or a single URL string if \code{preview = TRUE}}
#'   \item{parent_folder}{Derived folder name for the dataset}
#'   \item{file_extension}{File extension of the dataset files}
#'   \item{partition_key}{Dewey's suggested partition column, or \code{NULL}}
#'   \item{file_size_bytes}{Total size of the dataset in bytes}
#' }
#'
#' @keywords internal
#' @noRd
get_dewey_urls <- function(api_key, data_id, file_name = NULL, preview = FALSE,
                           partition_key_after = NULL, partition_key_before = NULL) {
  validate_required_string(api_key, "api_key")
  validate_required_string(data_id, "data_id")
  if (!check_uv()) {
    install_uv()
    message("Restarting the terminal will increase speed of future runs")
  }
  data_id <- parse_url(data_id)
  script <- system.file("python/get_dewey_urls.py", package = "deweyr")
  args <- build_get_dewey_urls_args(
    script = script,
    api_key = api_key,
    data_id = data_id,
    file_name = file_name,
    preview = preview,
    partition_key_after = partition_key_after,
    partition_key_before = partition_key_before
  )
  stderr_path <- tempfile("deweyr_stderr_", fileext = ".txt")
  on.exit(unlink(stderr_path), add = TRUE)
  result_raw <- system2("uv", args = args, stdout = TRUE, stderr = stderr_path)
  exit_status <- attr(result_raw, "status")
  err_lines <- if (file.exists(stderr_path)) readLines(stderr_path, warn = FALSE) else character()
  stdout_str <- paste(result_raw, collapse = "")

  # Treat empty stdout as failure — `system2(..., stdout = TRUE)` does not
  # always set the `status` attribute when the command crashes early
  # (e.g. uv not found on PATH after install_uv). The Python script always
  # prints non-empty JSON on success.
  failed <- (!is.null(exit_status) && exit_status != 0) || !nzchar(trimws(stdout_str))

  if (failed) {
    err_msg <- paste(err_lines, collapse = "\n")
    parsed_err <- tryCatch(jsonlite::fromJSON(err_msg), error = function(e) NULL)
    if (is.list(parsed_err) && !is.null(parsed_err$error)) {
      stop("get_dewey_urls failed: ", parsed_err$error)
    }
    stop(
      "get_dewey_urls failed",
      if (!is.null(exit_status)) paste0(" (exit ", exit_status, ")") else "",
      ". Stderr:\n",
      if (nzchar(err_msg)) err_msg else "(empty)"
    )
  }
  if (length(err_lines) > 0) cat(err_lines, sep = "\n")

  parsed <- tryCatch(
    jsonlite::fromJSON(stdout_str),
    error = function(e) {
      stop(
        "Failed to parse response from get_dewey_urls.py. ",
        "Raw output: ", paste(result_raw, collapse = "\n")
      )
    }
  )
  parsed
}

#' Preview a Dewey dataset
#'
#' Fetches a small sample of a Dewey dataset directly from the source without
#' downloading it. Useful for exploring column names, data types, and values
#' before committing to a full download.
#'
#' To get just column names with no data:
#' ```r
#' colnames(preview_dewey_duck(api_key, data_id, limit = 0))
#' ```
#'
#' @param api_key Your Dewey API key. Store in \code{.Renviron} as
#'   \code{DEWEY_API_KEY} and access with \code{Sys.getenv("DEWEY_API_KEY")}.
#' @param data_id The Dewey dataset ID (e.g. \code{"prj_xxx__fldr_yyy"}).
#' @param limit Number of rows to return. Defaults to \code{10}. Use \code{0}
#'   to return no rows and only retrieve column names and types.
#' @param where Optional SQL WHERE clause string (no validation — errors are on you).
#'   Example: \code{where = "CARRIER_GROUP = 'Major'"}
#'
#' @return A tibble of up to \code{limit} rows from the dataset.
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("DEWEY_API_KEY")
#' data_id <- "prj_xxx__fldr_yyy"
#'
#' # Preview first 10 rows
#' preview_dewey_duck(api_key, data_id)
#'
#' # Get column names only
#' colnames(preview_dewey_duck(api_key, data_id, limit = 0))
#'
#' # Filter preview
#' preview_dewey_duck(api_key, data_id, where = "CARRIER_GROUP = 'Major'")
#' }
#'
#' @export
preview_dewey_duck <- function(api_key, data_id, limit = 10) {
  result <- get_dewey_urls(api_key, data_id, preview = TRUE)
  urls <- result$urls
  file_extension <- result$file_extension

  read_fn <- ifelse(file_extension == ".snappy.parquet", "read_parquet", "read_csv")
  urls_sql <- paste0("['", paste(urls, collapse = "','"), "']")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

  tibble::as_tibble(DBI::dbGetQuery(con, glue::glue(
    "SELECT * FROM {read_fn}({urls_sql}) LIMIT {limit}"
  )))
}

#' Download a Dewey dataset to local parquet files
#'
#' Downloads a Dewey dataset to a local directory as parquet files. Optionally
#' partition by a column, filter rows, and select columns before downloading.
#' Returns the path to the downloaded folder invisibly, so you can pipe directly
#' into \code{read_dewey()}.
#'
#' @param api_key Your Dewey API key. Store in \code{.Renviron} as
#'   \code{DEWEY_API_KEY} and access with \code{Sys.getenv("DEWEY_API_KEY")}.
#' @param data_id The Dewey dataset ID (e.g. \code{"prj_xxx__fldr_yyy"}).
#' @param output_dir Character string specifying where to download files.
#'   Default is a "dewey-downloads" folder in the current working directory.
#' @param partition Column name to partition by. If omitted, deweyr will use
#'   Dewey's suggested partition column if one exists, otherwise it will error.
#'   Pass \code{NULL} explicitly to download as a single unpartitioned parquet file.
#' @param overwrite If \code{FALSE} (default), errors if the output folder already
#'   exists. Pass \code{TRUE} to delete and re-download.
#' @param where Optional SQL WHERE clause string (no validation — errors are on you).
#'   Example: \code{where = "CARRIER_GROUP = 'Major'"}
#' @param select Optional vector of column indices, ranges, or names to download.
#'   Accepts mixed input e.g. \code{c(1:3, 7, "CARRIER_NAME")}. The partition
#'   column will always be added automatically if missing.
#' @param partition_key_after Optional character string (typically YYYY-MM-DD).
#'   Pre-filters the file manifest to partitions on/after this key — drastically
#'   reduces manifest size on large datasets like SafeGraph Visits.
#' @param partition_key_before Optional character string (typically YYYY-MM-DD).
#'   Pre-filters the file manifest to partitions on/before this key.
#'
#' @return The path to the downloaded dataset folder, invisibly. Pipe into
#'   \code{read_dewey()} to read immediately after downloading.
#'
#' @seealso
#' `vignette("getting-started", package = "deweyr")` for a full walkthrough
#' of downloading and reading your first dataset.
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("DEWEY_API_KEY")
#' data_id <- "prj_xxx__fldr_yyy"
#'
#' # Use dewey's default partition
#' download_dewey_duck(api_key, data_id)
#'
#' # Supply your own partition column
#' download_dewey_duck(api_key, data_id, partition = "MONTH_DATE_PARSED")
#'
#' # No partitioning
#' download_dewey_duck(api_key, data_id, partition = NULL)
#'
#' # Filter and select columns
#' download_dewey_duck(api_key, data_id, base_dir,
#'   partition = "MONTH_DATE_PARSED",
#'   where = "CARRIER_GROUP = 'Major'",
#'   select = c(1:3, "TOTAL")
#' )
#'
#' # Date-bounded download (huge speedup on Visits-scale datasets)
#' download_dewey_duck(api_key, data_id,
#'   partition_key_after = "2024-01-01",
#'   partition_key_before = "2024-02-01"
#' )
#'
#' # Download and read in one step
#' df <- download_dewey_duck(api_key, data_id, partition = "MONTH_DATE_PARSED") |>
#'   read_dewey()
#' }
#'
#' @export
download_dewey_duck <- function(api_key, data_id, output_dir = get_download_dir(),
                                partition, overwrite = FALSE, file_name = NULL,
                                where = NULL, select = NULL,
                                partition_key_after = NULL,
                                partition_key_before = NULL) {
  validate_partition_key(partition_key_after, "partition_key_after")
  validate_partition_key(partition_key_before, "partition_key_before")

  result <- get_dewey_urls(
    api_key, data_id,
    file_name = file_name,
    partition_key_after = partition_key_after,
    partition_key_before = partition_key_before
  )
  cols <- result$cols # ✅ no second call needed

  # Fallback if Python's DuckDB schema-peek failed: query the FIRST URL we
  # already have. This stays within the partition_key range — going through
  # preview_dewey_duck() would peek at an unfiltered file and could mismatch
  # the actual download set.
  if (length(cols) == 0) {
    cols <- peek_cols_from_url(result$urls[[1]], result$file_extension)
  }

  if (missing(partition)) {
    if (!is.null(result$partition_key) && result$partition_key %in% cols) {
      partition_col <- result$partition_key
      message("Partitioning by '", partition_col, "' (dewey default)")
    } else {
      stop("No default partition found. Available columns: ", paste(cols, collapse = ", "), ". Supply a column name or pass partition = NULL for no partitioning.")
    }
  } else if (!is.null(partition)) {
    if (!partition %in% cols) {
      stop("'", partition, "' is not a valid column. Available columns: ", paste(cols, collapse = ", "))
    }
    partition_col <- partition
  } else {
    partition_col <- NULL # explicit NULL, User wants no partitioning
  }

  # Resolve select — accepts c() with mixed indices and column names e.g. c(1:3, 7, "CARRIER_NAME")
  if (!is.null(select)) {
    select_cols <- c()
    for (s in select) {
      num <- suppressWarnings(as.numeric(s))
      if (!is.na(num)) {
        # It's an index
        if (num < 1 || num > length(cols)) {
          stop("select index ", num, " out of range. Dataset has ", length(cols), " columns.")
        }
        select_cols <- c(select_cols, cols[num])
      } else {
        # It's a column name — validate
        if (!s %in% cols) {
          stop("'", s, "' is not a valid column. Available columns: ", paste(cols, collapse = ", "))
        }
        select_cols <- c(select_cols, s)
      }
    }
    # Remove duplicates
    select_cols <- unique(select_cols)

    # Always include partition column if partitioning
    if (!is.null(partition_col) && !partition_col %in% select_cols) {
      message("Adding '", partition_col, "' to select as it is required for partitioning.")
      select_cols <- c(select_cols, partition_col)
    }
    select_sql <- paste(select_cols, collapse = ", ")
  } else {
    select_sql <- "*"
  }

  # Passed Checks, now we can download
  urls <- result$urls
  parent_folder <- result$parent_folder
  file_extension <- result$file_extension

  out <- file.path(output_dir, parent_folder)
  out_read <- gsub("\\\\", "/", out)

  # Build optional WHERE clause — user supplied, no validation
  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""

  # Check if folder already exists to prevent duplicate/mixed data
  if (dir.exists(out) && !overwrite) {
    stop("'", out, "' already exists. Pass overwrite = TRUE to overwrite.")
  } else if (dir.exists(out) && overwrite) {
    unlink(out, recursive = TRUE)
  }

  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

  read_fn <- ifelse(file_extension == ".snappy.parquet", "read_parquet", "read_csv")
  urls_sql <- paste0("['", paste(urls, collapse = "','"), "']")

  if (!is.null(partition_col)) {
    DBI::dbExecute(con, glue::glue(
      "COPY (
        SELECT {select_sql} FROM {read_fn}({urls_sql})
        {where_clause}
      )
      TO '{out_read}'
      (FORMAT PARQUET,
       PARTITION_BY {partition_col},
       ROW_GROUP_SIZE 256000,
       COMPRESSION ZSTD,
       OVERWRITE_OR_IGNORE true)"
    ))
  } else {
    DBI::dbExecute(con, glue::glue(
      "COPY (
        SELECT {select_sql} FROM {read_fn}({urls_sql})
        {where_clause}
      )
      TO '{out_read}/data.parquet'
      (FORMAT PARQUET,
       ROW_GROUP_SIZE 256000,
       COMPRESSION ZSTD,
       OVERWRITE_OR_IGNORE true)"
    ))
  }

  message("Downloaded to: ", out)
  invisible(out)
}

#' Read a downloaded Dewey dataset
#'
#' Reads a locally downloaded Dewey dataset back into R as a tibble. Use after
#' \code{download_dewey_duck()} or pass a path directly to a previously downloaded dataset.
#'
#' For advanced queries, use DuckDB directly. deweyr sets up the path for you:
#'
#' ```r
#' path_read <- gsub("\\\\", "/", "C:/your/path/to/dataset")
#' con <- DBI::dbConnect(duckdb::duckdb())
#' DBI::dbGetQuery(con, paste(
#'   "SELECT CARRIER_NAME, SUM(FULL_TIME) as total",
#'   "FROM read_parquet('", paste0(path_read, "/**/*.parquet'"), ", hive_partitioning=true)",
#'   "GROUP BY CARRIER_NAME"
#' ))
#' DBI::dbDisconnect(con)
#' ```
#'
#' @param path Path to the downloaded dataset folder (e.g. \code{"C:/dewey-downloads/airline-employment"}).
#'   Accepts the invisible return value of \code{download_dewey()} for piping.
#' @param where Optional SQL WHERE clause string (no validation — errors are on you).
#'   Example: \code{where = "CARRIER_GROUP = 'Major'"}
#'
#' @return A tibble of the dataset.
#'
#' @seealso
#' `vignette("advanced-queries", package = "deweyr")` for details on using
#' raw DuckDB SQL, window functions, and aggregations over downloaded data.
#'
#' @examples
#' \dontrun{
#' # Read after download
#' df <- read_dewey_duck("C:/dewey-downloads/airline-employment")
#'
#' # Pipe directly from download
#' df <- download_dewey_duck(api_key, data_id, base_dir, partition = "MONTH_DATE_PARSED") |>
#'   read_dewey_duck()
#'
#' # Filter on read
#' df <- read_dewey_duck("C:/dewey-downloads/airline-employment", where = "CARRIER_GROUP = 'Major'")
#' }
#'
#' @export
read_dewey_duck <- function(path, where = NULL) {
  path_read <- gsub("\\\\", "/", path)

  # Build optional WHERE clause — user supplied, no validation
  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con))

  tibble::as_tibble(DBI::dbGetQuery(con, glue::glue(
    "SELECT * FROM read_parquet('{path_read}/**/*.parquet', hive_partitioning=true) {where_clause}"
  )))
}
