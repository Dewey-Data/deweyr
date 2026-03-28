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
get_dewey_urls <- function(api_key, data_id, file_name = NULL, preview = FALSE) {
  if (!check_uv()) {
    install_uv()
    message("Restarting the terminal will increase speed of future runs")
  }
  data_id <- parse_url(data_id)
  script <- system.file("python/get_dewey_urls.py", package = "deweyr")
  args <- c(
    "run", "--python", "3.13", script,
    api_key,
    data_id,
    ifelse(is.null(file_name), "None", file_name),
    tolower(as.character(preview))
  )
  result_raw <- system2("uv", args = args, stdout = TRUE, stderr = "stderr.txt")
  cat(readLines("stderr.txt"), sep = "\n")
  jsonlite::fromJSON(paste(result_raw, collapse = ""))
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
#' # Download and read in one step
#' df <- download_dewey_duck(api_key, data_id, partition = "MONTH_DATE_PARSED") |>
#'   read_dewey()
#' }
#'
#' @export
download_dewey_duck <- function(api_key, data_id, output_dir = get_download_dir(), partition, overwrite = FALSE, file_name = NULL, where = NULL, select = NULL) {
  result <- get_dewey_urls(api_key, data_id, file_name = file_name)
  cols <- result$cols # ✅ no second call needed

  # fallback in case cols came back empty
  if (length(cols) == 0) {
    cols <- colnames(preview_dewey_duck(api_key, data_id, limit = 0))
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
