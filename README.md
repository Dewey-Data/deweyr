# Working with R

This page provides a collection of practical, ready-to-use R examples to help you load, explore, filter, and analyze data on the Dewey platform. Whether you're new to R or looking for quick reference patterns, you'll find clear snippets, common workflows, and best-practice tips to support fast, reproducible research.

## Overview

Dewey makes it easy to work with large datasets in R using familiar, flexible tools like tidyverse, arrow, and DuckDB. These examples show how to load data efficiently, inspect schemas, filter rows before download, and quickly summarize large files without requiring heavy local setup.

With just a few lines of code, you can connect to your files, run performant queries, and start analyzing immediately — no complex configuration required.

## Downloading Data

### Using deweyr (Recommended)

The [`deweyr`](https://github.com/Dewey-Data/deweyr) package provides a simple way to download files from Dewey projects directly from R. The package offers two download methods:

- **`download_dewey()`** — Recommended method using [UV](https://docs.astral.sh/uv/) (automatic Python environment management, no Python installation required)
- **`download_dewey_py()`** — Traditional method using an existing Python installation with [deweypy](https://github.com/dewey-data/deweypy)

#### Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("Dewey-Data/deweyr")
```

#### Basic Download

```r
library(deweyr)

# Download to default location (./dewey-downloads)
download_dewey(
  api_key = "your-api-key",
  folder_id = "abc123"
)
```

> **First-time setup:** If UV isn't installed, `deweyr` will install it automatically. You may see a message recommending you restart your terminal for optimal performance in future runs.

#### Custom Download Location

```r
download_dewey(
  api_key = "your-api-key",
  folder_id = "abc123",
  download_path = "C:/Users/YourName/Documents/data"
)
```

#### Download from URL

You can use either a folder ID or the full Dewey URL:

```r
download_dewey(
  api_key = "your-api-key",
  folder_id = "https://api.deweydata.io/api/v1/external/data/abc123"
)
```

#### Multi-threaded Downloads

Adjust the number of workers for faster downloads (default is 8):

```r
download_dewey(
  api_key = "your-api-key",
  folder_id = "abc123",
  num_workers = 16  # Use 16 parallel workers
)
```

#### Date-Partitioned Datasets

For datasets partitioned by date, you can filter which partitions to download:

```r
# Download only data from 2024 onwards
download_dewey(
  api_key = "your-api-key",
  folder_id = "abc123",
  partition_key_after = "2024-01-01"
)

# Download only data up to a certain date
download_dewey(
  api_key = "your-api-key",
  folder_id = "abc123",
  partition_key_before = "2023-12-31"
)

# Download a specific date range
download_dewey(
  api_key = "your-api-key",
  folder_id = "abc123",
  partition_key_after = "2024-01-01",
  partition_key_before = "2024-03-31"
)
```

### Using the Dewey Client

Alternatively, you can use the [Quickstart: Dewey Client](https://deweydata.io) to download data to your local machine, and then load the downloaded files into R using the examples below.

## Loading Data into R(Studio)

### Handling Different Data Types

Datasets downloaded from Dewey may come in different file formats depending on storage requirements or download preferences. This section provides guidance on how to load your data into R, whether the files are in `.parquet` or `.csv.gz` format.

### Parquet Files

Many of Dewey's datasets are provided as `.parquet` files due to their efficient storage and query performance. To load `.parquet` files into R, you can use the `arrow` or `duckdb` packages, depending on whether you need to filter the data before bringing it into R.

#### Arrow

`arrow` provides a fast and memory-efficient way to read `.parquet` files into R. It supports loading single files or entire directories of `.parquet` files and returns the result as a tidy, in-memory dataset. `arrow` is ideal when you want to load the full dataset directly without applying filters first.

```r
# ----------------------------------------------------------------------------------
# Optional: Install packages
# Remove the "#" on the line below to install the arrow package (only needed once).
# ----------------------------------------------------------------------------------
# install.packages("arrow")

# -------------------------
# Load required libraries
# -------------------------
library(arrow)     # For working with Parquet datasets efficiently (no full in-memory load required)

# -------------------------------------------------------------
# Point to the local folder that contains Dewey Parquet files
# -------------------------------------------------------------
# This folder should contain one or more .parquet files downloaded from Dewey.
path <- "YOUR FILEPATH"

# Example:
# path <- "C:/Users/user1/Documents/dewey-downloads/mydata"

# ------------------------------------------------
# Create an Arrow Dataset from the Parquet files
# ------------------------------------------------
# open_dataset() creates a lazy Arrow Dataset that can be queried without immediately loading everything into memory.
lazy_data <- open_dataset(path, format = "parquet")

# --------------------------------------------------------------
# Materialize the full dataset into R as a data.frame / tibble
# --------------------------------------------------------------
# collect() pulls the data from disk (or remote storage) into R memory.
# For very large Dewey datasets, consider filtering or selecting columns before calling collect().
data <- collect(lazy_data)

# View the first six rows of your dataset
head(data)
```

### CSV Files

Some of Dewey's datasets are delivered in CSV format and are provided as compressed CSV files (`.csv.gz`) to reduce file size and improve download performance. These files can be loaded directly into R using the `readr` package, which efficiently reads and combines multiple compressed CSVs into a single dataset.

`duckdb` only queries and filters `.parquet` files. To utilize `duckdb` you will need to convert the files to `.parquet` first. There is a simple workflow to do this within R. The second tab of the code box below provides the coding for transforming `.csv.gz` files to `.parquet`.

```r
# ----------------------------------------------------------------------------------
# Optional: Install packages
# Remove the "#" below to install readr (only needed once)
# ----------------------------------------------------------------------------------
# install.packages("readr")

# -------------------------
# Load required libraries
# -------------------------
library(readr)      # For fast, tidy reading of CSV and CSV.GZ files

# -----------------------------------------------------------------------
# Point to the local folder that contains Dewey compressed CSV (.csv.gz)
# -----------------------------------------------------------------------
path <- "YOUR FILEPATH"

# Example:
# path <- "C:/Users/user1/Documents/dewey-downloads/mydata"

# ----------------------------------------------------------
# Load all .csv.gz files in the folder into a single dataset
# ----------------------------------------------------------
# read_csv() automatically decompresses .gz files.
files <- list.files(path, pattern = "\\.csv\\.gz$", full.names = TRUE)

data <- do.call(dplyr::bind_rows, lapply(files, read_csv))

# --------------------------------------------------------------
# View the first rows of your dataset to inspect the content
# --------------------------------------------------------------
head(data)
```

## Filter Data

### DuckDB

If you want to filter Dewey datasets in R, you can use DuckDB after the files have been downloaded to your local machine.

The best workflow is:

1. Download the dataset locally first (using `deweyr` or the Dewey Client).
2. Then use DuckDB in R to filter, reshape, and query the data before loading it into memory.

This gives you the full power of DuckDB, just after download rather than before.

#### Filtering .parquet Files

```r
# ----------------------------------------------------------------------------------
# Optional: Install packages
# Remove the "#" on the line below to install the duckdb package (only needed once).
# ----------------------------------------------------------------------------------
# install.packages("duckdb")
# install.packages("DBI")

# -------------------------
# Load required libraries
# -------------------------
library(DBI)       # For database connections
library(duckdb)    # For querying Parquet efficiently using SQL (filter before load)

# -------------------------------------------------------------
# Point to the local folder that contains Dewey Parquet files
# -------------------------------------------------------------
# This folder should contain one or more .parquet files downloaded from Dewey.
path <- "YOUR FILEPATH"

# Example:
# path <- "C:/Users/user1/Documents/dewey-downloads/mydata"

# -----------------------------------------------
# Create a DuckDB connection (in-memory database)
# -----------------------------------------------
con <- dbConnect(duckdb(), dbdir = ":memory:")

#---------------------------------------------------------------------------------
# Preview five rows from the Parquet files
# This helps view the data and see a sample of the column names and table values
#---------------------------------------------------------------------------------
sample_query <- paste0("
  SELECT *
  FROM read_parquet('", path, "/*.parquet')
  LIMIT 5
")

sample_preview <- dbGetQuery(con, sample_query)

head(sample_preview)

# -------------------------------------------------------------------------
# Query and FILTER the Parquet files BEFORE loading them into R
# -------------------------------------------------------------------------
# Replace the WHERE clause with your desired filters.
# DuckDB reads only the necessary row groups and columns from disk.
query <- paste0("
  SELECT *
  FROM read_parquet('", path, "/*.parquet')
  -- Example filters (remove the -- from the lines below to activate filters):
  -- WHERE state = 'WA'
  -- AND naics_code = '448120'
")

# --------------------------------------------------------------
# Materialize the filtered data into R as a data.frame / tibble
# --------------------------------------------------------------
# dbGetQuery() runs the SQL query and returns only the filtered rows.
data <- dbGetQuery(con, query)

# View the first six rows of your filtered dataset
head(data)

# -------------------------------
# Disconnect DuckDB when finished
# -------------------------------
dbDisconnect(con, shutdown = TRUE)
```

### DuckDB Options via deweyr

The `deweyr` package also provides convenience functions for working with DuckDB directly against Dewey datasets.

#### Download via DuckDB

```r
download_dewey_duck(
  api_key = "your-api-key",
  data_id = "dataset-from-deweydata",
  partition = "column-name-to-partition-by",
  where = NULL,
  select = NULL,
  overwrite = FALSE
)
```

#### Read Using DuckDB

```r
read_dewey_duck(
  path = "path-to-read-in-already-downloaded-data",
  where = NULL
)
```

#### Get Dewey URLs

```r
get_dewey_urls_duck(
  api_key = "your-api-key",
  data_id = "dataset-from-deweydata",
  preview = FALSE
)
```

#### Preview with DuckDB

```r
preview_dewey_duck(
  api_key = "your-api-key",
  data_id = "dataset-from-deweydata",
  limit = 10,
  where = NULL
)
```

## Data Exploration & Visualization

The following section provides a set of quick, practical Exploratory Data Analysis (EDA) tools you can run immediately after loading a Dewey dataset into R. These commands help you validate the structure of the dataset, check for missing values, understand column types, and identify potential issues before running deeper analysis. You'll generate summary statistics, inspect unique values, measure correlations between numeric fields, and visualize distributions across variables. This workflow is designed to give you a fast, high-level understanding of your dataset's shape, quality, and behavior so you can confidently move into more advanced filtering, modeling, or visualization steps.

```r
# -------------------------
# Load required libraries
# -------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

# ------------------------------------------------
# Check dataset dimensions (rows x columns)
# ------------------------------------------------
dim(data)

# ------------------------------------------------
# View structure, column types, and sample values
# ------------------------------------------------
str(data)

# ------------------------------------------------
# Get summary statistics for each column
# ------------------------------------------------
summary(data)

# ------------------------------------------------
# Count missing values in each column
# ------------------------------------------------
colSums(is.na(data))

# ------------------------------------------------
# Count unique values per column
# ------------------------------------------------
sapply(data, function(x) length(unique(x)))

# ------------------------------------------------
# Select numeric columns for correlation analysis
# ------------------------------------------------
num <- data %>% select(where(is.numeric))

# ------------------------------------------------
# Compute correlation matrix
# ------------------------------------------------
corr <- cor(num, use = "pairwise.complete.obs")
```

---

**Note:** This package requires an active Dewey account and API key. Visit [Dewey](https://deweydata.io) to learn more.
