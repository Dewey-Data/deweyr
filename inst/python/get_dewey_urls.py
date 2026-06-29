# /// script
# dependencies = ["deweypy", "duckdb"]
# ///
import sys
import json
import re
import traceback
import duckdb
from deweypy.auth import set_api_key
from deweypy.download.synchronous import api_request, get_dataset_files

# Sentinel string for "argument not provided". Chosen to be wildly unlikely
# to collide with any real partition_key, file_name, or other user value.
# Must stay in lockstep with R/duckdb.r build_get_dewey_urls_args().
NONE_SENTINEL = "__DEWEYR_NULL__"


def _arg(i):
    if i >= len(sys.argv):
        return None
    v = sys.argv[i]
    return None if v == NONE_SENTINEL else v


def fail(message, **extra):
    """Emit a structured JSON error to stderr and exit non-zero."""
    payload = {"error": message}
    payload.update(extra)
    print(json.dumps(payload), file=sys.stderr)
    sys.exit(1)


try:
    api_key = sys.argv[1]
    data_id = sys.argv[2]
    file_name = _arg(3)
    preview = sys.argv[4].lower() == "true"
    partition_key_after = _arg(5)
    partition_key_before = _arg(6)
except IndexError:
    fail(
        "Insufficient arguments. Expected: api_key data_id file_name preview "
        "partition_key_after partition_key_before"
    )

set_api_key(api_key)

try:
    if preview:
        # Single-page hit avoids paginating the full manifest, which hangs on
        # huge datasets like SafeGraph Visits.
        params = {"page": 1}
        if partition_key_after:
            params["partition_key_after"] = partition_key_after
        if partition_key_before:
            params["partition_key_before"] = partition_key_before
        resp = api_request(
            "GET",
            f"/v1/external/data/{data_id}/files",
            params=params,
        ).json()
        files = resp.get("download_links", [])
    else:
        files = get_dataset_files(
            data_id,
            partition_key_after=partition_key_after,
            partition_key_before=partition_key_before,
        )
except Exception as e:
    fail(
        f"Dewey API request failed: {type(e).__name__}: {e}",
        traceback=traceback.format_exc(),
    )

if not files:
    fail(
        "No files matched the given partition_key range",
        partition_key_after=partition_key_after,
        partition_key_before=partition_key_before,
        preview=preview,
    )

try:
    first = files[0]
    if preview:
        urls = first["link"]
        file_names = []
        file_sizes = []
    else:
        urls = [f["link"] for f in files]
        # Stable per-file identity. Unlike `link` (a fresh download-link UUID on
        # every call), file_name is constant, so callers can order the manifest
        # deterministically across runs — which is what makes resumed downloads
        # land on the same batches each time.
        file_names = [f["file_name"] for f in files]
        # Per-file sizes let callers verify a download is complete (every file
        # present at its expected size) without scraping console output.
        file_sizes = [f.get("file_size_bytes") for f in files]
    file_extension = first["file_extension"]
    raw_file_name = first["file_name"]
except KeyError as e:
    fail(f"Malformed file entry from Dewey API (missing key {e!s})")

if not file_name:
    parent_folder = re.sub(r"[-_]\d.*$", "", raw_file_name)
    parent_folder = re.sub(r"-data$", "", parent_folder) + "-duckdb"
else:
    parent_folder = file_name

# In preview mode, files only represents page 1 — total bytes would be
# misleading. Report None so callers don't treat it as a true total.
if preview:
    file_size_bytes = None
else:
    try:
        file_size_bytes = sum(f.get("file_size_bytes", 0) for f in files)
    except Exception:
        file_size_bytes = None

cols = []
if not preview:
    try:
        read_fn = "read_parquet" if file_extension == ".snappy.parquet" else "read_csv"
        first_url = urls[0] if isinstance(urls, list) else urls
        con = duckdb.connect()
        con.execute("INSTALL httpfs; LOAD httpfs;")
        cols = (
            con.execute(f"SELECT * FROM {read_fn}(['{first_url}']) LIMIT 0")
            .df()
            .columns.tolist()
        )
        con.close()
    except Exception:
        cols = []  # R will fall back gracefully

print(
    json.dumps(
        {
            "urls": urls,
            "file_names": file_names,
            "file_sizes": file_sizes,
            "parent_folder": parent_folder,
            "file_extension": file_extension,
            "partition_key": first.get("partition_key"),
            "file_size_bytes": file_size_bytes,
            "cols": cols,
        }
    )
)
