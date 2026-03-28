# /// script
# dependencies = ["deweypy", "duckdb"]
# ///
import sys
import json
import re
import duckdb
from deweypy.auth import set_api_key
from deweypy.download.synchronous import get_dataset_files

api_key = sys.argv[1]
data_id = sys.argv[2]
file_name = sys.argv[3] if sys.argv[3].lower() != "none" else None
preview = sys.argv[4].lower() == "true"

set_api_key(api_key)
files = get_dataset_files(data_id)

urls = files[0]["link"] if preview else [f["link"] for f in files]

if not file_name:
    file_name = files[0]["file_name"]
    parent_folder = re.sub(r"[-_]\d.*$", "", file_name)
    parent_folder = re.sub(r"-data$", "", parent_folder) + "-duckdb"
else:
    parent_folder = file_name

file_extension = files[0]["file_extension"]

# ✅ Get column names in the same subprocess
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
    except Exception as e:
        cols = []  # R will fall back gracefully

print(
    json.dumps(
        {
            "urls": urls,
            "parent_folder": parent_folder,
            "file_extension": file_extension,
            "partition_key": files[0]["partition_key"],
            "file_size_bytes": sum(f["file_size_bytes"] for f in files),
            "cols": cols,  # ✅ new field
        }
    )
)
