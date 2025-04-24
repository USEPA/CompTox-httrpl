import os
from pathlib import Path
import sys

httr_path = Path(__file__).resolve().parents[1] / 'lib'
sys.path.append(str(httr_path))
print(f"Added {httr_path} to package path")

from db.well import buildAllWells
from db.jsonDB import json_DB
from httrplcore import PipelineLogger

if __name__ == "__main__":
    args = sys.argv

    if len(args) == 1:
        json_db_dir = os.getenv("HTTRPL_DATA_DIR", "/var/lib/httrpl")
    elif args[1] in ["-h", "--help"]:
        print("Python scipt used to build the httr_well collection.\nUsage:\n\tpython build_wells.py [JSON_DB_DIRECTORY]") 
        print("Default json DB directory is the 'HTTRPL_DATA_DIR' environment variable or /var/lib/httrpl")
        exit(0)
    elif len(args) > 2:
        raise ValueError(f"Too many arguments were given, got {len(args)} expected 0 or 1.")
    else:
        json_db_dir = args[1]

    print(f"Running build_wells.py with local database: {json_db_dir}")
    db = json_DB(json_db_dir)
    buildAllWells(DB = db, rerun=True, log=PipelineLogger(strict=False, dbg=True))
