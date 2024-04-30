"""
test_countqcflag fires up qcflag.R which runs countQC for each sample found in httr_counts and writes out its results into
httr_counts_stats (qc_flag data) which this scripts reads and compare with what it expects
"""

import os
import subprocess
import sys

import pytest

TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-3])
TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB


from align_and_count import *
from align_and_count import align_and_count
from db.mongo import openMongoParam
from misc import Expected, check_and_build_httr_probe, configLoader


@pytest.mark.order(9)
def test_countqcflag(calibrate):
    """
    test_countqcflag fires up qcflag.R which runs countQC for each sample found in httr_counts and writes out its results into
    httr_counts_stats (qc_flag data) which this scripts reads and compare with what it expects

    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """

    # reading db info from config file
    
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_countqcflag_nodb.json",)
    else:  
        config_files = ("tests/data/test_countqcflag.json", "tests/data/test_countqcflag_nodb.json")
    
    for f in config_files:
    
        al = align_and_count([0, f])
        configLoader(al)
        DB = al.openMongoParam()
    
        check_and_build_httr_probe(DB)
    
        exp = Expected(DB)
    
        # loading expected dictionary
        exp.load_expected_coll(
            calibrate,
            "_test_countqcflag",
            "data/expected_countqcflag.json")
    
        # cleanup record if any
        DB[exp.expected["data/expected_countqcflag.json"]["coll_name"]].drop()
    
        # we need to make sure that httr_counts has data for our candidates:
        if exp.expected["data/expected_counts.json"]["coll_name"] in DB.list_collection_names():
            for elem in exp.sample_data["test_countqcflag"]:
                if len(
                    list(
                        DB
                        [exp.expected["data/expected_counts.json"]
                         ["coll_name"]].find({"sample_id": elem}))) == 0:
                    al.process_alignment_and_count()
                    break
    
        # running main process
        if "nodb" in f:
            os.environ["output_dir"] = "db"
        else:
            os.environ["output_dir"] = ""
        p = subprocess.Popen(["Rscript", "tests/tests/qcflag.R"])
        print('waiting', flush=True)
        p.wait()
        print('done waiting', flush=True)
    
        # checking results against expected
    
        exp.handle_expected_coll(
            "test_countqcflag",
            0,
            "data/expected_countqcflag.json",
            calibrate)
