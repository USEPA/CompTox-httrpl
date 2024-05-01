"""
test_update_qcFlag makes a small change in the httr_count_qc collection (for qc_flag), runs the qcflag_update.R script and expects the
httr_counts_qc to have generated a history update on that filled/value
We then compare that with what we know should be recorded there
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
from misc import Expected, configLoader


@pytest.mark.order(13)
def test_update_qcFlag(calibrate):
    """
    test_update_qcFlag makes a small change in the httr_count_qc collection (for qc_flag), runs the qcflag_update.R script and expects the
    httr_counts_qc to have generated a history update on that filled/value
    We then compare that with what we know should be recorded there


    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """
    # reading db info from config file
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_httrpl_userConfig_main_nodb.json",)
    else:
        config_files = ("tests/data/test_httrpl_userConfig_main.json", "tests/data/test_httrpl_userConfig_main_nodb.json")
        
    for f in config_files:
        al = align_and_count([0, f])
        configLoader(al)
        DB = al.openMongoParam()
    
        exp = Expected(DB)
    
        print(f"calibrate {calibrate}", flush=True)
    
        # loading expected dictionary
        exp.load_expected_coll(
            calibrate,
            "_test_update_qcFlag",
            "data/expected_update_qcFlag.json")
    
        # making sure we have the required data in httr_counts_qc to start with
        col = exp.expected["data/expected_update_qcFlag.json"]["coll_name"]
        DB[col].drop()
    
        for k, v in exp.expected["data/expected_update_qcFlag.json"][
                "expected_value"].items():
            DB[col].insert_one(v)
            DB[col].update_one(
                {'sample_id': k},
                {'$set': {'qc_flag': 'LOW_NSIG80'}})
            v.pop('_id')
    
        # running main process
        if "nodb" in f:
            os.environ["output_dir"] = "db"
        else:
            os.environ["output_dir"] = ""
        p = subprocess.Popen(["Rscript", "tests/tests/qcflag_update.R"])
        print('waiting', flush=True)
        p.wait()
        print('done waiting', flush=True)
    
        # checking results against expected
        exp.handle_expected_coll(
            "test_update_qcFlag",
            0,
            "data/expected_update_qcFlag.json",
            calibrate)
