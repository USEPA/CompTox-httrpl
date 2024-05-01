"""
test_getqcdefault simply calls the getQCdefault.R script to change some of the QCDefault values and verify that they have changed successfully.
Then the getQCdefault.R script checks for new values of these default parameters and writes out if they match what they were changed to. That status is written out
to the httr_counts_stats collection which this script checks
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
from misc import configLoader


@pytest.mark.order(11)
def test_getqcdefault(calibrate):
    """
    test_getqcdefault simply calls the getQCdefault.R script to change some of the QCDefault values and verify that they have changed successfully.
    Then the getQCdefault.R script checks for new values of these default parameters and writes out if they match what they were changed to. That status is written out
    to the httr_counts_stats collection which this script checks

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """

    # reading db info from config file
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_getqcdefault_nodb.json",)
    else:
        config_files = ("tests/data/test_getqcdefault.json", "tests/data/test_getqcdefault_nodb.json")
        
    for f in config_files:
        al = align_and_count([0, f])
        configLoader(al)
        DB = al.openMongoParam()
    
        # cleanup record if any
        DB["httr_counts_stats"].drop()
    
        # running main process
        if "nodb" in f:
            os.environ["output_dir"] = "db"
        else:
            os.environ["output_dir"] = ""
        p = subprocess.Popen(["Rscript", "tests/tests/getQCdefault.R"])
        print('waiting', flush=True)
        p.wait()
        print('done waiting', flush=True)
    
        # checking results against expected
        res = DB["httr_counts_stats"].find_one({"getQCdefaultTestResult": "OK"})
        assert res
