"""
test_fill_dose_Welltrt runs the filldosewelltrt.R script 
the filldosewelltrt.R writes out its results into the httr_dose_httr_well_trt which this script picks up and compares with what it expects

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


@pytest.mark.order(18)
def test_fill_dose_Welltrt(calibrate):
    """
    test_fill_dose_Welltrt runs the filldosewelltrt.R script 
    the filldosewelltrt.R writes out its results into the httr_dose_httr_well_trt which this script picks up and compares with what it expects


    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """

    # reading db info from config file
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_fill_dose_well_trt_nodb.json",)
    else:
        config_files = ("tests/data/test_fill_dose_well_trt.json", "tests/data/test_fill_dose_well_trt_nodb.json")
    for f in config_files:
        al = align_and_count([0, f])
        configLoader(al)
        DB = al.openMongoParam()
    
        exp = Expected(DB)
    
        # loading expected dictionary
        exp.load_expected_coll(
            calibrate,
            "_test_fill_dose_welltrt",
            "data/expected_fill_dose_welltrt.json")
    
        # cleanup record if any
        DB[exp.expected["data/expected_fill_dose_welltrt.json"]["coll_name"]].drop()
        
    
        # running main process
        if "nodb" in f:
            os.environ["output_dir"] = "db"
        else:
            os.environ["output_dir"] = ""
        p = subprocess.Popen(["Rscript", "tests/tests/fill_dose_welltrt.R"])
        print('waiting', flush=True)
        p.wait()
        print('done waiting', flush=True)
    
        # checking results against expected
    
        exp.handle_expected_coll(
            "test_fill_dose_welltrt",
            0,
            "data/expected_fill_dose_welltrt.json",
            calibrate)
        #assert(False)
