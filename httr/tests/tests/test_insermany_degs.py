
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


@pytest.mark.order(8)
def test_insertmany_degs(calibrate):
    """
    test_insertmany_degs runs the test_insertmany_degs.R script
    the test_insertmany_degs.R script reads degs form a file and inserts the data into httr_deg
    Finally this script reads the data and ensure it is what is expected 

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None

    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_insertmany_degs_nodb.json",)
    else:
        config_files = ("tests/data/test_insertmany_degs_nodb.json","tests/data/test_insertmany_degs.json")

    for f in config_files:

        # reading db info from config file
        al = align_and_count([0, f])
        configLoader(al)
        DB = al.openMongoParam()
    
        exp = Expected(DB)
    
        # loading expected dictionary
        exp.load_expected_coll(
            calibrate,
            "_test_insertmany_degs",
            "data/expected_insertmany_degs.json")
    
        # cleanup record if any
        DB[exp.expected["data/expected_insertmany_degs.json"]["coll_name"]].drop()
    
        # running main process
        if "nodb" in f:
            os.environ["output_dir"] = "db"
        else:
            os.environ["output_dir"] = ""
            
        p = subprocess.Popen(["Rscript", "tests/tests/insertmany_degs.R"])
        print('waiting', flush=True)
        p.wait()
        print('done waiting', flush=True)
    
        exp.handle_expected_coll(
            "test_insertmany_degs",
            0,
            "data/expected_insertmany_degs.json",
            calibrate)
        #assert(False)
