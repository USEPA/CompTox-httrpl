"""
test_countgini runs the qccountgini.R script after it made sure there were documents to process inside the httr_counts collection (if none found then
it runs the align_and_count process to create them)
the qccountgini.R writes out its results (in terms of gini_coef data) into the httr_counts_stats which this script picks up and compares with what it expects

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


@pytest.mark.order(8)
def test_countgini(calibrate):
    """
    test_countgini runs the qccountgini.R script after it made sure there were documents to process inside the httr_counts collection (if none found then
    it runs the align_and_count process to create them)
    the qccountgini.R writes out its results (in terms of gini_coef data) into the httr_counts_stats which this script picks up and compares with what it expects


    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_countgini_nodb.json",)
    else:
        config_files = ("tests/data/test_countgini.json","tests/data/test_countgini_nodb.json")
    
    for f in config_files:

        # reading db info from config file
        al = align_and_count([0, f])
        configLoader(al)
        DB = al.openMongoParam()
    
        exp = Expected(DB)
    
        # loading expected dictionary
        exp.load_expected_coll(
            calibrate,
            "_test_countgini",
            "data/expected_countgini.json")
    
        # cleanup record if any
        DB[exp.expected["data/expected_countgini.json"]["coll_name"]].drop()
    
        # we need to make sure that httr_counts has data for our candidates:
        if exp.expected["data/expected_counts.json"]["coll_name"] in DB.list_collection_names():
            for elem in exp.sample_data["test_countgini"]:
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
        p = subprocess.Popen(["Rscript", "tests/tests/qccountgini.R"])
        print('waiting', flush=True)
        p.wait()
        print('done waiting', flush=True)
    
        # checking results against expected
    
        exp.handle_expected_coll(
            "test_countgini",
            0,
            "data/expected_countgini.json",
            calibrate)
