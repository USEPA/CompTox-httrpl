"""
test_main drops the two main collections httr_raw and httr_counts and run the process_alignment_and_count process on a couple of sample_id
it then reads what datas was written into these two collections and compares with what it knows should be there
"""

import os
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


@pytest.mark.order(1)
def test_main(calibrate):
    """
    test_main drops the two main collections httr_raw and httr_counts and run the process_alignment_and_count process on a couple of sample_id
    it then reads what datas was written into these two collections and compares with what it knows should be there

    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_httrpl_userConfig_main_nodb.json",)
    else:
        config_files = ("tests/data/test_httrpl_userConfig_main.json","tests/data/test_httrpl_userConfig_main_nodb.json")

    for f in config_files:

        # reading db info from config file
        al = align_and_count([0,f])
        configLoader(al)
        DB = al.openMongoParam()
        exp = Expected(DB)
    
        # loading expected dictionary
        exp.load_expected(calibrate, "_test_main")
    
        DB['httr_raw'].drop()
        DB['httr_counts'].drop()
    
        # cleanup record if any
        #for key in exp.expected:
        for key in ("data/expected_raw.json","data/expected_counts.json"):
            if exp.expected[key]["coll_name"] in DB.list_collection_names():
                for elem in exp.sample_data["test_main"]:
                    DB[exp.expected[key]["coll_name"]].delete_many(
                        {"sample_id": elem})
    
        # running main process
        al.process_alignment_and_count()
    
        # checking results against expected
    
        exp.handle_expected("test_main", calibrate)

        
