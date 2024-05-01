"""
test_update_qc_frac drops the collection httr_raw  and run the process_gc_frac process on a couple of sample_id
it then reads what data was written into these collection and compares with what it knows should be there
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
from update_gc_frac import update_gc_frac
from db.mongo import openMongoParam
from misc import Expected, configLoader


@pytest.mark.order(21)
def test_update_gc_frac(calibrate):
    """
    test_update_gc_frac drops the collection httr_raw  and run the process_gc_frac process on a couple of sample_id
    it then reads what data was written into these collection and compares with what it knows should be there

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

        raw_filler = align_and_count([0, f])
        config = configLoader(raw_filler)
        DB = raw_filler.openMongoParam()
        #DB = openMongoParam(host=config['db_host'], db=config['db_name'])
        DB['httr_raw'].drop()
        
        # running main process
        raw_filler.process_alignment_and_count()
        
        # now httr_raw has data
        
        # reading db info from config file
        al = update_gc_frac([0, f])
        config = configLoader(al)
        #DB = openMongoParam(host=config['db_host'], db=config['db_name'])
        DB = al.openMongoParam()
        exp = Expected(DB)
    
        # loading expected dictionary
        exp.load_expected_coll(
            calibrate,
            "_test_update_gc_frac",
            "data/expected_test_update_gc_frac.json")
    
        # running process_gc_frac
        al.process_gc_frac()
    
        # checking results against expected
        
        exp.handle_expected_coll(
            "test_update_gc_frac",
            0,
            "data/expected_test_update_gc_frac.json",
            calibrate)
