"""
test_warnings sets the 'stop on warning' to false and checks that processing was successful and the documents were created (handle_expected is called with
True in last parameter) which asks to check the documents. Then test_warnings sets the 'stop on warning' to True and checks that no documents was created
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


@pytest.mark.order(5)
def test_warnings(calibrate):
    """
    test_warnings sets the 'stop on warning' to false and checks that processing was successful and the documents were created (handle_expected is called with
    True in last parameter) which asks to check the documents. Then test_warnings sets the 'stop on warning' to True and checks that no documents was created


    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        warnings = []
    else:
        warnings = ["tests/data/test_httrpl_userConfig_warning_false.json",
                "tests/data/test_httrpl_userConfig_warning_true.json"]
                
    warnings_nodb = ["tests/data/test_httrpl_userConfig_warning_false.json",
                "tests/data/test_httrpl_userConfig_warning_true.json"]
                
    db_schemes = (warnings,warnings_nodb)

    for db_scheme in db_schemes:
        for wtype in db_scheme:
    
            al = align_and_count([0, wtype])
            configLoader(al)
            DB = al.openMongoParam()
    
            exp = Expected(DB)
    
            # loading expected dictionary
            exp.load_expected(calibrate, "_test_warnings")
    
            # cleanup record if any
            for key in exp.expected:
                if exp.expected[key]["coll_name"] in DB.list_collection_names():
                    for elem in exp.sample_data["test_warnings"]:
                        print(f"elem is {elem}")
                        DB[exp.expected[key]["coll_name"]].delete_many(
                            {"sample_id": elem})
    
            try:
                print(f"strict is {al.config['strict']}", flush=True)
                al.log.strict = al.config['strict']
                al.log.warning("Generating a warning...")
                # running main process
                al.process_alignment_and_count()
    
            except SystemExit:
                print(f"SystemExit detected for {wtype}", flush=True)
    
            # checking results against expected
    
            exp.handle_expected("test_warnings", calibrate, "false" in wtype)
