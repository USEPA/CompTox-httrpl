"""
test_qcBatch_filter loads data into httr_well_trt and calls the qcBatch R function to process the data found there
and write out its results into httr_counts_qc
Then we compare what we find there with what we know (expected) we should expect
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


@pytest.mark.order(14)
def test_qcBatch_filter(calibrate):
    """
    test_qcBatch_filter loads data into httr_well_trt and calls the qcBatch R function to process the data found there
    and write out its results into httr_counts_qc
    Then we compare what we find there with what we know (expected) we should expect

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
    
        check_and_build_httr_probe(DB)
    
        exp = Expected(DB)
    
        print(f"calibrate {calibrate}", flush=True)
    
        # loading expected dictionary
        exp.load_expected_coll(
            calibrate, "_test_qcBatch_filter",
            "data/expected_counts_qc_filter.json")
    
        # cleanup record if any
        DB[exp.expected["data/expected_counts_qc_filter.json"]["coll_name"]].drop()
    
        # add/remove document into/from httr_well_trt if needed
        if DB.httr_well_trt.count_documents({'sample_id': 'TC00283151_D21_2'}) == 0:
            print('inserting TC00283151_D21_2 into httr_well_trt collection')
            DB.httr_well_trt.insert_one({'sample_id': 'TC00283151_D21_2'})
        if DB.httr_well_trt.count_documents({'sample_id': 'TC00283151_C16_2'}) != 0:
            print('removing TC00283151_C16_2 from httr_well_trt collection')
            DB.httr_well_trt.delete_many({'sample_id': 'TC00283151_C16_2'})
    
        # we need to make sure that httr_counts has data for our candidates:
        if exp.expected["data/expected_counts_qc_filter.json"]["coll_name"] in DB.list_collection_names():
            for elem in exp.sample_data["test_qcBatch_filter"]:
                if len(
                    list(
                        DB
                        [exp.expected
                            ["data/expected_counts_qc_filter.json"]
                            ["coll_name"]].find({"sample_id": elem}))) == 0:
                    al.process_alignment_and_count()
                    break
    
        # running main process
        if "nodb" in f:
            os.environ["output_dir"] = "db"
        else:
            os.environ["output_dir"] = ""
        p = subprocess.Popen(["Rscript", "tests/tests/qcBatch_filter.R"])
        print('waiting', flush=True)
        p.wait()
        print('done waiting', flush=True)
    
        # checking results against expected
    
        if calibrate:
            exp.handle_expected_coll(
                "test_qcBatch_filter",
                0,
                "data/expected_counts_qc_filter.json",
                calibrate)
        else:
            exp.check_expected_doc(
                expected_data="data/expected_counts_qc_filter.json",
                doc_id="TC00283151_D21_2",
                normal_processing=True)
            exp.check_expected_doc(
                expected_data="data/expected_counts_qc_filter.json",
                doc_id="TC00283151_C16_2",
                normal_processing=False)
        #assert(False)
