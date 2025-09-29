"""
test_missing_fastq changes the name of an expected fastq file, then runs the align_and_count process, and verify the data related
to the missing fastq file didn't lead to document created in the two collections
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


@pytest.mark.order(2)
def test_missing_fastq(calibrate):
    """
    test_missing_fastq changes the name of an expected fastq file, then runs the align_and_count process, and verify the data related
    to the missing fastq file didn't lead to document created in the two collections


    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_httrpl_userConfig_missingfastq_nodb.json",)
    else:
        config_files = ("tests/data/test_httrpl_userConfig_missingfastq.json","tests/data/test_httrpl_userConfig_missingfastq_nodb.json")

    for f in config_files:
        # reading db info from config file
        al = align_and_count([0, f])
        configLoader(al)
        DB = al.openMongoParam()
    
        exp = Expected(DB)
    
        # loading expected dictionary
        exp.load_expected(calibrate, "_test_missing_fastq")
    
        # cleanup record if any
        for key in exp.expected:
            if exp.expected[key]["coll_name"] in DB.list_collection_names():
                for elem in exp.sample_data["test_missing_fastq"]:
                    DB[exp.expected[key]["coll_name"]].delete_many(
                        {"sample_id": elem})
    
        # instead or renaming the file, we just alter the file name
        exp.sample_data["test_missing_fastq"][0] += "2"
    
        print(
            f"sample data is {exp.sample_data['test_missing_fastq'][0]}",
            flush=True)
    
        # running main process
        al.process_alignment_and_count()
    
        # checking results against expected for all sample except first one
        if calibrate:
            exp.write_expected("test_missing_fastq", start=1)
        else:
            exp.check_expected("test_missing_fastq", start=1)
    
            # check first sample not generated
            for key in exp.expected:
                res = DB[exp.expected[key]["coll_name"]].find_one({
                    "sample_id": exp.sample_data["test_missing_fastq"][0]},
                    exp.expected[key]["projection"])
                assert res is None
