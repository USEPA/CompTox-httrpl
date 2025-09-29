"""
test_norerun runs the align_and_count function with some samples to create some documents in the httr_raw and httr_counts collections,
and then runs again the align_and_count with the config option of norerun set to true
then it makes sure no new record have been created as expected
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


@pytest.mark.order(3)
def test_norerun():
    """
    test_norerun runs the align_and_count function with some samples to create some documents in the httr_raw and httr_counts collections,
    and then runs again the align_and_count with the config option of norerun set to true
    then it makes sure no new record have been created as expected


    Parameters:

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_httrpl_userConfig_norerun_nodb.json",)
    else: 
        config_files = ("tests/data/test_httrpl_userConfig_norerun.json", "tests/data/test_httrpl_userConfig_norerun_nodb.json")
    
    for f in config_files:
        # reading db info from config file
    
        al = align_and_count([0, f])
        config_dir = al.config_dir #al.config.rsplit('/', 1)[0] + '/'
        configLoader(al)
        DB = al.openMongoParam()
    
        # running main process with rerun set to Yes to make sure records are
        # created
        al.process_alignment_and_count()
    
        exp = Expected(DB)
    
        # let's save the _ids for later comparision
        _ids = {}
        for elem in exp.sample_data["test_norerun"]:
            _ids[elem] = {}
            for key in exp.expected:
                _ids[elem][exp.expected[key]["coll_name"]] = DB[exp.expected[key][
                    "coll_name"]].find_one({"sample_id": elem}, {"_id": 1})
        print(f"ids are {_ids}", flush=True)
    
        # running now with rerun False to prevent record creation
        al = align_and_count([0, "tests/data/test_httrpl_userConfig_norerun.json"])
        al.process_alignment_and_count()
    
        # checking no new records have been created, i.e. records have same _id
    
        for elem in exp.sample_data["test_norerun"]:
            for key in exp.expected:
                res = DB[exp.expected[key]["coll_name"]].find_one(
                    {"sample_id": elem}, {"_id": 1})
                assert res == _ids[elem][exp.expected[key]["coll_name"]]
