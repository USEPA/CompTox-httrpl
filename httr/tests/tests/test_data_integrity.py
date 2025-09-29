"""
test_data_integrity drops the two main collections httr_raw and httr_counts and run the process_alignment_and_count process on a couple of sample_id
it then reads what data were written into these two collections and compares with what it knows should be there
"""

import os
import sys

import pytest

TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-3])

TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)

from db.mongo import openMongoParam
from schema_checks import dbs_schema_checker
from misc import configLoader
from align_and_count import align_and_count


@pytest.mark.order(13)
def test_data_integrity(calibrate):
    """
    test_data_integrity drops the two main collections httr_raw and httr_counts and run the process_alignment_and_count process on a couple of sample_id
    it then reads what data were written into these two collections and compares with what it knows should be there

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
        os.environ["HTTR_CELERY_MP"] = "N"
        al = align_and_count([0, f])
        configLoader(al)
        DB = al.openMongoParam()
        
        DB["httr_trt_grp_cmp"].drop()
        DB["httr_well"].drop()
        DB["httr_well_trt"].drop()
    
    
        # adding the data into httr_chem
    
        collections = ['httr_chem']
        chem_data = []
        for _id in range(10):
            chem_data.append({'chem_id': f'TP000{_id}',
                              'chem_name': f'TP000{_id}_name',
                              'dtxsid': f'DTXSID_{_id}'})
    
        for c in collections:
            DB[c].drop()
            for chem in chem_data:
                DB[c].insert_one(
                    {'chem_id': chem['chem_id'],
                     'chem_name': chem['chem_name'],
                     'dtxsid': chem['dtxsid']})
    
        schema_checker = dbs_schema_checker()
        errors = schema_checker.check_data_integrity(DB, al.config['db_name'])
    
        assert len(errors) == 0
        
        DB["httr_trt_grp_cmp"].drop()
        DB["httr_well"].drop()
    
        # let's make it fail now
        DB['httr_chem'].update_one(
            {'chem_id': 'TP0003'},
            {'$set': {'bottle_id': 'b_id_3'}})
        errors = schema_checker.check_data_integrity(DB, al.config['db_name'])
    
        print(f'errors are {errors}')
    
        assert len(errors) == 1
