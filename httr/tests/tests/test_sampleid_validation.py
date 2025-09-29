"""
test_sampleid_validation checks that, provided a list of sample_ids that we feed into the different collections, when we
call check_sample_ids, it will be successfull in finding the same sample_ids in each collection. Then we remove one of the
sample_id from one collection and verify check_sample_ids return expected errors
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
from align_and_count import align_and_count
from misc import configLoader


@pytest.mark.order(14)
def test_sampleid_validation(calibrate):
    """
    test_sampleid_validation checks that, provided a list of sample_ids that we feed into the different collections, when we
    call check_sample_ids, it will be successfull in finding the same sample_ids in each collection. Then we remove one of the
    sample_id from one collection and verify check_sample_ids return expected errors

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
        al = align_and_count([0, f])
        configLoader(al)
        DB = al.openMongoParam()
    
        # adding the sample_ids in the collections
    
        collections = [
            'httr_raw',
            'httr_counts',
            'httr_counts_qc',
            'httr_well',
            'httr_well_trt']
        sample_ids = [
            'TC00283151_D21',
            'TC00283151_C16',
            'TC00283171_L10',
            'TC00283171_J02',
            'TC00283171_C09',
            'TC00283209_K19',
            'TC00283209_F02',
            'TC00283209_C07',
            'TC00283209_E07',
            'TC00283151_C16']
    
        for c in collections:
            DB[c].drop()
            for sample in sample_ids:
                DB[c].insert_one({'sample_id': sample})
    
        schema_checker = dbs_schema_checker()
        errors = schema_checker.check_sample_ids(DB, al.config['db_name'])
    
        assert len(errors) == 0
    
        # let's make it fail now
        DB['httr_raw'].delete_one({'sample_id': 'TC00283151_D21'})
        errors = schema_checker.check_sample_ids(DB, al.config['db_name'])
    
        # cleanup to prevent collection nb of docs mismatch
        DB['httr_raw'].drop()
        DB['httr_counts'].drop()
        
        # running main process to repopulate httr_raw and httr_counts
        al.process_alignment_and_count()
    
        assert len(errors) == 4
