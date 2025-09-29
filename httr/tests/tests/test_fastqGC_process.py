"""
test_fastqGC_process allows to run the fastqGC function from the fastq file and verifies that the output of that function
remains the same as first collected
"""

import os
import sys

import pytest
import gzip

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
from gexp.fastq import fastqGC


@pytest.mark.order(20)
def test_fastqGC_process(calibrate):
    """
    test_fastqGC_process leverages the misc features by getting the full path for the fq_file (the fastq file), loading it, 
    gzipping it and passing it as a parameter to fastqGC for processing.
    The results are then saved in a temporarly collection called httr_fastqGC 
    
    When calibrating, what is found in the httr_fastqGC is commited to expected file
    when checking, what is found in httr_fastqGC is compared to what is read from the expected file
    

    Parameters:
    calibrate: boolean  if True, then we run same process but do not compare in the end, simply build and save the results so that they can
    be loaded and compared with when we run the test with calibrate = False

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_fastqGC_process_nodb.json",)
    else:
        config_files = ("tests/data/test_fastqGC_process.json","tests/data/test_fastqGC_process_nodb.json")
    
    for f in config_files:
        # reading db info from config file
        al = align_and_count([0, f])
        configLoader(al)
        al.check_fastq_folders()
        DB = al.openMongoParam()
        exp = Expected(DB)
    
        # loading expected dictionary
        exp.load_expected_coll(
            calibrate,
            "_test_fastqGC_process",
            "data/expected_fastqGC.json")
    
    
        DB['httr_fastqGC'].drop()
         
        # running main process
        for elem in al.config['sample_key']:
          with open(elem['fq_file'],'rb') as f_in:
            with gzip.open(elem['fq_file']+'.gz','wb') as f_out:
              f_out.writelines(f_in)
              
          res = fastqGC(elem['fq_file'] + '.gz')
          res['sample_id']=elem['sample_id']
          DB['httr_fastqGC'].insert_one(res)
          
    
        # checking results against expected
    
        exp.handle_expected_coll(
            "test_fastqGC_process",
            0,
            "data/expected_fastqGC.json",
            calibrate)
        
        