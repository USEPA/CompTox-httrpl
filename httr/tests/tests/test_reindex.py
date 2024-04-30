"""
test_reindex uses two different configuration files, one called test_httrpl_userConfig_reindex_true that requires reindexing and one
test_httrpl_userConfig_reindex_false that doesn't. it runs align_and_count off of the first one and checks the alignmentFiles/v1.2_hisat2Index/.1.ht2 file
to make sure it has been rebuilt. It then runs align_and_count off of the second file and expects the alignmentFiles/v1.2_hisat2Index/.1.ht2 file to
not have been rebuilt

"""

import os
import sys

import pytest
from misc import configLoader

TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-3])
TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB


import json

from align_and_count import *
from align_and_count import align_and_count
from misc import configLoader


@pytest.mark.order(4)
def test_reindex():
    """
    test_reindex uses two different configuration files, one called test_httrpl_userConfig_reindex_true that requires reindexing and one
    test_httrpl_userConfig_reindex_false that doesn't. it runs align_and_count off of the first one and checks the alignmentFiles/v1.2_hisat2Index/.1.ht2 file
    to make sure it has been rebuilt. It then runs align_and_count off of the second file and expects the alignmentFiles/v1.2_hisat2Index/.1.ht2 file to
    not have been rebuilt

    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:
    None
    Returns:
    test runs assert statements and will post assertion failures on the screen through pytest. The test function itself returns None


    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = (
            "tests/data/test_httrpl_userConfig_reindex_true_nodb.json",\
            "tests/data/test_httrpl_userConfig_reindex_false_nodb.json")
    else:
        config_files = (
            "tests/data/test_httrpl_userConfig_reindex_true.json",\
            "tests/data/test_httrpl_userConfig_reindex_false.json",\
            "tests/data/test_httrpl_userConfig_reindex_true_nodb.json",\
            "tests/data/test_httrpl_userConfig_reindex_false_nodb.json")
              
    for i in range(0,len(config_files),2):

        # 1 - loading config with reindex set to true
        al = align_and_count([0, config_files[i]])
        configLoader(al)
    
        # collecting last modified times prior to running
        ind1 = al.config_dir + al.config['db_ind'] + '.1.ht2'
        time_prior = os.path.getmtime(ind1) if os.path.exists(ind1) else 0
    
        # running main process
        al.process_alignment_and_count()
    
        # checking that reindexing did occur
        assert(os.path.exists(ind1))
        time_after = os.path.getmtime(ind1)
    
        print(f"time_prior is {time_prior} time_after is {time_after}", flush=True)
        assert(time_after != time_prior)
    
        # 2 - loading config with reindex set to false
        al = align_and_count([0, config_files[i+1]])
        configLoader(al)
    
        # running main process
        al.process_alignment_and_count()
    
        # checking that reindexing did not occur
        assert(os.path.exists(ind1))
        time_after2 = os.path.getmtime(ind1)
    
        print(
            f"time_prior is {time_after} time_after is {time_after2}",
            flush=True)
        assert(time_after == time_after2)
