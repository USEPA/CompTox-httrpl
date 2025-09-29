"""
This test will run the align_and_count process and reports on
how many batches are being used and compare that with the expected number
"""


import os
import sys

import pandas as pd
import pytest
from misc import configLoader

TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-3])
print(f"TOP is {TOP}", flush=True)
TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB


import threading
import time

import psutil
from align_and_count import *
from align_and_count import align_and_count


class checker(threading.Thread):
    def __init__(self, processes, batchsize, samples):
        self.do_run = True
        self.processes = processes
        self.batchsize = batchsize
        self.samples = samples

        threading.Thread.__init__(self)

    def checkIfProcessRunning(self, processName):
        '''
        Check if there is any running process that contains the given name processName.

        Parameter:
        processName: str   name of process to watch
        Return:
        count: int    number of times the given process is fired up
        '''
        # Iterate over the all the running processes
        count = 0

        for proc in psutil.process_iter():
            try:
                # Check if process name contains the given name string.
                if processName == proc.name():
                    count += 1

            except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                print("zombie", flush=True)

        return count

    def run(self):
        '''
        run loop of the thread obeserving the target process

        Paramter:
        None
        Returns:
        None

        '''

        mode = 0
        detected_batches = 0

        while self.do_run:

            time.sleep(0.001)

            pytest_ins = self.checkIfProcessRunning("pytest")

            if pytest_ins == 1 and mode == 1:
                detected_batches += 1
                mode = 0
            if pytest_ins == self.processes + 1:
                mode = 1

        print(f"detected {detected_batches} batches", flush=True)
        offset = 1 if self.samples % self.batchsize != 0 else 0
        assert self.samples // self.batchsize + offset == detected_batches


@pytest.mark.order(7)
def test_batch_thread_nb():
    """
    test_batch_thread_nb fires up a checker thread that will check that align_and_count is batching its processing according to the number
    of batches as calculated from the config batchsize parameter

    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:
    None
    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_httrpl_userConfig_batch_check_nodb.json",)
    else:  
        config_files = ("tests/data/test_httrpl_userConfig_batch_check.json","tests/data/test_httrpl_userConfig_batch_check_nodb.json")

    for f in config_files:
        # reading db info from config file
    
        al = align_and_count([0, f])
        configLoader(al)
    
        th = checker(al.config['threads'], al.config['batch_size'], len(al.content))
        th.start()
    
        # running main process
        al.process_alignment_and_count()
    
        th.do_run = False
    
        th.join()
