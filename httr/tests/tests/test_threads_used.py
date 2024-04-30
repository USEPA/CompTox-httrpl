"""
test_thread_used_nb uses  a number of threads for running part of align_and_count in parallel. It starts a thread right before launching align_and_count
to observe how many threads are actually running and compares that with what it expects to find
"""

import os
import sys

import pandas as pd
import pytest
from misc import configLoader

TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-3])
TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB

import os
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
        processName: str  name of the process to observe
        Return:
        count: int number of threads running for that process
        pid: process Id

        '''
        # Iterate over the all the running processes
        count = 0
        pid = -1
        for proc in psutil.process_iter():
            try:
                # Check if process name contains the given name string.
                if processName == proc.name():
                    count += 1
                    pid = proc.pid  # usefull for hisat2

            except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                print("zombie", flush=True)
                pass
        return count, pid

    def run(self):
        """
        run finds the pytest and hisat2-align-s processes and identifies the number of threads they are using
        and compare that with the number of threads they should be using

        Parameter:
        None
        Retun:
        None
        """

        maxpytest_ins = 0
        maxhisat_ins = 0
        maxthnb = 1
        thcount = {}

        print(f"do_run is {self.do_run}", flush=True)

        while self.do_run:

            time.sleep(0.1)

            pytest_ins, pytest_pid = self.checkIfProcessRunning("pytest")
            hisat_ins, hisat2_pid = self.checkIfProcessRunning(
                "hisat2-align-s")

            if hisat2_pid != -1:
                try:
                    thinst = os.listdir(f"/proc/{hisat2_pid}/task")
                    maxthnb = max(len(thinst), maxthnb)
                    thcount[hisat2_pid] = maxthnb - 1
                except FileNotFoundError:
                    pass
            else:
                if maxthnb != 1:
                    assert maxthnb == min(self.processes, self.batchsize) + 1
                maxthnb = 1

            maxpytest_ins = max(maxpytest_ins, pytest_ins)

        assert self.processes + 1 == maxpytest_ins


@pytest.mark.order(6)
def test_thread_used_nb():
    """
    test_thread_used_nb uses  a number of threads for running part of align_and_count in parallel. It starts a thread right before launching align_and_count
    to observe how many threads are actually running. That thread is called 'checker'


    all the names of collection, files, columns to check and specific sample_id used are all defined in misc.py

    Parameters:

    Returns:
    test runs assert statements and will post assertion failures on the screen though pytest. The test function itself returns None


    """
    if "RUN_TEST_WITH_NO_MONGO" in os.environ and os.environ["RUN_TEST_WITH_NO_MONGO"]=="Y":
        config_files = ("tests/data/test_httrpl_userConfig_threads_check_nodb.json",)
    else:
        config_files = ("tests/data/test_httrpl_userConfig_threads_check.json","tests/data/test_httrpl_userConfig_threads_check_nodb.json")
    
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
