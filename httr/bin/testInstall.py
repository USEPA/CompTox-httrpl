#!/usr/bin/python3
# LJE - 9/20/19
# Test that all python modules and dependencies can be loaded in the current environment
# TO DO: Should also load all R modules
#
# Usage:
#  python3 testInstall.py
#

import os
import sys

# Setup path to lib/ - assumes this script is in bin/ subdir next to lib/
TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-2])
TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB

from db.mongo import *
from db.raw import *
from gexp.biospyder2 import *
from gexp.fastq import *
from httrplcore import *
from httrplcore import PipelineLogger

# Import all httrpl libs to make sure everything loads properly
from httrpl import *
from httrpl import envReport

stdoutlog = PipelineLogger(out=sys.stdout, dbg=True, strict=False)
envReport(log=stdoutlog)
