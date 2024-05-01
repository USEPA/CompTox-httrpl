#!/usr/bin/python3
#
# LJE - 9/24/19
#
# Replaces tempoBatch.py script
# Run alignment and probe counting step on a batch of fastq files, then output results to a TSV file and/or database
# This is essentially just a command-line interface to countBatch function in lib/httrpl.py module
#
# Usage:
#  countBatch.py count_config.json
#

import os
import sys

# Make sure a parameter was specified
if len(sys.argv) < 2:
    sys.exit("No config file specified.")

config_file = sys.argv[1]

# Setup path to lib/ - assumes this script is in bin/ subdir next to lib/
TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-2])
TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB

# Import Main API:
from httrpl import *
from httrpl import countBatch, envReport

# Check versions:
# TO DO: This function could also report versions of tools like md5sum,
# and other impt python libs like pymongo?
envReport()

# Run everything from master function
countBatch(config_file)
