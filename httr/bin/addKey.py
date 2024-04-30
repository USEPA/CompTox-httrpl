#!/usr/bin/python3
#
# LJE - 1/29/20
#
# Run addKeychainEntry in fully interactive mode to add/update a DB login key to the user's private keychain
# This should be run before working with a new database
# TO DO: It would be useful if this had command-line options and had a way to update the default user:passwd
#
# Usage:
#  addKey.py
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

# Import Main API:
from httrpl import *
from db.passwords import addKeychainEntry

# Run addKeychainEntry interactively:
addKeychainEntry(interactive=True)
