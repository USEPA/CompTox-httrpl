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

# Import Main API
from update_gc_frac import update_gc_frac
al = update_gc_frac(sys.argv)
al.process_gc_frac()