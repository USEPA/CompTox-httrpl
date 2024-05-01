import os
import sys

# Setup path to lib/ - assumes this script is in bin/ subdir next to lib/
TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-2])
TOP = TOP + '/'
LIB = TOP + 'lib'
LIB = os.path.abspath(__file__)
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB

from db.mongo import openMongo


class db_check():
    def __init__(self, db_list = []):
    """
    this class verifies all expected dbs have the required collections
    Parameter: None
    Return: None

    """
    	self.req_cols = set(
        	['httr_study', 'httr_well_trt', 'httr_trt_cmp_grp', 'httr_probes',
         	'httr_raw', 'httr_counts', 'httr_counts_qc', 'httr_well',
         	'httr_trt_cmp_grp', 'httr_deg', 'httr_sig_cr', 'httr_sig_cat'])

    	self.dbs = db_list

    def check_collections_exits(self, DB):
        """
        checks collections exist for a given db

        Parameter: Mongo DB object
        Returns:
        msg: str which contains errors found

        """
        if self.req_cols.issubset(set(DB.list_collection_names())) == False:
            msg = f"{DB.name} missing: {self.req_cols.difference(set(DB.list_collection_names()))}"
        if set(DB.list_collection_names()).issubset(self.req_cols) == False:
            msg += f" but has: {set(DB.list_collection_names()).difference(self.req_cols)}"
        if msg is not None:
            print(msg)

    def check_db(self, db, host):
        """
        checks collections exist for a given DB
        simply calls the check_collection_exists after getting a DB object from a db name

        Parameters:
        db : str  db name
        host: str optional, mongo host name

        """
        DB = openMongo(host, db)
        self.check_collections_exits(DB)

    def check_dbs(self):
        """
        check_dbs iterates through default dbs to check expected collections are present
        Parameters:
        None
        Return:
        None

        """
        for name in self.dbs:
            self.check_db(name)


checker = db_check()
checker.check_dbs()
