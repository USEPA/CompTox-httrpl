'''
 these classes make use of jsonschema validation functions
 The collection class loads its own json validation schema from a file (one per collection type)
 if the file hasn't been loaded already by a previous db that checked that type of collection
 likewise, exceptions raised by the jsonschema are displayed only once and the message, that can be long
 is truncated to first 100 characters.

'''


import datetime
import getpass
import json
import os

import pandas as pd
from celery import group
from db.getDB import get_DB_w_write_access
from db.mongo import openMongo
from jsonschema import exceptions, validate
from tasks import do_check_data_integrity, do_col_check

collection_schemas = {}
errors = set()

data_integrity_errors = []
test_host = os.getenv("TEST_HOST")
test_db = os.getenv("TEST_DB")
DBTest = get_DB_w_write_access(host=test_host, db=test_db)


class collection():

    """
    The most basic class: A dbs_schema_checker encompasses a group of dbs, while schema_check groups the collections of interest,
    while collection is the main object where the actual check happen


    """

    def __init__(self, name, col, db, type_check=False):
        """
        initializes the collection instances

        Parameters:
        name: str:  name of collection
        col: the collection object
        type_check: boolean indicating to check for type or not

         Returns:
         Nothing, but we now have a well formed collection object

        """
        self.name = name
        self.col = col
        self.db = db
        self.schema_json = f"schema_{self.name}.json"
        self.schema_obj = f"schema_{self.name}"
        self.type_check = type_check

        if self.schema_json in collection_schemas:
            self.schema = collection_schemas[self.schema_json]
        else:
            with open(os.path.join(os.path.dirname(__file__), '../validations/' + self.schema_json), "r") as jsonfile:
                if not self.type_check:
                    lines = jsonfile.readlines()
                    lines = list(
                        filter(
                            lambda
                            x:
                            "'type' : 'string'"
                            not
                            in
                            lines
                            and
                            "'type' : 'number'"
                            not
                            in lines and
                            "'type' : 'integer'" not in lines, lines))
                    self.schema = json.loads(''.join(map(str, lines)))
                else:
                    self.schema = json.load(jsonfile)
                collection_schemas[self.schema_json] = self.schema

# check will go through all documents in the collection and run the validation function
# when an error is raised for no compliance, the exception is raised and displayed only once
# the validation could be run in parallel in another vs. as on some collection like httr_deg
# they tend to be really long

    def check(self):
        """
        check method on the collection compares the collection layout with the schema defined in its .json definition file

        Parameter: itself, the collection object
        Returns:
        error encoutered by the validation process, usually returned to caller who deals with it

        """
        self.doc = self.col.find({})
        print(f"{self.doc.count()} docs found in {self.db}/{self.name}")
        for d in list(self.doc):
            try:
                validate(d, self.schema[self.schema_obj])
            except exceptions.ValidationError as e:
                err = f"{e.message[:100]} ... for {self.db}/{self.name}"
                if err not in errors:
                    print(err)
                    errors.add(err)
        return(errors)

    def check_data_integrity(self):
        """
        check method on the collection compares the collection data with the schema defined in its .json definition file
        to make sure if optional column are present, they are with data on every row

        Parameter: itself, the collection object
        Returns:
        set of errors encountered by the check, usually returned to caller who deals with it


        """
        errors = set()
        res = self.col.find({})
        df = pd.DataFrame(res)
        if self.name == "httr_chem":
            print(df)
        if 'optional' not in self.schema[self.schema_obj]:
            print(f"no optional columns found in {self.db}/{self.name}")
        else:
            for opt in self.schema[self.schema_obj]['optional']:
                print(f"found optional field {opt} in {opt in df}")
                
                if opt in df and df[opt].isnull().sum() != 0:
                    print(
                        f"{opt} not in all documents for collection {self.db}/{self.name}")
                    errors.add(
                        f"{opt} not in all documents for collection {self.db}/{self.name}")
        return(errors)


class schema_check():
    """
    the schema_check goes through the list of the collections on a given db
    and calls the collection.check function on each
    it also prints limited stats on the size of the collection


    """

    def __init__(self, DB, db_name, host=test_host):
        self.collections = [
            'httr_study',
            'httr_chem',
            'httr_raw',
            'httr_counts',
            'httr_counts_qc',
            'httr_well',
            'httr_trt_grp_cmp',
            'httr_deg',
            'httr_sig_cr',
            'httr_sig_cat',
            'httr_probe',
            'httr_well_trt']
        self.DB = DB
        print(f'db is {db_name}')
        if self.DB is None:
            self.DB = openMongo(host=host, db=db_name)

    def check(self, type_check=False):
        """
        drives the checks for all collections
        either by using the celery underlying multiprocessor capabilities (using 8 processes)
        or performing it one at a time
        when using multiprocessing, each collection is alloted one process
        except with httr_deg, which in turn gets the 8 processes to itself (each taking care of one slice 1/8th of the rows)

        Parameters:
        type_check: boolean:  instruct to perform the lengthy type checks or not, by default

        Returns: the set of errrors that were encountered

        """
        print(f"detected HTTR_CELERY_MP to be {os.getenv('HTTR_CELERY_MP')}")
        if os.getenv("HTTR_CELERY_MP") == "Y":
            DBTest['check_column_errors'].drop()
            jobs = []
            for c in self.collections:
                if c != 'httr_deg':
                    col = collection(
                        c, self.DB[c], self.DB.name, type_check=type_check)
                    jobs.append(
                        do_col_check.s(
                            col.name,
                            col.db,
                            col.schema,
                            col.schema_obj,
                            host=test_host,
                            test_db=test_db))
            jobs_group = group(jobs)
            result_groups = jobs_group.apply_async()
            results = result_groups.join()

            # special treatment for httr_deg
            col = collection(
                'httr_deg',
                self.DB['httr_deg'],
                self.DB.name,
                type_check=type_check)
            jobs = []

            for proc in range(8):
                jobs.append(
                    do_col_check.s(
                        col.name,
                        col.db,
                        col.schema,
                        col.schema_obj,
                        host=test_host,
                        test_db=test_db,
                        proc=proc,
                        thread_nb=8))
            jobs_group = group(jobs)
            result_groups = jobs_group.apply_async()
            results = result_groups.join()

            res = list(DBTest['check_column_errors'].find({}, {"_id": 0}))
            return(res)
        else:
            errors = set()
            for c in self.collections:
                errors |= self.check_col(c, type_check)
            return(errors)

    def check_col(self, c, type_check=False):
        """
        This function drives the collection check function for each collection it is called on

        Parameters:
        c: str  collection object
        type_check: boolean, whether type check should be included

        Returns: list of errors returned by the colection check method

        """
        col = collection(c, self.DB[c], self.DB.name, type_check)
        errors = col.check()
        if col.doc.count() == 0:
            print(f"No documents found for {self.DB.name} / {c}")
        return errors

    def check_data_integrity(self):
        """
        This function drives the collection data integrity functions for each collection it is called on
        it either does it in paralell if HTTR_CELERY_MP is set to "Y" or serially
        parallel function do_check_data_integrity located in tasks.py

        Parameters:
        None

        Returns: list or set of errors returned by the colection check method

        """

        if os.getenv("HTTR_CELERY_MP") == "Y":
            DBTest['data_integrity_errors'].drop()
            jobs = []
            for c in self.collections:
                col = collection(c, self.DB[c], self.DB.name)
                print(f"test_db is {test_db}")
                jobs.append(
                    do_check_data_integrity.s(
                        col.name,
                        col.db,
                        col.schema,
                        col.schema_obj,
                        host=test_host,
                        test_db=test_db))
            jobs_group = group(jobs)
            result_groups = jobs_group.apply_async()
            results = result_groups.join()

            res = list(DBTest['data_integrity_errors'].find({}, {"_id": 0}))
            return(res)

        else:
            errors = set()
            for c in self.collections:
                errors |= self.check_col_data_integrity(c)

            return(errors)

    def check_col_data_integrity(self, c):
        """
        This function calls the data_integrity function for the collection passed as input
        Parameter:
        c:str   collection name
        Return:
        None

        """
        print(f"checking {c}")
        col = collection(c, self.DB[c], self.DB.name)
        return(col.check_data_integrity())

    def check_sample_ids(self):
        """
        checks all collections have same sample_ids

        Parameters:
        None

        Returns:
        set of errors


        """
        s_ids = {}
        common = set()

        errors = set()

        for c in self.collections:
            res = self.DB[c].find(
                {'sample_id': {'$exists': 1}},
                {'sample_id': 1, '_id': 0})
            s_ids[c] = list(res)
            if len(s_ids[c]) == 0:
                continue
            print(f"checking sample ids for {c}")
            s_ids[c] = set([i['sample_id'] for i in s_ids[c]])
            if len(common) == 0:
                common = s_ids[c]
            else:
                common = common.intersection(s_ids[c])

        for c in self.collections:

            if len(s_ids[c]) != 0:
                s_ids[c] = s_ids[c].difference(common)
                if len(s_ids[c]) != 0:
                    err = f"{c} has {len(s_ids[c])} unique sample_ids not found in other collections"
                    errors.add(err)
                    print(err)
                else:
                    print(
                        f"{c}\' sample ids are all present in all other collections")

        return (errors)


class dbs_schema_checker():
    """

     This class defines a schema for each main dbs and calls the schem.check function on it
     that in turns will call each collection.check function with the relevant validation schema

    """

    def __init__(self, db_list = []):
        self.dbs = db_list

    def print_banner(self, db):
        """
        prints a usefull banner on the output
        prints a banner used to separate the different tests that we run

        Parameter:
        the db name that the test is running on and that is displayed

        Return:
        None

        """
        print("                                                                                                        ")
        print(
            f"--------------------------------------checking {db}-----------------------------------------------------")
        print("                                                                                                        ")

    def check(self, type_check=False):
        """
        driver to perform our checks on each db we have a reference for in our intialization code
        Parameter:
        type_check: boolean  whether type is to be checked (slow process) by defualt False
        Return:
        None

        """
        for db in self.dbs:
            self.check_db(db, type_check=type_check)

    def check_db(self, db, type_check=False):
        """
        check_db checks all collections in a particular db

        """
        self.print_banner(db)
        schema = schema_check(DB=None, db_name=db)
        return(schema.check(type_check=type_check))

    def check_col(self, col, type_check=False, DB=None):
        """
        check_col checks one type of collection throughout all dbs

        Parameter:
        col: str : the collection name
        type_check: boolean: whether we check for type or not - it does take a lot more time, hence the option
        DB: DB object to check on
        
        Return:
        None
        """

        for db in self.dbs:
            self.print_banner(f"collection: {db}/{col}")
            schema = schema_check(DB=DB, db_name=db)
            schema.check_col(col, type_check)

    def check_data_integrity(self, DB=None, db_name=''):
        """
        checks data integrity for a given DB
        calls the schema_check object check_data_integrity where the workload is located

        Parameters:
        DB : DB object
        db_name: str: db name

        Returns:
        None

        """
        self.print_banner(f"data integrity for collection: {db_name}")
        schema = schema_check(DB=DB, db_name=db_name)
        return(schema.check_data_integrity())

    def check_sample_ids(self, DB=None, db_name=''):
        """
        checks data check_sample_ids for a given DB
        calls the schema_check object check_sample_ids where the workload is located

        Parameters:
        DB : DB object
        db_name: str: db name

        Returns:
        None

        """
        self.print_banner(f"check sample_ids for collection: {db_name}")
        schema = schema_check(DB=DB, db_name=db_name)
        return(schema.check_sample_ids())
