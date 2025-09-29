'''
These two functions correspond to the two tasks run by the CELERY layer, i.e.
the data check integrity and the collection checks
'''


import datetime

import pandas as pd
from db.getDB import get_DB_w_write_access
from db.mongo import openMongo
from httr_celery import app
from jsonschema import exceptions, validate


@app.task
def do_check_data_integrity(
        col_name,
        db_name,
        schema,
        schema_obj,
        host,
        test_db):
    """
    when HTTR_CELERY_MP is set to "Y", we use multiprococessing for running
    the schema check code.
    CELERY uses the tasks defined in tasks.py that are run in parallel using apply_async.
    do_check_data_integrity is the task that checks the collection data against its schema
    to make sure if optional column are present, they are with data on every row

    Parameters:
    col_name: str   the name of the collection to check
    db_name: str the name of the database where the collection is
    schema: str   the schema json object as read from the schema_obj string, i.e.
        "schema_httr_deg" : {
          "type": "object",
          "required": [ "trt_grp_i...

    schema_obj: str  the string identifying the schema i.e "schema_httr_deg.json"
    host: str the mongo db host to connect to
    test_db: str  the sandbox db used for storing results

    Return:
    a list of potential integrity check errors

    """

    print(f"host = {host}")
    data_integrity_errors = []
    DB = openMongo(host=host, db=f'{db_name}')
    col = DB[col_name]

    res = col.find({})
    df = pd.DataFrame(res)
    if 'optional' not in schema[schema_obj]:
        print(f"no optional columns found in {db_name}/{col_name}")
    else:
        for opt in schema[schema_obj]['optional']:
            if opt in df and df[opt].isnull().sum() != 0:
                print(
                    f"{opt} not in all documents for collection {db_name}/{col_name}")
                data_integrity_errors.append(
                    f"{opt} not in all documents for collection {db_name}/{col_name}")
    print(f"data_integrity_errors {data_integrity_errors}")

    # connect to sandbox to write out results
    if len(data_integrity_errors) != 0:
        DBTest = get_DB_w_write_access(host=host, db=test_db)
        DBTest['data_integrity_errors'].insert_one(
            {col_name: data_integrity_errors})

    return(data_integrity_errors)


@app.task
def do_col_check(
        col_name,
        db_name,
        schema,
        schema_obj,
        host,
        test_db,
        proc=None,
        thread_nb=None):
    """
    when HTTR_CELERY_MP is set to "Y", we use multiprococessing for running
    the schema check code.
    CELERY uses the tasks defined in tasks.py that are run in parallel using apply_async.
    do_check_data_integrity is the task that checks the collection data against its schema
    to make sure if optional column are present, they are with data on every row

    Parameters:
    col_name: str   the name of the collection to check
    db_name: str the name of the database where the collection is
    schema: str   the schema json object as read from the schema_obj string, i.e.
        "schema_httr_deg" : {
          "type": "object",
          "required": [ "trt_grp_i...

    schema_obj: str  the string identifying the schema i.e "schema_httr_deg.json"
    host: str the mongo db host to connect to
    test_db: str  the sandbox db used for storing results
    proc: int   process number out of (hard coded to 8) used to identify the slice of documents to process
    thread_nb: int   the total number of processes allocated to this work, hard coded now to 8

    Return:
    a list of potential integrity check errors

    Note: proc and thread_nb are passed only for httr_deg, which is dealt with by 8 processes, each dealing with its own slice
    For other collections, each process takes care of all rows in the collection

    """

    check_column_errors = []
    DB = openMongo(host=host, db=f'{db_name}')
    col = DB[col_name]

    t = datetime.datetime.now()

    if proc is not None:
        step = col.count_documents() // thread_nb
        doc = col.find({}).skip(step * proc).limit(step)
    else:
        doc = col.find({})
        doc = list(doc)

    for d in doc:
        try:
            validate(d, schema[schema_obj])
        except exceptions.ValidationError as e:
            err = f"{e.message[:100]} ... for {db_name}/{col_name}"
            if err not in check_column_errors:
                print(err)
                check_column_errors.append(err)

    print(
        f"validating {col_name} documents took {datetime.datetime.now() - t}")

    # connect to sandbox to write out results
    if len(check_column_errors) != 0:
        test_host = host
        test_db = test_db
        DBTest = get_DB_w_write_access(host=test_host, db=test_db)
        records = DBTest['check_column_errors'].find_one(
            {col_name: {'$exists': True}})
        if records is None:
            DBTest['check_column_errors'].insert_one(
                {col_name: check_column_errors})
        elif len(list(set(check_column_errors) - set(records[col_name]))) != 0:
            updates = list(set(check_column_errors) | set(records[col_name]))
            DBTest['check_column_errors'].update_one(
                {col_name: check_column_errors},
                {"$set": {"col_name": updates}})

    return(check_column_errors)
