"""
These functions reports
  check_write_access_to_db: if the caller has write access to a given db/collection
  get_DB_w_write_access: return a mongo db object
"""

import pymongo
from db.mongo import openMongo, openMongoParam


def check_write_access_to_db(DB):
    """
    check that we have write access to the sanbox db provided by way of env. variables
    Parameter: None
    Return:
    None

    """

    # checking for write access:
    if 'test' in DB.list_collection_names():
        DB["test"].drop()
    res = DB["test"].insert_one({'_id': 101010})
    if DB["test"].find_one({'_id': 101010}):
        res = DB["test"].delete_one({'_id': 101010})
    else:
        DB = None


def get_DB_w_write_access(host, db):
    """
    checks write access able and get the mongo DB object

    Parameters:
    host: str    mongo host name
    db: str  mongo db name

    """
    DB = None
    try:
        DB = openMongo(host=host, db=db)
        check_write_access_to_db(DB)
    except Exception as e:
        print("trying parameter connection instead of uri", flush=True)
        if (
            isinstance(e, pymongo.errors.OperationFailure)
            and "Authentication failed." in str(e)
            or isinstance(e, pymongo.errors.ServerSelectionTimeoutError)
        ):
            DB = openMongoParam(host=host, db=db)
    return DB
