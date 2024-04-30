"""
Core functions for opening/managing Mongo connections and credentials
"""

import getpass
import os

# Required for protecting special characters in passwd strings when
# passing to mongoURL
from urllib.parse import quote_plus

import mongoengine
import pymongo

# Pull in the defkc object for finding username/passwords
from db.passwords import *
from db.passwords import defaultLogin, defkc

# Pull in core function for PipelineLogger class
from httrplcore import *
from httrplcore import PipelineLogger, deflog

# Generalized functions for working with any data collection in MongoDB

# Most of these are admin functions, and there is currently no support for storing the admin password anywhere
# current design is that needs to be typed in by user interactively to
# open the connection, so these functions are not supported for batch
# scripts

# Function for encoded all keychain fields into a mongo URL
# Analogous to the mongoURL function in mongo.R


def mongoURL(host, user, passwd, db, authSource=None, authMechanism=None):
    """
    Generate URL to connect to a MongoDB

    Given host address, username, password, database name, and optional authentication args, generate a URL that can be safely passed to pymongo.MongoClient to open a MongoDB connection.

    Parameters:
    host (str) = Host name or IP address of the MongoDB host server, e.g. myserver.epa.gov
    user (str) = Username to use for the MongoDB connection
    passwd (str) = Password to use for the MongoDB connection, special characters allowed and will be converted to URL encoded format
    db (str) = Name of the database to connect to on the MongoDB server
    authSource (str) = Which database to use for authentication (optional)
    authMechansim (str) = Authentication mechanism to use (optional)

    Return Value:
    (str) = MongoDB connection URL that can be passed directly to pymongo.MongoClient()
    """
    passwd = quote_plus(passwd)

    if authSource is None and authMechanism is None:
        return f"mongodb://{user}:{passwd}@{host}/{db}"
    else:
        return f"mongodb://{user}:{passwd}@{host}/{db}?authSource={authSource}&authMechanism={authMechanism}"

# Open connection to a MongoDB database for reading/writing data
# This is the primary function used to open DB connections for all httrpl
# Python code


def openMongo(
        host=None,
        user=None,
        passwd=None,
        db=None,
        authSource=None,
        authMechanism=None):
    """
    Open connection to a MongoDB database for reading/writing data.

    Given DB connection information, open a connection to a MongoDB database using pymongo API. This is the primary function used to open DB connections for all httrpl Python code. If user and/or passwd are not specified, this function will check the default keychain and password file in user home directory - this functionality is intended to help protect user credentials from being shared in main pipelining code.

    Parameters:
    host: str mongo host to connect to
    user: str  user name
    passwd: str  user password
    db: str db name
    authSource: str  typically 'admin'
    authMechanism: str  as read in credential json file and required by mongo admin

    Return Value:
    a mongo DB connection object
    """

    if not user or passwd:
        # Check defkc for host,db pair first
        if defkc.hasKey(host=host, db=db):
            myCred = defkc.getKey(host, db)
            # NOTE: If the keychain entry is missing user or passwd fields,
            # this will lead to an error.
            user = myCred['user']
            passwd = myCred['passwd']
            # authSource and authMechanism are optional, not all keys have/need
            # this information
            if 'authSource' in myCred:
                authSource = myCred['authSource']
            if 'authMechanism' in myCred:
                authMechanism = myCred['authMechanism']
        else:
            # NOTE: This is the original auto-load of user/passwd - if both
            # defkc default file and this file are missing, function will crash
            user, passwd = defaultLogin()

    con2 = pymongo.MongoClient(
        mongoURL(
            host=host,
            user=user,
            passwd=passwd,
            db=db,
            authSource=authSource,
            authMechanism=authMechanism))
    DB = con2[db]
    return DB


def openMongoParam(
        host=None,
        user=None,
        passwd=None,
        db=None
):
    """
    Open connection to a MongoDB database for reading/writing data.

    Given DB connection information, open a connection to a MongoDB database using pymongo API. This is the primary function used to open DB connections for all httrpl Python code. If user and/or passwd are not specified, this function will check the default keychain and password file in user home directory - this functionality is intended to help protect user credentials from being shared in main pipelining code.

    Parameters:
    host: str mongo host to connect to
    user: str  user name
    passwd: str  user password
    db: str db name

    Return Value:
    a mongo DB connection object
    """

    if not user or passwd:
        # Check defkc for host,db pair first
        if defkc.hasKey(host=host, db=db):
            myCred = defkc.getKey(host, db)
            # NOTE: If the keychain entry is missing user or passwd fields,
            # this will lead to an error.
            user = myCred['user']
            passwd = myCred['passwd']
        else:
            # NOTE: This is the original auto-load of user/passwd - if both
            # defkc default file and this file are missing, function will crash
            user, passwd = defaultLogin()
    con2 = pymongo.MongoClient(host=host, username=user, password=passwd)
    DB = con2[db]
    return DB


# Additions by LJE - 9/11/19

# Generalized functions for working with any data collection

# TO DO: Would be useful to build in error handling for when some documents are missing the desired ID field
# TO DO: Generalize the types on this so that id_field can be a mongoDB ID
# object, and docs can be a MongoDB cursor


def getDocIDs(docs: list, id_field: str = "sample_id") -> list:
    """
    Extract the list of IDs from a set of database documents.

    Given a list of dicts following a particular DB schema, extract the ID field to a list. This WILL result in error if any members of docs are missing id_field. Return a list of strings or other data types (e.g. id_field="_id" will get the raw Mongo IDs instead).

    Parameters:
    docs (list of dict) = List of dicts corresponding to one or more documents.
    id_field (str) = Key for id_field to extract, defaults to "sample_id" but "_id" should also work.

    Return Value:
    (list) = List of strings or Mongo IDs corresponding to the ID of interest that was extracted from the document collection.
    """
    myIDs = []
    for x in docs:
        myIDs.append(x[id_field])
    return(myIDs)


def findByID(
        DB: pymongo.MongoClient,
        collection: str,
        ids: list = [],
        id_field: str = "sample_id",
        id_only: bool = False,
        fields=None,
        dump: bool = False,
        **kwargs):
    """
    Find all documents in a collection with optional ID filtering.

    Given a list of IDs (typically either _id or sample_id), return an iterable object (pymongo.Cursor or list) corresponding to all matching documents in a collection. If ids is an empty list, return all documents in the collection. Also has options to limit the result to just the id_field or any other subset of fields, and to dump out the data as a list (stored in local memory) instead of returning the pymongo Cursor object.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to specific Mongo Database
    collection (str) = Name of collection to search
    ids (list) = List of IDs (str or mongo_id) to query on, gets all values if this list is empty, default: empty list.
    id_field (str) = Which ID field to match against ids, default: sample_id.
    id_only (bool) = If True, return only the values in the ID field being matched against, otherwise return the whole document
    fields (Mongo query) = If id_only = False, this will be used to filter the return fields instead.
    dump (bool) = If True, loop over the initial return object (pymongo.cursor.Cursor) and extract all results to a new list object.

    Return Value:
    (pymongo.cursor.Cursor or list) = Either a Cursor to return results from Mongo (dump=False) or a list of all results (dump=True)
    """
    # Set query based on ids, id_field
    if len(ids) > 0:
        my_query = {id_field: {"$in": ids}}
    else:
        my_query = {}
    # Set filter based on id_only or fields
    if id_only:
        if id_field == "_id":
            my_filter = {id_field: 1}
        else:
            my_filter = {"_id": 0, id_field: 1}
    else:
        my_filter = fields
    # Run the query with appropriate query and filter:
    query_res = DB[collection].find(my_query, my_filter)
    # Dump to list?
    if dump:
        query_dump = []
        for x in query_res:
            if id_only:
                query_dump.append(x[id_field])
            else:
                query_dump.append(x)
        return query_dump
    else:
        return query_res


def splitIDs(
        DB: pymongo.MongoClient,
        collection: str,
        ids: list,
        id_field: str = "sample_id") -> dict:
    """
    Split a list of IDs based on which are present/absent in a collection.

    Given a list of IDs, search against a particular collection, and return a dict with two lists keyed by "present" and "absent".

    Parameters:
    DB (pymongo.MongoClient) = Open connection to a Mongo DB
    collection (str) = Name of the collection to use
    ids (list) = List of IDs (either strings or objects for _id)
    id_field (str) = Name of ID field, default: sample_id

    Return Value:
    (dict) = 'present'= list of IDs that were found in collection; 'absent'=list of IDs that were NOT found in collection
    """
    # First get the set of matching IDs that are in the DB
    found_id = findByID(
        DB,
        collection=collection,
        ids=ids,
        id_field=id_field,
        id_only=True,
        dump=True)
    found_id = set(found_id)
    my_res = {}
    my_res['present'] = list(filter(lambda x: x in found_id, ids))
    my_res['absent'] = list(filter(lambda x: x not in found_id, ids))
    return my_res

# TO DO: It would be useful to have a query parameter that can be used to
# specify a more generic query for deleting, or additional parameters for
# specific types of filters?


def deleteByID(
        DB: pymongo.MongoClient,
        collection: str,
        ids: list = [],
        id_field: str = "sample_id",
        delete_all: bool = False,
        log: PipelineLogger = deflog) -> int:
    """
    Delete documents from a collection.

    Given a list of IDs, delete all matching documents from a collection, or delete ALL documents. The primary purpose is to remove documents from a collection before replacing them when rerun=True in a particular pipeline step.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database
    collection (str) = Collection to delete from
    ids (list) = List of IDs for documents to delete, default: [], which defers to delete_all
    id_field (str) = Alternate ID field to use, default: "sample_id"
    delete_all (bool) = Setting to True will delete the whole collection, default: False
    log (PipelineLogger) = Log stream for warnings/debug messages

    Return Value:
    (int) = Number of documents that were deleted, will also warn if != len(ids) when delete_all=False
    """
    # If delete_all, query is {}, otherwise construct from ids, id_field
    if delete_all:
        my_query = {}
    else:
        my_query = {id_field: {"$in": ids}}
        # Warn if ids=[]
        if len(ids) == 0:
            log.warning(
                "deleteByID called with ids=[] and delete_all=False, no documents will be deleted.")
    # Debug message:
    log.write("Deleting documents in %s with %s matching %i IDs." %
              (collection, id_field, len(ids)), dbg=True, timestamp=True)
    # Call delete_many and get the number of actual docs deleted, warn if not
    # equal to len(ids)
    delete_res = DB[collection].delete_many(my_query)
    del_cnt = delete_res.deleted_count
    if (del_cnt != len(ids)) and not delete_all:
        log.warning(
            "Called deleteByID with %i IDs but %i documents were deleted (collection='%s', id_field='%s')." %
            (len(ids), del_cnt, collection, id_field))
    return del_cnt

# NOTE: For this function in particular, it would make sense to have a
# custom wrapper for each collection, e.g. to make sure there are matching
# IDs in other collections


def insertByID(
        DB: pymongo.MongoClient,
        collection: str,
        docs: list,
        id_field: str = "sample_id",
        rerun=False,
        log: PipelineLogger = deflog) -> list:
    """
    Insert a list of documents into a MongoDB collection without creating redundant entries.

    Takes a list of dicts and first checks whether any documents with matching IDs are already present. If so, these documents are either skipped for insert (rerun=False) or replaced (rerun=True)

    Parameters:
    DB (MongoClient) = Open database connection for inserting
    collection (str) = Name of collection to insert into
    docs (list of dict) = Documents to insert into collection
    id_field (str) = The field to use for matching up documents, default: sample_id
    rerun (bool) = Whether to overwrite existing data with same sample_id
    log (PipelineLogger) = Log stream for any output (all output is considered debug level here), warns if some data is in DB already and rerun=False

    Return Value:
    (list) = List of mongo _ids for the inserted results
    """
    # Check if any matching sample_id in the collection already
    id_status = splitIDs(
        DB,
        collection=collection,
        ids=getDocIDs(
            docs,
            id_field=id_field),
        id_field=id_field)
    # Check if any of these sample_ids are already in the DB:
    if len(id_status['present']) > 0:
        if rerun:
            # If rerun = True, drop these IDs from the DB before final insert
            # below
            deleteByID(
                DB,
                collection=collection,
                ids=id_status['present'],
                id_field=id_field,
                log=log)
        else:
            # rerun = False, so only insert the 'absent' values
            if len(id_status['absent']) == 0:
                log.warning(
                    "%s already contains documents with %s matching all %i entries in docs, no DB insert to be done (rerun=False)." %
                    (collection, id_field, len(docs)))
                return []
            # Otherwise, filter docs to just those with sample_id matching
            # absent
            docs = list(
                filter(
                    lambda x: x[id_field] in id_status['absent'],
                    docs))
            log.warning(
                "%s already contains %i matching %s, only %i new documents will be inserted (rerun=False)" %
                (collection, len(
                    id_status['present']), id_field, len(
                    id_status['absent'])))

    log.write("Inserting %i new documents into %s (rerun=%s)" %
              (len(docs), collection, rerun), dbg=True, timestamp=True)

    ins_result = DB[collection].insert_many(docs)
    # One final check to make sure the correct number of docs were inserted
    if len(ins_result.inserted_ids) != len(docs):
        log.warning(
            "Attempted to insert %i documents into %s, but DB only returned %i new IDs." %
            (len(docs), collection, len(
                ins_result.inserted_ids)))
    return ins_result.inserted_ids
