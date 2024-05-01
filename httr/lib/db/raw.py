"""
Functions for reading to/from the httr_raw collection in a database for an HTTr-type study
Currently all functions take a pymongo DB connection as one of the arguments
Use db.mongo openMongo function to open mongo connections to specific DB
with standardized credentials
"""
import pymongo
from httrplcore import *
from httrplcore import PipelineLogger, deflog

# The default collection name for storing raw data is "httr_raw" but can
# always be changed
HTTR_RAW_NAME = "httr_raw"

# TO DO: Make this a wrapper for getDocIDs in mongo.py, OR replace calls to this function with calls to getIDs
# TO DO: Would be useful to build in error handling for when some
# documents are missing the desired ID field


def rawSampleIDs(raw_data: list, id_field: str = "sample_id") -> list:
    """
    Extract the list of sample_ids from a set of httr_raw documents.

    Given a list of dicts following httr_raw schema, extract the sample_id field to a list. This WILL result in error if any members of raw_data are missing sample_id. Return a list of strings or other data types (e.g. id_field="_id" will get the raw Mongo IDs instead).

    Parameters:
    raw_data (list) = List of dicts corresponding to one or more httr_raw documents.
    id_field (str) = Key for id_field to extract, defaults to "sample_id" but "_id" should also work.

    Return Value:
    (list) = List of strings or Mongo IDs corresponding to the ID of interest that was extracted from the document collection.
    """
    myIDs = []
    for x in raw_data:
        myIDs.append(x[id_field])
    return(myIDs)

# This is now redundant with findByID in mongo.py
# TO DO: Make this function a wrapper for that one or replace calls to
# findRaw with findByID


def findRaw(
        DB: pymongo.MongoClient,
        ids: list = [],
        id_field: str = "sample_id",
        collection: str = HTTR_RAW_NAME,
        id_only: bool = False,
        fields=None,
        dump: bool = False,
        **kwargs):
    """
    Find all documents in httr_raw with optional filtering.

    Given a list of IDs (either _id or sample_id), return a list of dicts containing all matching documents in httr_raw. If ids is an empty list, return all documents in httr_raw. Also has options to limit the result to just the id_field or any other subset of fields, and to dump out as a list instead of returning the pymongo Cursor object.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to specific Mongo Database
    ids (list) = List of IDs (str or mongo_id) to query on, gets all values if this list is empty, default: empty list.
    id_field (str) = Which ID field to match against ids, default: sample_id.
    collection (str) = Name of httr_raw collection
    id_only (bool) = If True, return only the values in the ID field being matched against, otherwise return the whole document
    fields (Mongo query) = If id_only = False, this will be used to filter the return fields instead.
    dump (bool) = If True, loop over the initial return object (pymongo.cursor.Cursor) and extract all results to a new list object.

    Return Value:
    (pymongo.cursor.Cursor or list) = Either a Cursor to return results from Mongo (dump=False) or a list of all results (dump=True)
    """
    httr_raw = DB[collection]
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
    query_res = httr_raw.find(my_query, my_filter)
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

# This is now redundant with splidIDs in mongo.py
# TO DO: Make this a wrapper for splitIDs, or replace all calls to this
# function with calls to splitIDs


def splitRawIDs(
        DB: pymongo.MongoClient,
        ids: list,
        id_field: str = "sample_id",
        collection: str = HTTR_RAW_NAME) -> dict:
    """
    Split a list of IDs based on which are present/absent from current httr_raw collection.

    Given a list of IDs, search against a particular DB.httr_raw collection, and return a dict with two lists keyed by "present" and "absent".

    Parameters:
    DB (pymongo.MongoClient) = Open connection to a Mongo DB
    ids (list) = List of IDs (either strings or objects for _id)
    id_field (str) = Name of ID field, default: sample_id
    collection (str) = Name of httr_raw-like collection to use

    Return Value:
    (dict) = 'present'= list of IDs that were found in collection; 'absent'=list of IDs that were NOT found in collection
    """
    # First get the set of matching IDs that are in the DB
    found_id = findRaw(
        DB,
        ids=ids,
        id_field=id_field,
        collection=collection,
        id_only=True,
        dump=True)
    found_id = set(found_id)
    my_res = {}
    my_res['present'] = list(filter(lambda x: x in found_id, ids))
    my_res['absent'] = list(filter(lambda x: x not in found_id, ids))
    return my_res

# This is redundant with deleteByID in mongo.py
# TO DO: Make this a wrapper to deleteByID or replace calls to deleteRaw
# with deleteByID


def deleteRaw(
        DB: pymongo.MongoClient,
        ids: list = [],
        id_field: str = "sample_id",
        collection: str = HTTR_RAW_NAME,
        delete_all: bool = False,
        log: PipelineLogger = deflog) -> int:
    """
    Delete documents from httr_raw collection.

    Given a list of IDs, delete all matching documents from httr_raw, or delete ALL documents from httr_raw.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_raw collection
    ids (list) = List of IDs to for documents to delete, default: []
    id_field (str) = Alternate ID field to use, default: "sample_id"
    collection (str) = Alternate collection name to use
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
                "deleteRaw called with ids=[] and delete_all=False, no documents will be deleted.")
    # Debug message:
    log.write("Deleting documents matching %i IDs (%s) from %s" %
              (len(ids), id_field, collection), dbg=True, timestamp=True)
    # Call delete_many and get the number of actual docs deleted, warn if not
    # equal to len(ids)
    httr_raw = DB[collection]
    delete_res = httr_raw.delete_many(my_query)
    delete_cnt = delete_res.deleted_count
    if (delete_cnt != len(ids)) and not delete_all:
        log.warning(
            "Called deleteRaw with %i IDs but %i documents were deleted." %
            (len(ids), delete_cnt))
    return delete_cnt

# This functionality is now redundant with insertByID in mongo.py
# TO DO: Make this a wrapper to insertByID, can also add some verification
# function specific to httr_raw schema here


def insertRaw(
        DB: pymongo.MongoClient,
        raw_data: list,
        collection: str = HTTR_RAW_NAME,
        rerun=False,
        log: PipelineLogger = deflog) -> list:
    """
    Insert a list of documents into httr_raw.

    Takes a list of dicts structured for httr_raw schema and inserts into appropriate collection in DB.

    Parameters:
    raw_data (list) = Data to insert into httr_raw collection
    DB (MongoClient) = Open database connection for inserting (see db.mongo for helper functions to create connections)
    collection (str) = Name of collection to insert into
    rerun (bool) = Whether to overwrite existing data with same sample_id
    log (PipelineLogger) = Log stream for any output (all output is considered debug level here), warns if some data is in DB already and rerun=False

    Return Value:
    (list) = List of mongo _ids for the inserted results
    """
    httr_raw = DB[collection]
    # Check if any matching sample_id in the collection already
    id_status = splitRawIDs(
        DB, ids=rawSampleIDs(raw_data),
        collection=collection)
    # Check if any of these sample_ids are already in the DB:
    if len(id_status['present']) > 0:
        if rerun:
            # If rerun = True, drop these IDs from the DB before final insert
            # below
            deleteRaw(
                DB,
                ids=id_status['present'],
                collection=collection,
                log=log)
        else:
            # rerun = False, so only insert the 'absent' values
            if len(id_status['absent']) == 0:
                log.warning(
                    "%s already contains documents with sample_id matching all %i entries in raw_data, no DB insert to be done (rerun=False)." %
                    (collection, len(raw_data)))
                return []
            # Otherwise, filter raw_data to just those with sample_id matching
            # absent
            raw_data = list(
                filter(
                    lambda x: x['sample_id'] in id_status
                    ['absent'],
                    raw_data))
            log.warning(
                "%s already contains %i matching sample IDs, only %i new documents will be inserted (rerun=False)" %
                (collection, len(
                    id_status['present']), len(
                    id_status['absent'])))

    log.write("Inserting %i new documents into %s (rerun=%s)" %
              (len(raw_data), collection, rerun), dbg=True, timestamp=True)
    ins_result = httr_raw.insert_many(raw_data)
    return ins_result.inserted_ids


def getReadLenRaw(
        DB: pymongo.MongoClient,
        sample_id: str,
        collection: str = HTTR_RAW_NAME,
        log: PipelineLogger = deflog) -> int:
    """
    Query sample read length from database

    Query database for the rd_len of a specific sample_id in httr_raw or similar collection

    Parameters:
    DB (MongoClient) = Open database connection for inserting (see db.mongo for helper functions to create connections)
    sample_id (str) = Sample_ID to match - warn if not found, or multiple matching entries in DB
    collection (str) = Name of collection to insert into
    log (PipelineLogger) = Log stream for any output (all output is considered debug level here), warns if some data is in DB already and rerun=False

    Return Value:
    (int) = The value in rd_len field for first document matching sample_id, or None if not found
    """
    # Make sure there is one document matching sample_id
    sid_query = {"sample_id": sample_id}
    httr_raw = DB[collection]
    sid_count = httr_raw.count(sid_query)
    if sid_count < 1:
        log.warning(
            "No documents in %s matched sample_id:%s" %
            (collection, sample_id))
        return None
    if sid_count > 1:
        log.warning(
            "Multiple documents in %s matched sample_id:%s, returning first match only." %
            (collection, sample_id))
    # Perform the query and return:
    return httr_raw.find_one(sid_query, projection={'rd_len': 1})['rd_len']

# Functions to create:
# - countRaw - get the size of httr_raw collection, or number with matching IDs?
# - verifyRaw - Apply some verification checks to httr_raw collection, including:
#      - Make sure all sample_id are unique
#      - Make sure all fastq paths are unique
#      - Make sure there are no QC flags in this DB
# - statRaw - Generate some overall stats on httr_raw collection
# - queryRaw - apply more generic queries to httr_raw?
