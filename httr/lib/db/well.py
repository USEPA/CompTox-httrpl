"""
Functions for building the httr_well collection based on httr_well_trt, httr_raw, httr_counts, and httr_counts_qc
Currently all functions take a pymongo DB connection as one of the arguments
Use db.mongo openMongo function to open mongo connections to specific DB
with standardized credentials
"""
import sys
from time import time

import pymongo
from db.mongo import *
from db.mongo import deleteByID, findByID
from httrplcore import *
from httrplcore import PipelineLogger, deflog

# Default collection names:
HTTR_TRT_NAME = "httr_well_trt"    # well treatment (sample sheet) data
HTTR_RAW_NAME = "httr_raw"         # raw fastq data
HTTR_COUNTS_NAME = "httr_counts"   # probe count data
HTTR_QC_NAME = "httr_counts_qc"    # count QC data
HTTR_PROBE_NAME = "httr_probe"     # probe manifest data
HTTR_WELL_NAME = "httr_well"       # combined well data

# TO DO: This should go in a separate module of functions for accessing
# the httr_probe collection


def getGoodProbes(
        DB: pymongo.MongoClient,
        collection: str = HTTR_PROBE_NAME,
        id_field: str = "probe_name",
        flags: list = ["OK"]) -> set:
    """
    Function to get the set of all good probes from the database.

    Pulls all probe_name fields for probes with acceptable probe_flag in httr_probe collection of a DB, returns the probe_name's as a set.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_probe collection
    collection (str) = Name of probe manifest collection, defaults to "httr_probe"
    id_field (str) = Name of field in probe collection to return, defaults to "probe_name"
    flags (list of str) = List of acceptable probe_flag values to consider as "good" flags

    Return Value:
    (set of str) = Set of all probes that are good to use
    """
    httr_probe = DB[collection]
    good_probes = httr_probe.distinct(id_field, {"probe_flag": {"$in": flags}})
    return set(good_probes)

# TO DO: This should go in a separate module of functions for accessing httr_counts collection?
# TO DO: Option to add in 0s for missing probe names?


def getGoodCounts(probe_cnts: dict, good_probes: set) -> dict:
    """
    Function to filter probe counts to a subset of probe names

    Given probe counts as a dict, with keys = probe_name, filter to a subset of probe_names in set good_probes.

    Parameters:
    probe_cnts (dict of int): Dictionary of probe counts from httr_count table, with keys = probe_name, values = read count
    good_probes (set of str): Set of probe names to keep (any missing will still be missing after this call)

    Return Value:
    (dict of int) = Filtered version of probe_cnts with only those keys in good_probes
    """
    keep_cnts = {probe_name: probe_cnts[probe_name]
                 for probe_name in good_probes
                 if probe_name in probe_cnts}
    return keep_cnts

# TO DO: There might be a way to do this with built-in mongo functions?
# TO DO: If log.dbg=True, do some additional checks and generate a warning if they fail?
# TO DO: Compute probe_cnts_nrm here?
 

def buildSampleWell(DB: pymongo.MongoClient,
                    sample_id: str,
                    good_probes: set,
                    collection: str = HTTR_WELL_NAME,
                    well_trt: str = HTTR_TRT_NAME,
                    raw: str = HTTR_RAW_NAME,
                    counts: str = HTTR_COUNTS_NAME,
                    qc: str = HTTR_QC_NAME,
                    rerun: bool = False,
                    db_insert: bool = True,
                    log: PipelineLogger = deflog,
                    suppress_multilen: bool = False,
                    **kwargs):
    """
    Function to combine other collections into httr_well doc for one sample

    Extract documents from httr_well_trt, httr_raw, httr_counts, and httr_counts_qc for one sample_id and combine into a single document for httr_well

    Parameters:
    DB (pymongo.MongoClient) = Open connection to DB containing all necessary collections
    sample_id (str) = Sample ID for a sample that has already been entered in all necessary collections
    good_probes (set of str) = Set of probes to use to filter probe_cnts
    collection (str) = Name of collection for storing combined well data, defaults to "httr_well"
    well_trt (str) = Name of collection with well treatment (sample key) data, defaults to "httr_well_trt"
    raw (str) = Name of collection with raw fastq meta-data, defaults to "httr_raw"
    counts (str) = Name of collection with initial probe count data, defaults to "httr_counts"
    qc (str) = Name of collection with all count QC data, defaults to "httr_counts_qc"
    rerun (bool) = Should the document be regenerated if present? Defaults to False
    db_insert (bool) = Should the document be inserted into the database? Defaults to True, if set to False will return the full doc as a dict
    log (httrplcore.PipelineLogger) = Log handler, defaults to stderr with preset options

    Return Value:
    (str, dict, or None) = If db_insert is True, returns either the _id of the inserted doc, or None if no insert happened, otherwise returns the whole doc as a dict.
    """
    # First check if sample_id in httr_well:
    httr_well = DB[collection]
    if db_insert and (httr_well.count_documents({"sample_id": sample_id}) > 0):
        if rerun:
            log.write(
                "Replacing %s data for sample_id=%s" %
                (collection, sample_id), dbg=True)
            deleteByID(DB=DB, collection=collection, ids=[sample_id], log=log)
        else:
            log.warning(
                "%s already contains doc for sample_id=%s, nothing to do (rerun=False)" %
                (collection, sample_id))
            return None
    # Extract the doc for sample_id from each of the relevant collections
    sample_trt = findByID(
        DB=DB,
        collection=well_trt,
        ids=[sample_id],
        dump=True)
    sample_raw = findByID(DB=DB, collection=raw, ids=[sample_id], dump=True)
    sample_counts = findByID(
        DB=DB,
        collection=counts,
        ids=[sample_id],
        dump=True)
    sample_qc = findByID(DB=DB, collection=qc, ids=[sample_id], dump=True)
    # Make sure each of these returned single doc
    doc_cnts = {
        well_trt: len(sample_trt),
        raw: len(sample_raw),
        counts: len(sample_counts),
        qc: len(sample_qc)}
    for coll, dcnt in doc_cnts.items():
        # If any are missing, warn and return None
        if dcnt < 1:
            log.warning(
                "%s does not contain data for sample_id=%s" %
                (coll, sample_id))
            return None
        # If any are > 1, send warning to log but proceed
        if dcnt > 1:
            log.warning(
                "%s has multiple documents with sample_id=%s, only the first will be used." %
                (coll, sample_id))
    # For each one, extract just the first member of the list
    sample_trt = sample_trt[0]
    sample_raw = sample_raw[0]
    sample_counts = sample_counts[0]
    sample_qc = sample_qc[0]
    # Use sample_trt as the base for httr_well
    # Move _id -> trt_id
    sample_trt['trt_id'] = sample_trt['_id']
    del sample_trt['_id']
    # Add raw_id from sample_raw
    sample_trt['raw_id'] = sample_raw['_id']
    # If current qc_flag = OK, copy flag from sample_raw
    if sample_trt['qc_flag'] == "OK":
        if not suppress_multilen or sample_raw['qc_flag'] != 'MULTI_LEN':
            sample_trt['qc_flag'] = sample_raw['qc_flag']
    # Copy _id, n_reads from sample_counts
    sample_trt['counts_id'] = sample_counts['_id']
    sample_trt['n_reads'] = sample_counts['n_reads']
    # Extract probe_cnts, filter against good_probes
    probe_cnts = sample_counts['probe_cnts']
    if (good_probes is not None) and (len(good_probes) > 0):
        probe_cnts = getGoodCounts(probe_cnts, good_probes)
    sample_trt['probe_cnts'] = probe_cnts
    # Copy multiple qc fields from sample_qc
    sample_trt['qc_id'] = sample_qc['_id']
    for field in [
        'n_reads_mapd',
        'mapd_frac',
        'bad_probe_count',
        'n_cov5',
        'n_sig80',
        'top10_prop',
            'gini_coef']:
        sample_trt[field] = sample_qc[field]
    # If current qc_flag = OK, copy flag from sample_qc
    if sample_trt['qc_flag'] == "OK":
        sample_trt['qc_flag'] = sample_qc['qc_flag']
    # Either insert into DB and return the _id field OR return the new doc
    # structure
    if db_insert:
        result = httr_well.insert_one(sample_trt)
        return result.inserted_id
    else:
        return sample_trt

# TO DO: Add parameters that allow filtering, e.g. to a single plate, so that results can be dumped to a JSON or TSV file?
# TO DO: If this loop is very slow it could be parallelized since each
# sample is processed independently


def buildAllWells(DB: pymongo.MongoClient,
                  collection: str = HTTR_WELL_NAME,
                  well_trt: str = HTTR_TRT_NAME,
                  raw: str = HTTR_RAW_NAME,
                  counts: str = HTTR_COUNTS_NAME,
                  qc: str = HTTR_QC_NAME,
                  probe: str = HTTR_PROBE_NAME,
                  rerun: bool = False,
                  db_insert: bool = True,
                  interval: int = 1000,
                  log: PipelineLogger = deflog,
                  **kwargs) -> int:
    """
    Function to combine other collections into httr_well for all samples

    Determine the set of sample_ids which have documents in all necessary collections and combines the data into httr_well collection. This function also extracts the list of good_probes from the httr_probe collection. Loops over buildSampleWell to construct and insert each document into the DB.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to DB containing all necessary collections
    collection (str) = Name of collection for storing combined well data, defaults to "httr_well"
    well_trt (str) = Name of collection with well treatment (sample key) data, defaults to "httr_well_trt"
    raw (str) = Name of collection with raw fastq meta-data, defaults to "httr_raw"
    counts (str) = Name of collection with initial probe count data, defaults to "httr_counts"
    qc (str) = Name of collection with all count QC data, defaults to "httr_counts_qc"
    probe (str) = Name of collection with all probe manifest data, defaults to "httr_probe", set to None to suppress filtering of bad probes
    rerun (bool) = Should the document be regenerated if present? Defaults to False
    db_insert (bool) = Should the document be inserted into the database? Defaults to True, if set to False will return the full doc as a dict
    interval (int) = For every X records processed, update log - set to None to supress this output
    log (httrplcore.PipelineLogger) = Log handler, defaults to stderr with preset options
    **kwargs = Pass additional params to buildSampleWell

    Return Value:
    (int) = Number of samples processed.
    """
    # Get the list of sample IDs in each collection:
    well_samples = set(DB[collection].distinct('sample_id'))
    trt_samples = set(DB[well_trt].distinct('sample_id'))
    raw_samples = set(DB[raw].distinct('sample_id'))
    count_samples = set(DB[counts].distinct('sample_id'))
    qc_samples = set(DB[counts].distinct('sample_id'))
    # First compute intersection of sample_ids in all precursor collections:
    ready_samples = trt_samples & raw_samples & count_samples & qc_samples
    # Check if any IDs were found in some but not all precursor collections:
    extra_samp_cnts = {well_trt: len(trt_samples - ready_samples),
                       raw: len(raw_samples - ready_samples),
                       counts: len(count_samples - ready_samples),
                       qc: len(qc_samples - ready_samples)}
    for coll, ecnt in extra_samp_cnts.items():
        if ecnt > 0:
            log.warning(
                "%s has %i sample IDs that are missing from at least one other collection needed to build %s" %
                (coll, ecnt, collection))
    # Check if any IDs are in httr_well already
    completed_samples = ready_samples & well_samples
    if len(completed_samples) > 0:
        if rerun:
            log.write(
                "%i samples are already present in %s - these will be reprocessed." %
                (len(completed_samples), collection))
            if db_insert:
                # If re-running and inserting into DB, delete the existing
                # samples first
                deleteByID(
                    DB=DB,
                    collection=collection,
                    ids=list(completed_samples),
                    log=log)
        else:
            # Not re-running, so issue a warning
            log.warning(
                "%i samples are already present in %s - these will skipped, rerun=False" %
                (len(completed_samples), collection))
            # Remove completed_samples from the set of sample IDs to process
            ready_samples = ready_samples - completed_samples
    # If there are no samples to process, issue a warning and return 0 here
    if len(ready_samples) == 0:
        log.warning("There are no samples to process here.")
        return 0
    # Extract the set of good_probes to keep
    if probe is not None:
        good_probes = getGoodProbes(DB=DB, collection=probe)
        if len(good_probes) == 0:
            log.warning(
                "Collection %s is missing or contains no usable probes, filtering of probe_cnts will not be performed" %
                probe)
    # Loop over all ready_samples and process each one
    log.write("Generating %s data for %i wells." %
              (collection, len(ready_samples)), timestamp=True)
    sample_cnt = 0
    startTime = time()
    for sample_id in ready_samples:
        result = buildSampleWell(
            DB=DB,
            sample_id=sample_id,
            good_probes=good_probes,
            collection=collection,
            well_trt=well_trt,
            raw=raw,
            counts=counts,
            qc=qc,
            rerun=rerun,
            db_insert=db_insert,
            log=log,
            **kwargs)
        if result is not None:
            sample_cnt += 1
        if (interval is not None) and (sample_cnt > 0) and (
                (sample_cnt % interval) == 0):
            log.write("%i samples processed." % sample_cnt, timestamp=True)
    endTime = time()
    runTime = endTime - startTime
    log.write(
        "Successfully combined data for %i of %i wells in %i seconds." %
        (sample_cnt, len(ready_samples), runTime), timestamp=True)
    return sample_cnt

# TO DO: This could be adapted into a more generic function that tabulates
# how many times each value seen in any field of any collection


def countFlags(
        DB: pymongo.MongoClient,
        collection: str = HTTR_WELL_NAME) -> dict:
    """
    Count total number of each QC flag in httr_well collection.

    Get all distinct flags in httr_well collection and then count how many docs have each one. This will also work on any collection with a qc_flag field.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to DB
    collection (str) = Name of httr_well or similar collection, defaults to "httr_well"

    Return Value:
    (dict of int) = Dictionary where each key is a distinct value in qc_flag and each value is the number of documents with that flag.
    """
    httr_well = DB[collection]
    flag_vals = httr_well.distinct("qc_flag")
    flag_counts = {}
    for x in flag_vals:
        flag_counts[x] = httr_well.count_documents({"qc_flag": x})
    return flag_counts

# TO DO: Fill out this function


def filterByFlag(
        DB: pymongo.MongoClient,
        ids: list = [],
        collection: str = HTTR_WELL_NAME,
        good_flags: list = ["OK"]) -> list:
    """
    Filter a list of sample IDs to only those with acceptable QC flags

    Build a query that filters sample_id values to only those with acceptable qc_flag values, return the matching sample_id values only.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to DB
    ids (list of str) = List of sample_id values to filter, if empty then get all passing sample_id values in collection
    collection (str) = Name of collection to use for filtering, defaults to "httr_well"
    good_flags (list of str) = Acceptable flags for samples included in the filtered results

    Return Value:
    (list of str) = List of samples in ids (or in whole collection if ids=[]) with passing QC fields
    """
    sys.exist("ERROR: filterByFlag function in db.well module not written yet.")

# TO DO: A function to identify individual treatments (within each plate
# group) where only 1 replicate remains unflagged
