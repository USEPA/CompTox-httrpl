"""
Functions for building the httr_trt_grp_cmp collection based on httr_well
Currently all functions take a pymongo DB connection as one of the arguments
Use db.mongo openMongo function to open mongo connections to specific DB with standardized credentials
These functions are currently tailored to standard EPA chemical screen
designs
"""

import pymongo
from db.mongo import *
from db.mongo import deleteByID, findByID
from httrplcore import *
from httrplcore import PipelineLogger, deflog

# The default collection name for storing well data:
# Technically the httr_well_trt collection is also compatible with the code below,
# but that collection does not include additional QC flags based on fastq and counts
# For the most complete QC filtering, httr_well should be generated first
# and used here
HTTR_WELL_NAME = "httr_well"

# The default collection name for storing treatment group information:
HTTR_GRP_NAME = "httr_trt_grp_cmp"

# Given a list of treated and control sample IDs, make an entry in httr_trt_grp_cmp and auto-fill all fields as intelligently as possible
# TO DO: Could check kwargs to overwrite any field where needed
# TO DO: Add a verify option to re-run this function and just check that
# the existing results in the DB match what is generated here


def treatGroupFromSamples(
        DB: pymongo.MongoClient,
        trt_wells: list,
        ctrl_wells: list,
        trt_prop_fields: list = [
            "chem_id",
            "conc",
            "conc_unit",
            "dose_level",
            "stype"],
    both_prop_fields: list = [
            "media",
            "timeh",
            "pg_id",
            "block_id"],
        ctrl_desc_field: str = "stype",
        grp_id_opts: list = [],
        well: str = HTTR_WELL_NAME,
        trt_grp: str = HTTR_GRP_NAME,
        rerun: bool = False,
        db_insert=True,
        log: PipelineLogger = deflog,
        **kwargs) -> dict:
    """
    Construct a document for httr_trt_grp_cmp collection.

    Given a list of treatment and control samples, construct an appropriate document and insert into httr_trt_grp_cmp

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    trt_wells (list of str) = Sample IDs of all treated wells in the comparison group
    ctrl_wells (list of str) = Sample IDs of all control wells in the the comparison group
    trt_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate chem_id, conc, conc_unit, dose_level, and stype fields.
    both_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells AND ctrl_wells, and the singular value should be propagated to to field of same name in httr_trt_grp_cmp collection. Default is to propagate media, timeh, pg_id, and block_id.
    ctrl_desc_field (str) = Field to propagate from ctrl_wells to ctrl field in httr_trt_grp_cmp document, defaults to "stype"
    grp_id_opts (list of str) = Additional modifiers to determine trt_grp_id, "bl" = append block ID, "pg" = append plate group ID, "pl" = append plate ID, "vs" = use trt_name for trt_wells and ctrl_wells, applied in the order specified, defaults to simple use only trt_name from trt_wells
    well (str) = Name of collection with individual well treatment data, default is "httr_well"
    trt_grp (str) = Name of collection with treatment group data, default is "httr_trt_grp_cmp"
    rerun (bool) = If document with same trt_grp_id exists already, should it be replaced? Default is False
    db_insert (bool) = Whether to insert the new document into database at all, Default is True
    log (httrplcore.PipelineLogger) = Log handler

    Return Value:
    (dict) = Document that was inserted into trt_grp collection
    """
    # Get the collection objects and build standard queries
    httr_well = DB[well]
    httr_trt_grp_cmp = DB[trt_grp]
    get_trt_wells = {"sample_id": {"$in": trt_wells}}
    get_ctrl_wells = {"sample_id": {"$in": ctrl_wells}}
    get_all_wells = {"sample_id": {"$in": trt_wells + ctrl_wells}}
    # All trt_wells should have matching trt_name
    trt_grp_id = httr_well.distinct("trt_name", query=get_trt_wells)
    if len(trt_grp_id) != 1:
        log.error(
            "In treatGroupFromSamples, trt_wells contained list of %i sample_id values, expected all to have matching trt_name but found %i distinct values in DB." %
            (len(trt_wells), len(trt_grp_id)))
    trt_grp_id = trt_grp_id[0]
    # Add optional suffixes to trt_grp_id - base on trt wells until after -vs-
    get_name_wells = get_trt_wells
    for id_opt in grp_id_opts:
        if id_opt in ["bl", "pg", "pl"]:
            # block, pg, and palte suffixes:
            if id_opt == "bl":
                opt_field = "block_id"
            elif id_opt == "pg":
                opt_field = "pg_id"
            elif id_opt == "pl":
                opt_field = "plate_id"
            opt_val = httr_well.distinct(opt_field, query=get_name_wells)
            if len(opt_val) != 1:
                log.error(
                    "In treatGroupFromSamples, name_wells contained list of %i sample_id values, expected all to have matching %s but found %i distinct values in DB." %
                    (len(
                        get_name_wells['sample_id']['$in']),
                        opt_field,
                        len(opt_val)))
            trt_grp_id += "_" + id_opt + opt_val[0]
        if id_opt == "vs":
            # Add optional -vs-(ctrl.trt_name) suffix
            ctrl_name = httr_well.distinct("trt_name", query=get_ctrl_wells)
            if len(ctrl_name) != 1:
                log.error(
                    "In treatGroupFromSamples, ctrl_wells contained list of %i sample_id values, expected all to have matching trt_name but found %i distinct values in DB." %
                    (len(ctrl_wells), len(ctrl_name)))
            trt_grp_id += "-vs-" + ctrl_name[0]
            # switch name_wells to both trt_wells and ctrl_wells for any
            # subsequent suffixes
            get_name_wells = get_all_wells
    # Check if this is in DB already (db_insert=True only)
    if db_insert and (httr_trt_grp_cmp.count(dict(trt_grp_id=trt_grp_id)) > 0):
        if rerun:
            # If rerun=True, just delete the existing entry
            deleteByID(
                DB=DB,
                collection=trt_grp,
                ids=[trt_grp_id],
                id_field="trt_grp_id",
                log=log)
        else:
            # If rerun=False, just print a warning and return
            log.warning(
                "%s already contains document for trt_grp_id=%s, rerun=False - nothing new will be written." %
                (trt_grp, trt_grp_id))
            return {}
    # Construct the document for httr_trt_grp_cmp
    trt_grp_doc = dict(trt_grp_id=trt_grp_id)
    # Populate the dicts of trt_wells and ctrl_wells
    trt_grp_doc['ctrl_wells'] = findByID(
        DB=DB,
        collection=well,
        ids=ctrl_wells,
        fields={
            "_id": 0,
            "sample_id": 1,
            "plate_id": 1,
            "well_id": 1},
        dump=True)
    trt_grp_doc['trt_wells'] = findByID(
        DB=DB,
        collection=well,
        ids=trt_wells,
        fields={
            "_id": 0,
            "sample_id": 1,
            "plate_id": 1,
            "well_id": 1},
        dump=True)
    # Get list of all plates
    trt_grp_doc['plates'] = httr_well.distinct("plate_id", query=get_all_wells)
    # Set ctrl = ctrl_desc_field from all ctrl_wells - expect these to be the
    # same for all wells:
    ctrl_type = httr_well.distinct(ctrl_desc_field, query=get_ctrl_wells)
    if len(ctrl_type) == 0:
        log.warning(
            "All %i ctrl_wells in %s for %s were missing %s field" %
            (len(ctrl_wells, well, trt_grp_id, ctrl_desc_field)))
    else:
        if len(ctrl_type) > 1:
            log.warning("%i ctrl_wells in %s for %s had %i %s values (expected 1) - ignoring: %s" %
                        (len(ctrl_wells), well, trt_grp_id, len(ctrl_type), ctrl_desc_field, ctrl_type[1:]))
        trt_grp_doc['ctrl'] = ctrl_type[0]
    # Get chem_id, conc, conc_unit, dose_level, stype for all trt_wells - this
    # should be a single value in each case
    for field in trt_prop_fields:
        field_val = httr_well.distinct(field, query=get_trt_wells)
        if len(field_val) == 0:
            log.write(
                "Treatment wells for %s do not contain %s field" %
                (trt_grp_id, field), dbg=True)
        else:
            if len(field_val) > 1:
                log.warning("Sample IDs for %i trt_wells in %s for %s had %i different %s (expected 1) - ignoring: %s" %
                            (len(trt_wells), well, trt_grp_id, len(field_val), field, field_val[1:]))
            trt_grp_doc[field] = field_val[0]
    # Get media, timeh, pg_id, block_id for all wells - this should be a
    # single value in each case
    for field in both_prop_fields:
        field_val = httr_well.distinct(field, query=get_all_wells)
        if len(field_val) == 0:
            log.write(
                "All wells for %s do not contain %s field" %
                (trt_grp_id, field), dbg=True)
        else:
            if len(field_val) > 1:
                log.warning("Sample IDs for %i wells in %s for %s had %i different %s (expected 1) - ignoring: %s" %
                            (len(trt_wells + ctrl_wells), well, trt_grp_id, len(field_val), field, field_val[1:]))
            trt_grp_doc[field] = field_val[0]
    # Insert into the DB
    if db_insert:
        trt_grp_doc['_id'] = httr_trt_grp_cmp.insert_one(
            trt_grp_doc).inserted_id
    # Return the final document:
    return trt_grp_doc

# Function that generates the trt_grp_cmp for a specific test chemical
# treatment vs DMSO


def chemTreatGroup(DB: pymongo.MongoClient,
                   trt_chem: str,
                   trt_dose: int,
                   trt_type: str = "test sample",
                   ctrl_type: str = "vehicle control",
                   ctrl_chem: str = "DMSO",
                   pg_id: str = None,
                   media: str = None,
                   timeh: int = None,
                   trt_src: str = None,
                   ctrl_src: str = None,
                   qc_flags: list = ["OK"],
                   min_reps: int = 2,
                   well: str = HTTR_WELL_NAME,
                   log: PipelineLogger = deflog,
                   **kwargs) -> dict:
    """
    Construct a document for httr_trt_grp_cmp collection corresponding to a specific chemical and dose level treatment.

    Given a specific chemical and dose, plus optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. Note, if filtering reduces either trt or ctrl group to < min_reps samples this treatment group will be skipped.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    trt_chem (str) = Match to chem_id in httr_well
    trt_dose (int) = Match to dose_level in httr_well (set to None to exclude)
    trt_type (str) = Match to stype in httr_well, defaults to "test sample"
    ctrl_type (str) = Match to stype in httr_well
    ctrl_chem (str) = Match to chem_id in httr_well
    pg_id (str) = Filter both trt and ctrl wells, match to pg_id in httr_well, defaults to None
    media (str) = Filter both trt and ctrl wells, match to media in httr_well, defaults to None
    timeh (str) = Filter both trt and ctrl wells, match to timeh in httr_well, defaults to None
    trt_src (str) = Filter trt wells, match to rna_src field
    ctrl_src (str) = Filter ctrl wells, match to rna_src field
    qc_flags (list of str) = Filter both trt and ctrl wells, match to qc_flag field, defaults to OK only
    min_reps (int) = Minimum number of replicates in trt and ctrl groups, respectively - if either is less, will generate an output message and return empty dict
    well (str) = Name of collection with individual well treatment data, default is "httr_well"
    log (httrplcore.PipelineLogger) = Log handler
    **kwargs = All additional args passed to treatGroupFromSamples

    Return Value:
    (dict) = Document that was inserted into trt_grp collection
    """
    # Build the query for treatment and ctrl wells
    trt_well_query = {}
    ctrl_well_query = {}
    if trt_chem is not None:
        trt_well_query["chem_id"] = trt_chem
    if trt_dose is not None:
        trt_well_query["dose_level"] = trt_dose
    if trt_type is not None:
        trt_well_query["stype"] = trt_type
    if ctrl_type is not None:
        ctrl_well_query["stype"] = ctrl_type
    if ctrl_chem is not None:
        ctrl_well_query["chem_id"] = ctrl_chem
    if pg_id is not None:
        trt_well_query["pg_id"] = pg_id
        ctrl_well_query["pg_id"] = pg_id
    if media is not None:
        trt_well_query["media"] = media
        ctrl_well_query["media"] = media
    if timeh is not None:
        trt_well_query["timeh"] = timeh
        ctrl_well_query["timeh"] = timeh
    if trt_src is not None:
        trt_well_query["rna_src"] = trt_src
    if ctrl_src is not None:
        ctrl_well_query["rna_src"] = ctrl_src
    if qc_flags is not None:
        trt_well_query["qc_flag"] = {"$in": qc_flags}
        ctrl_well_query["qc_flag"] = {"$in": qc_flags}
    # Get sample IDs for both treatments and ctrl wells
    httr_well = DB[well]
    trt_wells = httr_well.distinct("sample_id", trt_well_query)
    ctrl_wells = httr_well.distinct("sample_id", ctrl_well_query)
    # Make sure both trt_wells and ctrl_wells > min_reps long
    if (len(trt_wells) < min_reps) or (len(ctrl_wells) < min_reps):
        log.write(
            "SKIPPING COMPARISON GROUP: Query returned %i trt wells and %i ctrl wells with\n trt_well_query = %s\n ctr_well_query = %s" %
            (len(trt_wells), len(ctrl_wells), trt_well_query, ctrl_well_query))
        return {}
    # Otherwise, pass to treatGroupFromSamples
    return treatGroupFromSamples(
        DB=DB,
        trt_wells=trt_wells,
        ctrl_wells=ctrl_wells,
        well=well,
        log=log,
        **kwargs)

# Function to loop over all plate groups and call chemTreatGroup for each media, timeh, chem_id, dose_level
# TO DO: Parameters to limit to a subset of plate groups, etc.


def allTestGroups(DB: pymongo.MongoClient,
                  well: str = HTTR_WELL_NAME,
                  exp_doses: int = None,
                  log: PipelineLogger = deflog,
                  **kwargs) -> list:
    """
    Construct documents for httr_trt_grp_cmp collection corresponding to each test chemical and dose level treatment.

    Loop over all plate groups, media, timeh, chem_id, and dose_level and create a httr_trt_grp_cmp document for each one.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    well (str) = Name of collection with individual well treatment data, default is "httr_well"
    exp_doses (int) = Expected number of doses for each test chemical - this will suppress some debug messages and only warn if the number of doses does not match
    log (httrplcore.PipelineLogger) = Log handler
    **kwargs = All additional args passed to chemTreatGroup

    Return Value:
    (list of dict) = List of documents that were inserted into trt_grp collection
    """
    # Store all documents in a list, also track the number that were skipped
    # due to insufficient replicates with passing QC flags:
    trt_grp_docs = []
    skip_cnt = 0
    # Get list of pg_id values in httr_well
    httr_well = DB[well]
    all_pg = httr_well.distinct("pg_id")
    for pg_id in sorted(all_pg):
        # Get list of all media on pg_id
        pg_media = httr_well.distinct(
            "media", dict(
                stype="test sample", pg_id=pg_id))
        for media in sorted(pg_media):
            # Get list of all timeh for pg_id, media
            pgm_timeh = httr_well.distinct("timeh", dict(
                stype="test sample", pg_id=pg_id, media=media))
            for timeh in sorted(pgm_timeh):
                pgmt_chems = httr_well.distinct(
                    "chem_id",
                    dict(
                        stype="test sample",
                        pg_id=pg_id,
                        media=media,
                        timeh=timeh))
                log.write(
                    "Generating treatment groups for %i chemicals on plate group %s, media = %s, timeh = %i" %
                    (len(pgmt_chems), pg_id, media, timeh))
                if exp_doses is not None:
                    log.write(
                        " Expecting each chemical to have %i dose levels." %
                        exp_doses, dbg=True)
                for chem_id in sorted(pgmt_chems):
                    doses = httr_well.distinct(
                        "dose_level",
                        dict(
                            stype="test sample",
                            pg_id=pg_id,
                            media=media,
                            timeh=timeh,
                            chem_id=chem_id))
                    # Report number of doses or warn if not matching
                    # expectations
                    if exp_doses is None:
                        log.write(
                            " chem_id = %s => %i dose_levels" %
                            (chem_id, len(doses)), dbg=True)
                    elif len(doses) != exp_doses:
                        log.warning(
                            "chem_id = %s had %i dose_levels, expected %i" %
                            (chem_id, len(doses), exp_doses))
                    for dose_level in sorted(doses):
                        db_doc = chemTreatGroup(
                            DB=DB,
                            trt_chem=chem_id,
                            trt_dose=dose_level,
                            pg_id=pg_id,
                            media=media,
                            timeh=timeh,
                            well=well,
                            log=log,
                            **kwargs)
                        if len(db_doc) == 0:
                            skip_cnt += 1
                        else:
                            trt_grp_docs.append(db_doc)
    # Report how many were skipped
    if skip_cnt > 0:
        log.write(
            "%i treatment comparison groups were skipped due to insufficient replicates." %
            skip_cnt)
    log.write(
        "A total of %i treatment comparison groups were generated." %
        len(trt_grp_docs))
    return trt_grp_docs


def refChemGroup(
        DB: pymongo.MongoClient,
        trt_chem: str,
        pg_id: str = None,
        media: str = None,
        timeh: int = None,
        trt_dose: int = None,
        trt_type: str = "reference chemical",
        trt_prop_fields: list = [
            "chem_id",
            "conc",
            "conc_unit",
            "stype"],
    log: PipelineLogger = deflog,
        **kwargs) -> dict:
    """
    Construct a document for httr_trt_grp_cmp collection corresponding to a specific reference chemical treatment.

    Given a specific reference chemical and plate group (or media/timeh that specify plate group), plus any optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. This function is just a convenenience wrapper to chemTreatGroup with modified defaults.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    trt_chem (str) = Match to chem_id in httr_well
    pg_id (str) = Filter both trt and ctrl wells, match to pg_id in httr_well, defaults to None
    media (str) = Filter both trt and ctrl wells, match to media in httr_well, defaults to None
    timeh (str) = Filter both trt and ctrl wells, match to timeh in httr_well, defaults to None
    trt_dose (int) = Match to dose_level in httr_well - Excluded by default
    trt_type (str) = Match to stype in httr_well, defaults to "reference chemical"
    trt_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate chem_id, conc, conc_unit, and stype fields (exclude dose_level here).
    log (httrplcore.PipelineLogger) = Log handler
    **kwargs = All additional args passed to treatGroupFromSamples

    Return Value:
    (dict) = Document that was inserted into trt_grp collection
    """
    # Warn if neither pg_id nor media+timeh were specified:
    if (pg_id is None) and ((media is None) or (timeh is None)):
        log.warning("Neither pg_id or media,timeh were specified in call to refChemGroup, this will probably not be able to generate a valid trt_grp_cmp document.")
    # Pass to chemTreatGroup:
    return chemTreatGroup(
        DB=DB,
        trt_chem=trt_chem,
        trt_dose=trt_dose,
        trt_type=trt_type,
        pg_id=pg_id,
        media=media,
        timeh=timeh,
        trt_prop_fields=trt_prop_fields,
        log=log,
        **kwargs)

# Function to loop over all plate groups, media, and times and then call
# refChemGroup


def allRefGroups(DB: pymongo.MongoClient,
                 trt_type: str = "reference chemical",
                 well: str = HTTR_WELL_NAME,
                 log: PipelineLogger = deflog,
                 **kwargs) -> list:
    """
    Construct documents for httr_trt_grp_cmp collection corresponding to each reference chemical (single conc).

    Loop over all plate groups, media, timeh, chem_id (reference chemicals only) and create a httr_trt_grp_cmp document for each one.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    trt_type (str) = Filter stype to get list of plate groups, media, timeh, and chem_id
    well (str) = Name of collection with individual well treatment data, default is "httr_well"
    log (httrplcore.PipelineLogger) = Log handler
    **kwargs = All additional args passed to chemTreatGroup

    Return Value:
    (list of dict) = List of documents that were inserted into trt_grp collection
    """
    # Store all documents in a list, also track the number that were skipped
    # due to insufficient replicates with passing QC flags:
    trt_grp_docs = []
    skip_cnt = 0
    # Get list of pg_id values in httr_well
    httr_well = DB[well]
    all_pg = httr_well.distinct("pg_id")
    for pg_id in sorted(all_pg):
        # Get list of all media on pg_id
        pg_media = httr_well.distinct(
            "media", dict(stype=trt_type, pg_id=pg_id))
        for media in sorted(pg_media):
            # Get list of all timeh for pg_id, media
            pgm_timeh = httr_well.distinct("timeh", dict(
                stype=trt_type, pg_id=pg_id, media=media))
            for timeh in sorted(pgm_timeh):
                pgmt_chems = httr_well.distinct(
                    "chem_id",
                    dict(
                        stype=trt_type,
                        pg_id=pg_id,
                        media=media,
                        timeh=timeh))
                log.write(
                    "Generating treatment groups for %i reference chemicals on plate group %s, media = %s, timeh = %i" %
                    (len(pgmt_chems), pg_id, media, timeh))
                for chem_id in sorted(pgmt_chems):
                    db_doc = refChemGroup(
                        DB=DB,
                        trt_chem=chem_id,
                        trt_type=trt_type,
                        pg_id=pg_id,
                        media=media,
                        timeh=timeh,
                        well=well,
                        log=log,
                        **kwargs)
                    if len(db_doc) == 0:
                        skip_cnt += 1
                    else:
                        trt_grp_docs.append(db_doc)
    # Report how many were skipped
    if skip_cnt > 0:
        log.write(
            "%i treatment comparison groups were skipped due to insufficient replicates." %
            skip_cnt)
    log.write(
        "A total of %i treatment comparison groups were generated." %
        len(trt_grp_docs))
    return trt_grp_docs

# Alternate version of allRefGroups that also loops over and handles
# reference chemicals with multiple dose levels


def allRefDoseGroups(
        DB: pymongo.MongoClient,
        trt_type: str = "reference chemical",
        trt_prop_fields=[
            "chem_id",
            "conc",
            "conc_unit",
            "dose_level",
            "stype"],
    grp_id_opts=["pg"],
    well: str = HTTR_WELL_NAME,
    exp_doses: dict = None,
    log: PipelineLogger = deflog,
        **kwargs) -> list:
    """
    Construct documents for httr_trt_grp_cmp collection corresponding to each reference chemical (single or multi conc).

    Loop over all plate groups, media, timeh, chem_id, dose_level (reference chemicals only) and create a httr_trt_grp_cmp document for each one. Note that to use this version, all reference chemical wells in httr_well_trt MUST have dose_level defined (even single conc reference chemicals). For studies with only single conc reference chemicals that do NOT have a dose_level field, use allRefGroups instead. Also, unlike allRefGroups, this will automatically append plate group ID as part of the trt_name when creating docs.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    trt_type (str) = Filter stype to get list of plate groups, media, timeh, and chem_id
    trt_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate chem_id, conc, conc_unit, dose_level, and stype fields
    grp_id_opts (list) = Options to modify trt_grp_id generation, "pg" is usually sufficient for reference chemicals to create distinct group IDs for each plate group
    well (str) = Name of collection with individual well treatment data, default is "httr_well"
    exp_doses (dict) = Expected number of doses for each reference chemical (key = chem_id, value = doses) - this will suppress some debug messages and only warn if the number of doses does not match
    log (httrplcore.PipelineLogger) = Log handler
    **kwargs = All additional args passed to chemTreatGroup

    Return Value:
    (list of dict) = List of documents that were inserted into trt_grp collection
    """
    # Store all documents in a list, also track the number that were skipped
    # due to insufficient replicates with passing QC flags:
    trt_grp_docs = []
    skip_cnt = 0
    # Get list of pg_id values in httr_well
    httr_well = DB[well]
    all_pg = httr_well.distinct("pg_id")
    for pg_id in sorted(all_pg):
        # Get list of all media on pg_id
        pg_media = httr_well.distinct(
            "media", dict(stype=trt_type, pg_id=pg_id))
        for media in sorted(pg_media):
            # Get list of all timeh for pg_id, media
            pgm_timeh = httr_well.distinct("timeh", dict(
                stype=trt_type, pg_id=pg_id, media=media))
            for timeh in sorted(pgm_timeh):
                pgmt_chems = httr_well.distinct(
                    "chem_id",
                    dict(
                        stype=trt_type,
                        pg_id=pg_id,
                        media=media,
                        timeh=timeh))
                log.write(
                    "Generating treatment groups for %i reference chemicals on plate group %s, media = %s, timeh = %i" %
                    (len(pgmt_chems), pg_id, media, timeh))
                for chem_id in sorted(pgmt_chems):
                    doses = httr_well.distinct(
                        "dose_level",
                        dict(
                            stype=trt_type,
                            pg_id=pg_id,
                            media=media,
                            timeh=timeh,
                            chem_id=chem_id))
                    # Report number of doses or warn if not matching
                    # expectations
                    if exp_doses is None:
                        log.write(
                            " chem_id = %s => %i dose_levels" %
                            (chem_id, len(doses)), dbg=True)
                    elif chem_id not in exp_doses:
                        log.warning(
                            " chem_id = %s was not expected, found %i dose_levels" %
                            (chem_id, len(doses)))
                    elif len(doses) != exp_doses[chem_id]:
                        log.warning(
                            "chem_id = %s had %i dose_levels, expected %i" %
                            (chem_id, len(doses), exp_doses))
                    for dose_level in sorted(doses):
                        db_doc = refChemGroup(
                            DB=DB,
                            trt_chem=chem_id,
                            trt_type=trt_type,
                            pg_id=pg_id,
                            media=media,
                            timeh=timeh,
                            trt_dose=dose_level,
                            trt_prop_fields=trt_prop_fields,
                            grp_id_opts=grp_id_opts,
                            well=well,
                            log=log,
                            **kwargs)
                        if len(db_doc) == 0:
                            skip_cnt += 1
                        else:
                            trt_grp_docs.append(db_doc)
    # Report how many were skipped
    if skip_cnt > 0:
        log.write(
            "%i treatment comparison groups were skipped due to insufficient replicates." %
            skip_cnt)
    log.write(
        "A total of %i treatment comparison groups were generated." %
        len(trt_grp_docs))
    return trt_grp_docs


def bulkLysateGroup(
        DB: pymongo.MongoClient,
        pg_id: str,
        trt_chem: str = "TSA",
        trt_dose: int = None,
        trt_type: str = "QC sample",
        ctrl_type: str = "QC sample",
        ctrl_chem: str = "DMSO",
        rna_src: str = "Bulk Lysate",
        trt_prop_fields: list = [
            "chem_id",
            "conc",
            "conc_unit",
            "stype"],
    both_prop_fields: list = [
            "pg_id",
            "block_id"],
        ctrl_desc_field: str = "chem_id",
        grp_id_opts: list = ["pg"],
        log: PipelineLogger = deflog,
        **kwargs) -> dict:
    """
    Construct a document for httr_trt_grp_cmp collection corresponding to a specific bulk lysate chemical treatment.

    Given a specific plate group, plus any optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. This function is just a convenenience wrapper to chemTreatGroup with modified defaults.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    pg_id (str) = Filter both trt and ctrl wells, match to pg_id in httr_well, required for bulk lysate groups
    trt_chem (str) = Match to chem_id in httr_well, defaults to "TSA"
    trt_dose (int) = Match to dose_level in httr_well - Excluded by default
    trt_type (str) = Match to stype in httr_well, defaults to "QC sample"
    ctrl_type (str) = Match to stype in httr_well, defaults to "QC sample"
    ctrl_chem (str) = Match to chem_id in httr_well, defaults to "DMSO"
    rna_src (str) = Match to rna_src in httr_well for both trt and ctrl wells, defaults to "Bulk Lysate"
    trt_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate chem_id, conc, conc_unit, and stype fields (exclude dose_level here).
    both_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells AND ctrl_wells, and the singular value should be propagated to to field of same name in httr_trt_grp_cmp collection. Default is to propagate pg_id and block_id only for bulk lysate comparisons
    ctrl_desc_field (str) = Field to propagate from ctrl_wells to ctrl field in httr_trt_grp_cmp document, defaults to "chem_id" for bulk lysate comparisons
    grp_id_opts (list) = Options to modify trt_grp_id generation, "pg" is usually sufficient for bulk lysate to create distinct group IDs for each plate group
    log (httrplcore.PipelineLogger) = Log handler
    **kwargs = All additional args passed to treatGroupFromSamples

    Return Value:
    (dict) = Document that was inserted into trt_grp collection
    """
    # Pass to chemTreatGroup:
    return chemTreatGroup(
        DB=DB,
        trt_chem=trt_chem,
        trt_dose=trt_dose,
        trt_type=trt_type,
        ctrl_type=ctrl_type,
        pg_id=pg_id,
        trt_src=rna_src,
        ctrl_src=rna_src,
        trt_prop_fields=trt_prop_fields,
        both_prop_fields=both_prop_fields,
        ctrl_desc_field=ctrl_desc_field,
        grp_id_opts=grp_id_opts,
        log=log,
        **kwargs)


def allBulkLysateGroups(DB: pymongo.MongoClient,
                        trt_type: str = "QC sample",
                        rna_src: str = "Bulk Lysate",
                        ctrl_chem: str = "DMSO",
                        well: str = HTTR_WELL_NAME,
                        log: PipelineLogger = deflog,
                        **kwargs) -> list:
    """
    Construct documents for httr_trt_grp_cmp collection corresponding to each bulk lysate QC sample comparison (single conc treatment).

    Loop over all plate groups, chem_id (bulk lysate chemicals only) and create a httr_trt_grp_cmp document for each one.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    trt_type (str) = Filter stype to get list of plate groups, chem_id
    rna_src (str) = Filter rna_src to get list of plate groups, chem_id
    ctrl_chem (str) = Skip this chemical when looping over chem_id
    well (str) = Name of collection with individual well treatment data, default is "httr_well"
    log (httrplcore.PipelineLogger) = Log handler
    **kwargs = All additional args passed to chemTreatGroup

    Return Value:
    (list of dict) = List of documents that were inserted into trt_grp collection
    """
    # Store all documents in a list, also track the number that were skipped
    # due to insufficient replicates with passing QC flags:
    trt_grp_docs = []
    skip_cnt = 0
    # Get list of pg_id values in httr_well
    httr_well = DB[well]
    all_pg = httr_well.distinct("pg_id")
    for pg_id in sorted(all_pg):
        # Get list of all chemicals with stype=trt_type and rna_src=rna_src,
        # but leave out ctrl_chem
        pg_chems = httr_well.distinct(
            "chem_id",
            dict(
                stype=trt_type,
                pg_id=pg_id,
                rna_src=rna_src))
        pg_chems = list(set(pg_chems) - set([ctrl_chem]))
        log.write(
            "Generating treatment groups for %i bulk lysate chemical treatments on plate group %s" %
            (len(pg_chems), pg_id))
        for chem_id in sorted(pg_chems):
            db_doc = bulkLysateGroup(
                DB=DB,
                trt_chem=chem_id,
                trt_type=trt_type,
                pg_id=pg_id,
                rna_src=rna_src,
                ctrl_chem=ctrl_chem,
                well=well,
                log=log,
                **kwargs)
            if len(db_doc) == 0:
                skip_cnt += 1
            else:
                trt_grp_docs.append(db_doc)
    # Report how many were skipped
    if skip_cnt > 0:
        log.write(
            "%i treatment comparison groups were skipped due to insufficient replicates." %
            skip_cnt)
    log.write(
        "A total of %i treatment comparison groups were generated." %
        len(trt_grp_docs))
    return trt_grp_docs


def refRNAGroup(DB: pymongo.MongoClient,
                pg_id: str,
                trt_type: str = "QC sample",
                ctrl_type: str = "QC sample",
                trt_src: str = "HBRR",
                ctrl_src: str = "UHRR",
                trt_prop_fields: list = ["stype"],
                both_prop_fields: list = ["pg_id", "block_id"],
                ctrl_desc_field: str = "trt_name",
                grp_id_opts: list = ["vs", "pg"],
                log: PipelineLogger = deflog,
                **kwargs) -> dict:
    """
    Construct a document for httr_trt_grp_cmp collection corresponding to a specific reference RNA QC sample comparison.

    Given a specific plate group, plus any optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. This function is just a convenenience wrapper to chemTreatGroup with modified defaults.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    pg_id (str) = Filter both trt and ctrl wells, match to pg_id in httr_well, required for ref RNA groups
    trt_type (str) = Match to stype in httr_well, defaults to "QC sample"
    ctrl_type (str) = Match to stype in httr_well, defaults to "QC sample"
    trt_src (str) = Match to rna_src in httr_well, defaults to "HBRR"
    ctrl_src (str) = Match to rna_src in httr_well, defaults to "UHRR"
    trt_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate just the stype field (exclude all chemical and dose information).
    both_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells AND ctrl_wells, and the singular value should be propagated to to field of same name in httr_trt_grp_cmp collection. Default is to propagate pg_id and block_id only for ref RNA comparisons
    ctrl_desc_field (str) = Field to propagate from ctrl_wells to ctrl field in httr_trt_grp_cmp document, defaults to "trt_name" for ref RNA comparisons
    grp_id_opts (list) = Options to modify trt_grp_id generation, "vs" will capture the pairwise comparison type, and "pg" is usually sufficient for ref RNA to create distinct group IDs for each plate group
    log (httrplcore.PipelineLogger) = Log handler
    **kwargs = All additional args passed to treatGroupFromSamples

    Return Value:
    (dict) = Document that was inserted into trt_grp collection
    """
    # Pass to chemTreatGroup:
    return chemTreatGroup(
        DB=DB,
        trt_chem=None,
        trt_dose=None,
        ctrl_chem=None,
        trt_type=trt_type,
        ctrl_type=ctrl_type,
        pg_id=pg_id,
        trt_src=trt_src,
        ctrl_src=ctrl_src,
        trt_prop_fields=trt_prop_fields,
        both_prop_fields=both_prop_fields,
        ctrl_desc_field=ctrl_desc_field,
        grp_id_opts=grp_id_opts,
        log=log,
        **kwargs)


def allRefRNAGroups(DB: pymongo.MongoClient,
                    well: str = HTTR_WELL_NAME,
                    log: PipelineLogger = deflog,
                    **kwargs) -> list:
    """
    Construct documents for httr_trt_grp_cmp collection corresponding to each reference RNA QC sample comparison (single conc treatment).

    Loop over all plate groups and create a httr_trt_grp_cmp document for each pair of reference RNAs.

    Parameters:
    DB (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
    well (str) = Name of collection with individual well treatment data, default is "httr_well"
    log (httrplcore.PipelineLogger) = Log handler
    **kwargs = All additional args passed to chemTreatGroup

    Return Value:
    (list of dict) = List of documents that were inserted into trt_grp collection
    """
    # Store all documents in a list, also track the number that were skipped
    # due to insufficient replicates with passing QC flags:
    trt_grp_docs = []
    skip_cnt = 0
    # Get list of pg_id values in httr_well
    httr_well = DB[well]
    all_pg = httr_well.distinct("pg_id")
    log.write(
        "Generating treatment groups for reference RNA comparisons on %i plate groups." %
        len(all_pg))
    for pg_id in sorted(all_pg):
        db_doc = refRNAGroup(DB=DB, pg_id=pg_id, well=well, log=log, **kwargs)
        if len(db_doc) == 0:
            skip_cnt += 1
        else:
            trt_grp_docs.append(db_doc)
    # Report how many were skipped
    if skip_cnt > 0:
        log.write(
            "%i treatment comparison groups were skipped due to insufficient replicates." %
            skip_cnt)
    log.write(
        "A total of %i treatment comparison groups were generated." %
        len(trt_grp_docs))
    return trt_grp_docs

# TO DO: Functions to generate all reference chem, bulk lysate, and
# reference RNA comparisons within each plate (as opposed to plate group)
