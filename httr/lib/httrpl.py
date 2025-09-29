''' Primary API to Python httrpl Functions
 This module consists of high-level functions for performing key steps in pipelining of HTTr data

 TO DO: Config functions should be refactored to have a single config
 parsing function for all tasks, and maybe should be moved to config.py?
'''
import json
import os
import subprocess

import pandas as pd
from bson import json_util
from db.mongo import insertByID, openMongo
from db.raw import findRaw, insertRaw
from gexp.biospyder2 import biospider2

# Import all relevant httrpl sub-modules
# Path to this lib directory must be in sys.path/PYTHONPATH
from httrplcore import PipelineLogger, cleanPath, deflog

biospider = biospider2()
# TO DO: This function should check versions of all relevant tools and
# modules, simply warn when not present
def envReport(
        hisat2: str = 'hisat2',
        samtools: str = 'samtools',
        log: PipelineLogger = deflog):
    """
    Report versions and paths for external tools and critical libraries.

    Checks versions and paths of HISAT2 and Samtools and reports this information to a provided log stream (stderr by default).

    Parameters:
    hisat2 (str): Path to hisat2 binary (by default, assumes hisat2 is in user PATH)
    samtools (str): Path to samtools binary (by default, assumes samtools is in user PATH)
    log (PipelineLogger): Where to write all path/version info (stderr by default)
    """
    log.write('HISAT2 path and version:')
    log.write(
        subprocess.check_output(
            hisat2 + ' --version',
            stderr=subprocess.STDOUT,
            shell=True).decode())
    log.write('Samtools path and version:')
    if samtools == 'samtools':
        log.write(
            subprocess.check_output(
                'which samtools',
                stderr=subprocess.STDOUT,
                shell=True).decode())
    else:
        log.write(samtools)
    log.write(
        subprocess.check_output(
            'samtools --version',
            stderr=subprocess.STDOUT,
            shell=True).decode())
    # HTSeq is currently only used in biospyderDev, so not reporting here
    # TO DO: There may be a way to check this without loading the package?
    # log.write('\nUsing HTSeq lib version: %s\n' % HTSeq.__version__)
    log.write('Using pandas version: %s\n' % pd.__version__)


def rawConfig(config: dict, log: PipelineLogger = deflog) -> dict:
    """
    Validate and fill in default values to a dictionary of config settings for httr_raw processing.

    Starting from a dict of user-specified values, fill in remaining values and apply any additional logic.

    Parameters:
    config (dict): Parameter,setting pairs of user-specified values
    log (PipelineLogger): Where to send any warning/debug messages

    Returns:
    dict with original user-specified values + defaults for missing parameters
    """
    # The config file should have a "task" parameter set to "raw" - fill in if
    # missing, warn if set to something else
    if 'task' not in config:
        config['task'] = "raw"
        log.write("Setting 'task':'raw' by default.", dbg=True)
    elif config['task'] != "raw":
        log.warning(
            "config['task'] = '%s'; may not be a config setup for raw step?" %
            config['task'])

    # The following settings are required (WARN if missing, fill with default values):
    # fq_files must equal one of:
    # 1) a list of fastq files
    # 2) a string indicating wildcard pattern for matching target fastq files
    # 3) a list of dicts with fq_file and sample_id keys
    # When missing, set to *.fastq by default, which will lead to trying to
    # find all fastq files in curwd or dir specified by fq_path
    if 'fq_files' not in config:
        log.warning("Missing fq_files parameter - using *.fastq by default")
        config['fq_files'] = "*.fastq"
    elif isinstance(config['fq_files'], str):
        log.write(
            "fq_files is a single string - will be treated as glob pattern to identify multiple files, sample_id will be inferred for each matching file.",
            dbg=True)
    elif isinstance(config['fq_files'], list):
        if len(config['fq_files']) is 0:
            log.warning(
                "fq_files is an empty list - no way to infer files to process.")
        else:
            log.write(
                "fq_files is a list, each entry is assumed to either be a file name, or a dict with fq_file and sample_id fields.",
                dbg=True)

    # out_file = the output json file to save to
    if 'out_file' not in config:
        log.warning(
            "Missing out_file parameter - setting to raw.json by default")
        config['out_file'] = "raw.json"

    # DB settings - at a minimum need a DB host and name,
    # if these are missing, set to None by default (should be interpreted as
    # writing to disk only)
    if 'db_host' not in config:
        config['db_host'] = None
        config['db_name'] = None
    else:
        # TO DO: Check that db_host is a valid URL?
        # Check that db_name is also defined, otherwise give a warning
        if 'db_name' not in config:
            log.warning("db_host is defined but db_name is missing.")
            config['db_name'] = None
        # Set defaults for other DB-related params - what collection to use
        if 'db_collection' not in config:
            if config['task'] == "raw":
                config['db_collection'] = "httr_raw"
            else:
                config['db_collection'] = None

    # p = number of threads to use, if this is missing check for alternate
    # "threads", otherwise default to 1
    if 'p' not in config:
        if 'threads' in config:
            log.write("Inferring 'p' from 'threads'.", dbg=True)
            config['p'] = config['threads']
            del config['threads']
        else:
            log.warning(
                "No values specified for p (threads) - defaulting to 1.",
                dbg=True)
            config['p'] = 1
    # TO DO: Validate p as int, warn if p and threads are both specified

    # Modifier parameters - these are applied to core parameters before config gets passed to functions
    # If fq_path specified, append this to the front of each fq_files entry
    if 'fq_path' in config:
        if config['fq_path'] != '':
            if not config['fq_path'].endswith('/'):
                config['fq_path'] += '/'
            if isinstance(config['fq_files'], str):
                config['fq_files'] = config['fq_path'] + config['fq_files']
            elif isinstance(config['fq_files'][0], str):
                config['fq_files'] = map(
                    lambda x: config['fq_path'] + x, config['fq_files'])
            else:
                for i in config['fq_files']:
                    i.update(fq_file=config['fq_path'] + i['fq_file'])

    # If out_path specified, append this to the front of out_file
    if 'out_path' in config:
        if config['out_path'] != '':
            if not config['out_path'].endswith('/'):
                config['out_path'] += '/'
            config['out_file'] = config['out_path'] + config['out_file']

    return config


def rawBatch(config, rerun: bool = False, log: PipelineLogger = deflog):
    """
    Run all httr_raw processing on a batch of samples and output to a file.

    Starting from either a config dict object or a path to a JSON file containing config settings, check all defaults, run the fastqRawInfoMulti function, then output the resulting data to a JSON file and/or to MongoDB.

    Parameters:
    config (str or dict): If string, assume a path to file and attempt to load with json.load, otherwise treat as dict of config settings
    rerun (bool): Default to use for rerun IF not defined in config
    log (PipelineLogger): Where to send any warning/debug messages
    """
    if isinstance(config, str):
        # NOTE: Previously used yaml.safe_load in place of json.load because in Python 2, the json package converted all strings to unicode and rest of code is not built for that. However, in Python 3 it appears to be safe to just use json.load, and this is more consistent with rest of JSON handling throughout this code library
        # config = yaml.safe_load(open(config, 'r'))
        config = json.load(open(config, 'r'))

    # Override dbg/rerun/strict if defined in config, including the settings
    # in log
    if 'dbg' in config:
        log.dbg = config['dbg']
    if 'rerun' in config:
        rerun = config['rerun']
    if 'strict' in config:
        log.strict = config['strict']

    # Fill in defaults for config, handle extra logic of fastq/out paths:
    config = rawConfig(config, log=log)

    # Clean out_file of escape chars
    # TO DO: Could also handle clean up of all other paths here just to be on
    # the safe side, and ideally identify any problems at the start
    config['out_file'] = cleanPath(config['out_file'], log=log)

    # If out_file exists, do nothing
    if os.path.exists(config['out_file']) and not rerun:
        log.write("%s already exists - nothing to run." % config['out_file'])
    else:
        RAW0 = biospider.fastqRawInfoMulti(
            fq_files=config['fq_files'],
            p=config['p'],
            log=log)
        # Output to a single json file...
        # Uses handler from bson module to correctly handle datetime objects
        # Note that this will add a tzinfo member to the datetime object, which should be removed when reading back in
        # e.g. when reading httr_raw data from json files, each mtime member should be transformed using:
        #  mtime = datetime.combine(mtime.date(), mtime.time(), None)
        log.write(
            'Writing final raw output for %i fastq files to: %s' %
            (len(RAW0), config['out_file']), timestamp=True)
        json.dump(
            RAW0,
            open(
                config['out_file'],
                'w'),
            default=json_util.default,
            indent=1)
        # If config['db_host'] is set, then attempt to connect to DB and push data into there as well
        # NOTE: If output JSON file already exists but data didn't end up in DB, this script won't do anything
        # For these cases, will need to either move/delete the output JSON
        # file, or manually insert the JSON content into DB
        if ('db_host' in config) and (config['db_host'] is not None):
            log.write(
                'Connecting to %s/%s' %
                (config['db_host'],
                 config['db_name']),
                timestamp=True)
            # NOTE: As currently designed, the user/passwd params MUST come from other files in user's home directory
            # see lib/db/mongo.py for more info on that
            DB = openMongo(host=config['db_host'], db=config['db_name'])
            id_list = insertRaw(
                DB, RAW0, collection=config['db_collection'],
                rerun=rerun, log=log)
            log.write('Inserted %i documents into %s collection.' %
                      (len(id_list), config['db_collection']), timestamp=True)
            if len(id_list) != len(RAW0):
                log.warning(
                    'Attempted to insert %i documents, but database confirmed insertion of %i documents.' %
                    (len(RAW0), len(id_list)))
        log.write('Task completed successfully!', timestamp=True)

# TO DO: At some point should make it so that fasta/index related fields
# can be pulled from httr_study collection


def countConfig(config: dict, log: PipelineLogger = deflog) -> dict:
    """
    Fill in default values to a dictionary of config settings for httr_counts processing

    Starting from a dict of user-specified values, fill in remaining values and apply any additional logic.

    Parameters:
    config (dict): Parameter,setting pairs of user-specified values, should have task=counts
    log (PipelineLogger): Where to send any warning/debug messages

    Returns:
    dict with original user-specified values + defaults for missing parameters
    """
    # The config file should have a "task" parameter set to "counts" - fill in
    # if missing, warn if set to something else
    if 'task' not in config:
        config['task'] = "counts"
        log.write("Setting 'task':'counts' by default.", dbg=True)
    elif config['task'] != "counts":
        log.warning(
            "config['task'] = '%s'; may not be a config setup for counts step?" %
            config['task'])

    # The following settings are required (WARN if missing, fill with default values):
    # sample_ids = must be a list of sample_ids to look up in httr_raw, empty
    # by default
    if 'sample_ids' not in config:
        log.warning(
            "Missing sample_ids parameter - no sample processing will be run.")
        config['sample_ids'] = []

    # Settings for specifying the fasta/hisat2 paths for the probe index
    # db_ind or db_fa = If db_fa only, then auto-fill db_ind with a matching pattern
    # If db_ind specified, then db_fa is not needed
    if ('db_fa' not in config) and ('db_ind' not in config):
        log.warning(
            "Missing both db_fa and db_ind - using human-wt-biospyder.fa by default")
        config['db_fa'] = "human-wt-biospyder.fa"
    if 'db_ind' not in config:
        def_ind = config['db_fa']
        if def_ind.endswith('.fa'):
            def_ind = def_ind.replace('.fa', '')
        if def_ind.endwith('.fasta'):
            def_ind = def_ind.replace('.fasta', '')
        def_ind += '/' + os.path.basename(def_ind)
        log.warning(
            "db_ind not specified, setting to '%s' based on db_fa." %
            def_ind)
        config['db_ind'] = def_ind

    # out_file = the output tsv file to save to
    if 'out_file' not in config:
        log.warning(
            "Missing out_file parameter - setting to counts.tsv by default")
        config['out_file'] = "counts.tsv"

    # DB settings - at a minimum need a DB host and name, and db_insert = True/False (assume True)
    # if these are missing, set to None by default
    # NOTE: DB settings can be given with db_insert=False, this allows reading
    # supporting info from DB without inserting the counts results
    if 'db_host' not in config:
        config['db_host'] = None
        config['db_name'] = None
        if 'db_insert' not in config:
            config['db_insert'] = False
    else:
        # TO DO: Check that db_host is a valid URL?
        # If not defined, assume db_insert = True when db_host is given
        if 'db_insert' not in config:
            config['db_insert'] = True
        # Check that db_name is also defined, otherwise give a warning
        if 'db_name' not in config:
            log.warning("db_host is defined but db_name is missing.")
            config['db_name'] = None
    # Set defaults for other DB-related params - what collection to use for
    # inserting count data?
    if 'db_collection' not in config:
        if config['task'] == "counts":
            config['db_collection'] = "httr_counts"
        else:
            config['db_collection'] = None
    # Also specify names of other relevant collections that may need to pull
    # data from:
    if 'db_well_trt' not in config:
        config['db_well_trt'] = "httr_well_trt"
    if 'db_raw' not in config:
        config['db_raw'] = "httr_raw"
    if 'db_study' not in config:
        config['db_study'] = "httr_study"

    # If db_path specified, append this to the front of db_ind and/or db_fa:
    if 'db_path' in config:
        if config['db_path'] != '':
            if not config['db_path'].endswith('/'):
                config['db_path'] += '/'
            if 'db_ind' in config:
                config['db_ind'] = config['db_path'] + config['db_ind']
            if 'db_fa' in config:
                config['db_fa'] = config['db_path'] + config['db_fa']

    # If out_path specified, append this to the front of out_file
    if 'out_path' in config:
        if config['out_path'] != '':
            if not config['out_path'].endswith('/'):
                config['out_path'] += '/'
            config['out_file'] = config['out_path'] + config['out_file']

    return config


def countBatch(config, rerun: bool = False, log: PipelineLogger = deflog):
    """
    Run all processing on a batch of samples and output to a file/insert into DB

    Starting from either a config dict object or a path to a JSON file containing config settings, check all defaults, run the alignCountBatchRaw function, then buildCountTable and output the resulting table to a TSV file, and optionally insert into httr_counts collection of DB.

    Parameters:
    config (str or dict): If string, assume a path to file and attempt to load with json.load, otherwise treat as dict of config settings
    rerun (bool): Default to use for rerun IF not defined in config
    log (PipelineLogger): Where to send any warning/debug messages
    """
    if isinstance(config, str):
        # NOTE: Previously used yaml.safe_load in place of json.load because in Python 2, the json package converted all strings to unicode and rest of code is not built for that. However, in Python 3 it appears to be safe to just use json.load, and this is more consistent with rest of JSON handling throughout this code library
        # config = yaml.safe_load(open(config, 'r'))
        config = json.load(open(config, 'r'))

    # Override dbg/rerun/strict if defined in config, including the settings
    # in log
    if 'dbg' in config:
        log.dbg = config['dbg']
    if 'rerun' in config:
        rerun = config['rerun']
    if 'strict' in config:
        log.strict = config['strict']

    # Fill in defaults for config:
    config = countConfig(config, log=log)

    # Clean out_file of escape chars
    # TO DO: Could also handle clean up of all other paths here just to be on
    # the safe side, and ideally identify any problems at the start
    config['out_file'] = cleanPath(config['out_file'], log=log)

    if len(config['sample_ids']) == 0:
        # If no sample_ids specified, do nothing
        log.write(
            "No sample_ids specified in config - nothing to run.",
            dbg=True)
    elif os.path.exists(config['out_file']) and not rerun:
        # If out_file exists, do nothing
        log.write(
            "%s already exists - nothing to run." %
            config['out_file'], dbg=True)
    else:
        # Otherwise, open the DB connection and run the full loop
        # Open DB connection:
        log.write(
            "Connecting to %s/%s..." %
            (config['db_host'],
             config['db_name']),
            dbg=True,
            timestamp=True)
        DB = openMongo(host=config['db_host'], db=config['db_name'])
        # Dump the httr_raw entries associated with sample_ids into a list
        log.write("Extracting data from %s for %i sample_ids..." %
                  (config['db_raw'], len(config['sample_ids'])))
        RAW0 = findRaw(
            DB, ids=config['sample_ids'],
            collection=config['db_raw'],
            dump=True, timestamp=True)
        if len(RAW0) == len(config['sample_ids']):
            log.write(
                "Database returned %i matching documents." %
                len(RAW0), dbg=True, timestamp=True)
        else:
            log.warning(
                "Database returned %i matching documents, expected %i." %
                (len(RAW0), len(
                    config['sample_ids'])))
        log.write(
            "Starting main align and count loop...",
            dbg=True,
            timestamp=True)
        COUNTS0 = biospider.alignCountBatchRaw(raw_batch=RAW0, log=log, **config)
        # TO DO: Make sure all fields are present?
        log.write(
            'Reshaping data to TSV output format.',
            dbg=True,
            timestamp=True)
        # Convert to a pandas data frame for output to TSV:
        # TO DO: buildCountTable should also drop some unnecessary columns like
        # raw_id?
        TSV0 = biospider.buildCountTable(COUNTS0)
        # Output to a single table in the style that Imran has been using (wide
        # table)
        log.write(
            'Writing final table to: %s' %
            config['out_file'],
            dbg=True,
            timestamp=True)
        TSV0.to_csv(config['out_file'], sep="\t")
        if config['db_insert']:
            # Insert final data structure into DB - will automatically warn if not all documents get inserted
            # TO DO: Replace this with a call to insertCounts when there is a
            # function that does additional checks specific to httr_counts
            # collection
            insertByID(
                DB=DB,
                collection=config['db_collection'],
                docs=COUNTS0,
                rerun=rerun,
                log=log)
        log.write('Script completed successfully!', dbg=True, timestamp=True)

