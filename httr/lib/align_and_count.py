"""Alignment and counting of sequencing data is performed by two discrete functions: *rawBatch.py* and *countBatch.py*

*Goal (desired implementation):*
# User provides a single JSON file with general config parameters and a single CSV file listing all expected samples/fastq files for the project.
# A single python script/function that runs both the rawBatch() and countBatch() functions on all available fastq files with appropriate
 parallelization and batching of samples.
# Outputs should be stored in MongoDB only (no disk outputs).
*Additional Requirements:*
* User-provided JSON config file should have all parameters included in the existing config files for the steps above *except for* the individual
 sample IDs and input/output file paths. This new config file format should include a new parameter pointing to the provided CSV file with all
  sample information.

 +A function to validate this structure and fill in missing parameters with default values will be needed, similar to rawConfig() and countConfig()
  functions in [lib/httrpl.py|https://github.com/USEPA/httrpl_pilot/blob/master/lib/httrpl.py]+

** The config file needs a file name for the corresponding sample table (CSV format) that will now contain the sample_id and fq_file info.
** This config file should keep the fq_path field that was in the original httr_raw config files.  When specified, the script will need to append
 this path to the front of ALL fq_file values in the accompanying sample table, but it should be optional (e.g. the fq_file fields in the table
  could alternatively have the full path to each file).

* The user-provided CSV sample table file should contain a row for each fastq file expected from vendor (currently BioSpyder). This table must
 contain a sample_id column with the sample ID values to be used when databasing the results, and a fq_file column
(*Note:* _This column is labeled fastq_path in the file Derik sent Lionel. This column name will need to be changed to fq_file to reflect the column
 name here_) with complete path to the corresponding fastq file. This table may contain other columns, but all other columns should be ignored at
  this step of the pipeline. +A validation function will be needed to make sure the table has the required columns, and that each sample_id and
   fq_file are unique+.

* Create new top-level function(s) that take the config structure (as a dict) and the sample table (as a pandas data frame) and run the processing
 and DB insert steps on all available fastq files.
* The new top-level function should verify *before* processing that the target DB exists and that the appropriate credentials for write access are
 present in the users key file.

* Existing code should be re-used as much as possible, but without changing existing function - This is to ensure reproductivity of previous.
pipelining analysis scripts. New versions of rawBatch or countBatch functions will likely need to be forked and modified to skip the
output file.
These new versions can be named rawBatchAuto and countBatchAuto

* Missing fastq files should generate a message to the stderr stream, but *should not stop* the processing of the other available files.

* When this function is run with rerun=False, it should skip all samples that already have entries (by sample_id) in the corresponding httr_raw and
 httr_counts, and just output a message to stderr for each sample that is already present in the database.

* Use the deflog instance of PipelineLogger class from [lib/httrplcore.py|https://github.com/USEPA/httrpl_pilot/blob/master/lib/httrplcore.py] for
 all progress messages.
* Samples to be processed (after removing missing files and files already processed) should be split into batches that maximize parallel processing
(rawBatch step) while also minimizing the overhead for storing results before inserting into database. A 'batch_sz' parameter could be added to
 control this, with a default of 100.

* A new command-line python3 script, 'rawCountAuto.py' will take the name of the JSON config file as the only parameter, and pass to the new top
-level function to run all automated processing, similar to the design of rawBatch.py and countBatch.py in the current implementation.

*Acceptance criteria:*
* New function processes fastq files from the user-provided JSON config and sample key CSV files and successfully sotres output of each step into
 the correct mongoDB collections (i.e. 'httr_raw' and 'httr_counts')
* If final function uses a global JSON file (derived from the user-providedd config and sample key files), save this as an output to act as a
 reference
* Acceptance of this ticket depend on completion/incorporation of, HTTR-20, and HTTR-21 into final script/function product
* Correct parallelization for each step is correct (e.g. use of Celery for *rawBatch.py* step)
* All output are stored as collection in mongoDB (_httr_raw_ and _httr_counts_) and not to disk


"""

import json
import os
import subprocess
import sys

import pandas as pd
import pymongo

# Setup path to lib/ - assumes this script is in bin/ subdir next to lib/
TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-2])
TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB


from db.getDB import check_write_access_to_db
from db.mongo import insertByID, openMongo, openMongoParam
from db.jsonDB import json_DB, json_DB_collection 

# Core functions for managing logs, warnings, special handling of path strings:
from gexp.biospyder2 import biospider2 #alignCountBatchRaw, buildCountTable, fastqRawInfoMulti
from httrplcore import PipelineLogger, cleanPath, deflog


class align_and_count:
    def __init__(self, config=None):
        """
        init initializes the class instance by identifying the config file input to be
        processed in the class other methods

        Parameter:
        config info: tuple
        Return:
        None

    """
        if len(config) < 2:
            sys.exit("No config file specified.")
        else:
            self.config = config[1]
        self.log = deflog
        self.config_dir = ""
        if '/' in self.config:
            self.config_dir = self.config.rsplit('/', 1)[0] + '/'

        self.biospider = biospider2()
            
        self.config = json.load(open(self.config, 'r'))
        self.output_dir = self.config.get('output_dir', None)
        if self.output_dir != None and len(self.output_dir.strip()) == 0:
            self.output_dir = None
        self.DB = None
        self.find_fastq_files()
            
    def openMongoParam(self):
        self.get_db()
        return self.DB
        
    def openMongo(self):
        self.get_db()
        return self.DB        

    def get_db(self):
        """
        get_db uses the credentials and db and obtain a connection to the db
        Parameter:
        None - credential and target db are read from config member
        Return:
        None - but the db attribute now is a valid db object that can be used

        """
        if self.DB: return
        if self.output_dir == None:
            self.DB = None
            print(f"config is {self.config}")
            if ('db_host' in self.config) and (self.config['db_host'] is not None):
                self.log.write(
                    'Connecting to %s/%s' %
                    (self.config['db_host'],
                     self.config['db_name']),
                    timestamp=True)
                # NOTE: As currently designed, the user/passwd params MUST come from other files in user's home directory
                # see lib/db/mongo.py for more info on that
                try:
                    self.DB = openMongo(
                        host=self.config['db_host'],
                        db=self.config['db_name'])
                    # self.check_write_access_to_db()
                    check_write_access_to_db(self.DB)

                except Exception as e:
                    print("trying parameter connection instead of uri", flush=True)
                    if (
                        isinstance(e, pymongo.errors.OperationFailure)
                        and "Authentication failed." in str(e)
                        or isinstance(e, pymongo.errors.ServerSelectionTimeoutError)
                    ):
                        self.DB = openMongoParam(
                            host=self.config['db_host'],
                            db=self.config['db_name'])
                        # self.check_write_access_to_db()
                        check_write_access_to_db(self.DB)
        else:
            self.DB = json_DB(self.output_dir)
                                
    def prepare_processing(self):
        rerun = False
        if type(self.config) is str:
            if '/' in self.config:
                self.config_dir = self.config.rsplit('/', 1)[0] + '/'
                print(f"self.config_dir is {self.config_dir}")
            self.config = json.load(open(self.config, 'r'))
        if 'rerun' in self.config:
            rerun = self.config['rerun']
        if self.rawConfig() != 0:
            return rerun, False
        self.get_db()
        if self.DB is None:
            self.log.write("error: no db or write access to db")
            return rerun, False
        return rerun, True

    def process_alignment_and_count(self):
        """
        process_alignment_and_count does the heavy lifting by calling alignCountBatchRaw on each fastq file,
        using multiprocessing and building out the httr_raw and httr_count collections with the results of that process

        various checks occur in that function and also in the biospyder2 file and the HISAT2 library function that are invoked

        Parameter:
        None
        Return:
        None


        """
        
        if os.environ.get("AM_I_IN_A_DOCKER_CONTAINER", None) != "Yes":
        
            if subprocess.getstatusoutput('which samtools')[0] != 0 or subprocess.getstatusoutput('which hisat2')[0] != 0: 
                self.log.write("error: samtools and/or hisat2 not accessible- Quitting")
                return
        rerun, status = self.prepare_processing() 
        if status == False: return

        fq_files = self.config['sample_key']

        # let's get rid of those records that have been processed already when
        # rerun is False
        
        

        if rerun is False:
            fq_files = [
                e for e in fq_files
                if self.DB[self.config['db_collection_counts']].find_one(
                    {'sample_id': e['sample_id']}) is None]
            # necessary potential cleanup to remove inconsistent records (1 in
            # db_collection_raw while none in db_collection_count
            for e in fq_files:
                self.DB[self.config['db_collection_raw']].delete_one(
                    {'sample_id': e['sample_id']})
                    
        offset = 1 if len(fq_files) % self.batch_size != 0 else 0
        for e in range(len(fq_files) // self.batch_size + offset):

            RAW0 = self.biospider.fastqRawInfoMulti(fq_files=fq_files[e * self.batch_size:min(
                len(fq_files), (e + 1) * self.batch_size)], p=self.config['p'], log=self.log)

            id_list = insertByID(
                self.DB,
                collection=self.config['db_collection_raw'],
                docs=RAW0,
                rerun=rerun,
                log=self.log)

            if len(id_list) != len(RAW0):
                self.log.warning(
                    f"Attempted to insert {len(RAW0)} documents into {self.config['db_collection_raw']}, but DB only returned {len(id_list)} new IDs.")

            self.log.write(
                'Inserted %i documents into %s collection.' %
                (len(id_list), self.config['db_collection_raw']), timestamp=True)
            self.log.write('Task completed successfully!', timestamp=True)

            #**************************************************************#

            self.log.write(
                "Starting main align and count loop...",
                dbg=True,
                timestamp=True)
            COUNTS0 = self.biospider.alignCountBatchRaw(
                raw_batch=RAW0,
                log=self.log,
                threads=min(
                    self.config['p'],
                    self.batch_size),
                **self.config)  # HTTR_25
            # TO DO: Make sure all fields are present?
            self.log.write(
                'Reshaping data to TSV output format.',
                dbg=True,
                timestamp=True)
            TSV0 = self.biospider.buildCountTable(COUNTS0)

            if self.config['db_insert']:
                # Insert final data structure into DB - will automatically warn
                # if not all documents get inserted
                id_inserted = insertByID(
                    DB=self.DB, collection=self.config
                    ['db_collection_counts'],
                    docs=COUNTS0, rerun=rerun, log=self.log)
                if len(id_inserted) != len(COUNTS0):
                    self.log.warning(
                        f"Attempted to insert {len(COUNTS0)} documents into {self.config['db_collection_counts']}, but DB only returned {len(id_inserted)} new IDs.")
                if len(id_inserted) != len(id_list):
                    self.log.warning(
                        f"total inserted docs in {self.config['db_collection_raw']} is different than those inserted in {self.config['db_collection_counts']}")

            print(self.DB[self.config['db_collection_raw']].distinct("sample_id",{"md5": "0b646cafa44b4f4d6e2902dbe3007477"}))
            
        self.check_data()
        self.log.write(
            'Script completed successfully!',
            dbg=True,
            timestamp=True)

    def check_data(self):
        """
        check_data checks that the sample_ids are the same in both collections httr_counts and httr_raw
        Paramter:
        None
        Return:
        prints out a log warning which could lead to processing being halted (if strict option set)


        """
        # test that the sample_ids are the same in both collections
        raw_items = self.DB[self.config['db_collection_raw']].find(
            {}, {'sample_id': 1, '_id': 0})
        counts_items = self.DB[self.config['db_collection_counts']].find(
            {}, {'sample_id': 1, '_id': 0})
        raw_items = set([e['sample_id'] for e in list(raw_items)])
        counts_items = set([e['sample_id'] for e in counts_items])
        if len(raw_items.difference(counts_items)) != 0 or len(
                counts_items.difference(raw_items)) != 0:
            self.log.warning(
                f"discrepency between sample ids in {self.config['db_collection_raw']} and those in {self.config['db_collection_counts']}")

    def check_fastq_folders(self):
        """
        checks that all .fastq files path identified in the fq_path key of config are found and that no other files are present in
        those paths

        Parameter:
        None
        Return:
        message on the screen indicating the findings without triggering a halt

        """
        if type(self.config) is str:  # did we go there already?
            if '/' in self.config:
                self.config_dir = self.config.rsplit('/', 1)[0] + '/'
            self.config = json.load(open(self.config, 'r'))
            #self.find_fastq_files()

        files_dict = {}
        missing_files = []
        folders = set()
        extra_files = []
        for elem in self.config['sample_key']:
            # find the file
            if open(elem['fq_file']) is not None:
                files_dict[elem['fq_file']] = elem['sample_id']
            else:
                missing_files.append(elem['fq_file'])
            folders.add(elem['fq_file'].rsplit('/', 1)[0] + '/')
        for fo in folders:
            files = [
                f for f in os.listdir(fo) if os.path.isfile(
                    os.path.join(
                        fo, f))]
            for f in files:
                if fo + f not in files_dict:
                    extra_files.append(fo + f)
        missing_files = [e for e in missing_files if e.endswith(".fastq") or e.endswith(".fastq.gz")]
        extra_files = [e for e in extra_files if e.endswith(".fastq") or e.endswith(".fastq.gz")]
        return (
            f"missing_files = {missing_files}, extra_files = {extra_files}")

    def find_fastq_files(self):
        """
        find_fastq_files goes thorugh each fastq files location to make sure they exist and the actual path is gathered
        in the fq_file key in the config dictionary

        Parameter:
        None
        Return:
        None

        """
        self.content=None
        if 'sample_key' not in self.config:
            self.log.warning(
                "Missing sample_key parameter - using *.fastq by default")
            self.config['sample_key'] = "*.fastq"
        else:
            self.config['sample_key'] = self.config_dir + self.config['sample_key']
            self.content = pd.read_csv(self.config['sample_key'])

            # making sure both sample_id and fq_file comuns are present
            if 'sample_id' not in self.content or 'fq_file' not in self.content:
                self.log.write(
                    f"missing required columns sample_id or fq_file. Exiting process")
                return -1

            # eliminating dups and keeping last one as not duplicate - give
            # warning if dups found
            self.content.drop_duplicates(
                subset='sample_id', keep='last', inplace=True)

            self.content.loc[self.content["fq_file"].isnull(),
                        "fq_file"] = self.content['sample_id'] + '.fastq.gz'

            if 'fq_path' in self.config and len(
                    self.config['fq_path']) != 0:  # add back the slash check
                self.content['fq_file'] = self.config['fq_path'] + self.content['fq_file']

            # checking fasq files exist
            found_content = self.content[[os.path.isfile(
                self.config_dir + i) for i in self.content['fq_file']]]

            for e in pd.concat(
                    [found_content, self.content]).drop_duplicates(
                    keep=False).to_dict('records'):
                self.log.write(f'{e["fq_file"]} not found')
            self.content = found_content

            self.config['sample_key'] = self.content[[
                'sample_id', 'fq_file']].to_dict('records')
            for elem in self.config['sample_key']:
                elem["fq_file"] = self.config_dir + elem["fq_file"]

    def rawConfig(self):
        """
        Validate and fill in default values to a dictionary of config settings for httr_raw processing.

        Starting from a dict of user-specified values, fill in remaining values and apply any additional logic.

        The following settings are required (WARN if missing, fill with default values):
        sample_key must equal one of:
        1) a list of fastq files
        2) a string indicating wildcard pattern for matching target fastq files
        3) a list of dicts with fq_file and sample_id keys
        When missing, set to *.fastq by default, which will lead to trying to

        Parameters:
        config (dict): Parameter,setting pairs of user-specified values
        log (PipelineLogger): Where to send any warning/debug messages

        Returns:
        dict with original user-specified values + defaults for missing parameters
        """

        #self.find_fastq_files()

        # DB settings - at a minimum need a DB host and name,
        # if these are missing, set to None by default (should be interpreted
        # as writing to disk only)
        if self.output_dir == None:
            if 'db_host' not in self.config:
                self.config['db_host'] = os.getenv("TEST_HOST")
                self.config['db_name'] = os.getenv("TEST_DB")
                if 'db_insert' not in self.config:
                    if self.config['db_host'] is not None:
                        self.config['db_insert'] = True
                    else:
                        self.config['db_insert'] = False
            else:
                if 'db_insert' not in self.config:
                    self.config['db_insert'] = True
                # TO DO: Check that db_host is a valid URL?
                # Check that db_name is also defined, otherwise give a warning
                if 'db_name' not in self.config or self.config['db_name'] == "":
                    self.config['db_name'] = os.getenv("TEST_DB")
                    if 'db_name' not in self.config or self.config['db_name'] is None:
                        self.log.warning(
                            "db_host is defined but db_name is missing.")
        else:
            if 'db_insert' not in self.config:
                self.config['db_insert'] = True
            if 'db_host' in self.config or 'db_name' in self.config:
                print("WARNING: db_host or db_name is defined but output_dir is specified. Reading/writing to json file takes precedence.")

            # Set defaults for other DB-related params - what collection to use
        # moved out of else branch
        self.config['db_collection_raw'] = "httr_raw"
        # moved out of else branch
        self.config['db_collection_counts'] = "httr_counts"

        if 'db_well_trt' not in self.config:
            self.config['db_well_trt'] = "httr_well_trt"
        if 'db_raw' not in self.config:
            self.config['db_raw'] = "httr_raw"
        if 'db_study' not in self.config:
            self.config['db_study'] = "httr_study"

        # If db_path specified, append this to the front of db_ind and/or
        # db_fa:

        self.config['db_ind'] = self.config_dir + self.config['db_ind']
        self.config['db_fa'] = self.config_dir + self.config['db_fa']

        if 'db_path' in self.config:
            if self.config['db_path'] != '':
                if not self.config['db_path'].endswith('/'):
                    self.config['db_path'] += '/'
                if 'db_ind' in self.config:
                    self.config['db_ind'] = self.config_dir + \
                        self.config['db_path'] + self.config['db_ind']
                if 'db_fa' in self.config:
                    self.config['db_fa'] = self.config_dir + \
                        self.config['db_path'] + self.config['db_fa']

        # p = number of threads to use, if this is missing check for alternate
        # "threads", otherwise default to 1
        if 'p' not in self.config:
            if 'threads' in self.config:
                self.log.write("Inferring 'p' from 'threads'.", dbg=True)
                self.config['p'] = self.config['threads']
                del self.config['threads']
            else:
                self.log.warning(
                    "No values specified for p (threads) - defaulting to 1.", dbg=True)
                self.config['p'] = 1
        # TO DO: Validate p as int, warn if p and threads are both specified

        #***********************************************************#
        if 'sample_key' not in self.config:
            self.log.warning(
                "Missing sample_key parameter - no sample processing will be run.")
            self.config['sample_key'] = []

        if ('db_fa' not in self.config) and ('db_ind' not in self.config):
            self.log.warning(
                "Missing both db_fa and db_ind - using human-wt-biospyder.fa by default")
            self.config['db_fa'] = "human-wt-biospyder.fa"
        if 'db_ind' not in self.config:
            def_ind = self.config['db_fa']
            if def_ind.endswith('.fa'):
                def_ind = def_ind.replace('.fa', '')
            if def_ind.endswith('.fasta'):  # typo fix
                def_ind = def_ind.replace('.fasta', '')
            def_ind += '/' + os.path.basename(def_ind)
            self.log.warning(
                "db_ind not specified, setting to '%s' based on db_fa." %
                def_ind)
            self.config['db_ind'] = def_ind

        if self.output_dir == None:
            if 'batch_size' in self.config:
                self.batch_size = self.config['batch_size']
            else:
                self.batch_size = 1000
        else:
            self.batch_size = 1000
                
        if 'reindex' in self.config:
            self.reindex = self.config['reindex']
        else:
            self.reindex = False

        return 0
