'''
# Main module for processing fastq files from BioSpyder TempO-seq platform
# Current approach uses HISAT2 to align reads to known probes
# This process is optimizated by piping the HISAT2 output directly into samtools to compute counts.
# Functions for direclty reading fastq files are in fastq.py module, which is imported here
# (but NOT functions for managing hisat2 runs or building larger httr_raw DB - those are still in this module)
# Top-level functions called by bin/ scripts (envReport, top-level config
# and batch functions) are in ../httrpl.py

# Modifcation of the original biospyder.py module from Imran's workflow tutorial code
# This fixes several issues with the previous pipeline (counting multi-mapped reads)
# and provides several optimizations, especially piping the HISAT2 output directly into
# samtools to compute counts.
# These functions were moved out of biospyderLJE.py on 8/7/19
# Functions not currently in use were moved to biospyderDev.py
# This module was updated for use in python3 on 8/28/19
#  The main differences so far are:
#   + subprocess always returns stdout/stderr streams as byte strings instead of regular strings, so these always need to be converted using .decode()
#   + file() function in default namespace is no longer supported, use open() instead
#   + yaml module is no longer needed - json module can safely read from files without doing weird type conversions
# Functions for direclty reading fastq files were moved to fastq.py module which is imported here (but NOT functions for managing hisat2 runs or building larger httr_raw DB - those are still in this module) on 9/25/19
# Top-level functions called by bin/ scripts (envReport, top-level config and batch functions) were moved to ../httrpl.py on 9/25/19
#
# TO DO: Make a separate module for QC functions

'''

import copy
import datetime
import glob
import json
import os
import shlex
import subprocess
import tempfile

# For parallelization of some batch functions:
from multiprocessing import Pool
from time import time

import pandas as pd
from bson import json_util

# These are only needed for the top-level batch functions which should
# probably be moved to httrpl module with main API
from db.mongo import insertByID, openMongo, openMongoParam

# Low-level functions for parsing useful info out of fastq files:
from gexp.fastq import fastqMD5, fastqModTime, fastqScan, getReadLen

# Core functions for managing logs, warnings, special handling of path strings:
from httrplcore import PipelineLogger, cleanPath, deflog

class biospider2:

    # Convenience function for protecting pathnames for input into hisat2
    # HISAT2 is very picky about paths - requires any path with spaces to be
    # quote-protected AND have \ in front of each space
    def hisat2Path(self, path: str) -> str:
        """
        Use shlex to put quotes and escape characters on spaces in path
    
        HISAT2 is apparently very picky about spaces in paths - it will still parse as separate args even though this is not POSIX-compliant. So this function is needed specifically for passing paths safely to HISAT2.
    
        Parameters:
        path (str) = The path to protect with escape characters on spaces
    
        Return Value:
        (str) = The path with all spaces escaped (based on splitting with shlex)
        """
        return shlex.quote('\\ '.join(shlex.split(path)))
    
    
    # --- UPDATED FUNCTIONS --- #
    
    # These functions came from Imran's previous version of biospyder.py, but
    # have been modified by Logan for use in updated pipeline
    
    def createHSIndex(
            self,
            db_fa: str,
            db_ind: str,
            threads: int = 20,
            hisat2_ind: str = 'hisat2-build',
            rerun: bool = False,
            log: PipelineLogger = deflog,
            **kwargs):
        """
        Create HISAT2 index from probe fasta file
    
        Runs hisat2-build on a fasta file of probe sequences, creates index file needed for HISAT2 alignment of TempO-seq fastq files.
    
        Parameters:
        db_fa (str): path to input fasta file containing sequences for probes (required)
        db_ind (str): output path + prefix for hisat2 index (required)
        threads (int): Number of threads to use for hisat2 run
        hisat2_ind (str): path to hisat2-build binary for indexing (default assumes it's in user path)
        rerun (bool): If the index already exists, should it be over-written? (default = False)
        log (PipelineLogger): Logging object for warnings/progress messages
        **kwargs: Ignored
        """
    
        # Clean up all paths in case of escape chars
        db_fa = cleanPath(db_fa, log=log)
        db_ind = cleanPath(db_ind, log=log)
        # NOTE: hisat2-index still can't have spaces or escape chars in the path!
    
        ind_path = '/'.join(db_ind.split('/')[:-1])
        ind_pref = db_ind.split('/')[-1]
        if not os.path.exists(db_fa):
            log.write("Either " + db_fa + " or " + ind_path + " do not exist!")
            return
    
        # If ind_path does not exist, it should be created!
        if not os.path.exists(ind_path):
            mdCmd = "mkdir -p %s" % shlex.quote(ind_path)
            log.write("Creating directory for index files:")
            log.write(mdCmd)
            cmd_status = os.system(mdCmd)
            if cmd_status is not 0:
                log.warning("Previous command returned non-zero exit status")
    
        #log.write(f" --- {kwargs}")
    
        # Check if (db_ind).1.ht2 exists - if so, delete all associated *.ht2 files
        ind1 = db_ind + '.1.ht2'
        if os.path.exists(ind1):
            # if rerun:
            if 'reindex' in kwargs and kwargs['reindex']:
                log.write("Removing the existing index files")
                rmCmd = "rm %s.*.ht2" % shlex.quote(db_ind)
                log.write("TEST: " + rmCmd)
                cmd_status = os.system(rmCmd)
                if cmd_status is not 0:
                    log.warning("Previous command returned non-zero exit status")
            else:
                log.write(
                    "Index %s exists already in %s - nothing new will be created." %
                    (ind_pref, ind_path))
                return
    
        cmd = "%s -p %i %s %s" % (hisat2_ind, threads,
                                  self.hisat2Path(db_fa), self.hisat2Path(db_ind))
        log.write("Running: " + cmd)
        cmd_status = os.system(cmd)
        if cmd_status is not 0:
            log.warning("Previous command returned non-zero exit status")
    
    # --- NEW FUNCTIONS added by LJE on/after 7/17/19 --- #
    
    # TO DO: Identify other characters that are not allowed in MongoDB keys
    
    
    def fixProbeName(self, probe_name: str) -> str:
        """
        Fix probe names to prevent issues with MongoDB
    
        Removes characters that aren't allowed in MongoDB keys, namely replace '.' with '-'.
    
        Parameters:
        probe_name (str) = BioSpyder Probe Name
    
        Return Value:
        (str) = BioSpyder Probe Name with problematic characters replaced
        """
        return probe_name.replace('.', '-')
    
    
    def fixProbeNameList(self, probe_names: list, log: PipelineLogger = deflog) -> list:
        """
        Fix probe names in a list and check for collisions
    
        Given a list of probe names, apply fixProbeName to each one, but whenever a change occurs make sure there are no name collisions.
    
        Parameters:
        probe_names (list of str) = List of BioSpyder probe names to pass to fixProbeName for fixing.
        log (PipelineLogger) = Log stream, each change is passed as a debug message, collisions cause an error.
    
        Return Value:
        (list of str) = List of BioSpyder probe names that have been fixed
        """
        fixed_names = []
        for pn in probe_names:
            fixed_pn = self.fixProbeName(pn)
            if pn != fixed_pn:
                log.write(
                    "Changed probe name '%s' -> '%s'" %
                    (pn, fixed_pn), dbg=True)
                if fixed_pn in fixed_names:
                    log.error(
                        "Probe name collision occurred in fixProbeNameList when changing '%s' -> '%s'" %
                        (pn, fixed_pn))
            fixed_names.append(fixed_pn)
        if len(probe_names) != len(fixed_names):
            log.error(
                "Bug in fixProbeNameList: started with %i probe names, ended with %i fixed names." %
                (len(probe_names), len(fixed_names)))
        return fixed_names
    
    
    def fixProbeCounts(self, probe_cnts: dict, log: PipelineLogger = deflog) -> dict:
        """
        Fix probe names in a count dictionary and check for collisions
    
        Given a probe count dictionary, like that returned by alignAndCount in probe_cnts field, apply fixProbeName to each key to make sure they're safe for bson/MongoDB encoding. Whenever a probe name actually has to change, make sure there is no name collision.
    
        Parameters:
        probe_cnts (dict of int) = Dict of probe counts where keys are probe names, values are read counts
        log (PipelineLogger) = Log stream, any name changes generate debug messages, name collisions generate errors
    
        Return Value:
        (dict of int) = Probe count dictionary with any problematic probe names fixed in the keys
        """
        fixed_cnts = {}
        for pn in probe_cnts:
            fixed_pn = self.fixProbeName(pn)
            if pn != fixed_pn:
                log.write(
                    "Changed probe name '%s' -> '%s'" %
                    (pn, fixed_pn), dbg=True)
                if fixed_pn in fixed_cnts:
                    log.error(
                        "Probe name collision occurred in fixProbeCounts when changing '%s' -> '%s'" %
                        (pn, fixed_pn))
            fixed_cnts[fixed_pn] = probe_cnts[pn]
        if len(probe_cnts) != len(fixed_cnts):
            log.error(
                "Bug in fixProbeCounts: started with %i probe counts, ended with %i fixed probe counts." %
                (len(probe_cnts), len(fixed_cnts)))
        return fixed_cnts
    
    
    def alignAndCount(
            self,
            fq_file: str,
            db_ind: str,
            trim='auto',
            threads: int = 20,
            aln_qual: int = 60,
            tmp_dir: str = './',
            hisat2: str = 'hisat2',
            samtools: str = 'samtools',
            log: PipelineLogger = deflog,
            **kwargs) -> dict:
        """
        Align fastq file and count uniquely aligned reads per probe.
    
        Run HISAT2 to align a fastq file to a probe index, then pipe the output directly to samtools to count uniquely aligned reads for each probe. The resulting counts are piped directly into Python and converted to a dict structure with sample meta-data and probe-level counts that is ready for combining into larger tables or dumping into mongo DB. NOTE: This also differs from alignFastq parameters in the addition of '--no-spliced-alignment' parameter to HISAT2 to match Derik's settings.
    
        Parameters:
        fq_file (str): path to fastq file to align
        db_ind (str): path + prefix for hisat2 index
        trim: Either a number of bases to trim off the end of the reads, or 'auto' (default) which checks the read length from the fastq file and then sets param to len-50. When this value is >0, it adds the --trim3 option to the HISAT2 command
        threads (int): Number of processors to use (passed to -p option on hisat2)
        aln_qual0 (int): Quality filter to use for samtools (default drops all multi-mapping reads)
        tmp_dir (str): Path to use for temp files created by samtools sort (default = current dir, files will also have prefix based on fastq file name, temp files will be removed if they still exist)
        hisat2 (str): Path to hisat2 binary (default assumes it's in PATH and just name of binary is needed)
        samtools (str): Path to samtools binary (default assumes it's in PATH and just name of binary is needed)
        log (PipelineLogger): Output stream for debug/warning/progress messages
        **kwargs: Ignored
    
        Returns:
        Dict structure with some meta-data fields and all probe counts in probe_cnts field
        """
        # Start the dict structure that will be returned at the end
        # First key,value pair is the path to target fastq file
        totalReads = dict(fastq=fq_file, rd_len=0)
    
        # Determine --trim3 parameter in case of read len > 50
        if trim == 'auto':
            # Use getReadLen to fill in this parameter
            rdlen = getReadLen(fq_file, log=log)
            if rdlen is not None:
                totalReads['rd_len'] = rdlen
                trim = rdlen - 50
                log.write(
                    "Auto-determined read length = %i, setting --trim3 %i for HISAT2." %
                    (rdlen, trim), dbg=True, timestamp=True)
            else:
                log.warning("Could not determine read length from %s" % fq_file)
                trim = 0
        if trim < 0:
            trim = 0
    
        # Set number of threads for samtools to 4, unless threads < 4
        if threads < 4:
            sortp = threads
        else:
            sortp = 4
    
        # Determine prefix for temp files generated by samtools sort
        if not tmp_dir.endswith('/'):
            tmp_dir += '/'
        tmp_dir += os.path.basename(fq_file)
        for sfx in ['.gz', '.fastq', '.fq']:
            if tmp_dir.endswith('.gz'):
                tmp_dir = tmp_dir.replace(sfx, '')
        log.write(
            "Storing samtools temp files as %s.nnnn.bam" %
            tmp_dir, dbg=True, timestamp=True)
    
        # Construct the call to hisat2 and pipe the alignment output through
        # samtools to get final counts
        log.write(f"using {threads} threads to process hisat2 in alignAndCount")
        Cmd = [hisat2, '-q',
               '-p', str(threads),
               '--dta', '--no-spliced-alignment',
               '--trim3', str(trim),
               '-x', self.hisat2Path(db_ind),
               '-U', self.hisat2Path(fq_file),
               '|', samtools, 'view', '-b', '-q', str(aln_qual),
               '|', samtools, 'sort', '-@', str(sortp), '-T', tmp_dir, '-O', 'bam',
               '|', samtools, 'idxstats', '-'
               ]
        run_hisat2 = " ".join(Cmd)
        # Save the full command that was run
        totalReads['aln_cmd'] = run_hisat2
    
        # Determine host machine ID and save in host_name field
        totalReads['host_name'] = os.environ.get('HOSTNAME')
        if totalReads['host_name'] is None:
            log.warning("Could not determine a HOSTNAME value.")
            totalReads['host_name'] = "UNKNOWN"
    
        # Save the datetime at start of the run in run_time field:
        totalReads['run_time'] = datetime.datetime.now()
    
        log.write(
            "Running the following HISAT2/Samtools piped commands:\n%s" %
            run_hisat2, dbg=True, timestamp=True)
    
        # Run the command in a subprocess and collect data from both stdout and stderr
        # stdout of the final process will contain the counts in 4-column format (col 1 = probe ID, col 3 = read count)
        # stderr will contain summary info from HISAT2 which can be parsed to get total, unmapped, and multi-mapped read counts
        # Both stdout and stderr will be the complete output of each stream as a
        # single string
        startTime = time()
        alnCmd = subprocess.Popen(
            run_hisat2,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            shell=True)
        stdoutdata, stderrdata = alnCmd.communicate()
        stdoutdata = stdoutdata.decode()
        stderrdata = stderrdata.decode()
        endTime = time()
        runTime = endTime - startTime
    
        # In debug mode, dump the whole stderr stream to log
        log.write(
            "Finished core HISAT2/Samtools toolchain in %i seconds, console output shown below:\n%s" %
            (runTime, stderrdata), dbg=True, timestamp=True)
    
        # Assuming the command ran successfully, stderr contains summary info from HISAT2
        # Parse this to get total, unaligned, uniquely aligned, and multi-aligned
        # reads
        for errLine in stderrdata.split('\n'):
            dKey = ''
            # Determine which total read count this line corresponds to:
            if 'reads; of these:' in errLine:
                dKey = 'n_reads'
            elif 'aligned 0 times' in errLine:
                dKey = 'n_reads_unmapd'
            elif 'aligned exactly 1 time' in errLine:
                dKey = 'n_reads_mapd'
            elif 'aligned >1 times' in errLine:
                dKey = 'n_reads_ambig'
            # Now parse out the number if this is a useful line:
            if dKey != '':
                totalReads[dKey] = int(errLine.lstrip().split(' ')[0])
    
        # Make sure all expected keys for totalReads have been filled in and n_reads_unmapd >= 0
        # Then make sure n_reads = n_reads_mapd + n_reads_ambig + n_reads_unmapd
        # If not, write a warning to log
        reqKeys = ['n_reads', 'n_reads_mapd', 'n_reads_ambig', 'n_reads_unmapd']
        missingKeys = False
        for rk in reqKeys:
            if rk not in totalReads:
                log.warning(
                    "Was not able to parse %s from HISAT2 stderr stream." %
                    rk)
                missingKeys = True
                totalReads[rk] = -1
            elif totalReads[rk] < 0:
                log.warning(
                    "Parsed invalid value '%s' for %s from HISAT2 stderr stream." %
                    (totalReads[rk], rk))
                totalReads[rk] = -1
    
        if not missingKeys:
            if totalReads['n_reads'] != (
                    totalReads['n_reads_mapd'] +
                    totalReads['n_reads_ambig'] +
                    totalReads['n_reads_unmapd']):
                log.warning(
                    "Total mapped/unmapped read counts did not sum to total reads based on values parsed from HISAT2 stderr stream.")
    
        # Now remove n_reads_unmapd since we haven't stored or tracked this
        # previously
        del[totalReads['n_reads_unmapd']]
    
        # Next parse through stdout which contains the individual probe counts from samtools idxstats
        # NOTE: This contains a count for EVERY probe listed in the SAM header, including those with 0 reads
        # The format of each line is (tab-delimited):
        # probe len alnCnt unalnCnt
        # For now just take probe and alnCnt (but ignore the line with probe = '*')
        # len should always be 50 and unalnCnt should always be 0
        # TO DO: Could add sanity checks on len and unalnCnt, maybe in dbg mode
        # only?
        countDict = dict()
        for countLine in stdoutdata.split('\n'):
            if countLine != '':
                countLine = countLine.rstrip().split('\t')
                # TO DO: Change this to dbg only or clean up output?
                if len(countLine) != 4:
                    log.warning("WARNING: Line has number of fields != 4")
                probeName = countLine[0]
                probeCount = int(countLine[2])
                if probeName != '*':
                    countDict[probeName] = probeCount
    
        # In debug mode, sum the total counts and number of probes
        if log.dbg:
            countTotal = 0
            probeTotal = 0
            for n in countDict.values():
                if n > 0:
                    countTotal += n
                    probeTotal += 1
            log.write("Index stats counted total of %i across %i probes." %
                      (countTotal, probeTotal), timestamp=True, dbg=True)
            # Make sure countTotal == totalReads['n_reads_mapd']
            if countTotal != totalReads['n_reads_mapd']:
                log.warning(
                    "Total probe counts did not equal n_reads_mapd scraped from HISAT2 summary log.",
                    dbg=True)
    
        # Build final dict object and return
        totalReads['mapd_frac'] = 1.0 * totalReads['n_reads_mapd'] / totalReads['n_reads']
        # Make sure to fix problematic probe counts before returning
        totalReads['probe_cnts'] = self.fixProbeCounts(countDict, log=log)
    
        return totalReads
    
    def buildCountTable(
        self,
        COUNTS0: list,
        metaFields: list = [
            'sample_id',
            'raw_id',
            'fastq',
            'rd_len',
            'aln',
            'aln_cmd',
            'host_name',
            'run_time',
            'sam',
            'n_reads',
            'n_reads_mapd',
            'n_reads_ambig',
            'mapd_frac']) -> pd.DataFrame:
        """
        Convert a list of count data for multiple samples into a pandas data frame.
    
        Take a list of count dict structures (each one in the format returned by alignAndCount, readSam, or countClusters) and convert to a pandas data frame with rows = samples and cols = meta-data fields and probes. Automatically determines the sample_id if not present.
    
        Parameters:
        COUNTS0 (list of dict): List, with each entry = dict matching the structure returned by alignAndCount or similar functions.
        metaFields (list of strings): Which fields should be treated as indexes in order to shift to left side of table, before count fields. This list also specifies the order of these fields, and any field not actually present in COUNTS0[0] will be dropped.
    
        Returns:
        pandas data.frame: Row for each sample, index column for each meta-data field followed by value column for each probe. This can be dumped to file with the to_csv member method.
        """
        # NOTE: This code originally transformed COUNTS0 to a flat dict IN PLACE but then the underlying data structure is modified before going into the DB
        # So instead making a deep copy here, which is more memory inefficient but
        # prevents mangling the original COUNTS0
        COUNTS1 = copy.deepcopy(COUNTS0)
    
        # Fill in sample_id if not present
        # TO DO: This should probably be in a separate function
        for i in COUNTS1:
            if 'sample_id' not in i:
                # Infer sample_id from the basename of the sam file input in each
                # case
                sample_id = os.path.basename(i['fastq'])
                if sample_id.endswith('.gz'):
                    sample_id = sample_id.replace('.gz', '')
                if sample_id.endswith('.fq'):
                    sample_id = sample_id.replace('.fq', '')
                if sample_id.endswith('.fastq'):
                    sample_id = sample_id.replace('.fastq', '')
                i['sample_id'] = sample_id
    
        # Define meta-data fields to bump to front of table cols:
        obsFields = set(COUNTS1[0].keys())
        tmp = [value for value in metaFields if value in obsFields]
        metaFields = tmp
    
        for i in COUNTS1:
            # Pull the probe_cnts dict out
            PC = i.pop('probe_cnts')
            # Attach it to the main dict of params
            i.update(PC)
    
        # Convert to pandas data frame, move the main params to the first columns
        RAW0 = pd.DataFrame(COUNTS1).set_index(metaFields)
        # Fill the missing values with 0s and convert all count columns to integer
        RAW0 = RAW0.fillna(0).astype('int64')
        del COUNTS1
        return RAW0
    
    
    def writeCountTSV(
            self,
            samCounts: dict,
            outFile: str,
            rerun: bool = False,
            log: PipelineLogger = deflog):
        """
        Take meta-data and counts for one sample and write to a TSV file similar to the output from HTSeq-count
    
        Takes as input a dict structure as returned by countClusters, writes a two-column TSV file similar to the output from HTSeq-count. First column = probe ID or meta-data key, second column = value. Meta-data fields are prefixed with '__' in this format. NOTE: There is a slight precision loss that can happen on mapd_frac and any other float field when storing in a text-based format.
    
        Parameters:
        samCounts (dict) = Two-level dict as returned by countClusters. First level is primarily single value meta-data fields, except for probe_cnts member which is a dict with keys = probe/cluster ID and values = counts.
        outFile (str) = The filename to write to (will be overwritten if rerun=True)
        rerun (bool) = Whether or not to overwrite existing file (default: False). If False and outFile exists, this will output a warning to stderr and skip output.
        log (PipelineLogger) = Stream for writing warning/progress messages
        """
        # Check if output file path needs to be cleaned up
        outFile = cleanPath(outFile, log=log)
    
        # Check if output file exists
        if os.path.exists(outFile):
            if rerun:
                rmCmd = "rm %s" % outFile
                log.write("Removing the existing output file:\n" + rmCmd, dbg=True)
                cmd_status = os.system(rmCmd)
                if cmd_status is not 0:
                    log.warning("rm command returned non-zero exit status")
            else:
                log.warning(
                    "Not writing output because rerun=False and target file exists at %s" %
                    outFile)
                return
    
        # Check if output path exists - create if missing
        out_path = '/'.join(outFile.split('/')[:-1])
        if out_path == '':
            out_path = '.'
        if not os.path.exists(out_path):
            mdCmd = "mkdir -p %s" % out_path
            log.write("Creating directory for output file:\n" + mdCmd, dbg=True)
            cmd_status = os.system(mdCmd)
            if cmd_status is not 0:
                log.warning("mkdir command returned non-zero exit status")
    
        # Open file for writing
        of = open(outFile, 'w')
        # First loop over the primary keys in samCounts
        # for all scalar values (should be everything other than probe_cnts)
        # write: __(key)<TAB>(value)
        for mf in samCounts.keys():
            if mf != 'probe_cnts':
                if isinstance(samCounts[mf], dict):
                    log.warning(
                        "samCounts contains additional non-scalar member %s - this will not be written to output." %
                        mf)
                else:
                    of.write("__" + mf + "\t" + str(samCounts[mf]) + "\n")
        # Next loop over each key,value pair in probe_cnts member
        for probe, count in samCounts['probe_cnts'].items():
            of.write(probe + "\t" + str(count) + "\n")
        # Close file
        of.close()
    
    # TO DO: This needs to be able to decode datetime objects from run_time field
    # NOTE: It might be better to just store the initial httr_counts data in JSON format
    # Have separate function to dump/convert to TSV format and drop some of
    # the extra meta-data fields
    
    
    def readCountTSV(self, countFile: str, log: PipelineLogger = deflog) -> dict:
        """
        Read a count object from text file on disk.
    
        Read in a count object file as written by writeCountTSV and convert back to a dict structure as returned by countClusters.
    
        Parameters:
        countFile (str) = Path to file to read in and parse back to dict structure
        log (PipelineLogger) = Stream for outputing warning/progress messages
    
        Returns:
        A dict structure containing the meta-data fields and probe_cnts member containing all probe counts (or None if countFile does not exist)
        """
        # Check if countFile path needs to be cleaned up
        countFile = cleanPath(countFile, log=log)
        # Make sure countFile exists - otherwise write a warning to stderr and
        # return None
        if not os.path.exists(countFile):
            log.warning(
                "Count file for readCountTSV does not exist: %s" %
                countFile)
        # Create initial dict structures - probeCounts will eventually go in
        # probe_cnts member of countDict at the end
        countDict = dict()
        probeCounts = dict()
        # Open countFile, loop over all lines
        cf = open(countFile, 'r')
        for cfLine in cf:
            # Trim trailing white space, split on tab sep
            cfLine = cfLine.rstrip().split('\t')
            # Warn If < 2 fields
            if len(cfLine) < 2:
                log.warning(
                    "Invalid line in " +
                    countFile +
                    ":\n" +
                    '\t'.join(cfLine))
            else:
                # Warn If extra fields - these will be ignored:
                if len(cfLine) > 2:
                    log.warning(
                        "Line contained extra fields in " +
                        countFile +
                        " - only first 2 columns are used:\n" +
                        '\t'.join(cfLine))
                field, value = cfLine[0:2]
                # Figure out if this is a meta-data or primary count field
                if field[0:2] == '__':
                    # If field starts with '__', remove this prefix, treat as
                    # meta-data
                    field = field[2:]
                    # Warn if same meta-data field seen multiple times
                    if field in countDict.keys():
                        log.warning(
                            "Meta-data field '__" +
                            field +
                            "' listed multiple times in " +
                            countFile +
                            " - only last instance will be stored.")
                    # Test if value should be converted based on the following
                    # criteria:
                    if field in [
                        'n_reads',
                        'n_reads_mapd',
                        'n_reads_mapd_ambig_probe',
                            'n_reads_ambig']:
                        value = int(value)
                    elif field == 'mapd_frac':
                        value = float(value)
                    elif field not in ['sam', 'sample_id', 'aln_cmd', 'fastq']:
                        # TO DO: Could try to auto-determine the write type, but
                        # just warn for now
                        log.warning(
                            "Unknown meta-data field '__" +
                            field +
                            "' in " +
                            countFile +
                            " - no type conversion will be performed.")
                    countDict[field] = value
                else:
                    # Non meta-data field, assumed to be probe_name, count
                    # First warn if probe name already in the dict
                    if field in probeCounts.keys():
                        log.warning(
                            "Probe name '" +
                            field +
                            "' listed multiple times in " +
                            countFile +
                            " - only last instance will be stored.")
                    probeCounts[field] = int(value)
        cf.close()
        countDict['probe_cnts'] = probeCounts
        return countDict
    
    
    def fastqRawInfo(self, fq_file: str, sample_id: str = None,
                     log: PipelineLogger = deflog) -> dict:
        """
        Build a complete document for httr_raw collection
    
        Gather all info on a fastq file to fill in a complete document for httr_raw collection
    
        Parameters:
        fq_file (str) = Full path to fastq file
        sample_id (str) = If None, fill in by stripping all suffixes from file name
        log (PipelineLogger) = Object for logging/handling warnings
    
        Return Type:
        (dict) = Full data structure to add to httr_raw collection (see DB specifications)
        """
        # Check if fastq file has '\ ' escape chars in it - if so use shlex to
        # convert to POSIX format
        fq_file = cleanPath(fq_file, log=log)
        # Check to see if fastq file exists - if missing, check for .gz version
        if not os.path.exists(fq_file):
            if os.path.exists(fq_file + '.gz'):
                fq_file = fq_file + '.gz'
        # Split fastq path into path and basename
        (fqPath, fqBase) = os.path.split(fq_file)
        raw_doc = dict(path=fqPath, fastq=fqBase)
        # Infer sample_id from basename if not specified - ideally fill in missing
        # zeros here
        if sample_id is None:
            sample_id = fqBase
            if sample_id.endswith('.gz'):
                sample_id = sample_id[:-3]
            if sample_id.endswith('.fastq'):
                sample_id = sample_id[:-6]
            if sample_id.endswith('.fq'):
                sample_id = sample_id[:-3]
            log.write('Inferred sample_id="%s" for %s at %s' %
                      (sample_id, fqBase, fqPath), dbg=True)
        raw_doc['sample_id'] = sample_id
        # Get mtime
        raw_doc['mtime'] = fastqModTime(fq_file, log=log)
        # Get md5
        raw_doc['md5'] = fastqMD5(fq_file, log=log)
        # Get all other params from fastqScan
    
        # seconds = time.time()#optim
        raw_scan = fastqScan(fq_file, log=log)
        # log.write(f"TTTTTTTTTTTTTTTTTTTOOOOOOOOOOOOOOOOOOOOOOOOOOOOOKKKKKKKKKKKKKKKKKKKKKKKKKK
        # {time.time() - seconds} seconds to process fastqScan on
        # {fq_file}")#optim
    
        raw_doc['rd_len'] = raw_scan['len_mode']
        raw_doc['qc_flag'] = "OK"
        # Determine QC flag (MULTI_LEN when lens > 1)
        if raw_scan['lens'] == 0:
            log.warning(
                "Was not able to determine any read lengths from %s" %
                fq_file)
        elif raw_scan['lens'] > 1:
            raw_doc['qc_flag'] = "MULTI_LEN"
        raw_doc['read_grps'] = raw_scan['read_grps']
        return raw_doc
    
    
    def fastqRawInfoProc(
            self,
            fq_file: str,
            sample_id: str = None,
            dbg: bool = True,
            strict: bool = True) -> dict:
        """
        Multiprocessing-safe wrapper for fastqRawInfo function
    
        Multiprocessing worker function that first creates a tmp file for storing logging information, then calls fastqRawInfo. As long as fastqRawInfo returns successfully, the final log output for the call is written to the main log (this functions log param) as a single block, and the tmp file is deleted. If the fastqRawInfo call fails, the temp file will remain for debugging purposes later. Any unhandled interpreter errors (not calls to log.error) will appear in the main stderr stream of the calling script. The purpose of this is to allow running fastqRawInfo on multiple fastq files in parallel (e.g. using multiprocessing.Pool) - this will keep the final log output orderly, while not losing critical debugging information when an error occurs. All tmp files are stored in the current working directory, and have a format like raw-(sample_id).*.tmp.
    
        Parameters:
        fq_file (str) = Full path to fastq file
        sample_id (str) = If None, fill in by stripping all suffixes from file name
        dbg (bool) = Pass through to the tmp file logger
        strict (bool) = Pass through to the tmp file logger
    
        Return Type:
        (str,dict) = Tuple with the name of the tmp log file, and full httr_raw document structure (see DB specifications)
        """
        # Need a job ID to help identify the tmp log file
        # If defined, this will simply be sample_id, otherwise chop something
        # useful out of fq_file path
        if sample_id is None:
            job_id = fq_file.split('/')[-1].split('.')[0]
        else:
            job_id = sample_id
        # Create tmp file that will NOT be deleted
        if not os.path.exists('tmp/'):
            os.mkdir('tmp/')
        ntf = tempfile.NamedTemporaryFile(
            mode='w+',
            dir='tmp/',
            suffix='.tmp',
            prefix="raw.%s." %
            job_id,
            delete=False)
        tmpfl = ntf.name
        # Create internal log in tmp file with desired settings
        tmplog = PipelineLogger(out=ntf, dbg=dbg, strict=strict)
        rawInfo = self.fastqRawInfo(fq_file=fq_file, sample_id=sample_id, log=tmplog)
        # Inner process finished - close tmp file handle, return both tmpfl and
        # rawInfo
        ntf.close()
        return((tmpfl, rawInfo))
    
    
    def fastqRawInfoMulti(
            self,
            fq_files,
            p: int = 1,
            log: PipelineLogger = deflog) -> list:
        """
        Run fastqRawInfo on multiple fastq files/sample IDs with optional parallelization.
    
        Creates a new multiprocessing.Pool object with p processes, and then uses this pool to call (map) fastqRawInfoProc on each element of fq_files. By using the fastqRawInfoProc wrapper, each running process writes output messages to a tmp file, which is collated into the main log only after each process completes. The final output of this function is a list where each entry is a dict returned by fastqRawInfo.
    
        Parameters:
        fq_files (str or list) = Either a string with wildcard pattern to pass to glob, or list of fastq files to gather raw info on. If list, each entry can either be a string with full path to fastq file, or a dict with fq_file,sample_id members.
        p (int) = How many processes to run in parallel - defaults to 1, but generally should be >1 for actual parallelization.
        log (PipelineLogger) = main log for writing all final output messages. Logs from each running process will be stored in a tmp file first to retain useful info in case of an error. As each process completes, its full log is dumped into this main log and the tmp file cleaned up.
    
        Return Type:
        (list) = Each entry is a dict in the format returned by fastqRawInfo function.
        """
        # Check if fq_files is a string - if so, pass to glob to get list of
        # strings back
        if isinstance(fq_files, str):
            fq_pattern = fq_files
            fq_files = glob.glob(fq_files)
            log.write(
                "Found %i files matching %s" %
                (len(fq_files), fq_pattern), dbg=True)
        # Convert fq_files -> argList where each entry is a tuple with all 4
        # params of fastqRawInfoProc
        argList = []
        for x in fq_files:
            if isinstance(x, str):
                # When x is just a str, assume it is fq_file param
                argList.append((x, None, log.dbg, log.strict))
            elif isinstance(x, dict):
                # When x is a dict, extract fq_file and sample_id param
                if 'fq_file' in x:
                    fq_file = x['fq_file']
                else:
                    # NOTE: I expect this will cause an error downstream, should
                    # probably just throw error here or at least warn via log
                    fq_file = None
                if 'sample_id' in x:
                    sample_id = x['sample_id']
                else:
                    sample_id = None
                argList.append((fq_file, sample_id, log.dbg, log.strict))
            else:
                # Skip this entry and generate a warning:
                log.warning(
                    "fq_files contained an entry of unexpected type %s: %s" %
                    (type(x), x))
        # Create directory for tmp log files - better to do this here rather than
        # in the parallel worker functions
        if not os.path.exists('tmp/'):
            os.mkdir('tmp/')
        # Create multiprocessing Pool object with p processes
        raw_pool = Pool(processes=p)
        # Use starmap to call fastqRawInfoProc on each tuple of order params
        log.write(
            "Starting job pool with %i tasks using %i subprocesses." %
            (len(argList), p), dbg=True, timestamp=True)
        rawInfoResult = raw_pool.starmap(self.fastqRawInfoProc, argList)
        log.write("Job pool finished.\n", dbg=True, timestamp=True)
        # Loop over the tuples in rawInfoList, paste the tmp log content into main
        # log stream and then remove the file:
        rawInfoList = []
        for (tmplf, rawInfo) in rawInfoResult:
            tmplog = open(tmplf, 'r')
            log.write(
                "### Log from processing: %s ###\n\n%s\n### End of Log %s ###\n" %
                (rawInfo['sample_id'], tmplog.read(), rawInfo['sample_id']))
            tmplog.close()
            log.write("Deleting tmp file: %s\n" % tmplf, dbg=True)
            os.remove(tmplf)
            rawInfoList.append(rawInfo)
        # TO DO: Could do some validation on the return data structure?
        # Return the list of return dicts
        return(rawInfoList)
    
    # TO DO: If other databases have datetime fields, it would be more useful
    # to generalize this to any data structure and just scan every field for
    # datetime objects and clear out tzinfo
    
    
    def readRawJson(self, raw_json) -> list:
        """
        Read httr_raw data from a JSON file into list of dicts structure.
    
        Loads httr_raw data from a file - assumes data is in JSON format as written by rawBatch format. Automatically clears tzinfo out of mtime fields (artifact of encoding to JSON on disk) so that dict data should match exactly how it came out of fastqRawInfo() call. Note this does not do any further validation to make sure the returned object conforms to the httr_raw schema.
    
        Parameters:
        raw_json (str or IOstream): If str, assumes this is a file path and opens with open, otherwise assumes it is a readable IOstream and passes directly to json json.load
    
        Return Type:
        (list of dict): Same data structure as returned by fastqRawInfoMulti.
        """
        if isinstance(raw_json, str):
            raw_json = open(raw_json, 'r')
        raw_data = json.load(raw_json, object_hook=json_util.object_hook)
        for i in range(0, len(raw_data)):
            if 'mtime' in raw_data[i]:
                i_mtime = raw_data[i]['mtime']
                raw_data[i]['mtime'] = datetime.datetime.combine(
                    i_mtime.date(), i_mtime.time(), None)
        return raw_data
    
    
    def alignAndCountRaw(self, raw_data: dict, threads, log: PipelineLogger = deflog, **kwargs):  # HTTR_25
        """
        Wrapper to alignAndCount that pulls key info from an httr_raw-schema document first
    
        Given a document from httr_raw collection (or same schema), fill in key parameters (fq_file,trim) before calling alignAndCount
    
        Parameters:
        raw_data (dict) = Should match document schema for httr_raw collection, must have path, fastq, and rd_len fields
        log (PipelineLogger) = Log stream for warning/debug messages
        **kwargs = All additional args passed to alignAndCount
    
        Return Value:
        (dict) Structure with some meta-data fields and all probe counts in probe_cnts field
        """
        # Error if raw_data missing path or fastq fields
        if ('path' not in raw_data) or ('fastq' not in raw_data):
            log.error("raw_data missing path or fastq fields at alignAndCountRaw call.")
        fq_file = raw_data['path']
        if not fq_file.endswith('/'):
            fq_file += '/'
        fq_file += raw_data['fastq']
        # Warning if rd_len missing
        if 'rd_len' not in raw_data:
            log.warning("raw_data missing rd_len at alignAndCountRaw call.")
            trim = 'auto'
            trim_dbg_msg = "Missing rd_len => trim=%s" % trim
        else:
            trim = raw_data['rd_len'] - 50
            trim_dbg_msg = "rd_len=%i => trim=%i" % (raw_data['rd_len'], trim)
        log.write(
            "Running alignAndCount with fq_file=%s, %s" %
            (fq_file, trim_dbg_msg), dbg=True)
        count_data = self.alignAndCount(
            fq_file=fq_file,
            trim=trim,
            log=log,
            threads=threads,
            **kwargs)  # HTTR_25
        # Copy over sample_id and _id -> raw_id from raw_data if present, warn if
        # not
        if 'sample_id' in raw_data:
            count_data['sample_id'] = raw_data['sample_id']
            log.write(
                "Copying sample_id=%s from raw_data." %
                count_data['sample_id'], dbg=True)
        else:
            log.warning(
                "raw_data missing sample_id field at alignAndCountRaw call.")
        if '_id' in raw_data:
            count_data['raw_id'] = raw_data['_id']
            log.write(
                "Copying raw_id=%s from raw_data['_id']." %
                count_data['raw_id'], dbg=True)
        else:
            log.warning("raw_data missing _id field at alignAndCountRaw call.")
        # Remove fastq and rd_len from count_data before returning
        if 'fastq' in count_data:
            del count_data['fastq']
        if 'rd_len' in count_data:
            del count_data['rd_len']
        return count_data
    
    # TO DO: Allow db_ind to be None and auto-generate appropriate path based
    # on db_fa (at least one will need to be defined)
    
    
    def alignCountBatchRaw(self, raw_batch: list, db_ind: str, threads, db_fa=None, log: PipelineLogger = deflog, **kwargs) -> list:  # HTTR_25
        """
        Run HISAT2 alignment on a batch of samples pulled from DB.
    
        Runs HISAT2 alignment on multiple Fastq files. The HISAT2 runs are performed one at a time, since each HISAT2 call can be multi-threaded. The HISAT2 index will be created if it does not exist. The list of fastq files comes from a list of samples dumped out of httr_raw collection in DB.
    
        Parameters:
        raw_batch (list of dict): A list of documents matching httr_raw schema, each one will be passed to alignAndCountRaw to infer fq_file and trim
        db_ind (str): Path and prefix of the HISAT2 index - if it doesn't exist, will attempt to find corresponding .fa file and create it
        db_fa (str): If defined, the fasta file to convert into hisat2 index
        log (PipelineLogger): Where to write all logging info (STDERR by default)
        kwargs: All other args passed to createHSIndex or alignAndCount
    
        Return Type:
        list: Each count dict that was returned by alignAndCountRaw
        """
        # Check if db_ind exists, otherwise try to create it using createHSIndex
        # If db_fa is defined in **kwargs, then attempt to build HSIndex
        if db_fa is not None:
            self.createHSIndex(db_fa, db_ind, log=log, **kwargs)
        else:
            log.write(
                'Using existing HISAT2 index at: %s\n\n' %
                db_ind, dbg=True, timestamp=True)
    
        # Loop over each member of raw_batch
        log.write(
            'Processing %i fastq files.\n\n' %
            len(raw_batch),
            dbg=True,
            timestamp=True)
        COUNTS0 = []
        for raw_data in raw_batch:
            # Call alignAndCountRaw with fastq and db_ind paths along with other parameters
            # Store return value of alignFastq (full command log) in a list
            # structure
            if 'sample_id' in raw_data:
                log.write(
                    'Processing %s...\n' %
                    raw_data['sample_id'],
                    dbg=True,
                    timestamp=True)
            else:
                log.write(
                    'Processing raw_data with no sample_id...\n',
                    dbg=True,
                    timestamp=True)
            COUNTS0.append(
                self.alignAndCountRaw(
                    raw_data=raw_data,
                    db_ind=db_ind,
                    log=log,
                    threads=threads,
                    **kwargs))  # HTTR_25
        log.write('Finished processing %i fastq files.\n' %
                  len(raw_batch), dbg=True, timestamp=True)
        return COUNTS0

