'''
 Low-level functions for parsing information out of fastq files
 Functions for managing alignment/counting of TempO-seq fastq files are
 in biospyder2.py
'''

import datetime
import gzip
import os
import shlex
import subprocess

# Get core logging, path functions
from httrplcore import PipelineLogger, cleanPath, deflog


def getReadLen(fq_file: str, log: PipelineLogger = deflog) -> int:
    """
    Determine the read length of a fastq file

    Read in the first read from a fastq file (gzipped or uncompressed) and determine the read length. This is primarily intended to check whether a trimming parameter needs to be set on the aligner for libraries with 51bp reads. This function assumes that all reads in a fastq file are the same length (valid for raw TempO-seq fastq files that have not been passed through any other trimming or pre-processing tools)

    Parameters:
    fq_file (str): Path to file to read in for checking the read length
    log (PipelineLogger): where to send log messages

    Returns:
    An integer indicating the read length (or None if fq_file does not exist)
    """
    # TO DO: Should be able to replace this with a call to cleanPath function
    # First check if fastq file has '\ ' escape chars in it - if so use shlex
    # to convert to POSIX format
    if '\\' in fq_file:
        log.warning(
            "fq_file contains backslashes - this is not necessary, attempting to convert to POSIX-compatible string with shlex.")
        fq_file = ' '.join(shlex.split(fq_file))
        log.write("         fq_file changed to: " + fq_file + "\n")
    # Check to see if fastq file exists - if missing, check for .gz version
    if not os.path.exists(fq_file):
        if os.path.exists(fq_file + '.gz'):
            fq_file = fq_file + '.gz'
        else:
            log.warning("Could not find file " + fq_file)
            return
    # If fq_file ends with ".gz", open with gzip, otherwise open as plain text:
    if fq_file.endswith('.gz'):
        fqIn = gzip.GzipFile(fq_file, 'r')
    else:
        fqIn = open(fq_file, 'r')
    # Read the first 4 lines of the fastq file - this corresponds to all info
    # for the first read
    rdHdr = fqIn.readline().rstrip()
    rdStr = fqIn.readline().rstrip()
    rdDiv = fqIn.readline().rstrip()
    rdQul = fqIn.readline().rstrip()
    # Python3-specific fix - GzipFile readline() returns objects of type bytes
    # instead of str, so need to be decoded first in those cases:
    if isinstance(rdHdr, bytes):
        rdHdr = rdHdr.decode()
        rdStr = rdStr.decode()
        rdDiv = rdDiv.decode()
        rdQul = rdQul.decode()
    # Check some assumptions on format:
    # TO DO: Could make this dbg only
    if not rdHdr.startswith("@"):
        log.warning("Fastq file does not match expected format in " + fq_file)
    elif rdDiv != "+":
        log.warning("Fastq file does not match expected format in " + fq_file)
    elif len(rdStr) != len(rdQul):
        log.warning(
            "Read and quality string lengths did not match in " +
            fq_file)
    # Return the length of the main read string:
    fqIn.close()  # added httr_tests git repo
    return len(rdStr)


def parseFqHdr(fq_hdr: str, log: PipelineLogger = deflog) -> dict:
    """
    Parse Fastq Read Header into dictionary of useful fields

    Parses a read header line from a fastq file based on Illumina Casava 1.8+ format.

    Parameters:
    fq_hdr (str): The read header line from a fastq file
    log (PipelineLogger): Where to output warning/debug messages

    Return Type:
    dict of named strings parsed from header line
    """
    # Strip the leading @ from header line
    if fq_hdr.startswith('@'):
        fq_hdr = fq_hdr[1:]
    # Split on the space and colons:
    fq_hdr_fields = []
    for f1 in fq_hdr.split(' '):
        fq_hdr_fields += f1.split(':')
    # There should be exactly 11 fields, otherwise does not match expected
    # format
    if len(fq_hdr_fields) != 11:
        log.write('@' + fq_hdr, dbg=True)
        log.warning(
            "FastQ read header contained %i fields, expected 11" %
            len(fq_hdr_fields))
        return {}
    else:
        return dict(
            ins_id=fq_hdr_fields[0],
            run_id=fq_hdr_fields[1],
            flowcell_id=fq_hdr_fields[2],
            lane_id=fq_hdr_fields[3],
            tile_num=fq_hdr_fields[4],
            clust_x=fq_hdr_fields[5],
            clust_y=fq_hdr_fields[6],
            pair=fq_hdr_fields[7],
            filter=fq_hdr_fields[8],
            ctl_bits=fq_hdr_fields[9],
            barcode=fq_hdr_fields[10])


def fastqScan(fq_file: str, log: PipelineLogger = deflog) -> dict:
    """
    Parse an entire fastq file and return meta-data for httr_raw collection

    Parses through an entire fastq file (can be compressed), checks that each read has the same length, and collates data on each run/lane that was combined into this fastq file

    Parameters:
    fq_file (str): Path to fastq file, opens as gzip if ends in .gz
    log (PipelineLogger): Where to send status messages

    Return Type:
    dict with the following structure:
    lens: how many different read lengths were seen
    len_mode: what read length was most common (mode)
    read_grps: list of dict describing each read group
    """
    # Set up hashes for counting/mapping:
    # Count how many reads have each length
    rd_len = {}
    # Count reads by read group
    # Key = ins:run:flowcell:lane; Value = read count
    rd_grps = {}
    # Check if fastq file has '\ ' escape chars in it - if so use shlex to
    # convert to POSIX format
    fq_file = cleanPath(fq_file, log=log)
    # Check to see if fastq file exists - if missing, check for .gz version
    if not os.path.exists(fq_file):
        if os.path.exists(fq_file + '.gz'):
            fq_file = fq_file + '.gz'
        else:
            log.warning("Could not find file " + fq_file)
            return {}
    # If fq_file ends with ".gz", open with gzip, otherwise open as plain text:
    if fq_file.endswith('.gz'):
        fqIn = gzip.GzipFile(fq_file, 'r')
    else:
        fqIn = open(fq_file, 'r')
    # Loop through the file 4 lines at a time - this corresponds to all info
    # for the first read
    while True:
        rdHdr = fqIn.readline()
        if not rdHdr:
            break
        rdStr = fqIn.readline()
        rdDiv = fqIn.readline()
        rdQul = fqIn.readline()
        # Strip trailing line breaks and white space
        rdHdr = rdHdr.rstrip()
        rdStr = rdStr.rstrip()
        rdDiv = rdDiv.rstrip()
        rdQul = rdQul.rstrip()
        # Python3-specific fix - GzipFile readline() returns objects of type
        # bytes instead of str, so need to be decoded first in those cases:
        if isinstance(rdHdr, bytes):
            rdHdr = rdHdr.decode()
            rdStr = rdStr.decode()
            rdDiv = rdDiv.decode()
            rdQul = rdQul.decode()
        # Check some assumptions on format:
        if log.dbg:
            if not rdHdr.startswith("@"):
                log.warning(
                    "Fastq file does not match expected format in " +
                    fq_file,
                    dbg=True)
            if rdDiv != "+":
                log.warning(
                    "Fastq file does not match expected format in " +
                    fq_file,
                    dbg=True)
            if len(rdStr) != len(rdQul):
                log.warning(
                    "Read and quality string lengths did not match in " +
                    fq_file,
                    dbg=True)
        # Store length of rdStr in rd_len dict
        rdLenStr = str(len(rdStr))
        if rdLenStr in rd_len:
            rd_len[rdLenStr] += 1
        else:
            rd_len[rdLenStr] = 1
        # Parse the header line
        rdHdrDict = parseFqHdr(rdHdr, log=log)
        # Read Group = instrument:run:flowcell:lane
        rdGrp = rdHdrDict['ins_id'] + ":" + rdHdrDict['run_id'] + \
            ":" + rdHdrDict['flowcell_id'] + ":" + rdHdrDict['lane_id']
        if rdGrp in rd_grps:
            rd_grps[rdGrp] += 1
        else:
            rd_grps[rdGrp] = 1
    # After looping through whole file, count how many things in each hash
    len_mode = 0
    len_mode_cnt = 0
    for rl in rd_len:
        if rd_len[rl] > len_mode_cnt:
            len_mode = rl
            len_mode_cnt = rd_len[rl]
    # Convert rd_grps to final read_grps list structure
    # In debug mode, also make sure each flowcell maps to exactly one
    # instrument and run ID
    if log.dbg:
        fc_ins = {}
        fc_run = {}
    read_grps = []
    for rdGrp in rd_grps:
        rdGrp_split = rdGrp.split(":")
        if len(rdGrp_split) != 4:
            log.warning(
                "Read group contained wrong number of fields: %s has %i fields" %
                (rdGrp, len(rdGrp_split)))
        ins_id = rdGrp_split[0]
        run_id = rdGrp_split[1]
        fc_id = rdGrp_split[2]
        lane_id = rdGrp_split[3]
        # In debug mode, make sure each flowcell maps to exactly one instrument
        # and run ID
        if log.dbg:
            if fc_id in fc_ins:
                if fc_ins[fc_id] != ins_id:
                    log.warning(
                        "Flowcell mapped to multiple instruments across read groups: %s mapped to %s and %s" %
                        (fc_id, fc_ins[fc_id], ins_id), dbg=True)
                if fc_run[fc_id] != run_id:
                    log.warning(
                        "Flowcell mapped to multiple run IDs across read groups: %s mapped to runs %s and %s" %
                        (fc_id, fc_run[fc_id], run_id), dbg=True)
            else:
                fc_ins[fc_id] = ins_id
                fc_run[fc_id] = run_id
        group_info = dict(
            ins_id=ins_id,
            run_id=run_id,
            flowcell_id=fc_id,
            lane_num=int(lane_id),
            n_reads=rd_grps[rdGrp])
        read_grps.append(group_info)
    fqIn.close()
    return dict(lens=len(rd_len), len_mode=int(len_mode), read_grps=read_grps)


def fastqMD5(fq_file: str, log: PipelineLogger = deflog) -> str:
    """
    Compute MD5 Checksum for a fastq file

    Compute MD5 Checksum for the contents of a fastq file - if file is compressed, the uncompressed content will be piped into md5 command

    Parameters:
    fq_file (str) = Full path to fastq file to generate checksum for
    log (PipelineLogger) = Object for logging/handling warnings

    Return Type:
    (str) = The md5 checksum (or None if file not found)
    """
    # TO DO: The step of cleaning a path and checking whether the file or .gz version exists could be moved into reusable function
    # Check if fastq file has '\ ' escape chars in it - if so use shlex to
    # convert to POSIX format
    fq_file = cleanPath(fq_file, log=log)
    # Check to see if fastq file exists - if missing, check for .gz version
    if not os.path.exists(fq_file):
        if os.path.exists(fq_file + '.gz'):
            fq_file = fq_file + '.gz'
        else:
            log.warning("Could not find file " + fq_file)
            return None
    # Build command for generating md5
    if fq_file.endswith('.gz'):
        # If fastq file is compressed, pipe into md5sum with zcat
        md5Cmd = "zcat " + shlex.quote(fq_file) + " | md5sum"
    else:
        # Otherwise just run md5sum directly
        md5Cmd = "md5sum " + shlex.quote(fq_file)
    log.write(
        "Computing MD5 checksum for %s by running:\n %s" %
        (fq_file, md5Cmd), dbg=True)
    chksum = subprocess.check_output(
        md5Cmd,
        stderr=subprocess.STDOUT,
        shell=True).decode()
    if chksum == "":
        log.warning("MD5 Checksum is blank")
    chksum = shlex.split(chksum)[0]
    return chksum


def fastqModTime(
        fq_file: str,
        log: PipelineLogger = deflog) -> datetime.datetime:
    """
    Get mtime of a fastq file

    Get modification time of a fastq file on disk, return as datetime.datetime object (this is the object type used in previous iterations of the database)

    Parameters:
    fq_file (str) = Full path to fastq file
    log (PipelineLogger) = Object for logging/handling warnings

    Return Type:
    (datetime.datetime) = datetime object built from file modification timestamp
    """
    # Check if fastq file has '\ ' escape chars in it - if so use shlex to
    # convert to POSIX format
    fq_file = cleanPath(fq_file, log=log)
    # Check to see if fastq file exists - if missing, check for .gz version
    if not os.path.exists(fq_file):
        if os.path.exists(fq_file + '.gz'):
            fq_file = fq_file + '.gz'
        else:
            log.warning("Could not find file " + fq_file)
            return None
    rawTime = os.path.getmtime(fq_file)
    return datetime.datetime.fromtimestamp(rawTime)
    
    
def gcProcess(lines=None):

    """
    returns the data as a dictionary with 'name', 'sequence', 'optional', 'quality' as keys

    Parameters:
    lines (list) = list of 4 objects
    
    Return dict:
    with 'name', 'sequence', 'optional', 'quality' as keys and values from lines
    """
    
    ks = ['name', 'sequence', 'optional', 'quality']
    return {k: v for k, v in zip(ks, lines)}
    
def fastqGC(path):

    """
    for each set of 4 lines in the passed fastq file, count the number of 'G' and 'C' in each sequence
    and return the ratio of that occurence to the total number of character found in sequence data

    Parameters:
    path (str) = path to the compressed file to process
    
    Return dict:
    with 'sample_gc' as key, and number of 'G' and 'C' found in sequence as a ratio to total characters in sequence as value
    """
    
    total_len = 0
    total_gc = 0
    
    n = 4
    
    fh = gzip.open(path, 'rb') if '.gz' in path else open(path, 'rb')
    lines = []
    for line in fh:
        lines.append(line.rstrip())
        if len(lines) == n:
            record = gcProcess(lines)
            
            total_len += len(record['sequence'])
            total_gc += str(record['sequence']).count('G') + str(record['sequence']).count('C')
            
            lines = []
    
    sample_gc = total_gc / total_len
    fh.close()
    return({"sample_gc": sample_gc})
