library("jsonlite")
library("httrlib")
library("data.table")

#if running in nomongo mode:
#options(output_dir="httr/db")

#
# NOTES

#1 - to select a different set of tests, provide your own relevant_tests.json file with the list of tests to run
#2 - To run rom scratch on a sampleId file, use:
#       sampleID_wrapper(status="RERUN", relevant_tests=<your_test_list>)
#3   To process only updates to avoid recreating everything again, use:
#        sampleID_scan_and_update(<file1>,<file2>,relevant_tests=<your_test_list>)

name_and_db <- Sys.getenv(c("TEST_HOST","TEST_DB"))

# Please check relevant_tests.json for test accronyms

skipped_tests = c()

#running from scratch on the defaults input file minus the skipped tests

sampleID_wrapper(status="KEEP", skipped_tests=skipped_tests, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], sampleID_file='httr/data/httrpl_automationTestData/samplekeyfile/httr_u2os_toxcast_well_trt_v2_sans_dose.csv',max_dose_level=10,required_cols = c("sample_id", "plate_id", "well_id", "trt_name", "qc_flag"))

# running from scratch on different input file:
#sampleID_wrapper(status="REPLACE", skipped_tests=skipped_tests, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"] , sampleID_file='httr/data/httrpl_automationTestData/samplekeyfile/httr_mcf7_pilot_well_trt.csv')

# detecting changes between one file and another and dealing with updates:

sampleID_scan_and_update('httr/data/httrpl_automationTestData/samplekeyfile/httr_u2os_toxcast_well_trt_v2_sans_dose.csv','httr/data/httrpl_automationTestData/samplekeyfile/httr_u2os_toxcast_well_trt_v2_sans_dose_2.csv',skipped_tests=skipped_tests, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], output_dir="httr/db")


# a way to check the ProbeManifest - experimental place to run this:
#validateProbeManifest("httr/data/httrpl_automationTestData/httr_probe_data/human_wt_1.2.csv")

#testing with file location
pbstring <- "httr/data/httrpl_automationTestData/httr_probe_data/human_wt_1.2.csv"

#validateProbeManifest(pbstring, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"])

#cat("---------------------------------------\n")
#testing with data.frame:

probeData <- read.csv(pbstring, na.strings = "") 
#validateProbeManifest(probeData, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"])
tryCatch(insert_probe_manifest(probeData, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"]), error=function(e){cat(e[[1]])})
# should have displayed: Collection already exists in database and RERUN=FALSE

insert_probe_manifest(probeData, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"],status = "RERUN")
#should have inserted all probes


#cat("---------------------------------------\n")
#testing with data.table

probeData <- as.data.table(probeData)
#validateProbeManifest(probeData, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"])
insert_probe_manifest(probeData, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], status = "RERUN")

#let's check what the new gene_symbol is in httr_probe
httr_probe <- getDB(NULL, name_and_db["TEST_HOST"],name_and_db["TEST_DB"],"httr_probe")
probe <-httr_probe$find(mongoQuery(probe_name = "ZYX_7929"), mongoQuery(gene_symbol = 1))
cat(paste("probe gene_symbol is", probe$gene_symbol), "\n")
# should read: probe gene_symbol is ZYX

#let's change it through the insert_probe_manifest function
probeData[probe_name == "ZYX_7929", gene_symbol := "ZYXX"]
cat(paste("*****", probeData[probe_name == "ZYX_7929", gene_symbol], "*******"))
probeData <- probeData[probe_name == "ZYX_7929"]
insert_probe_manifest(probeData, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"],status = "REPLACE")

#let's check the new gene_symbol is in httr_probe
httr_probe <- getDB(NULL, name_and_db["TEST_HOST"],name_and_db["TEST_DB"],"httr_probe")
probe <-httr_probe$find(mongoQuery(probe_name = "ZYX_7929"), mongoQuery(gene_symbol = 1))
cat(paste("probe gene_symbol is now", probe$gene_symbol), "\n")
# should read: probe gene_symbol is now ZYXX

#cat("---------------------------------------\n")
#somethnig else

#validateProbeManifest(1, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"])
#insert_probe_manifest(1, db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"])

#update_attenuation_factors(db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], input_file="httr/data/httrpl_automationTestData/httr_probe_data/mcf7.csv")

#insert_into_httr_study(host = name_and_db["TEST_HOST"], db = name_and_db["TEST_DB"])
#fill_dose_for_well_trt_wrapper('httr/data/httrpl_automationTestData/samplekeyfile/httr_u2os_toxcast_well_trt_v2_sans_dose.csv')

#test for create_fasta_from_probe
pbstring <- "httr/data/httrpl_automationTestData/httr_probe_data/itap_pilot_s1500_probe.csv"
probeData <- read.csv(pbstring, na.strings = "", stringsAsFactors = FALSE) 
probeData <- as.data.table(probeData)

create_fasta_from_probe(file_or_data=probeData, DB=NULL, db_host=NULL, db_name=NULL, output_fasta_file="fasta_from_probe.txt")
# short of a full UAT test, we can check content of fasta_from_probe.txt
create_fasta_from_probe(file_or_data=NULL, DB=NULL, db_host=name_and_db["TEST_HOST"], db_name=name_and_db["TEST_DB"], output_fasta_file="fasta_from_probe_col.txt")
# short of a full UAT test, we can check content of fasta_from_probe_col.txt


