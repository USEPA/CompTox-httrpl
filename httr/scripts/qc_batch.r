library(httrlib)
print("Loaded libraries!")
STRICT <- FALSE
DEBUG <- TRUE
OUTPUT_DIR <- Sys.getenv("HTTRPL_DATA_DIR", "/var/lib/httrpl")
options(stringsAsFactors = F, warn = if (STRICT) {
    2
} else {
    1
}, debug = DEBUG, output_dir = OUTPUT_DIR)
cat("Running qc_batch.r in local database ", OUTPUT_DIR, "\n")
qcBatch(output_dir = OUTPUT_DIR, rerun = TRUE, calc_flag = TRUE)
