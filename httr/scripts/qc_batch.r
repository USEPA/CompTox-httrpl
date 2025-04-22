library(httrlib)
print("Loaded libraries!")
STRICT <- FALSE
DEBUG <- TRUE
OUTPUT_DIR <- "/workspace/docker_vol/db"
plate_effect <- TRUE # Set to FALSE to remove plate effect from DESeq2 model equation
options(stringsAsFactors = F, warn = if (STRICT) {2} else {1}, debug = DEBUG, output_dir = OUTPUT_DIR)
qcBatch(output_dir=OUTPUT_DIR, rerun=TRUE, calc_flag=TRUE)