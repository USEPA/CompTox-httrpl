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

# Check for config file and set options appropriately
config_path = file.path(OUTPUT_DIR, "config.json")
if (file.exists(config_path)) {
    config <- jsonlite::read_json(config_path)
    # Loop through each key in config, setting global options
    for (key in names(config)) {
        options(setNames(list(config[[key]]), key))
    }
}

qcBatch(output_dir = OUTPUT_DIR, rerun = TRUE, calc_flag = TRUE)
