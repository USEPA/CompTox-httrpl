library(httrlib)
print("Loaded library!")
STRICT <- FALSE
DEBUG <- TRUE
OUTPUT_DIR <- Sys.getenv("HTTRPL_DATA_DIR", "/var/lib/httrpl")
options(stringsAsFactors = F, warn = if (STRICT) {
  2
} else {
  1
}, debug = DEBUG, output_dir = OUTPUT_DIR)

cat("Running build_treatment_groups.r in local database: ", OUTPUT_DIR, "\n")

httr_well <- openMongo(output_dir = OUTPUT_DIR, collection = "httr_well")

# get all control wells
ctrl_wells <- httr_well$distinct(
  "sample_id",
  mongoQuery(stype = "vehicle control")
)

trt_names <- httr_well$distinct("trt_name")
cat("Adding groups for ", length(trt_names), " treatment groups...\n")

for (trt in trt_names) {
  cat(
    "-----------------------------------------------\n",
    "Processing treatment group:", trt, "\n"
  )

  sample_IDs <- httr_well$distinct(
    "sample_id",
    mongoQuery(stype = "test sample", trt_name = trt)
  )

  get_trt_wells <- mongoQuery(sample_id = sample_IDs)
  trt_grp_id <- httr_well$distinct(key = "trt_name", query = get_trt_wells)

  cat("TRT Group ID contains ", length(trt_grp_id), " different trt_names.\n")
  if (length(sample_IDs) == 0) {
    cat("No valid samples found for treatment", trt, "\n")
    next
  }

  tryCatch(
    {
      treatGroupFromSamples(
        output_dir = OUTPUT_DIR,
        trt_wells = sample_IDs,
        ctrl_wells = ctrl_wells,
        rerun = FALSE
      )
    },
    error = function(e) {
      cat("Error processing", trt, ":", e$message, "\n")
    }
  )

  cat("-----------------------------------------------\n")
}
