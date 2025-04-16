library(httrlib)
print("Loaded library!")
STRICT <- FALSE
DEBUG <- TRUE
OUTPUT_DIR <- "/workspace/docker_vol/db"
options(stringsAsFactors = F, warn = if (STRICT) {
  2
} else {
  1
}, debug = DEBUG, output_dir = OUTPUT_DIR)

ctrl_wells <- list(
  "TC00000651_A05", "TC00000651_A12", "TC00000651_A22", "TC00000651_C08",
  "TC00000651_C24", "TC00000651_E03", "TC00000651_E08", "TC00000651_E19",
  "TC00000651_E21", "TC00000651_E22", "TC00000651_G07", "TC00000651_G12",
  "TC00000651_I15", "TC00000651_K16", "TC00000651_K24", "TC00000651_M09",
  "TC00000651_M11", "TC00000651_O05", "TC00000651_O07", "TC00000651_O10",
  "TC00000651_O19", "TC00000651_O20", "TC00000652_B18", "TC00000652_B19",
  "TC00000652_D05", "TC00000652_D12", "TC00000652_D19", "TC00000652_F09",
  "TC00000652_F10", "TC00000652_F11", "TC00000652_F15", "TC00000652_F24",
  "TC00000652_H08", "TC00000652_J08", "TC00000652_J10", "TC00000652_J11",
  "TC00000652_L07", "TC00000652_L13", "TC00000652_L24", "TC00000652_N06",
  "TC00000652_P10", "TC00000652_P16", "TC00000652_P19", "TC00000652_P21"
)

httr_well <- openMongo(output_dir = OUTPUT_DIR, collection = "httr_well")
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
