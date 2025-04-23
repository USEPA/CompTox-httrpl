print("Loaded library!")
library(httrlib)
STRICT <- FALSE
DEBUG <- TRUE
OUTPUT_DIR <- Sys.getenv("DB_DIR", "/workspace/docker_vol/db/")
shrinkage <- "normal" # Set to "none" to remove DESeq2 shrinkage
THREADS <- 4
plate_effect <- TRUE # Set to FALSE to remove plate effect from DESeq2 model eq
options(stringsAsFactors = FALSE, warn = if (STRICT) {
  2
} else {
  1
}, debug = DEBUG, output_dir = OUTPUT_DIR)

httr_trt_grp_cmp <- openMongo(
  output_dir = OUTPUT_DIR,
  collection = "httr_trt_grp_cmp"
)
httr_deg <- openMongo(output_dir = OUTPUT_DIR, collection = "httr_deg")

# TREATMENT GROUPS
trt_grp_names <- httr_trt_grp_cmp$distinct("trt_grp_id")
cat(
  "\n-- Running all analyses with shrinkage =", shrinkage,
  "and plate_effect =", plate_effect, "--\n\n"
)
anl_name <- getAnlName(plate_effect = plate_effect, shrinkage = shrinkage, )

cat("DESeq2 will be run on", length(trt_grp_names), "treatment groups.")
for (trt_grp_name in trt_grp_names) {
  cat("\n--- Running DESeq2 on ", trt_grp_name, "---\n\n")
  res <- runDESeq2Single(trt_grp_id = trt_grp_name, output_dir = OUTPUT_DIR)
  # Define output filename with chem_id
  output_filename <- file.path(OUTPUT_DIR, "deseq2_results", paste0(trt_grp_name,".csv"))

  # Save results to CSV
  write.csv(res, file = output_filename, row.names = FALSE)
}
