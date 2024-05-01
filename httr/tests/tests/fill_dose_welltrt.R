library(httrlib)
options(output_dir = Sys.getenv("output_dir"))

if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package \"data.table\" must be installed to use this function.",
      call. = FALSE
    )
  }
library(data.table)

name_and_db <- Sys.getenv(c("TEST_HOST","TEST_DB"))

httr_dose_httr_well_trt <- openMongo(db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], collection = "httr_dose_httr_well_trt")
results <- fill_dose_for_well_trt_wrapper('data/httrpl_automationTestData/samplekeyfile/httr_u2os_toxcast_well_trt_v2_sans_dose.csv')
httr_dose_httr_well_trt$insert(results, auto_unbox=T)
