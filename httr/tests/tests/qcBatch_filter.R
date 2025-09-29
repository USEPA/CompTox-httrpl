DEBUG = FALSE
STRICT = TRUE
#HTTRPL_LIB = "Rlib/httrpl.R"
#UNIT_TEST_LIB = "RTesting/DESeq2/unit_test_DESeq2_functions.R"

# Load the main API
#source(HTTRPL_LIB, chdir=TRUE)
# Load unit test functions
#source(UNIT_TEST_LIB, chdir=TRUE)

options(stringsAsFactors = F, warn = if(STRICT){2}else{1}, debug=DEBUG, output_dir = Sys.getenv("output_dir"))
library(foreach)  # NOTE: It looks like this doesn't play well with the parallels option in DESeq2
if (!requireNamespace("rlist", quietly = TRUE)) {
    stop(
      "Package \"rlist\" must be installed to use this function.",
      call. = FALSE
    )
  }
library(rlist)
library(httrlib)

name_and_db <- Sys.getenv(c("TEST_HOST","TEST_DB"))

qcBatch(name_and_db["TEST_HOST"], name_and_db["TEST_DB"],'sample_id'='TC00283151_D21_2')
