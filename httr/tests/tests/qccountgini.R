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

httr_counts_stats <- openMongo(db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], collection = "httr_counts_stats")
httr_counts <- openMongo(db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], collection = "httr_counts", check_collection_present=TRUE)

count_samples <- sort(httr_counts$distinct(key="sample_id"))

for(sample_id in count_samples) {
  counts_doc <- httr_counts$iterate(query=paste0('{"sample_id":"',sample_id,'"}'), fields='{}')$one()
  
  d <- counts_doc$probe_cnts[[1]]
  counts_doc$probe_cnts <- unlist(counts_doc$probe_cnts)
  names(counts_doc$sample_id) <- 'sample_id' 
  st <- list()
  res <- countGini(counts_doc$probe_cnts)
  names(res) <- 'gini_coef'
  st <- append(st, res)
  
  qc_doc <- append(counts_doc$sample_id, st)
  httr_counts_stats$insert(qc_doc, auto_unbox=T)
}
