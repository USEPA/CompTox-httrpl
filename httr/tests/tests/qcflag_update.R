DEBUG = FALSE
STRICT = TRUE
HTTRPL_LIB = "Rlib/httrpl.R"

# Load the main API
#source(HTTRPL_LIB, chdir=TRUE)
library(httrlib)

options(stringsAsFactors = F, warn = if(STRICT){2}else{1}, debug=DEBUG, output_dir = Sys.getenv("output_dir"))

name_and_db <- Sys.getenv(c("TEST_HOST","TEST_DB"))

options(httrQCflags = c("LOW_MAPD_FRAC","LOW_NSIG80","LOW_NCOV5","HIGH_GINI"))

updateqcFlags(name_and_db["TEST_HOST"], name_and_db["TEST_DB"]) #,'sample_id'='TC00283151_D21_2')
