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
 
writeval <- function(status){
  st <- list()
  flag <- status
  names(flag) <- 'getQCdefaultTestResult'
  st <- append(st, flag)   
  httr_counts_stats$insert(st, auto_unbox=T)
}
  

options("httrQCflags"=c("LOW_MAPD_FRAC","LOW_READS","LOW_NSIG80","LOW_NCOV5","HIGH_GINI","HIGH_TOP10"))

options("httrMinMapdFrac" = 0.5)
options("httrMinMapdN"=3*(10^5))
options("httrMinNsig80"=1000)
options("httrMinNcov5"=5000)
options("httrMaxTop10Prop"=0.1)
options("httrMaxGini"=0.95)

if ((getQCdefault("mapd_frac") == 0.5) && (getQCdefault("n_reads") == 3*(10^5)) && (getQCdefault("log10_n_reads")==log10(3*(10^5))) && (getQCdefault("n_sig80")==1000) &&
  (getQCdefault("n_cov5")==5000) && (getQCdefault("top10_prop")==0.1) && (getQCdefault("gini_coef")==0.95)){
    writeval("OK")
  } else{
    writeval("PB")
  }




