DEBUG = FALSE
STRICT = TRUE
HTTRPL_LIB = "Rlib/httrpl.R"

#load reference object containing counts and metadata for DESeq2 analysis and saved results
load("RTesting/DESeq2/DESeq2_unit_test_object.Rdata")

options(stringsAsFactors = F, warn = if(STRICT){2}else{1}, debug=DEBUG)
libs = c('foreach', 'DESeq2', 'rlist', 'httrlib')
for (l in libs){
  if (!requireNamespace(l, quietly = TRUE)) {
    stop(
      paste("Package ", l, "must be installed to use this function."),
      call. = FALSE
    )
  }
}
library(foreach)
library(DESeq2)
library(rlist)
library(httrlib)

#this test should pass
DESeq2_cut(saved_results = saved_results, treatments = treatments)


# Reproducibility receipt:
cat("\n\n\nEnvironment Information:\n\n")

cat("This script was run on", date(), "on HOST:", Sys.getenv("HOSTNAME"), "\n\n")

cat("Session information:\n")
print(sessionInfo())
