options(warn = 1, stringsAsFactors = F, output_dir = Sys.getenv("output_dir"))
library(jsonlite)
#library(devtools)
library(mongolite)
library(foreach)
library(data.table)
library(httrlib)

name_and_db <- Sys.getenv(c("TEST_HOST","TEST_DB"))

deg_query <- mongoQuery(chem_id = paste0("U2OS_HTTR_HTPP_COMBO_Pilot_Source_Plate_A0", 1:4))

sboxDEG <- openMongo(db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], collection = "httr_deg_2")

sboxDEG$drop()
degTestSub <- read.csv("data/httrpl_automationTestData/degs/degs.csv", stringsAsFactors = FALSE)  #Factors should not be referenced by integers, but the string themselves
sboxDEG$insert(degTestSub)

deg_results <- list()
deg_iter <- iterate(sboxDEG,query=deg_query)

while(!is.null(deg_doc <- deg_iter$one())) {
  deg_results[[length(deg_results)+1]] <- deg_doc
}

sboxDEG <- openMongo(db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], collection = "httr_deg")
sboxDEG$drop()

insertManyDEG(db_host = name_and_db["TEST_HOST"], db_name = name_and_db["TEST_DB"], deg_docs = deg_results, rerun = F, debug = T)

