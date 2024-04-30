#' getProbeManifest
#' Function to pull out all probe info as a data.frame
#' This excludes the transcript field in order to get a flat table, but includes everything else
#' The table is also sorted on the index field to ensure the row order of the original manifest
#' 
#' @param DB (\emph{mongo object}) = If specified, use this open connection to httr_probe collection, ignore db_host, db_name, collection
#' @param db_host (\emph{character}) = If DB is NULL, this specifies the host to open a connection to
#' @param db_name (\emph{character}) = If DB is NULL, this specifies the DB name to connect to
#' @param collection (\emph{character}) = If DB is NULL, this specifies the collection to connect to, default: httr_probe
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @export getProbeManifest
#' @return (\emph{data.frame}) = Data frame with complete manifest 

getProbeManifest <- function(DB=NULL, db_host=NULL, db_name=NULL, collection="httr_probe",fields='{"transcripts":0}', output_dir = "not_set", ...) {

  DB <- getDB(DB,db_host,db_name,collection, output_dir = output_dir)
  
  probeManifest <- Find(DB,fields=fields,...)
  probeManifest <- probeManifest[order(probeManifest[,"index"], decreasing = F),]
  return(probeManifest)
}

#' validateProbeManifest
#' Function to validate a probe manifest (taken as a data.frame), so that there is a check to ensure the probe manifest is formatted correctly, 
#' is not missing and required data, and can be used.
#' 
#' @param file_or_data: file path or data.frame/data.table of probe manifest; 
#' defaults to ../data/httrpl_automationTestData/httr_probe_data/human_wt_1.2.csv
#'
#' constraints are define @ https://teams.microsoft.com/l/entity/com.microsoft.teamspace.tab.wiki/tab::79582d6c-f6ec-490c-87e1-685a2c988b22?context=%7B%22subEntityId%22%3A%22%7B%5C%22pageId%5C%22%3A2%2C%5C%22sectionId%5C%22%3A8%2C%5C%22origin%5C%22%3A2%7D%22%2C%22channelId%22%3A%2219%3A2580b487b7e74a1fa1d99e8e133fc556%40thread.skype%22%7D&tenantId=88b378b3-6748-4867-acf9-76aacbeca6a7
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @import data.table
#' @export validateProbeManifest
#' @return a list of validation errors or an empty list if all tests pass
#'

validateProbeManifest <- function(file_or_data="../data/httrpl_automationTestData/httr_probe_data/human_wt_1.2.csv", DB=NULL, db_host=NULL, db_name=NULL, collection="httr_probe", output_dir = "not_set"){

  if (inherits(file_or_data, "character")){
    probe_data <- read.csv(file_or_data, na.strings = "") 
    probe_data <- as.data.table(probe_data)
  } 
  else if (is.data.frame(file_or_data) && !inherits(file_or_data, "data.table")){
    probe_data <- as.data.table(file_or_data)
  }
  else if (inherits(file_or_data, "data.table")){
    probe_data <- file_or_data
  }
  else{
    warning("validateProbeManifest takes only file location, data.table or data.frame")
    return()
  }
  
  #fetching httr_probe data
  DB <- getDB(DB,db_host,db_name,collection, output_dir = output_dir)
  data <- Find(DB)
  probe_data_table <- as.data.table(data)
  

  dbCols <- c("index", "probe_name", "probe_idnum", "probe_seq", "transcripts", "entrez_id", "gene_symbol", "ensembl_gene", "ref_transcript", "probe_flag", "attenuation")
  
  required_cols <- c("index", "probe_name", "probe_idnum", "probe_seq", "transcripts", "entrez_id", "gene_symbol", "probe_flag")
  
  probeData_and_mongo_data <- list(probe_data_table, probe_data)
  probeData_and_mongo_data_names <- c("httr_probe data", "probe_data passed")
  for (i in  seq_along(probeData_and_mongo_data)){
  
    cat("checking ", probeData_and_mongo_data_names[[i]], "\n")
    probeData <- probeData_and_mongo_data[[i]]
    results <- list()
    
    print("checking columns integrity...")
    if(!all(required_cols %in% colnames(probeData))) {
      newelem <- paste("Missing the following mandatory columns: ", paste(setdiff(required_cols, colnames(probeData)), collapse=", "), "\n")
      results <- c(results, newelem)
    }
    
    print("checking unexpected columns integrity...")
    if(!all(colnames(probeData) %in% dbCols)) {
      newelem <- paste("probeData contains unexpected columns: ", paste(setdiff(colnames(probeData), dbCols), collapse=", "), "\n")
      results <- c(results, newelem)
    }
    
    print("checking that index is of integer type ...")
    if ('index' %in% colnames(probeData) && class(probeData[, index]) != "integer"){
      newelem <- paste("index should be of integer type \n")
      results <- c(results, newelem)
    }
    
    print("checking that probe_name is of character type ...")
    if ('probe_name' %in% colnames(probeData) && typeof(class(probeData[, probe_name])) != "character"){
      newelem <- paste("probe_name should be of character type \n")
      results <- c(results, newelem)
    }
      
    print("checking that probe_idnum is of integer type ...")
    if ('probe_idnum' %in% colnames(probeData) && class(probeData[, probe_idnum]) != "integer"){
      newelem <- paste("probe_idnum should be of integer type \n")
      results <- c(results, newelem)
    }
      
    print("checking that probe_seq is of character type ...")
    if ('probe_seq' %in% colnames(probeData) && typeof(class(probeData[, probe_seq])) != "character"){
      newelem <- paste("probe_seq should be of character type \n")
      results <- c(results, newelem)
    }
       
    #transcripts
      
    print("checking that entrez_id_seq is of character type ...")
    if ('entrez_id' %in% colnames(probeData) && typeof(class(probeData[, entrez_id])) != "character"){
      newelem <- paste("entrez_id should be of character type \n")
      results <- c(results, newelem)
    }
      
    print("checking that gene_symbol is of character type ...")
    if ('gene_symbol' %in% colnames(probeData) && typeof(class(probeData[, gene_symbol])) != "character"){
      newelem <- paste("gene_symbol should be of character type \n")
      results <- c(results, newelem)
    } 
      
    print("checking that ensembl_gene is of character type ...")
    if ('ensembl_gene' %in% colnames(probeData) && 'ensembl_gene' %in% colnames(probeData) && (typeof(class(probeData[, ensembl_gene])) != "character")){
      newelem <- paste("ensembl_gene should be of character type \n")
      results <- c(results, newelem)
    } 
      
    print("checking that ref_transcript is of character type ...")
    if ('ref_transcript' %in% colnames(probeData) && typeof(class(probeData[, ref_transcript])) != "character"){
      newelem <- paste("ref_transcript should be of character type \n")
      results <- c(results, newelem)
    }
      
    print("checking that probe_flag is of character type ...")
    if ('probe_flag' %in% colnames(probeData) && typeof(class(probeData[, probe_flag])) != "character"){
      newelem <- paste("probe_flag should be of character type \n")
      results <- c(results, newelem)
    }
      
    print("checking that attenuation is of integer type ...")
    if ('attenuation' %in% colnames(probeData) && 'attenuation' %in% colnames(probeData) && class(probeData[, attenuation]) != "integer"){
      newelem <- paste("attenuation should be of integer type \n")
      results <- c(results, newelem)
    }
    cat("***********************************************************\n")
  }
  
  return(list(results, probeData))
  
}

#' insert_probe_manifest
#' Function to insert a probe manifest into the httr_probe collection after validating it
#' 
#' @param file_or_data (\emph{character}) = defaults to ../data/httrpl_automationTestData/httr_probe_data/human_wt_1.2.csv
#' @param DB: mongoDB object if not provided db_host/db_name used
#' @param db_host: (\emph{character}) mongo url with or without port
#' @param db_name: (\emph{character}) mongo database or sandbox
#' @param status: (\emph{character}) whether of not we keep prior data if found in httr_probe (status="KEEP"), erase all items in collection (status="RERUN"), or just replace the probe_name found (status="REPLACE")
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#'
#' @import data.table
#' @export insert_probe_manifest
#' @return TRUE if insertion went through
#'

insert_probe_manifest <- function(file_or_data="../data/httrpl_automationTestData/httr_probe_data/human_wt_1.2.csv", DB= NULL, db_host = NULL, db_name = NULL, status = "KEEP", output_dir = "not_set"){

  validation_results <- validateProbeManifest(file_or_data, DB, db_host, db_name, output_dir = output_dir)
  
  if (length(validation_results[[1]]) == 0){

      httr_probe <- getDB(DB,db_host,db_name,"httr_probe")
      existing_count <- httr_probe$count()
      if(existing_count > 0) {
          if(status=="RERUN") {
            cat("Dropping all existing documents in httr_probe before insert.\n")
            httr_probe$drop()
          }
          else {
              if (status=="REPLACE"){
                my_query = list(list('$in'=validation_results[[2]]$probe_name))
                names(my_query)[1] <- 'probe_name'
                my_query = toJSON(my_query)
                cat(paste("Removing ", validation_results[[2]]$probe_name, " from httr_probe \n"))
                httr_probe$remove(query=my_query)
            }
            else
                stop("Collection already exists in database and status = 'KEEP'.\n")
            
          }
      }
      insertion_res <- httr_probe$insert(validation_results[[2]])
      cat(paste("inserted ", insertion_res$nInserted, "out of", nrow(validation_results[[2]])))
      return(insertion_res$nInserted == nrow(validation_results[[2]]))
  }
  return(FALSE)
}

#' create_fasta_from_probe by filtering data.table by columns probe_name, probe_seq, prepending each probe_name with '>'
#' and followed by its sequence and writing it out to file
#' creates a fasta file holding the info presented in the passed probe data
#'
#' @param file_or_data: file path or data.frame/data.table of probe information; if not provided this will be generated from the httr_probe collection of corresponding DB
#' @param output_fasta_file file to be written to
#' @param DB DB object corresponding the mongo collection when provided
#' @param db_host when no DB mongo server url
#' @param db_name when no DB db or sandbox
#' @import data.table
#' @export create_fasta_from_probe
#' @return nothing explicit

create_fasta_from_probe <- function(file_or_data, DB=NULL, db_host=NULL, db_name=NULL, output_fasta_file="fasta_from_probe.txt"){
  if (inherits(file_or_data, "character")){
    probeTbl <- read.csv(file_or_data, na.strings = "", stringsAsFactors = FALSE) 
    probeTbl <- as.data.table(probeTbl)
  } 
  else if (is.data.frame(file_or_data) && !inherits(file_or_data, "data.table")){
    probeTbl <- as.data.table(file_or_data)
  }
  else if (inherits(file_or_data, "data.table")){
    probeTbl <- file_or_data
  }
  else{
      DB <- getDB(DB, db_host, db_name, "httr_probe")
      data <- Find(DB)
      probeTbl <- as.data.table(data)
    }
    
    
  fastaEntries <- character(0)

  for (i in 1:nrow(probeTbl)) {
    nameLine <- paste0(">", probeTbl$probe_name[i]) # Construct the name line
    seqLine <- probeTbl$probe_seq[i] # Get the sequence line
    fastaEntries <- c(fastaEntries, nameLine, seqLine)
  }

  writeLines(fastaEntries, output_fasta_file)

}

#' update_attenuation_factors
#' Create function in httr/Rlib/db/probe.R to update/replace attenuations factors in an existing httr_probe collection
#'
#' @param db_host: mongo url with or without port
#' @param db_name: mongo database or sandbox
#' @param collection: mongo collection to update - default httr_probe
#'
#'   changes attenuation values for probes found in dataframe in the collection and change to '1' any probe in collection not in the data.frame
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @import data.table
#' @return nothing explicit
#' @export update_attenuation_factors

update_attenuation_factors <- function(db_host, db_name, input_file, collection='httr_probe', output_dir = "not_set"){

  httr_probe_coll <- openMongo(db_host = db_host, db_name = db_name, collection = collection, check_collection_present=TRUE, output_dir = output_dir)
  
  probes_df <- read.csv(input_file, na.strings = "") 

  probes_df <- as.data.table(probes_df)
  
  probes_db <- Find(httr_probe_coll,query = mongoQuery(),fields = mongoQuery('_id'=0, 'probe_name'=1, 'attenuation'=1))
  
  if(!is.null(probes_db)) {
    
    # documents in collection that aren't in dataframe and whose attenuation is not 1 need updating
    results <- probes_db$probe_name[!(probes_db$probe_name %in% probes_df$probe_name) & (probes_db$attenuation != 1)]
    results = (as.data.table(results))
     
    names(results)[1] <- 'probe_name'
    
    if (nrow(results) != 0){
      cat("updating ", nrow(results), " documents not found in dataframe and whose attenuation is not 1 \n")
      m_upd = '{"$set":{"attenuation":1}}'
      for (i in 1:nrow(results)){
        m_q = paste0('{"probe_name":"', results$probe_name[i], '"}')      
        httr_probe_coll$update(query = m_q, update = m_upd)
      }
    }
    else
      cat("no documents found in collection that had attenuation different than 1 \n")
    
    #now dealing with updating documents in dataframe with new attenuation
     
    cat("now dealing with updating documents in dataframe with new attenuation factor \n")
    for (i in 1:nrow(probes_df)){
      m_q = paste0('{"probe_name":"', probes_df$probe_name[i], '"}') 
      m_upd = paste0('{"$set":{"attenuation":', probes_df$attenuation[i] , '}}')  
      httr_probe_coll$update(query = m_q, update = m_upd)
    }
  }
  else cat("colection ", collection, " is empty \n")

}
