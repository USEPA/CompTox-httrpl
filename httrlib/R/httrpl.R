#' qcBatch
#' Function to run QC functions on all documents in httr_counts and store results in httr_counts_qc
#' Also pulls probe info out of httr_probe (should delegate to functions in db/probes.R),
#' NOTE: There's no real reason here to do sample_id batching, but could still use config files to store the DB info and collection names?
#'
#' @param db_host (\emph{character}) = MongoDB host URL
#' @param db_name (\emph{character}) = MongoDB name
#' @param db_probe (\emph{character}) = Name of probe collection, default: httr_probe
#' @param db_counts (\emph{character}) = Name of counts collection, default: httr_counts
#' @param db_collection (\emph{character}) = Name of counts_qc collection, default: httr_counts_qc
#' @param keep_flags (\emph{character vector}) = Keep all probes with these flags in the downstream QC (everything else is considered BAD), default: "OK"
#' @param calc_flag (\emph{logical}) = Whether or not to calculate qc_flag, default True but when set to False all flags will be set to "OK"
#' @param rerun (\emph{logical}) = Whether to overwrite existing data in httr_counts_qc
#' @param debug (\emph{logical}) = Whether to report debug messages
#' @param min_mapd_frac (n\emph{umeric}) = minimum mapd_frac, flag with LOW_MAPD_FRAC below this cutoff
#' @param min_n_reads_mapd (\emph{integer}) = minimum n_reads_mapd, flag with LOW_READS below this cutoff
#' @param min_n_sig80 (\emph{integer}) = minimum n_sig80, flag with LOW_NSIG80 below this cutoff
#' @param min_n_cov5 (\emph{integer}) = minimum n_cov5, flag with LOW_NCOV5 below this cutoff
#' @param max_top10_prop (\emph{numeric}) = maximum top10_prop, flag with HIGH_TOP10 above this cutoff
#' @param max_gini_coef (\emph{numeric}) = maximum gini_coef, flag with HIGH_GINI above this cutoff
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' in the ... param, a query can be passed that will select the documents in httr_well_trt used to further filter 
#' the sample_ids to run the qc_flag calculation and storage in the httr_counts_qc collection
#' @export qcBatch

# TO DO: Add a verify option to check the current results against what is already in the DB


qcBatch <- function(db_host, db_name,
  db_probe="httr_probe", db_counts="httr_counts", db_collection="httr_counts_qc", keep_flags="OK", 
  calc_flag=TRUE, rerun=FALSE, 
  min_mapd_frac = get_global_option("min_mapd_frac"),
  min_n_reads_mapd = get_global_option("min_n_reads_mapd"),
  min_n_sig80 = get_global_option("min_n_sig80"),
  min_n_cov5 = get_global_option("min_n_cov5"),
  max_top10_prop = get_global_option("max_top10_prop"),
  max_gini_coef = get_global_option("max_gini_coef"),
  qc_flags = get_global_option("qc_flags"),   
  debug=getOption("debug", default=FALSE),
  output_dir = "not_set",
  ...
) {

  # Get probe manifest
  #probeManifest <- getProbeManifest(db_host=db_host, db_name=db_name, collection=db_probe,fields='{"transcripts":0}')
  probeManifest <- getProbeManifest(db_host=db_host, db_name=db_name, collection=db_probe, output_dir = output_dir)
  
  if(debug) cat("[", date(), "] Pulled table of ", nrow(probeManifest), " probes from ", db_probe, "\n", sep="")
  
  # Pull out proben and bad_probes
  bad_probes <- probeManifest[!(probeManifest[,"probe_flag"] %in% keep_flags),"probe_name"]
  proben <- nrow(probeManifest)
  if(debug) cat(length(bad_probes), "out of", proben, "probes will be filtered out as bad.\n")
  
  # Open connection to httr_counts, get all sample_id fields

  httr_counts <- openMongo(db_host = db_host, db_name = db_name, collection = db_counts, check_collection_present=TRUE, output_dir=output_dir)
  count_samples <- sort(httr_counts$distinct(key="sample_id"))
  if(debug) cat("[", date(), "] ", db_counts, " contains ", length(count_samples), " sample IDs.\n", sep="")
  
  # Open connection to httr_counts_qc, split count_samples by what's in qc collection already
  httr_counts_qc <- openMongo(db_host = db_host, db_name = db_name, collection = db_collection, output_dir=output_dir)
  if(debug) cat("[", date(), "] ", db_collection, " contains ", httr_counts_qc$count(), " samples before running QC pipeline.\n", sep="")
  
  # open connection to httr_well_trt, handle only the sample_ids present in httr_well_trt corresponding to the provided query and also in count_samples
  if (length(list(...)) != 0){
    httr_well_trt <- getDB(NULL, db_host=db_host, db_name=db_name, collection="httr_well_trt", output_dir = output_dir)
    js = mongoQuery(...)
    well_trt_samples = Find(DB=httr_well_trt, query=js, fields=mongoQuery("sample_id"=1,"_id"=0))    
    count_samples = intersect(unlist(well_trt_samples),count_samples)
  }
  count_samples <- splitIDs(DB=httr_counts_qc, ids=count_samples)
  
  qc_count_orig <- httr_counts_qc$count()

  # Decide what to do with existing samples based on rerun param
  deletedCount <- 0
  if(length(count_samples$present) > 0) {
    if(rerun) {
      # Delete the QC data for these sample_id
      warning("Dropping QC data for ", length(count_samples$present), " samples; rerun=TRUE\n")
      deletedCount <- deleteByID(DB=httr_counts_qc, ids=count_samples$present, delete_all=FALSE, debug=debug)
      count_samples <- unique(unlist(count_samples))
    } else {
      warning("Skipping ", length(count_samples$present), " samples already in ", db_collection, "; rerun=FALSE\n")
      count_samples <- count_samples$absent
    }
  } else {
    # Doesn't matter, nothing has been processed yet
    count_samples <- count_samples$absent
  }
  
  # Loop over count_samples and process QC for each one
  cat("Running QC pipeline on", length(count_samples), "samples...\n")
  
  # NOTE: Could use foreach here to parallelize this and return the debug strings from each one to output in linear log
  # However, parallelizing DB I/O probably won't speed things up and may lead to errors, so would need to separate out just the countQC call and parallelize that part
  for(sample_id in count_samples) {
    if(debug) cat("[", date(), "] Running QC on ", sample_id, "\n", sep="")
    counts_doc <- httr_counts$iterate(query=paste0('{"sample_id":"',sample_id,'"}'), fields='{}')$one()
    # Convert probe_cnts to vector
    # TO DO: There should be a standardized access function for pulling a document out of httr_counts that also does this conversion
    counts_doc$probe_cnts <- unlist(counts_doc$probe_cnts)
    # Make sure all probe_cnt in probeManifest
    if(!all(names(counts_doc$probe_cnts) %in% probeManifest[,"probe_name"])) {
      unknown_probes <- setdiff(names(counts_doc$probe_cnts), probeManifest[,"probe_name"])
      warning(sample_id, " contains ", length(unknown_probes), " probe names not in ", db_probe, ": ", paste(head(unknown_probes), collapse=", "), "...\n")
    }
    qc_doc <- countQC(counts_doc=counts_doc, bad_probes=bad_probes, proben=proben, calc_flag=calc_flag, 
      min_mapd_frac=min_mapd_frac, min_n_reads_mapd=min_n_reads_mapd, min_n_sig80=min_n_sig80, min_n_cov5=min_n_cov5, max_top10_prop=max_top10_prop, max_gini_coef=max_gini_coef, qc_flags=qc_flags, debug=debug)
    httr_counts_qc$insert(qc_doc, auto_unbox=T)
  }
 
  # Make sure httr_counts and httr_counts_qc contain the same number of documents, same sample_ids
  counts_sz <- httr_counts$count()
  qc_sz <- httr_counts_qc$count()
  if(debug) cat("[", date(), "] ", db_collection, " contains ", qc_sz, " documents after QC pipeline.\n", sep="")
  
  if (qc_count_orig + length(count_samples) - deletedCount != qc_sz){
    warning(db_collection, " has now", qc_sz, " documents, but should have ", qc_count_orig + length(count_samples) - deletedCount, " documents after running full QC pipeline. \n")
  }
}
#' updateqcFlags
#' Function to update QC_flag for samples based on changed QC_flag threshold. Updates are saved into httr_counts_qc collection
#' function iterates through all documents found in httr_count_qc or iterates through all documents found
#' in httr_count_qc and the result of a passed query to httr_well_trt (query passed in the ...)
#'
#' @param db_host (\emph{character}) = MongoDB host URL
#' @param db_name (\emph{character}) = MongoDB name
#' @param db_collection (\emph{character}) = Name of counts_qc collection, default: httr_counts_qc
#' @param min_mapd_frac (\emph{numeric}) = minimum mapd_frac, flag with LOW_MAPD_FRAC below this cutoff
#' @param min_n_reads_mapd (\emph{integer}) = minimum n_reads_mapd, flag with LOW_READS below this cutoff
#' @param min_n_sig80 (\emph{integer}) = minimum n_sig80, flag with LOW_NSIG80 below this cutoff
#' @param min_n_cov5 (\emph{integer}) = minimum n_cov5, flag with LOW_NCOV5 below this cutoff
#' @param max_top10_prop (\emph{numeric}) = maximum top10_prop, flag with HIGH_TOP10 above this cutoff
#' @param max_gini_coef (\emph{numeric}) = maximum gini_coef, flag with HIGH_GINI above this cutoff
#' @param qc_flags (\emph{character vector}) = list of flags to apply in priority order
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @param debug (\emph{logical}) = Whether to report debug messages
#' @export updateqcFlags
#' @return nothing explicitly


updateqcFlags <- function(db_host, db_name, db_collection="httr_counts_qc",
  min_mapd_frac = get_global_option("min_mapd_frac"),
  min_n_reads_mapd = get_global_option("min_n_reads_mapd"),
  min_n_sig80 = get_global_option("min_n_sig80"),
  min_n_cov5 = get_global_option("min_n_cov5"),
  max_top10_prop = get_global_option("max_top10_prop"),
  max_gini_coef = get_global_option("max_gini_coef"),
  qc_flags = get_global_option("qc_flags"), 
  debug=getOption("debug", default=FALSE),
  output_dir = "not_set",
  ...
) {

  # Open connection to httr_counts_qc, get count_samples 
  httr_counts_qc <- openMongo(db_host = db_host, db_name = db_name, collection = db_collection, check_collection_present=TRUE, output_dir=output_dir)
  if(debug) cat("[", date(), "] ", db_collection, " contains ", httr_counts_qc$count(), " samples before running QC pipeline.\n", sep="")
  count_samples <- httr_counts_qc$distinct(key="sample_id")
  
  # open connection to httr_well_trt, handle only the sample_ids present in httr_well_trt corresponding to the provided query and also in count_samples
  if (length(list(...)) != 0){
    httr_well_trt <- getDB(NULL, db_host=db_host, db_name=db_name, collection="httr_well_trt", output_dir = output_dir)
    js = mongoQuery(...)
    well_trt_samples = Find(DB=httr_well_trt, query=js, fields=mongoQuery("sample_id"=1,"_id"=0))    
    count_samples = intersect(unlist(well_trt_samples),count_samples)
  }
  
  cat("Running QC Falgs update on ", length(count_samples), "samples...\n")
  
  httr_well <- getDB(NULL, db_host=db_host, db_name=db_name, collection="httr_well", output_dir = output_dir)
  
  for(sample_id in count_samples) {
    if(debug) cat("[", date(), "] Running QC on ", sample_id, "\n", sep="")
    
    my_fields = '{}'

    qc_doc <- httr_counts_qc$iterate(query=paste0('{"sample_id":"',sample_id,'"}'), fields = my_fields)$one()
    
    current_qc_flag <- qc_doc$qc_flag
    qc_doc$qc_flag <- countQCflag(
        qc_doc, 
        min_mapd_frac = min_mapd_frac,
        min_n_reads_mapd = min_n_reads_mapd, 
        min_n_sig80 = min_n_sig80, 
        min_n_cov5 = min_n_cov5, 
        max_top10_prop = max_top10_prop, 
        max_gini_coef = max_gini_coef, 
        qc_flags = qc_flags, 
        debug=debug)

    if (current_qc_flag != qc_doc$qc_flag){
      if(debug) cat("Updating QC Flags for ", sample_id, " old: ", current_qc_flag, " new: ", qc_doc$qc_flag, "\n")
      
      updtime   <- format(Sys.time(), "%a %b %d %X %Y ")
      updchange <- paste0(current_qc_flag, "->", qc_doc$qc_flag, " / ")
      
      d <- paste(qc_doc$update_history, updtime, updchange)      
      
      sample_q = mongoQuery("sample_id" = sample_id)
      m_upd = paste0('{"$set":{"qc_flag":', '"',  qc_doc$qc_flag , '"', ',"update_history":', '"', d , '"',   '}}') 
      
      httr_counts_qc$update(query = sample_q, update = m_upd)
      
      query_well =  httr_well$find(query=sample_q)
      if (length(list(query_well)) != 0)
        httr_well$update(query = sample_q, update = m_upd)
    }
  }
}
