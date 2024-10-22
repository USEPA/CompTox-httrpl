#' filterSamplesByTotal - Function to prefilter DESeq2 input data by removing low-depth samples
#' Removes any sample with colsum < min_colsum

#' @param COUNTS (\emph{data.frame}) = Data frame containing all counts to be analyzed, row.names = probes, colnames = samples
#' @param min_colsum (\emph{integer}) = Remove any samples with colsum below this value
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @export filterSamplesByTotal
#' @return (\emph{data.frame}) = COUNTS after removing the samples with low colsum


# TO DO: Set a default cutoff here, make it a global option
# TO DO: Add an optional CONDS param - if specified, apply filter there as well and return both as list

filterSamplesByTotal <- function(COUNTS, min_colsum=getOption("min_colsum"), debug=getOption("debug",default=FALSE)) {
  # Compute colsums:
  my_colsums <- colSums(COUNTS)
  if(debug) { 
    if(any(my_colsums < min_colsum)) {
      cat("Removing", sum(my_colsums < min_colsum), "samples with <", min_colsum, "total reads.\n")
    } else {
      cat("All samples have >", min_colsum, "total reads.\n")
    }
  }
  return(COUNTS[,my_colsums >= min_colsum])
}

#' filterProbesByMean - Function to prefilter DESeq2 input data by removing probes with low mean count
#' Removes any probes with mean count < mean_cnt
#'
#' @param COUNTS (\emph{data.frame}) = Data frame containing all counts to be analyzed, row.names = probes, colnames = samples
#' @param mean_cnt (\emph{integer}) = Remove any samples with colsum below this value
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#'
#' @return (\emph{data.frame}) = COUNTS after removing the probes with low mean counts
#' @export filterProbesByMean

# TO DO: Set a default cutoff here, make it a global option
filterProbesByMean <- function(COUNTS, mean_cnt=getOption("mean_cnt"), debug=getOption("debug",default=FALSE)) {
  row_means <- apply(COUNTS, 1, mean)
  if(debug) {
    if(any(row_means < mean_cnt)) {
      cat("Removing", sum(row_means < mean_cnt), "probes with mean count <", mean_cnt, "\n")
    } else {
      cat("All probes have mean count >", mean_cnt, "\n")
    }
  }
  COUNTS <- COUNTS[row_means >= mean_cnt,]
  if(debug) { cat(nrow(COUNTS), "probes with mean count >=", min(apply(COUNTS, 1, mean)), "remain for DESeq2 analysis.\n") }
  return(COUNTS)
}

#' runDESeq2 - Core function for running DESeq2 on a set of wells
#' This was adapted from Imran's previous DESeq2 function and I tried to keep all parameters the same
#' Previous version can be found in httr-wf-dev repo in BitBucket, httr-ph-i branch, lib/gexp/deseq2.py, runDESeq4 function
#' The main change here is that the default shrinkage method is now done with the lfcShrink function in DESeq2 package, instead of the old method
#' The column used for primary treatment groups is now trt_name instead of trt_id to keep with the DB schema
#' NOTE: This function does no sample or probe filtering - that was implemented on the python side in Imran's code,
#'       and will be handled by a separate function in the R lib
#' NOTE: Derik's version converted conc as a factor and used that as the primary model - results should be equivalent 
#'       but might be a good idea to sort samples in increasing conc order, ideally in a higher level function that's specific to handling dose-response sets
#'
#' @param COUNTS (\emph{data.frame}) = Data frame containing all counts to be analyzed, row.names = probes, colnames = samples
#' @param CONDS (\emph{data.frame}) = Data frame containing treatment data for all samples, rows = samples in same order as COUNTS, must have trt_name and plate_id columns
#' @param ref_level (\emph{character}) = Value of "reference" treatment in trt_name column (will be used as model intercept)
#' @param plate_effect (\emph{logical}) = Should the DESeq2 model include plate effects?
#' @param plate_cont (\emph{logical}) = Should the plate contrasts be returned in addition to the primary treatment contrasts? (CURRENTLY NOT SUPPORTED)
#' @param shrinkage (\emph{character}) = Type of shrinkage to use, current options are "normal" for new default shrinkage, "none" for no shrinkage
#' @param threads (\emph{integer}) = Number of threads to use, if > 1 parallelization is accomplished with BiocParallel package
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @export runDESeq2
#' @return (\emph{list of data.frame}) = List with two members, first is the data.frame of all results for primary treatments, second is for plate effects
#'                        Each data.frame has columns probe_id, trt_name, baseMean, log2FoldChange, lfcSE, stat, pvalue, padj

# TO DO: Add other shrinkage options: apeglm, ashr, old
# TO DO: Set ref_level default to NULL and have it auto-detect the treatment with conc=0
# TO DO: Make the trt_name column name adjustable? (Low priority)


runDESeq2 <- function(COUNTS, CONDS, ref_level="DMSO_0",
                      plate_effect=F, plate_cont=F, shrinkage="normal",
                      threads=1, debug=getOption("debug",default=FALSE)
){

  libs = c('DESeq2', 'doParallel')
  for (l in libs){
    if (!requireNamespace(l, quietly = TRUE)) {
      stop(
        paste("Package ", l, "must be installed to use this function."),
        call. = FALSE
      )
    }
  }
  # Time the whole process
  start <- Sys.time()
  # Make sure shrinkage is an accepted value:
  shrinkage_opts <- c("normal","none")
  if(!(shrinkage %in% shrinkage_opts)) {
    stop('shrinkage = "', shrinkage, '" is not an accepted value, must be one of: ', paste(shrinkage_opts, collapse=', '), '\n')
  }
  require(DESeq2)
  if(threads > 1) {
    require(BiocParallel)
    register(MulticoreParam(threads))
  }
  # Report the version of DESeq2 being used here if in debug mode
  if(debug) {
    # TO DO: any other dependent packages that should be reported here?  Maybe BiocParallel, rcppArmadillo, ashr/apeglm?
    deseq2_info <- sessionInfo(package="DESeq2")
    cat("Using DESeq2 v", deseq2_info$otherPkgs$DESeq2$Version, " installed at ", attr(deseq2_info$otherPkgs$DESeq2, "file"), "\n", sep="")
    cat("on", deseq2_info$R.version$version.string, "\n")
  }
  # Take care of factors for comparisons:
  CONDS$trt_name = as.factor(CONDS$trt_name)
  CONDS$trt_name = relevel(CONDS$trt_name, ref = ref_level)
  # Create analysis obj
  if (plate_effect){
    CONDS$plate_id = as.factor(CONDS$plate_id)
    DDS = DESeqDataSetFromMatrix(countData = COUNTS,
                                 colData = CONDS,
                                 design = ~plate_id+trt_name)
  } else {
    DDS = DESeqDataSetFromMatrix(countData = COUNTS,
                                 colData = CONDS,
                                 design = ~trt_name)
  }
  if(debug) {
    cat("Running DESeq2 with design: ", as.character(attr(DDS, "design")), "\n", sep="")
    cat(" trt_name has", nlevels(DDS$trt_name), "levels:", paste(levels(DDS$trt_name), collapse=", "), "\n")
    cat(" ref_level is", ref_level, "\n")
    if(plate_effect) {
      cat(" plate_id has", nlevels(DDS$plate_id), "levels:", paste(levels(DDS$plate_id), collapse=", "), "\n")
    }
    cat(" shrinkage method:", shrinkage, "\n")
    if(threads > 1) {
      cat("Parallelizing with", threads, "threads.\n")
    } else {
      cat("Running without parallelization.\n")
    }
    cat("\n")
  }
  
  # Run DESeq
  parallel <- (threads > 1)
  # This version is for the "new" shrinkage method - shrinkage is done per 
  # contrast below after the initial model fit using lfcShrink function
  if(debug) { cat("[", date(), "] Constructing DESeq object with betaPrior=FALSE, parallel=", parallel, "...\n", sep="") }
  Out = DESeq(DDS, betaPrior=FALSE, parallel=parallel, quiet=!debug)
  # This version is the "old" shrinkage method
  # TO DO: Run this when shrinkage = "old"
  # Out = DESeq(DDS,test="Wald", betaPrior=TRUE, 
  #            modelMatrixType="expanded")
  
  # Processing the output
  DEG <- NULL
  PEF <- NULL
  Cont <- levels(DDS$trt_name)[1]
  for (j in 2:nlevels(DDS$trt_name)) {
    Trt <- levels(DDS$trt_name)[j]
    # This version is for the "new" shrinkage method(s)
    # Shrinkage is done by the lfcShrink function instead of in the primary DESeq call above
    # Must specifiy a model coefficient rather than a contrast
    # TO DO: type param specifies the shrinkage method, open up options for apeglm and ashr
    if(shrinkage == "none") {
      if(debug) { cat("[", date(), "] Extracting l2fc data for ", Trt, " with no shrinkage (results function).\n", sep="") }
      deg <- results(Out, name=paste("trt_name", Trt, "vs", Cont, sep="_"))
    } else {
      if(debug) { cat("[", date(), "] Extracting l2fc data for ", Trt, " using normal shrinkage (lfcShrink function).\n", sep="") }
      deg <- lfcShrink(Out, coef=paste("trt_name", Trt, "vs", Cont, sep="_"), type=shrinkage, parallel=parallel, quiet=!debug)
    }
    deg <- as.data.frame(deg)
    # This is the "old" shrinkage method
    # deg <- as.data.frame(results(Out, contrast=c("trt_name",Trt,Cont)))
    deg <- cbind(trt_name=Trt, deg)
    deg <- cbind(probe_id=row.names(deg), deg)
    row.names(deg) <- c()
    DEG <- rbind(DEG,deg)
  }
  
  if (plate_effect & plate_cont){
    # TO DO: This part needs to be re-done with new shrinkage method options
    stop("runDESeq2 has not been updated to return plate effects.\n")
    rnms = resultsNames(Out)
    ctrl = rnms[grep('DMSO',rnms)]
    Cont <- structure(rep(0, length(rnms)), names=rnms)
    Cont["Intercept"] <- Cont["trt_nameDMSO__0.0"] <- 1
    PEF = NULL
    for (pid in rnms[grep('^plate_id',rnms)]){
      Cont0=Cont
      Cont0[pid]=1
      pef = as.data.frame(results(Out,contrast=Cont0))
      pef$plate_id=gsub('plate_id','',pid)
      pef$probe_id=row.names(pef)
      row.names(pef)=c()
      PEF = rbind(PEF,pef)
    }
  }
  
  end <- Sys.time()
  if(debug) {
    cat("[", date(), "] DESeq2 analysis completed successfully in ", round(difftime(end, start, units="secs")), " seconds\n", sep="")
    cat(" DEG table contains", nrow(DEG), "rows for", length(unique(DEG$trt_name)), "treatment effects x", length(unique(DEG$probe_id)), "probes.\n")
    if(is.null(PEF)) {
      cat(" PEF table is empty.\n")
    } else {
      cat(" PEF table contains", nrow(PEF), "rows for", length(unique(PEF$plate_id)), "plate effects x", length(unique(PEF$probe_id)), "probes.\n")
    }
    cat("\n")
  }
  
  # Return as list with placeholder for plate effects
  return(list(DEG,PEF))
}

#' check_collection_remapped
#' Check if any collections are re-mapped, fill in defaults for everything else
#' @param collections (\emph{named character vector}) = vector to re-map collection names used, where name=default collection; value=new collection name
#' @export check_collection_remapped
#' @return collections (\emph{named character vector}) = vector to re-map collection names used, where name=default collection; value=new collection name

check_collection_remapped <- function(collections){



  def_collections <- c("httr_well", "httr_trt_grp_cmp", "httr_deg")
  def_collections <- setdiff(def_collections, names(collections))
  names(def_collections) <- def_collections
  collections <- c(collections, def_collections)
  return(collections)
  
}

#' extract_and_run - Run DESeq2 on a single chemical dose-response series
#' Uses DB access functions to extract all httr_trt_grp_id entries corresponding to a dose-response series, then extracts counts for all wells,
#' filters by min read depth and mean floor, and passes through runDESeq2. 
#' 
#' @param chem_id (\emph{character}) = ID for the chemical to run, matched against chem_id field in httr_trt_grp_cmp
#' @param db_host (\emph{character}) = Host for DB connection
#' @param db_name (\emph{character}) = Name of DB
#' @param pg_id (\emph{character}) = Limit httr_trt_grp_cmp to this plate group, useful if multiple dose response series for same chem_id
#' @param media (\emph{character}) = Limit httr_trt_grp_cmp to this media type, useful if multiple dose response series for same chem_id
#' @param timeh (\emph{integer}) = Limit httr_trt_grp_cmp to this timeh, useful if multiple dose response series for same chem_id
#' @param max_dose_level (\emph{integer}) = Max number of dose levels expected
#' @param min_colsum (\emph{integer}) = Filter samples by min read total, set to NULL to skip, defaults to 100k
#' @param mean_cnt (\emph{integer}) = Filter probes by mean floor, set to NULL to skip, defaults to 5
#' @param plate_effect (\emph{logical}) = Whether or not to include plate effect in model, defaults to FALSE
#' @param shrinkage (\emph{character}) = Type of shrinkage to use, defaults to new default, a.k.a. "normal"
#' @param threads (\emph{integer}) = Number of threads to use for core DESeq2 functions, defaults to 1 (no multi-threading)
#' @param collections (\emph{named character vector}) = Optional vector to re-map collection names used, where name=default collection; value=new collection name
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @export extract_and_run
#' @return vector of my_results, chemTrts, trt_grp_name


extract_and_run <- function(chem_id, db_host, db_name, 
                                 pg_id=NULL, media=NULL, timeh=NULL, max_dose_level=8,
                                 min_colsum=getOption("min_colsum"), mean_cnt=getOption("mean_cnt"),
                                 plate_effect=F, shrinkage="normal", threads=1,
                                 collections=character(0),
                                 debug=getOption("debug",default=FALSE),
                                 output_dir = ""){




  # Extract httr_trt_grp_cmp for chem_id, pg_id, media, timeh - check max_dose_level
  chemTrts <- getChemTrts(chem_id=chem_id, db_host=db_host, db_name=db_name, collection=collections["httr_trt_grp_cmp"], pg_id=pg_id, media=media, timeh=timeh, max_dose_level=max_dose_level, debug=debug, output_dir=output_dir)
  if(!sameCtrlWells(chemTrts, debug=debug)) {
    warning("Extracted ", length(chemTrts), " docs from httr_trt_grp_cmp that were expected to correspond to a chemical dose-response series, but the control wells differ for some treatments.\n")
  }
  
  # Extract counts for all relevant wells
  my_samples <- trtGrpWells(chemTrts)
  my_counts <- getWellCounts(sample_id=my_samples, db_host=db_host, db_name=db_name, collection=collections["httr_well"], debug=debug)
  
  COUNTS <- my_counts$counts
  CONDS <- my_counts$treatments
  
  # Determine appropriate ref_level (should be the trt_name for all ctrl wells)
  ctrl_samples <- trtGrpWells(chemTrts, subset="ctrl")
  ref_level <- unique(as.character(CONDS[ctrl_samples,"trt_name"]))
  if(length(ref_level) > 1) {
    stop("Control wells had more than one trt_name, could not determine appropriate ref_level.\n")
  }
  if(debug) { cat("Using ref_level =", ref_level, "\n") }
  
  # Fixed on 8/11/20
  # Create a mapping from trt_grp_id -> trt_name levels
  trt_grp_name <- unlist(lapply(chemTrts, function(x){
    x_trt_samples <- as.character(x[["trt_wells"]][,"sample_id"])
    x_trt_name <- unique(as.character(CONDS[x_trt_samples, "trt_name"]))
    stopifnot(length(x_trt_name) == 1)
    return(x_trt_name)
  }))
  names(trt_grp_name) <- getDocIDs(chemTrts, id_field = "trt_grp_id")
  stopifnot(sum(duplicated(trt_grp_name)) == 0)
  if(debug) {
    if(any(names(trt_grp_name) != trt_grp_name)) {
      cat("trt_name and trt_grp_id differ, mapping as:\n")
      for(i in names(trt_grp_name)) {
        cat("", i, "->", trt_grp_name[i], "\n")
      }
      cat("\n")
    }
  }
  
  # Filter by colsum and mean_cnt
  COUNTS <- filterSamplesByTotal(COUNTS = COUNTS, min_colsum = min_colsum, debug=debug)
  CONDS <- CONDS[colnames(COUNTS), ]
  COUNTS <- filterProbesByMean(COUNTS = COUNTS, mean_cnt = mean_cnt, debug=debug)
  
  # Pass to runDESeq2 function with addl params
  my_results <- runDESeq2(COUNTS, CONDS, ref_level=ref_level, plate_effect=plate_effect, shrinkage=shrinkage, threads=threads, debug=debug)
  
  return(list(my_results, chemTrts, trt_grp_name))
  
}


#' format_for_db_insert 
#' description: optionally saves the results provided by prior call to extract_and_run function back to the DB in addition to 
#' returning a single data.frame with all results that can be written to a TSV file or saved in Rdata format.
#' @param extract_and_run_return vector containing the results computed by prior call to runDESeq2 function and to be inserted into db
#' @param db_host (\emph{character}) = Host for DB connection
#' @param db_name (\emph{character}) = Name of DB
#' @param mean_cnt (\emph{integer}) = Filter probes by mean floor, set to NULL to skip, defaults to 5
#' @param plate_effect (\emph{logical}) = Whether or not to include plate effect in model, defaults to FALSE
#' @param shrinkage (\emph{character}) = Type of shrinkage to use, defaults to new default, a.k.a. "normal"
#' @param collections (\emph{named character vector}) = Optional vector to re-map collection names used, where name=default collection; value=new collection name
#' @param db_insert (\emph{logical}) = Whether to insert the results back into the DB, defaults to TRUE
#' @param rerun (\emph{logical}) = Whether to re-run the DB insertion if results are in there already (ideally this should check the DB before running...)
#' @param note (\emph{character}) = Overwrite default note fields for insert if this is specified.
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @import jsonlite
#' @export format_for_db_insert
#' @return (\emph{data.frame}) = Contains DESeq2 results for all treatments in the dose-response series


format_for_db_insert <- function(extract_and_run_return, mean_cnt, plate_effect, shrinkage, collections, db_host, db_name, note, rerun, debug, db_insert, output_dir = ""){

  my_results <- extract_and_run_return[[1]]
  chemTrts <- extract_and_run_return[[2]]
  trt_grp_name <- extract_and_run_return[[3]]
  
  
  if(db_insert) {
    require(jsonlite)
    # Generate anl_name and anl_opt for the analysis settings used here
    anl_name <- getAnlName(mean_cnt = mean_cnt, plate_effect = plate_effect, shrinkage = shrinkage)
    anl_opt <- list(meanncnt0=mean_cnt, plateteffect=as.integer(plate_effect), shrinkage=shrinkage)
    if(debug) {
      cat("Storing all results in", collections['httr_deg'], " with anl_name =", anl_name, "and anl_opt =", toJSON(anl_opt, auto_unbox = T), "\n")
    }
    
    # TO DO: Could move this code to fc.R?
    
    # Loop over entries in chemTrts
    chemDEG <- lapply(chemTrts, function(trt){
      deg_doc <- list(
        trt_grp_id=trt$trt_grp_id, 
        anl_name=anl_name, 
        anl_opt=anl_opt,
        # FIXED ON 8/11/20 - use the trt_grp_name mapping created above
        degs=my_results[[1]][my_results[[1]]$trt_name==trt_grp_name[trt$trt_grp_id], setdiff(colnames(my_results[[1]]), "trt_name")]
      )
      # Copy any optional meta-data fields from trt
      trt_meta <- setdiff(names(trt), c("_id", "trt_grp_id", "ctrl_wells", "trt_wells", "plates"))
      for(j in trt_meta) {
        deg_doc[[j]] <- trt[[j]]
      }
      # ADDED THIS ON 8/11/20 - Add a trt_name field if not present
      if(!("trt_name" %in% names(deg_doc))) {
        deg_doc[["trt_name"]] <- setNames(trt_grp_name[trt$trt_grp_id], NULL)
      }
      return(deg_doc)
    })
    
    # Check for mismatch in trt_name
    # FIXED ON 8/11/20 - Use the trt_grp_name mapping created above
    if(!all(my_results[[1]]$trt_name %in% trt_grp_name[names(chemDEG)])) {
      warning("Not all DEG results are being inserted.\n")
    }
    if(sum(unlist(lapply(chemDEG, function(x){nrow(x$degs)}))) != nrow(my_results[[1]])) {
      warning("Total DEGs for insert does not add up to total DESeq2 results.\n")
    }
    if(any(unlist(lapply(chemDEG, function(x){nrow(x$degs)})) == 0)) {
      warning("Some DEG result docs have empty $degs table.\n")
    }
    
    # Insert into DB - with or without modified notes:
    if(is.null(note)) {
      insertManyDEG(deg_docs=chemDEG, db_host=db_host, db_name=db_name, collection=collections["httr_deg"], rerun=rerun, debug=debug, output_dir=output_dir)
    } else {
      insertManyDEG(deg_docs=chemDEG, db_host=db_host, db_name=db_name, collection=collections["httr_deg"], rerun=rerun, init_note = note, update_note = note, debug=debug, output_dir=output_dir)
    }
  }
  
  # Return single data.frame
  return(my_results[[1]])
}

  
  


#' runDESeq2ForChemCond - Run DESeq2 on a single chemical dose-response series
#' Uses DB access functions to extract all httr_trt_grp_id entries corresponding to a dose-response series, then extracts counts for all wells,
#' filters by min read depth and mean floor, and passes through runDESeq2. Also optionally saves the results back to the DB in addition to 
#' returning a single data.frame with all results that can be written to a TSV file or saved in Rdata format.
#' 
#' @param chem_id (\emph{character}) = ID for the chemical to run, matched against chem_id field in httr_trt_grp_cmp
#' @param db_host (\emph{character}) = Host for DB connection
#' @param db_name (\emph{character}) = Name of DB
#' @param pg_id (\emph{character}) = Limit httr_trt_grp_cmp to this plate group, useful if multiple dose response series for same chem_id
#' @param media (\emph{character}) = Limit httr_trt_grp_cmp to this media type, useful if multiple dose response series for same chem_id
#' @param timeh (\emph{integer}) = Limit httr_trt_grp_cmp to this timeh, useful if multiple dose response series for same chem_id
#' @param max_dose_level (\emph{integer}) = Max number of dose levels expected
#' @param min_colsum (\emph{integer}) = Filter samples by min read total, set to NULL to skip, defaults to 100k
#' @param mean_cnt (\emph{integer}) = Filter probes by mean floor, set to NULL to skip, defaults to 5
#' @param plate_effect (\emph{logical}) = Whether or not to include plate effect in model, defaults to FALSE
#' @param shrinkage (\emph{character}) = Type of shrinkage to use, defaults to new default, a.k.a. "normal"
#' @param threads (\emph{integer}) = Number of threads to use for core DESeq2 functions, defaults to 1 (no multi-threading)
#' @param collections (\emph{named character vector}) = Optional vector to re-map collection names used, where name=default collection; value=new collection name
#' @param db_insert (\emph{logical}) = Whether to insert the results back into the DB, defaults to TRUE
#' @param rerun (\emph{logical}) = Whether to re-run the DB insertion if results are in there already (ideally this should check the DB before running...)
#' @param note (\emph{character}) = Overwrite default note fields for insert if this is specified.
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @import jsonlite
#' @export runDESeq2ForChemCond
#' @return (\emph{data.frame}) = Contains DESeq2 results for all treatments in the dose-response series


runDESeq2ForChemCond <- function(chem_id, db_host, db_name, 
                                 pg_id=NULL, media=NULL, timeh=NULL, max_dose_level=8,
                                 min_colsum=10^5, mean_cnt=getOption("mean_cnt"),
                                 plate_effect=F, shrinkage="normal", threads=1,
                                 collections=character(0), db_insert=T, rerun=F, note=NULL,
                                 debug=getOption("debug",default=FALSE),output_dir="") {
  
  
  collections <- check_collection_remapped(collections)
  
  extract_and_run_return  <- extract_and_run(chem_id, db_host, db_name, 
                                 pg_id, media, timeh, max_dose_level,
                                 min_colsum, mean_cnt,
                                 plate_effect, shrinkage, threads,
                                 collections, debug, output_dir=output_dir)
                                 
  return(format_for_db_insert(extract_and_run_return, mean_cnt, plate_effect, shrinkage, collections, db_host, db_name, note, rerun, debug, db_insert, output_dir))
  
  
}

#' runDESeq2Single - Run DESeq2 on a single contrast (e.g. single conc reference chemical, bulk lysate, or ref RNA QC samples)
#' Uses DB access functions to extract the httr_trt_grp_cmp entry corresponding to a specific comparison, then extracts counts for all 
#' wells, filters by min read depth and mean floor, and passes through runDESeq2. Also optionally saves the results back to the DB in 
#' addition to returning a single data.frame with all results that can be written to a TSV file or saved in Rdata format.
#' 
#' @param trt_grp_id (\emph{character}) = ID for the trt_grp_cmp to run, matched against trt_grp_id field in httr_trt_grp_cmp
#' @param db_host (\emph{character}) = Host for DB connection
#' @param db_name (\emph{character}) = Name of DB
#' @param min_colsum (\emph{integer}) = Filter samples by min read total, set to NULL to skip, defaults to 100k
#' @param mean_cnt (\emph{integer}) = Filter probes by mean floor, set to NULL to skip, defaults to 5
#' @param plate_effect (\emph{logical}) = Whether or not to include plate effect in model, defaults to FALSE
#' @param shrinkage (\emph{character}) = Type of shrinkage to use, defaults to new default, a.k.a. "normal"
#' @param threads (\emph{integer}) = Number of threads to use for core DESeq2 functions, defaults to 1 (no multi-threading)
#' @param collections (\emph{named character vector}) = Optional vector to re-map collection names used, where name=default collection; value=new collection name
#' @param db_insert (\emph{logical}) = Whether to insert the results back into the DB, defaults to TRUE
#' @param rerun (\emph{logical}) = Whether to re-run the DB insertion if results are in there already (ideally this should check the DB before running...)
#' @param note (\emph{character}) = Overwrite default notes with this one in update_notes field.
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @import jsonlite
#' @export runDESeq2Single
#' @return  (\emph{data.frame}) = Contains DESeq2 results for all treatments in the dose-response series

runDESeq2Single <- function(trt_grp_id, db_host, db_name, 
                            min_colsum=10^5, mean_cnt=getOption("mean_cnt"),
                            plate_effect=F, shrinkage="normal", threads=1,
                            collections=character(0), db_insert=T, rerun=F, note=NULL,
                            debug=getOption("debug",default=FALSE), 
                            output_dir = "", ...
) {
  # Check if any collections are re-mapped, fill in defaults for everything else
  # TO DO: This could be useful to put in a separate function for all functions that need to access multiple collections
  def_collections <- c("httr_well", "httr_trt_grp_cmp", "httr_deg")
  def_collections <- setdiff(def_collections, names(collections))
  names(def_collections) <- def_collections
  collections <- c(collections, def_collections)
  
  # Extract httr_trt_grp_cmp for trt_grp_id - should get exactly one hit
  httr_trt_grp_cmp <- openMongo(db_host = db_host, db_name = db_name, collection = collections['httr_trt_grp_cmp'], check_collection_present=TRUE, output_dir = output_dir)
  testTrt <- getTrts(DB = httr_trt_grp_cmp, query = mongoQuery(trt_grp_id = trt_grp_id), debug = debug)
  if(length(testTrt) != 1) {
    stop("getTrts returned multiple docs with trt_grp_id = ", trt_grp_id, " in ", collections['httr_trt_grp_cmp'], "\n")
  }
  
  # Extract counts for all relevant wells
  my_samples <- trtGrpWells(testTrt)
  #my_counts <- getWellCounts(sample_ids=my_samples, db_host=db_host, db_name=db_name, collection=collections["httr_well"], debug=debug)
  my_counts <- getWellCounts(sample_id=my_samples, db_host=db_host, db_name=db_name, collection=collections["httr_well"], debug=debug, output_dir = output_dir)
  COUNTS <- my_counts$counts
  CONDS <- my_counts$treatments
  
  # Determine appropriate ref_level (should be the trt_name for all ctrl wells)
  ctrl_samples <- trtGrpWells(testTrt, subset="ctrl")
  ref_level <- unique(as.character(CONDS[ctrl_samples,"trt_name"]))
  if(length(ref_level) > 1) {
    stop("Control wells had more than one trt_name, could not determine appropriate ref_level.\n")
  }
  if(debug) { cat("Using ref_level =", ref_level, "\n") }
  
  # Filter by colsum and mean_cnt
  COUNTS <- filterSamplesByTotal(COUNTS = COUNTS, min_colsum = min_colsum, debug=debug)
  CONDS <- CONDS[colnames(COUNTS), ]
  COUNTS <- filterProbesByMean(COUNTS = COUNTS, mean_cnt = mean_cnt, debug=debug)
  
  # Pass to runDESeq2 function with addl params
  my_results <- runDESeq2(COUNTS, CONDS, ref_level=ref_level, plate_effect=plate_effect, shrinkage=shrinkage, threads=threads, debug=debug)
  
  # Reformat for DB insert (optional)
  if(db_insert) {
    require(jsonlite)
    testTrt <- testTrt[[1]]
    # This check is only valid for test samples, in larger screens other sample types may have plate group or other indicator in trt_grp_id but not trt_name
    if(testTrt[["stype"]] == "test sample") {
      stopifnot(all(my_results[[1]][,"trt_name"] == testTrt$trt_grp_id))
    }
    
    # TO DO: Could move this code to fc.R?
    
    # Create a single doc for httr_deg:
    deg_doc <- list(
      trt_grp_id = testTrt$trt_grp_id, 
      anl_name = getAnlName(mean_cnt = mean_cnt, plate_effect = plate_effect, shrinkage = shrinkage), 
      anl_opt = list(meanncnt0=mean_cnt, plateteffect=as.integer(plate_effect), shrinkage=shrinkage),
      degs = my_results[[1]][, setdiff(colnames(my_results[[1]]), "trt_name")]
    )
    # Copy any optional meta-data fields from testTrt
    trt_meta <- setdiff(names(testTrt), c("_id", "trt_grp_id", "ctrl_wells", "trt_wells", "plates"))
    for(j in trt_meta) {
      deg_doc[[j]] <- testTrt[[j]]
    }
    
    # Insert into DB - with or without modified notes:
    if(is.null(note)) {
      insertOneDEG(deg_doc=deg_doc, db_host=db_host, db_name=db_name, collection=collections["httr_deg"], rerun=rerun, debug=debug, output_dir = output_dir)
    } else {
      insertOneDEG(deg_doc=deg_doc, db_host=db_host, db_name=db_name, collection=collections["httr_deg"], rerun=rerun, init_note = note, update_note = note, debug=debug, output_dir = output_dir)
    }
  }
  
  # Return single data.frame
  return(my_results[[1]])
}