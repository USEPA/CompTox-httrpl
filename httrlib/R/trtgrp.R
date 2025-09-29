#' getTrts
#' - Low-level function to extract all docs from httr_trt_grp_cmp that match a specific query
#' Before returning, the ctrl_wells and trt_wells members are converted to data frames
#'
#' @param DB (\emph{mongo object}) = Open DB connection to httr_trt_grp_cmp collection
#' @param query (\emph{json}) = Query for returning docs from httr_trt_grp_cmp collection
#' @param db_host (\emph{character}) = If DB is NULL, this specifies the host to open a connection to
#' @param db_name (\emph{character}) = If DB is NULL, this specifies the DB name to connect to
#' @param collection (\emph{character}) = If DB is NULL, this specifies the collection to connect to, default: httr_trt_grp_cmp
#
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @export getTrts
#' @return (\emph{list of lists}) = Names come from trt_grp_id field, each member is a complete httr_trt_grp_cmp doc (list)

getTrts <- function(DB, query, 
                    db_host=NULL, db_name=NULL, collection="httr_trt_grp_cmp",
                    debug=getOption("debug",default=FALSE), output_dir="") {
                    
  if(debug) {cat("Querying treatments groups for:", query, "\n")}
  # Run the query with iterate and then process each returned value
  DB <- getDB(DB,db_host,db_name,collection, output_dir = output_dir)
  trt_iter <- iterate(DB,query = query)
  trt_list <- list()
  while(!is.null(trt_data <- trt_iter$one())) {
    trt_data$ctrl_wells <- do.call(rbind.data.frame, trt_data$ctrl_wells)
    row.names(trt_data$ctrl_wells) <- trt_data$ctrl_wells$sample_id
    trt_data$trt_wells <- do.call(rbind.data.frame, trt_data$trt_wells)
    row.names(trt_data$trt_wells) <- trt_data$trt_wells$sample_id
    trt_list[[length(trt_list)+1]] <- trt_data
  }
  if(debug) {cat("DB returned", length(trt_list), "treatment groups.\n")}
  names(trt_list) <- getDocIDs(trt_list, id_field = "trt_grp_id")
  return(trt_list)
}


#' getChemTrts
#'  - Pull out documents from httr_trt_grp_cmp collection corresponding to a single chemical dose-response series
#' Returns the relevant documents from httr_trt_grp_cmp as a named list, where names come from trt_grp_id field, and are sorted by dose_level
#' 
#' @param DB (\emph{mongo object}) = If specified, use this open connection to httr_trt_grp_cmp collection, ignore db_host, db_name, collection
#' @param chem_id (\emph{character}) = Match against chem_id field, this is required
#' @param db_host (\emph{character}) = If DB is NULL, this specifies the host to open a connection to
#' @param db_name (\emph{character}) = If DB is NULL, this specifies the DB name to connect to
#' @param collection (\emph{character}) = If DB is NULL, this specifies the collection to connect to, default: httr_trt_grp_cmp
#' @param pg_id (\emph{character}) = Optional addl filter on pg_id field, useful if there is more than one series for same chem_id
#' @param media (\emph{character}) = Optional addl filter on media field, useful if there is more than one series for same chem_id
#' @param timeh (\emph{numeric}) = Optional addl filter on timeh field, useful if there is more than one series for same chem_id
#' @param max_dose_level (\emph{numeric}) = Expected max number of doses, will give a warning if more than this number of treatment groups returned, default is 8
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @export getChemTrts
#' @return (\emph{named list}) = Names come from trt_grp_id field, each member is a complete httr_trt_grp_cmp doc (list type), members are sorted by dose_level field

getChemTrts <- function(DB=NULL, chem_id, 
                        db_host=NULL, db_name=NULL, collection="httr_trt_grp_cmp",
                        pg_id=NULL, media=NULL, timeh=NULL,
                        max_dose_level=8, debug=getOption("debug",default=FALSE),
                        output_dir = ""
) {
  # Make sure chem_id defined
  if(is.null(chem_id)) {
    stop("chem_id must be specified.")
  }
  # TO DO: Addl checks: make sure chem_id and any other filter field is length 1 if defined
  # Open DB connection if an open connection object was not provided
  
  DB <- getDB(DB,db_host,db_name,collection, output_dir = output_dir)
  trt_query <- mongoQuery(chem_id=chem_id, pg_id=pg_id, media=media, timeh=timeh)
  trt_list <- getTrts(DB=DB, query=trt_query, debug=debug)
  # Check that trt_list contains <= max expected doses, and all dose_level values are unique
  if(length(trt_list) > max_dose_level) {
    warning("DB returned ", length(trt_list), " treatment groups, but max_dose_level = ", max_dose_level, "\n")
  }
  trt_dose <- unlist(lapply(trt_list, "[[", "dose_level"))
  if(any(duplicated(trt_dose))) {
    warning("DB returned treatment groups with ", sum(duplicated(trt_dose)), " redundant dose levels out of ", length(trt_list), " total treatment groups.\n")
  }
  if(any(trt_dose > max_dose_level)) {
    warning("DB returned ", sum(trt_dose > max_dose_level), " dose levels above the max_dose_level of ", max_dose_level, " out of ", length(trt_list), " total treatment groups.\n")
  }
  # Re-order the trt_list with ascending dose_level
  # TO DO: This can probably be built into the mongo query using sort parameter, not sure which approach is faster
  trt_list <- trt_list[order(trt_dose, decreasing = F)]
  # Extract trt_grp_id to names(trt_list)
  names(trt_list) <- getDocIDs(trt_list, id_field="trt_grp_id")
  # Return the cleaned up list
  return(trt_list)
}


#' sameCtrlWells
#' - Function to check that a list of httr_trt_grp_cmp docs all have the same control group
#' This is useful when extracting treatment groups that should correspond to a dose-response series
#' 
#' @param trt_list (\emph{list of list}) = List of docs from httr_trt_grp_cmp (list) that are expected to all have the same control wells
#' @param debug (logical) = Whether to print debug messages, default: FALSE, overridden by options(debug=...) 
#' 
#' @return (\emph{logical}) = FALSE if there are multiple entries in trt_list with different ctrl_well sets - automatically TRUE if length(trt_list) < 2 although this will trigger a debug message
#' @export sameCtrlWells

sameCtrlWells <- function(trt_list, debug=getOption("debug",default=FALSE)) {
  if(length(trt_list) < 2) {
    if(debug){cat("trt_list had length", length(trt_list), "in sameCtrlWells() call - no ctrl_wells to compare.\n")}
    return(TRUE)
  }
  ctrl_1 <- trt_list[[1]]$ctrl_wells$sample_id
  for(j in 2:length(trt_list)) {
    ctrl_j <- trt_list[[j]]$ctrl_well$sample_id
    if(length(ctrl_1) != length(ctrl_j)) {
      return(FALSE)
    }
    if(!all(ctrl_1 %in% ctrl_j)) {
      return(FALSE)
    }
    if(!all(ctrl_j %in% ctrl_1)) {
      return(FALSE)
    }
  }
  return(TRUE)
}


#' trtGrpWells - Extract the complete set of wells involved in a series of treatment groups
#' 
#' @param trt_list (\emph{list of list}) = List of docs from httr_trt_grp_cmp (list) that are expected to all have the same control wells
#' @param subset (character) = Single character value indicating which subset of wells to extract. "all" (default) retrieves both ctrl and trt wells, "ctrl" and "trt" extrct just those well subsets respectively.
#' @export trtGrpWells
#' @return (\emph{character vector}) = Unique set of sample_ids for all or subset of wells in trt_list

trtGrpWells <- function(trt_list, subset="all") {
  if(!(subset %in% c("all","ctrl","trt"))) {
    stop("Invalid option for subset: ", subset, " - must be one of ('all','ctrl','trt').\n")
  }
  all_wells <- character(0)
  for(i in 1:length(trt_list)) {
    if(subset %in% c("all","ctrl")) {
      all_wells <- union(all_wells, trt_list[[i]]$ctrl_wells$sample_id)
    }
    if(subset %in% c("all","trt")) {
      all_wells <- union(all_wells, trt_list[[i]]$trt_wells$sample_id)
    }
  }
  return(all_wells)
}

