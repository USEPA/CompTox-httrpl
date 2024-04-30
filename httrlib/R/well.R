#' getWellCounts
#' Function to pull out counts and treatment info as data.frames for a set of sample IDs
#' 
#' @param DB (\emph{mongo object}) = If specified, use this open connection to httr_well collection, ignore db_host, db_name, collection
#' @param db_host (\emph{character}) = If DB is NULL, this specifies the host to open a connection to
#' @param db_name (\emph{character}) = If DB is NULL, this specifies the DB name to connect to
#' @param collection (\emph{character}) = If DB is NULL, this specifies the collection to connect to, default: httr_well
#' @param sample_id (\emph{character}) = Used to query counts for a specific subset of sample IDs, if NULL this will not be part of query
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' ... = Additional parameters passed to mongoQuery and used to specify which wells to extract counts for
#' @export getWellCounts
#' @return (\emph{list}) = Has two members, both data.frames: $treatments has rows = samples, $counts has rows = probes, columns = samples

# TO DO: Extra parameters to request a subset of treatment columns

getWellCounts <- function(DB=NULL, db_host=NULL, db_name=NULL, collection="httr_well", 
                          debug=getOption("debug",default=FALSE), sample_id=NULL, 
                          output_dir = "not_set", ...
) {
  if (hasArg("sample_ids"))
    stop("sample_ids must be replaced with sample_id when calling getWellCounts")
  
  # Open DB connection if an open connection object was not provided
  DB <- getDB(DB,db_host,db_name,collection, output_dir = output_dir)
  well_query <- mongoQuery(sample_id=sample_id, ...)
  if(debug) {cat("Setting well_query = '", well_query, "' in getWellCounts().\n", sep="")}
  # Run the query with iterate and then process each returned value
  # TO DO: There might be a way to handle this more elegantly by creating a handler for the find function
  well_iter <- iterate(DB,query=well_query)
  treatments <- list()
  counts <- list()
  rm_fields <- character(0)
  while(!is.null(well_data <- well_iter$one())) {
    # Put probe_cnts in one list, all other singular fields in another
    well_sid <- well_data$sample_id
    counts[[well_sid]] <- well_data$probe_cnts
    well_data$probe_cnts <- NULL
    # Remove any other non-singular fields
    well_field_sz <- unlist(lapply(well_data, length))
    well_rm_fields <- names(well_field_sz)[well_field_sz > 1]
    for(field in well_rm_fields) {
      well_data[[field]] <- NULL
      rm_fields <- union(rm_fields, field)
    }
    treatments[[well_sid]] <- well_data
  }
  if(length(rm_fields) > 0) {
    warning("Dropped ", length(rm_fields), " fields with nested data: ", paste(rm_fields, collapse=", "), "\n")
  }
  # Get union of field names and types in all treatments tables - infers type from first observed case of each field
  field_types <- character(0)
  for(i in 1:length(treatments)) {
    for(j in names(treatments[[i]])) {
      if(!(j %in% names(field_types))) {
        field_types[j] <- class(treatments[[i]][[j]])
      }
    }
  }
  # Fill in missing values with NA of correct type and re-order to match across all entries in treatments
  treatments <- lapply(treatments, function(trt){
    missing_fields <- setdiff(names(field_types), names(trt))
    for(j in missing_fields) {
      if(field_types[j] == "character") {
        trt[[j]] <- as.character(NA)
      } else if(field_types[j] == "integer") {
        trt[[j]] <- as.integer(NA)
      } else if(field_types[j] == "numeric") {
        trt[[j]] <- as.numeric(NA)
      } else if(field_types[j] == "logical") {
        trt[[j]] <- as.logical(NA)
      } else {
        warning("Don't know how to fill in missing values of type: ", field_types[j], " - will convert to character\n")
        trt[[j]] <- as.character(NA)
      }
    }
    trt[names(field_types)]
  })
  # Collapse the counts and treatments lists into data frames
  treatments <- do.call(rbind.data.frame, treatments)
  # NOTE: This may not properly handle cases where probes are missing in some sample
  counts <- do.call(rbind.data.frame, counts)
  # Transpose counts to probes x samples
  counts <- as.data.frame(t(counts))
  # Make sure counts and treatments both have same sample_id
  stopifnot(nrow(treatments)==ncol(counts))
  stopifnot(all(row.names(treatments)==colnames(counts)))
  # If sample_id was specified, warn if not all sample_id found
  if(!is.null(sample_id)) {
    if(!all(sample_id %in% row.names(treatments))) {
      warning("Database only returned data for ", sum(sample_id %in% row.names(treatments)), " of ", length(sample_id), " sample_id values specified.\n")
      # Subset sample_id to those in both tables
      sample_id <- intersect(sample_id, row.names(treatments))
    }
    # Re-order rows and columns to match sample_id order
    treatments <- treatments[sample_id,]
    counts <- counts[,sample_id]
  }
  # Combine into a list and return
  return(list(treatments=treatments, counts=counts))
}

#' getWellInfo
#' Function to pull out treatment info ONLY from httr_well as a data.frame for any relevant query
#' 
#' @param DB (\emph{mongo object}) = If specified, use this open connection to httr_well collection, ignore db_host, db_name, collection
#' @param db_host (\emph{character}) = If DB is NULL, this specifies the host to open a connection to
#' @param db_name (\emph{character}) = If DB is NULL, this specifies the DB name to connect to
#' @param collection (\emph{character}) = If DB is NULL, this specifies the collection to connect to, default: httr_well
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' ... = Any additional parameters are passed to mongoQuery to constrain the query
#' @export getWellInfo
#' @return (\emph{data.frame}) = Table of well treatment info from httr_well, currently excludes _id, probe_cnts, raw_id, counts_id, trt_id, sorts by sample_id

# TO DO: Extra parameters to request a subset of treatment columns
getWellInfo <- function(DB=NULL, db_host=NULL, db_name=NULL, collection="httr_well", 
                        debug=getOption("debug",default=FALSE), output_dir = "not_set", ...
) {
  # Open DB connection if an open connection object was not provided
  
  DB <- getDB(DB,db_host,db_name,collection, output_dir = output_dir)
  # Pass ... to query
  well_query <- mongoQuery(...)
  if(debug) {cat("Setting well_query = '", well_query, "' in getWellCounts().\n", sep="")}
  # Setup the field filter: exclude _id, probe_cnts, raw_id, counts_id, trt_id
  well_filter <- mongoQuery('_id'=0, probe_cnts=0, raw_id=0, counts_id=0, trt_id=0)
  # Run the query with iterate so we can handle missing data and any other nested fields
  # TO DO: There might be a way to handle this more elegantly by creating a handler for the find function
  well_iter = iterate(DB,query = well_query, fields=well_filter)
  treatments <- list()
  rm_fields <- character(0)
  while(!is.null(well_data <- well_iter$one())) {
    well_sid <- well_data$sample_id
    # No probe_cnts because of the filter above
    # Remove any other non-singular fields
    well_field_sz <- unlist(lapply(well_data, length))
    well_rm_fields <- names(well_field_sz)[well_field_sz > 1]
    for(field in well_rm_fields) {
      well_data[[field]] <- NULL
      rm_fields <- union(rm_fields, field)
    }
    treatments[[well_sid]] <- well_data
  }
  if(length(rm_fields) > 0) {
    warning("Dropped ", length(rm_fields), " fields with nested data: ", paste(rm_fields, collapse=", "), "\n")
  }
  # Get union of field names and types in all treatments tables - infers type from first observed case of each field
  field_types <- character(0)
  for(i in 1:length(treatments)) {
    for(j in names(treatments[[i]])) {
      if(!(j %in% names(field_types))) {
        field_types[j] <- class(treatments[[i]][[j]])
      }
    }
  }
  # Fill in missing values with NA of correct type and re-order to match across all entries in treatments
  treatments <- lapply(treatments, function(trt){
    missing_fields <- setdiff(names(field_types), names(trt))
    for(j in missing_fields) {
      if(field_types[j] == "character") {
        trt[[j]] <- as.character(NA)
      } else if(field_types[j] == "integer") {
        trt[[j]] <- as.integer(NA)
      } else if(field_types[j] == "numeric") {
        trt[[j]] <- as.numeric(NA)
      } else if(field_types[j] == "logical") {
        trt[[j]] <- as.logical(NA)
      } else {
        warning("Don't know how to fill in missing values of type: ", field_types[j], " - will convert to character\n")
        trt[[j]] <- as.character(NA)
      }
    }
    trt[names(field_types)]
  })
  # Collapse the treatments list into data.frame
  treatments <- do.call(rbind.data.frame, treatments)
  # Sort by sample_id, use sample_id as row.names, then return
  treatments <- treatments[order(treatments[,"sample_id"], decreasing = F),]
  row.names(treatments) <- treatments[,"sample_id"]
  return(treatments)
}

