

#' MongoURL
#'
#' Just format a mongo URL based on mongoServer, username, password, and database
#' @param db_host (\emph{character})
#' @param user (\emph{character})
#' @param passwd (\emph{character})
#' @param db_name (\emph{character})
#' @param authSource (\emph{character})
#' @param authMechanism (\emph{character})
#'
#' @return formatted string to pass to mongolite library connect function
#' @export mongoURL
#' @importFrom utils URLencode
# @examples url=mongoURL(db_host=db_host, user=user, passwd=passwd, db_name=db_name, authSource = authSource)

mongoURL <- function(db_host, user, passwd, db_name, authSource=NULL, authMechanism=NULL) {
  passwd <- URLencode(passwd, reserved = TRUE)  # Proect any special characters in passwd
  if(is.null(authSource) & is.null(authMechanism)){
    paste0("mongodb://", user, ":", passwd, "@", db_host, "/", db_name)
  }else {
    if (is.null(authMechanism)){
      paste0("mongodb://", user, ":", passwd, "@", db_host, "/", db_name, "?", "authSource=", authSource)
    }
    else{
      paste0("mongodb://", user, ":", passwd, "@", db_host, "/", db_name, "?", "authSource=", authSource, "&", "authMechanism=", authMechanism)
    }
  }
}

#' check_collection_exists
#' checks for existance of specific collection
#' throws a warning (which turns into an error) if collection not found
#'     
#' Parameters:
#' @param target (string) = name of collection to check existence
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @return nothing
#' @export check_collection_exists

check_collection_exists <- function (db_host, db_name, collection, output_dir = "not_set"){

  # have to connect to a collecion that may not exist to scan for db collections!
  target_col  <- openMongo(db_host=db_host, db_name = db_name, collection= collection, output_dir = output_dir)
  collections <- target_col$run('{"listCollections":1}')
  if (!(collection %in% collections$cursor$firstBatch$name))
    warning(paste("target collection", collection, "doesn't exist"))
}


#' openMongo 
#' open a connection to a specific collection in a mongo db
#'
#' @param db_host (\emph{character})
#' @param user (\emph{character})
#' @param passwd (\emph{character})
#' @param db_name (\emph{character})
#' @param collection (\emph{character})
#' @param authSource (\emph{character})
#' @param authMechanism  (\emph{character})
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#'
#' @return formatted string to pass to mongolite library connect function

#' @export openMongo
#' @import mongolite
# @examples url=mongoURL(db_host=db_host, user=user, passwd=passwd, db_name=db_name, collection=NULL, authSource = authSource, authMechanism = NULL)


openMongo <- function(db_host=getOption("DB_HOST"), user=NULL, passwd=NULL, 
  db_name=NULL, collection=NULL, authSource = NULL, authMechanism = NULL, check_collection_present=FALSE, output_dir = "not_set", verbose = FALSE) {

  #let's read the mongo/nomongo global variable value
  if (output_dir == "not_set")
    output_dir = getOption("output_dir")
  if(!is.null(output_dir) && output_dir != "") {    
    json_DB_collection_instance  <- json_DB_collection$new(output_dir=output_dir, name = collection, verbose = verbose)
    if (check_collection_present)
      json_DB_collection_instance$check_collection_exists(collection= collection)  
    return(json_DB_collection_instance)
    
  } else{
  
    stopifnot(!is.null(db_host))
    stopifnot(!is.null(db_name))
    
    if(is.null(user) | is.null(passwd)) {
      myCred <- getCredentials(db_host=db_host, db_name=db_name)
      if(is.null(user)) {
        user <- myCred$user
      }
      if(is.null(passwd)) {
        passwd <- myCred$passwd
      }
      if(is.null(authSource)) {
        authSource <- myCred$authSource
      }
      if(is.null(authMechanism)) {
        authMechanism <- myCred$authMechanism
      }
    }
    
    if (check_collection_present)
      check_collection_exists(db_host=db_host, db_name = db_name, collection= collection, output_dir = output_dir)  
    
    # Open the mongo connection
  
    return(mongo(collection=collection, url=mongoURL(db_host=db_host, user=user, passwd=passwd, db_name=db_name, authSource = authSource), verbose=getOption("verbose")))
  }
}

#' getDocIDs
#'
#' extract a certain ID field (e.g. sample_id) out of a list of documents 
#' This is designed to mimic the function of the same name from lib/db/mongo.py
#' @param docs (\emph{list}) = List of documents dumped out from MongoDB as a list type
#' @param id_field (\emph{character}) = Name of field to pull out of every member of docs
#'
#' @return (\emph{vector}) = Vector of ID values from docs, atomic type depends on the type of ID (usually character)
#' @export getDocIDs

# examples url=mongoURL(db_host=db_host, user=user, passwd=passwd, db_name=db_name, authSource = authSource)


getDocIDs <- function(docs, id_field="sample_id") {
  return(unlist(lapply(docs, "[[", id_field)))
}



#' findByID 
#' Find all documents in a collection with optional ID filtering.
#'
#' Given a vector of IDs (typically either _id or sample_id), return an iterable object (mongo iterator or list) corresponding to all 
#' matching documents in a collection. If ids is an empty list, return all documents in the collection. Also has options to limit the result 
#' to just the id_field or any other subset of fields, and to dump out the data as a list (stored in local memory) instead of returning the 
#' pymongo Cursor object. 
#' @param DB (emph{mongo object}) = Open connection to specific Mongo Database and Collection
#' @param ids (character vector) = Vector of IDs (usually character) to query on, gets all values if empty, default: empty vector.
#' @param id_field (character) = Which ID field to match against ids, default: sample_id.
#' @param id_only (logical) = If True, return only the values in the ID field being matched against, otherwise return the whole document
#' @param fields (Mongo query, as character) = If id_only = False, this will be used to filter the return fields instead.
#' @param dump (logical) = If True, loop over the initial return object (pymongo.cursor.Cursor) and extract all results to a new list object.
#' @param debug (logical) = If True, dump out some extra debugging messages, can also set using options(debug=True)
#'
#' @return mongo iterator, list, or vector) = Either:
#'   an iterator object to return results from Mongo (dump=False), 
#'  a list of all results (dump=True, id_only=False)
#'   a vector of IDs (dump=True, id_only=True)
#' @export findByID
#' @import mongolite jsonlite





findByID <- function(DB, ids=c(), id_field="sample_id", id_only=FALSE, fields='{}', dump=FALSE, debug=getOption("debug",default=FALSE)) {
  # Set query based on ids, id_field
  if (length(ids) > 0) {
    my_query = list(list('$in'=ids))
    names(my_query)[1] <- id_field[1]
    my_query = toJSON(my_query)
  } else {
    my_query = '{}'
  }
  if(debug) {cat("Setting my_query = '", my_query, "'\n", sep="")}
  # Set filter based on id_only or fields
  if(id_only) {
    if(id_field == "_id") {
      my_fields = paste0('{ "', id_field, '": true }')
    } else {
      my_fields = paste0('{ "_id": false, "', id_field, '": true }')
    }
  } else {
    # TO DO: Might need to set the default for this to '{}'?
    my_fields = fields
  }
  if(debug) {cat("Setting my_fields = '", my_fields, "'\n", sep="")}
  # Run the query with appropriate query and filter, return iterator object
  query_res = DB$iterate(query=my_query, fields=my_fields)
  # Dump to list?
  if(dump) {
    query_dump <- list()
    while(!is.null(x <- query_res$one())) {
      if(id_only) {
        query_dump[[length(query_dump)+1]] <- x[[id_field]]
      } else {
        query_dump[[length(query_dump)+1]] <- x
      }
    }
    if(id_only) {
      query_dump <- unlist(query_dump)
    }
    return(query_dump)
  } else {
    return(query_res)
  }
}

#' Title splitIDs - Split a list of IDs based on which are present/absent in a collection.
#' Given a list of IDs, search against a particular collection, and return a dict with two lists keyed by "present" and "absent".
#'
#' @param DB (\emph{mongo object}) = Open connection to a Mongo DB and Collection
#' @param ids (\emph{vector}) = Vector of IDs (typically character)
#' @param id_field (\emph{character}) = Name of ID field, default: sample_id
#'
#' @return (\emph{list}) with two members:
#'   $present = vector of IDs that were found in the collection
#'   $absent  = vector of IDs that were NOT found in collection
#' @export splitIDs

splitIDs <- function(DB, ids, id_field="sample_id") {
  # First get the set of matching IDs that are in the DB
  found_id <- findByID(DB, ids=ids, id_field=id_field, id_only=TRUE, dump=TRUE)
  # Split by which ones overlap, return as two member list
  my_res <- list(
    present = ids[ids %in% found_id],
    absent = ids[!(ids %in% found_id)]
  )
  return(my_res)
}

#' deleteByID
#' - Delete documents from a collection.
#' Given a list of IDs, delete all matching documents from a collection, or delete ALL documents. The primary purpose is to remove documents 
#' from a collection before replacing them when rerun=True in a particular pipeline step.
#'
#' @param DB (mongo object) = Open connection to database and collection
#' @param ids (\emph{vector}) = Vector (usually character) of IDs for documents to delete, default=c(), which defers to delete_all
#' @param id_field (\emph{character}) = Alternate ID field to use, default: "sample_id"
#' @param delete_all (\emph{logical}) = Setting to True will delete the whole collection, default: False
#' @param debug (\emph{logical}) = Whether to post debug messages
#' 
#' @return (\emph{numeric}) = Number of documents that were deleted, will also warn if != length(ids) when delete_all=False
#' @export deleteByID
#' @import mongolite jsonlite

deleteByID <- function(DB, ids=c(), id_field="sample_id", delete_all=FALSE, debug=getOption("debug",default=FALSE)) {
  # If delete_all, query is {}, otherwise construct from ids, id_field
  if(delete_all) {
    my_query = '{}'
  } else {
    # Otherwise, build query for user-specified id list
    my_query = list(list('$in'=ids))
    names(my_query)[1] <- id_field[1]
    my_query = toJSON(my_query)
    # Warn if ids=[]
    if(length(ids) == 0) {
      warning("deleteByID called with ids=c() and delete_all=False, no documents will be deleted.\n")
    }
  }
  # Debug message:
  if(debug) {
    cat("Deleting documents with",id_field,"matching",length(ids),"IDs.\n")
  }
  # Call $remove to do the actual deletion - use $count to make sure the right number of documents were removed
  # TO DO: Does $remove return a list of counts similar to what $insert returns? If so, should use that to get del_cnt and make sure other numbers are 0?
  prev_cnt = DB$count()
  delete_res = DB$remove(query=my_query)
  new_cnt = DB$count()
  del_cnt = prev_cnt - new_cnt
  if((del_cnt != length(ids)) && !delete_all) {
    warning("Called deleteByID with ", length(ids), " IDs but ", del_cnt, " documents were deleted (id_field='",id_field,"').\n")
  }
  return(del_cnt)
}


#' title insertByID - Insert a list of documents into a MongoDB collection without creating redundant entries.
#' Takes a list of dicts and first checks whether any documents with matching IDs are already present. If so, these documents are either 
#' skipped for insert (rerun=False) or replaced (rerun=True)
#'
#' @param DB (\emph{mongo object}) = Open database/collection connection for inserting
#' @param docs (\emph{list}) = List of documents to insert into collection
#' @param id_field (\emph{character}) = The field to use for matching up documents, default: sample_id
#' @param rerun (\emph{logical}) = Whether to overwrite existing data with same sample_id
#' @param debug (\emph{logical}) = Whether to report debug messages
#' @import mongolite
#' @return (\emph{integer}) = The number of documents successfully inserted, 
#'  Note: Unlike the pymongo API, mongolite does not return the IDs after a successful insert
#' NOTE: For this function in particular, it makes sense to have a custom wrapper for each collection, e.g. to make sure there are matching IDs in other collections
#' @export insertByID

insertByID <- function(DB, docs, id_field="sample_id", rerun=FALSE, debug=getOption("debug",default=FALSE)) {
  # TO DO: Convert this python code to R
  # Check if any matching sample_id in the collection already
  id_status = splitIDs(DB, ids=getDocIDs(docs, id_field=id_field), id_field=id_field)
  # Check if any of these sample_ids are already in the DB:
  if(length(id_status$present) > 0) {
    if(rerun) {
      # If rerun = True, drop these IDs from the DB before final insert below
      deleteByID(DB, ids=id_status['present'], id_field=id_field, debug=debug)
    } else {
      # rerun = False, so only insert the 'absent' values
      if(length(id_status$absent) == 0) {
        warning("Collection already contains documents with ", id_field, " matching all ", length(docs), " entries in docs, no DB insert to be done (rerun=False).")
        return(0)
      }
      # Otherwise, filter docs to just those with sample_id matching absent
      subset_docs = sapply(docs, function(x){x[[id_field]] %in% id_status$absent})
      docs = docs[subset_docs]
      warning("Collection already contains ", length(id_status$present), " matching ", id_field, ", only ", length(docs), " new documents will be inserted (rerun=False)")
    }
  }
  if(debug) {
    cat("Inserting",length(docs),"new documents into collection, rerun =", rerun, "\n")
  }
  
  # Can use the list object returned by each insert statement to make sure the right number of docs went in
  # prev_count = DB$count()
  
  # Insert into the database - mongo$insert does NOT handle lists of documents so need to use lapply to insert each document
  # Alternatively, might be able to convert each document to a JSON string first and pass a vector of those strings to insert all at once?
  insert_return <- lapply(docs, function(x){DB$insert(x, auto_unbox=T)})
  # Summarize the insert results
  insert_summary <- list(
    nInserted = sum(unlist(lapply(insert_return, function(x){x$nInserted}))),
    nMatched = sum(unlist(lapply(insert_return, function(x){x$nMatched}))),
    nRemoved = sum(unlist(lapply(insert_return, function(x){x$nRemoved}))),
    nUpserted = sum(unlist(lapply(insert_return, function(x){x$nUpserted}))),
    nWriteErrors = sum(unlist(lapply(insert_return, function(x){length(x$writeErrors)})))
  )
  
  ins_cnt = insert_summary$nInserted
  # TO DO: Should also check that other count fields in insert_summary are all 0 and issue warnings if not?
  
  # One final check to make sure the correct number of docs were inserted
  if(ins_cnt != length(docs)) {
    warning("Attempted to insert ", length(docs), " documents, but collection only increased by ", ins_cnt, " documents.\n")
  }
  return(ins_cnt)
}


#' mongoQuery 
#' Build a mongo query from an arbitrary set of params or a list
#' Takes a list of named values/vectors and converts to a JSON-format mongo query. Each entry with length > 1 is interpreted
#' as a set of potential matching values and converted to the $in keyword for mongo
#' @export mongoQuery
#' @param  ... (any) = Any set of named parameters OR a single named list of all arguments
#' @import jsonlite

mongoQuery <- function(...) {
  # Get all args as a list:
  args <- list(...)
  # If singular list argument, elevate that up to args
  if((length(args)==1) && (class(args) == "list") && is.null(names(args))) {
    args <- args[[1]]
  }
  # Drop members that are NULL or have length 0
  for(key in names(args)) {
    if(is.null(args[[key]]) || (length(args[[key]])==0)) {
      args[[key]] <- NULL
    }
  }
  # If args is length 0, return '{}'
  if(length(args)==0) {
    null_query <- '{}'
    class(null_query) <- "json"
    return(null_query)
  }
  # Loop through args and convert any vector args to $in construct
  for(key in names(args)) {
    if((length(args[[key]]) > 1) && (class(args[[key]]) != "list")) {
      args[[key]] <- list('$in'=args[[key]])
    }
  }
  # Convert to JSON and return
  return(toJSON(args, auto_unbox = T))
}


#' findIDs
#' Generic function to get IDs matching any query from any collection
#' Given an open DB connection or connection parameters, run a query and get all distinct values of id_field
#'
#' @param DB (\emph{mongo object}) = If specified, use this open connection to a collection, ignore db_host, db_name, collection
#' @param db_host (\emph{character}) = If DB is NULL, this specifies the host to open a connection to
#' @param db_name (\emph{character}) = If DB is NULL, this specifies the DB name to connect to
#' @param collection (\emph{character}) = If DB is NULL, this specifies the collection to connect to
#' @param id_field (\emph{character}) = What field in the DB to return IDs from, defaults to the generic mongo _id field
#' @param debug (\emph{logical}) = Whether to print debug messages, default: FALSE, overridden by options(debug=...)
#' ... = All additional params passed to mongoQuery to form the query string
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @export findIDs
#' @return (character vector) = The ID values returned by DB$distinct call

findIDs <- function(DB=NULL, db_host=NULL, db_name=NULL, collection=NULL, id_field="_id", debug=getOption("debug", default=FALSE), output_dir="not_set", ...) {
  # Open DB connection if an open connection object was not provided
  if(is.null(DB)) {
    if(debug) {cat("Opening new connection to ", db_host, "/", db_name, ".", collection, "\n", sep="")}
    DB <- openMongo(db_host=db_host, db_name=db_name, collection=collection, output_dir=output_dir)

  }
  # Construct the query
  my_query <- mongoQuery(...)
  # Call DB$distinct and return the results
  my_ids <- DB$distinct(key=id_field, query=my_query)
  if((class(my_ids)=="list") && (length(my_ids) == 0)) {
    my_ids <- character(0)
  }
  return(my_ids)
}


#' as.mongo.date 
#' Convert "POSIXct" to a list object with one member: '$date'=(ISODate str)
#'
#' Given an R datetime object, e.g. as returned by Sys.time(), convert to a structure appropriate for conversion to JSON and mongo insert
#'
#' @param x (POSIXct) = R datetime object, e.g. as returned by Sys.time()
#'
#' @return (list) = Has one member: '$date'=(ISODate str), converting this to JSON with auto_unbox=T will insert this as a proper date
#' @export as.mongo.date

as.mongo.date <- function(x) {
  list('$date'=strftime(x, "%Y-%m-%dT%H:%M:%SZ", 'UTC'))
}

#' getDB
#' if DB not provided already, opens a connection to mongodb via the mongolite package to the collection provided
#' @param DB (\emph{mongo object}) = Opened connection to specific Mongo Database and Collection or null if not provided
#' @param db_host: (\emph{character}) string representing the db url
#' @param db_name: (\emph{character}) string representing the db name
#' @param collection: (\emph{character}) string representing the collection name we are trying to connect to
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @import mongolite
#' @return (\emph{mongo object}) a mongo 'environment', i.e. a DB object
#' @export getDB

getDB <- function(DB=NULL, db_host=NULL, db_name=NULL, collection=NULL, output_dir="not_set", verbose = FALSE){

  if(is.null(DB)) {
    DB = openMongo(db_host=db_host, db_name=db_name, collection=collection, output_dir=output_dir, verbose = verbose)
  }
  return(DB)
}

#' iterate
#' with DB object as well as fields and potentially query named list, inquire db for matching documents
#' @param DB (\emph{mongo object}) = Opened connection to specific Mongo Database and Collection
#' @param fields: (\emph{json list}) of fields to be included in our search
#' @param query: (\emph{json query}) to be used in our search
#' @export iterate
#' @return list of documents matching search specified by query/fields


iterate <- function(DB=NULL,query=NULL,fields=NULL){
  
  if (is.null(fields))
    return(DB$iterate(query = query))
  
  else
    return(DB$iterate(query = query, fields = fields))

}

#' title Find : with DB object as well as fields and potentially query named list, inquire db for matching documents
#' @param  DB (\emph{mongo object}) = Opened connection to specific Mongo Database and Collection
#' @param optional fields: json list of fields to be included in our search
#' other parameter (optional): query JSON query to use when querying the db
#' @return list of documents matching search specified by query/fields

Find <- function(DB=NULL, ...){

  js = mongoQuery(...)
  l = list(...)
  
  if (grepl("query",js) == TRUE){
    if (grepl("fields",js) == TRUE){
      return(DB$find(query=paste(l$query),fields=paste(l$fields)))
    }
    else{
      return(DB$find(query=paste(l$query)))
    }
  }
  else{
    if (grepl("fields",js) == TRUE){
      return(DB$find(fields=paste(l$fields)))    
    }
    else return(DB$find())
  }
}