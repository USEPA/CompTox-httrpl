library(methods) 

options(digits = 10)

#' mylistclass [ overload, can be usefull to implement hybrid data.table / list of lists type
#' not used currently
#' function overload for class mylistclass so object may be called [x,i,j], x being a list, i and j the
#' rows and columns
#' @return intercepts the call and return portion of the list as desired
#' @return nothing explicitly

`[.mylistclass` = function(x,i,j) {
    print("[ called")
    if (!missing(j) && missing(i)) {
      print("[ with j")
      return(unlist(sapply(x, function(sublist) sublist[[j]], simplify=FALSE)))
    } 
    else if (!missing(j) && !missing(i)){
      print("[ with i and j")
      filtered_x <- x[i]
      return(unlist(sapply(filtered_x, function(sublist) sublist[[j]], simplify=FALSE)))
    }
    else{
      print("[ by itself")
      return(unlist(x[i]))
    }
  }


#' json_DB_collection main collection class
#' class holds a few attributes or fields:
#' name, filename, data (the list of lists corresponding to the collection which is compatible with
#' the python side, output_dir (the directory location relative to the httr folder
#' and verbose to filter out debug outputs
#' @export json_DB_collection
#' @return nothing explicitly

json_DB_collection <- setRefClass("json_DB_collection", fields = list(name = "character",
          filename = "character", data = "list", output_dir="character", verbose="logical"), methods = list(
          #' initialize: json_DB_collection constructor
          #' initializes fields and make them permanent
          #' @param output_dir: str the location of the directory hosting the collections relative to the httr folder
          #' @param name : str name of collection to be created
          #' @param verbose to filter out debug outputs
          #' @return a well formed json_DB_collection object!
          
          initialize = function(output_dir, name, verbose=FALSE)
          {
            verbose <<- verbose
            output_dir <<- output_dir
            if (verbose){
              cat(paste("initialize", "output_dir", output_dir, "name", name, "\n"))
            }
            name <<- name
            filename <<- file.path(output_dir, name)
            if (verbose){
              cat(paste("filename", filename, "\n"))
            }
            
            if (file.exists(filename)){
              if (verbose){
                cat(paste("file", filename, "exists"))
              }
              if (file.info(filename)$isdir == FALSE){
                data <<- jsonlite::fromJSON(filename, simplifyDataFrame = FALSE) #if it looks like a dataframe, it's a list of lists ...
              }
            } else {
              data <<- list()
            }
          },
          
          #' format_number : utility function
          #' allows to turn float into string to conserve their precision to go around jsonlite::write_JSON
          #' that tends to limit the float precision to 5 digits
          #' @param x the float
          #' @param digits the precision
          #' @return the string representation
          
          format_number = function(x, digits = 9) {
            if (is.numeric(x) && !is.integer(x)) {
              sprintf(paste0("%.", digits, "f"), x)
            } else {
              x
            }
          },
          
          #' drop : drops a collection
          #' physically removes a file corresponding to the collection of interest
          #' no param: removes this collection file, re-initializes its data in memory
          #' @return the empty list corresponding to the new data
          
          drop = function()
          {
            if (verbose){
              cat("drop \n")
            }
            if (file.exists(filename)){
              file.remove(filename)
              data <<- list()
            }
          },
          
          #' format_number : utility function
          #' allows to print all rows
          #' that tends to limit the float precision to 5 digits
          #' @param no param
          #' @return N/A
          
          print_collection = function()
          {
            for (row in data) 
              { 
                r <- list() 
                for (k in names(row)) { 
                  r[[k]] <- row[[k]] 
                } 
                print(r) 
              }
          },
          
          #' save : commits collection to disk
          #' the trick is to turns each float into a string before it's passed to Jsonlite::write_JSON
          #' then modify the file to remove the quotes around the floats that were turned in strings!
          #' @param no param - acts on this collection (like self in python, this in c++)
          #' @return  N/A
          
          save = function(){
          # Apply formatting and track modified keys
            formatted_data <- list()
            keys_changed <- list()
            
            for (i in seq_along(data)) {
              sublist <- data[[i]]
              keys_changed[[i]] <- character() # Initialize empty character vector
              for (key in names(sublist)) {
                original_value <- sublist[[key]]
                new_value <- format_number(original_value)
                if (!identical(original_value, new_value)) {
                  keys_changed[[i]] <- c(keys_changed[[i]], key)
                }
                sublist[[key]] <- new_value
              }
              formatted_data[[i]] <- sublist
            }
            
            # Flatten the keys_changed list if necessary
            keys_changed <- unlist(keys_changed)
            if (verbose){
              cat(paste("keys_changed", keys_changed, "\n"))
            }

            jsonlite::write_json(formatted_data, filename, auto_unbox=T) #, auto_unbox=FALSE) #, simplifyDataFrame = F)
            if (length(keys_changed)!=0){
              file_content = readLines(filename)
              
              for (key in keys_changed){
                if (verbose){
                  cat(paste("key changed:", key, "\n"))   
                }
                # Modify the content
                # This regular expression looks for the specific string followed by a quoted number
                pattern <- paste0('"', key, '":\\s*"([0-9]+\\.?[0-9]*)\\s*"')
                replacement <- paste0('"', key, '": \\1')
                file_content <-  gsub(pattern, replacement, file_content, perl = TRUE)
              }
              writeLines(file_content, filename)
            }
          },
          
          #' check_collection_exists : 
          #' issues warning if collection does not exist
          #' warning supposed to be picked up by caller, who likely will turn it in an error
          #' @param collection, collection of interest - typically will be used on this collection
          #' @return N/A
          
          check_collection_exists = function(collection)
          {
            if (verbose){
              cat("check_collection_exists \n")
            }
            file_path <- file.path(output_dir, collection)
            if (!file.exists(file_path)){
              warning(paste("target collection", collection, "doesn't exist"))
            }
          },
          
          #' count 
          #' returns number of documents in the collection corresponding to given query
          #' @param query - the mongoQuery looking string to pass on to find to get the list of documents
          #' @param filter - largely ignored
          #' @return number of documents
          
          count = function(query=c(), filter=mongoQuery())
          {
            if (verbose){
              cat("count \n")
            }
            rows <- find_as_list(query, filter)
            return(length(rows))
          },
          
          #' remove 
          #' removes the document identified with the passed query
          #' @param query - the mongoQuery looking string to pass on to find to get the list of documents
          #' @return whether or not documents were deleted - both memory and disk
          #' not fully tested
          
          remove = function(query = list())
          {
            if (verbose){
              cat("remove \n")
            }
            row_indexes <- new.env()
            items <- find_as_list(query = query, row_indexes = row_indexes)
            
      
            # Remove rows in reverse order to avoid index shifting
            for (r in rev(row_indexes$row_nb)) { # added row_indexes
              data[[r]] <<- NULL
            }
      
            # Remove NULLs created by deletion
            data <<- data[!sapply(data, is.null)]
      
            # Save changes
            save()
      
            # Return True if items deleted
            length(row_indexes$row_nb) > 0
          },
          
          #' insert 
          #' inserts the documents passed by caller
          #' @param json_load_content - the dataframe or list of documents
          #' @param auto_unbox 
          #' @param na : whether na values should be defaulted to string
          #' @return a named list with specific names, nInserted being the one relevant
          
          insert = function(json_load_content, auto_unbox = TRUE, na = "string") {
            if (verbose){
              cat("insert")
            }
          
            if (auto_unbox) {
              names(json_load_content) <- gsub("\\.", "_", names(json_load_content))
            }
            
            if (is.data.frame(json_load_content)) {
              # If json_load_content is a dataframe
              for (i in seq_len(nrow(json_load_content))) {
                row_as_list <- as.list(json_load_content[i, ])
                
                row_as_list[["_id"]] <- paste0(Sys.getpid(), length(data))
                
                data <<- c(data, list(row_as_list))
              }
            } else if (is.list(json_load_content)) {
              json_load_content[["_id"]] <- paste0(Sys.getpid(), length(data))
              data <<- c(data, list(json_load_content))
            }
          
            # Save changes
            save()
          
            # Return result similar to mongolite's insert function
            list(nInserted = if (is.data.frame(json_load_content)) nrow(json_load_content) else length(list(json_load_content)), 
                 nMatched = 0, nRemoved = 0, nUpserted = 0)
          },
          
          #' distinct 
          #' returns all the values corresponding to a given column
          #' @param key - str that corresponds to the name of the column
          #' @param query, the query limiting the document for which to compute the value for that column 
          #' @return unique_values corresponding to these documents for given column

          distinct = function(key, query=c())
          {
            if (verbose){
              cat(paste("distinct key:", key,  "\n"))
            }
            rows <- find_as_list(query = query, fields = mongoQuery(setNames(list(1), key))) #, rows_as_list = rows_as_list)

            values <- sapply(rows, function(x) {
                if (key %in% names(x)) {
                  return(x[[key]])
                } else {
                  return(NA)
                }
            })   
            
            if (length(values) >0 && !all(is.na(values))) {
              unique_values <- unique(na.omit(values))
            } else{
              unique_values <- list()
            }           
            
            if (verbose){
              cat(paste("unique values are", unique_values, "\n"))
            }
            return(unique_values)
          },
          
          #' iterate 
          #' generator like function that keeps its state accross subsequent calls and deliver the next document
          #' corresponding to the passed query
          #' @param query - used to search for documents
          #' @param fields: the columns to limit our output to 
          #' @return a named list corresponding to next found document
          
          iterate = function(query = c(), fields = mongoQuery()){
            if (verbose){
              cat("iterate \n")
            }
            # Create an environment to hold the state
            state <- new.env()
            state$rows <- find_as_list(query = query, fields = fields)
            if (verbose){
              str(state$rows)
            }
            state$current_index <- 1
            
            if (verbose){
              cat(paste("state$rows", length(state$rows), "\n"))
            }
      
            list(
              one = function() {
                if (state$current_index > length(state$rows)) {
                #if (state$current_index > nrow(state$rows)) {
                  return(NULL)
                } else {
                  result <- state$rows[[state$current_index]]#DF CHANGE out
                  state$current_index <- state$current_index + 1
                  return(result)
                }
              }
            )
          }, 
          
          #' parse_query 
          #' utility function to turns the given json_query (a mongoquery object) into 
          #' easier to manipulate list
          #' @param json_query - the mongoquery object as used thoughout the pipeline
          #' @return the newly formatted string
          
          parse_query = function(json_query) {
              # Parse the JSON string
              parsed <- jsonlite::fromJSON(json_query)
              
              # Process each element
              result <- lapply(parsed, function(item) {
                  if (is.list(item) && "$in" %in% names(item)) {
                      # Return the vector within the $in element
                      return(item[["$in"]])
                  } else {
                      # Return the item as a single-element vector
                      return(item)
                  }
              })
              if (verbose){
                cat(paste("json_query", json_query))
                cat(paste("result", result))
              }
          
              return(result)
          },
          
          #' make_list 
          #' utility function to inherit some additional behavior from other class 
          #' @param lst - the lsit object we want to upgrade to new functionality
          #' @return N/A
          
          make_list = function(lst){
            class(lst) <- "mylistclass"
            lst
          },
          
          #' find_as_list 
          #' internal function that finds documents corresponding to query and filter 
          #' @param query to use
          #' #param fields : columns to limit our scope
          #' @param row_indexes - the row indexes of the found documents
          #' 
          #' @return the rows (directly) and row_indexes isolated in their own environment (as set by the caller) - only way to pass by reference in R!
          
          find_as_list = function(query = c(), fields =  mongoQuery(),  row_indexes = NULL) {
            if (verbose){
              cat(paste("find query", query, "fields", fields, "\n"))
            }
            rows <- list()
            if (length(query) == 0) {
              if (verbose){
                cat("find query not provided \n")
              }
              rows <- data
            } else{
              
              query <- parse_query(query)
        
              for (index in seq_along(data)) {
                row <- data[[index]]
                match <- TRUE
                for (key in names(query)) {
                  val <- query[[key]]
                  
                  if (!(key %in% names(row)) || !(row[[key]] %in% val)) {
                      match <- FALSE
                      break
                    }
                }
        
                if (match) {
                  rows <- c(rows, list(row))
                  if (!is.null(row_indexes)){
                    row_indexes$row_nb <- append(row_indexes$row_nb, list(index))
                  }  
                }
              }
            }      
            # Filter handling
            filter <- parse_query(fields)

            if (length(filter) > 0){
              if (verbose){
                cat(paste("names(filter)", names(filter), "\n"))
              }
              rows <- lapply(rows, function(row) {
                
                excluded_fields <- names(filter)[filter == 0]
                included_fields <- names(filter)[filter == 1]
                   
                if (length(included_fields) != 0){
                  if ("_id" %in% excluded_fields){
                    row <- row[c(included_fields)]
                  } else{
                    row <- row[c(included_fields, "_id")]
                  }
                } else{
                    row <- row[c(names(row)[!names(row) %in% excluded_fields])]               
                }
                row
              })
            }
            
            return(rows)
          },
          
          #' find 
          #' workhorse function that finds documents corresponding to query and filter 
          #' @param query to use
          #' #param fields : columns to limit our scope
          #' @param row_indexes - the row indexes of the found documents
          #' 
          #' @importFrom dplyr bind_rows
          #' @return the rows found
      
          find = function(query = c(), fields =  mongoQuery()){
            rows <- find_as_list(query, fields)
            
            filter <- parse_query(fields)
            if (length(filter) == 0){
              rows <- lapply(rows, function(row) {
                row["_id"] <- NULL
                row
              })
            }

            process_row <- function(row) {
              # Create an empty list to hold all items (both list and non-list)
              items <- list()
              list_detected <- FALSE
              
              for (name in names(row)) {
                item <- row[[name]]
                if (is.list(item)) {
                  items[[name]] <- as.data.frame(list(item), stringsAsFactors = FALSE)
                  list_detected <- TRUE
                } else {
                  items[[name]] <- item
                }
              }
              
              # Combine all items into a single data.frame
              if (list_detected){
                combined_df <- do.call(cbind, items)
              }
              else{
                combined_df <- as.data.frame(items)
              }
              return(combined_df)
            }

            rows <- lapply(rows, process_row) %>% bind_rows()
            return(rows)
          },
            
          #' update 
          #' function to update list of documents at once 
          #' @param query to use
          #' @param update : the $set statement to apply
          #' @return the row_indexes isolated in their own environment (as set by the caller) - only way to pass by reference in R!
          
          update = function(query = c(), update = c()) {
            if (verbose){
              cat("update \n")
            }
            row_indexes <- new.env()
            row_indexes$row_nb <- list()
            items <- find_as_list(query = query, row_indexes = row_indexes)
            if (verbose){
              cat(paste("row_nb", row_indexes$row_nb,  "\n"))
            }
            if (length(row_indexes$row_nb) > 0) {
              update <- parse_query(update)
              if ("$set" %in% names(update)) {
                set_fields <- update[["$set"]]
                if (verbose){
                  cat(paste("set_fields", set_fields))
                }
                for (index in row_indexes$row_nb) {
                  for (field in names(set_fields)) {
                    data[[index]][[field]] <<- set_fields[[field]]
                  }
                }
              }
            }

            save()
          }
          ))

