

#' defaultLogin
#'
#' looks for credentials
#' @param pwfile (\emph{character}) password file

#' @export defaultLogin


defaultLogin <- function(pwfile = DEFAULT_PASSWD) {
  # Expand ~ to home directory
  if (startsWith(pwfile, "~")) {
    pwfile <- file.path(Sys.getenv("HOME"), substring(pwfile, 2))
  }
  
  if (file.exists(pwfile)) {
    # Attempt to read and parse user:passwd
    lines <- tryCatch(readLines(pwfile, warn = FALSE), 
                      error = function(e) {
                        message(sprintf("WARNING: Could not read default login from %s due to error: %s", pwfile, e$message))
                        return(NULL)
                      })
    if (is.null(lines) || length(lines) == 0) {
      return(list(user = NULL, passwd = NULL))
    }
    parts <- strsplit(trimws(lines[1]), ":")[[1]]
    if (length(parts) != 2) {
      message(sprintf("WARNING: Could not parse default login from %s: expected 'user:passwd' format", pwfile))
      return(list(user = NULL, passwd = NULL))
    }
    return(list(user = parts[1], passwd = parts[2]))
  } else {
    return(list(user = NULL, passwd = NULL))
  }
}



#' Keychain class
#'
#' Represents a keychain of database credentials.
#'
#' @field keylist The list of credentials.
#'
#' @description
#' The Keychain class allows storing and retrieving MongoDB credentials.
#'
#' @details
#' Credentials can be added, found, or retrieved by host and database name.
#'
#' @param db_host Host to search (for getCredentials and findKey).
#' @param db_name Database name to search (for getCredentials and findKey).
#' @param host The MongoDB host (for addKey).
#' @param db The MongoDB database (for addKey).
#' @param user The username (for addKey).
#' @param passwd The password (for addKey).
#' @param authSource Optional authentication source (for addKey).
#' @param authMechanism Optional authentication mechanism (for addKey).
#' @param keyfile File path to save the keychain (for save).
#'
#' @return
#' For `getCredentials`, returns a list with user and password (or NULL if not found).  
#' For `findKey`, returns index or -1.  
#' For `addKey` and `save`, no return value (invisible NULL).
#'
#' @importFrom jsonlite fromJSON toJSON
#' @import R6
#' @import getPass
#' @export
Keychain <- R6Class("Keychain",
  public = list(
    keylist = NULL,
    
    initialize = function() {
      DEFAULT_PASSWD <- file.path(Sys.getenv("HOME"), ".mngdb", "passwd")
  
      if (Sys.getenv("MONGOPW_FILE") == "") {
        DEFAULT_KEYCHAIN <<- file.path(Sys.getenv("HOME"), ".mongopw")
      } else {
        DEFAULT_KEYCHAIN <<- Sys.getenv("MONGOPW_FILE")
      }
    
      keyfile <- path.expand(DEFAULT_KEYCHAIN)
      if (file.exists(keyfile)) {
        tryCatch({
          self$keylist <- fromJSON(keyfile, simplifyVector = FALSE)
        }, error = function(e) {
          message(sprintf("WARNING: Could not load keyfile from %s due to error: %s", keyfile, e$message))
          self$keylist <- list()
        })
      } else {
        self$keylist <- list()
      }
    },
    
    getCredentials = function(db_host=getOption("DB_HOST"), db_name=NULL) {
      if(!is.null(db_host) & !is.null(db_name)) {
        mongopw_file <- Sys.getenv("MONGOPW_FILE")
        if (mongopw_file == "")
          keyfile <- "~/.mongopw"
        else
          keyfile <- mongopw_file
        if(file.exists(keyfile)) {
          # Use jsonlite package to parse the keychain json file
          # Read in the keyfile - dataframe with columns host, db, user, passwd
          keydata <- read_json(keyfile, simplifyVector = T)
          # Check for a row matching db_host and db_name (if multiple rows, take first one but throw a warning)
          matchRows <- which((keydata[,"host"] == db_host) & (keydata[,"db"] == db_name))
          #let's also try adding/removing the potentially appended port number
          if (length(matchRows) == 0){
            if (grepl("27017", db_host) == TRUE)
              db_host=substr(db_host,0,nchar(db_host)-6)  
            else
              db_host = paste0(db_host,":27017")
            matchRows <- which((keydata[,"host"] == db_host) & (keydata[,"db"] == db_name))
          }
          if(length(matchRows) > 1) {
            warning("Found multiple entries in ", keyfile, " with host=", db_host, " and db=", db_name, ", using first such entry.")
          }
          if(length(matchRows) > 0) {
            matchRows <- matchRows[1]
            # Drop missing values so they don't get passed as NA
            myCred <- as.list(keydata[matchRows,])
            return(myCred[!is.na(myCred)])
          }
          print("STILL NO MATCH")
        }
      }
      # At this point, either no db_host/db_name specified, keyfile didn't exist, or no matching entries
      # Check for /.mngdb/passwd file which should just have "user:passwd"
      # TO DO: Make this file name configurable as well
      pwfile <- "~/.mngdb/passwd"
      if(file.exists(pwfile)) {
        pwdata <- readLines(pwfile)
        pwdata <- sub("^[ \t]","",pwdata)
        pwdata <- sub("[ \t]$","",pwdata)
        pwdata <- strsplit(pwdata, split=":", fixed=T)[[1]]
        return(list(user=pwdata[1], passwd=pwdata[2])) #might need to add the authentication info here
      }
      # At this point, defer to global options httrDefaultUser and httrDefaultPasswd if defined
      if(!is.null(getOption("httrDefaultUser")) & !is.null(getOption("httrDefaultPasswd"))) {
        return(list(user=getOption("httrDefaultUser"), passwd=getOption("httrDefaultPasswd")))
      }
      # At this point, no user,passwd was found - throw warning and return NULL
      warning("Could not find any credential files.")
      return(NULL)
    },  

      
    findKey = function(host, db) {
      for (i in seq_along(self$keylist)) {
        key <- self$keylist[[i]]
        if (!is.null(key$host) && !is.null(key$db)) {
          if (key$host == host && key$db == db) {
            return(i)
          }
        }
      }
      if (grepl("27017", host, fixed = TRUE)) {
        host <- sub(":27017$", "", host)
      } else {
        host <- paste0(host, ":27017")
      }
      for (i in seq_along(self$keylist)) {
        key <- self$keylist[[i]]
        if (!is.null(key$host) && !is.null(key$db)) {
          if (key$host == host && key$db == db) {
            return(i)
          }
        }
      }
      return(-1)
    },     

     
    addKey = function(host, db, user, passwd, authSource = NULL, authMechanism = NULL) {
      newKey <- list( host = host, db = db,  user = user, passwd = passwd )
      if (!is.null(authSource)) {
        newKey$authSource <- authSource
      }
      if (!is.null(authMechanism)) {
        newKey$authMechanism <- authMechanism
      }
      
      existingIndex <- self$findKey(host, db)
      if (existingIndex == -1) {
        self$keylist[[length(self$keylist) + 1]] <- newKey
      } else {
        self$keylist[[existingIndex]] <- newKey
      }
      self$save(DEFAULT_KEYCHAIN)
    },
      
    save = function(keyfile) {
      # Convert keylist to JSON string with indentation
      json_str <- toJSON(self$keylist, pretty = TRUE, auto_unbox = TRUE)
  
      # Write to file
      writeLines(json_str, keyfile)
  
      # Try to set permissions to owner read/write only (0600)
      tryCatch({
        Sys.chmod(keyfile, mode = "0600")
      }, warning = function(w) {
        message(sprintf("WARNING: Could not set permissions on %s: %s", keyfile, w$message))
      }, error = function(e) {
        message(sprintf("ERROR: Could not set permissions on %s: %s", keyfile, e$message))
      })
    }
  )
)


#' addKeychainEntry
#' provides an interactive way to add new keys to keychain corresponding to passed host and db, user password to a gieven keychain
#' @param host (\emph{character}) host to insert
#' @param db (\emph{mongo object}) db to insert
#' @param user (\emph{character}) user to insert
#' @param passwd (\emph{mongo object}) passwd to insert
#' @param authSource (\emph{character}) authSource to insert
#' @param authMechanism (\emph{mongo object}) authMechanism to insert
#' @param keyfile (\emph{character}) keyfile to insert
#' @param interactive (\emph{mongo object}) interactive mode
#' @return True (success) or False (failure) 
#' @export addKeychainEntry

    
addKeychainEntry <- function(
  host = NULL,
  db = NULL,
  user = NULL,
  passwd = NULL,
  authSource = NULL,
  authMechanism = NULL,
  keyfile = DEFAULT_KEYCHAIN,
  interactive = FALSE
) {
  # If interactive, prompt for any missing parameters
  if (interactive) {
    if (is.null(host)) host <- readline("Host: ")
    if (is.null(db)) db <- readline("Database Name: ")
    if (is.null(user)) user <- readline(sprintf("%s/%s Username: ", host, db))
    if (is.null(passwd)) {
      if (!requireNamespace("getPass", quietly = TRUE)) {
        stop("Please install the 'getPass' package to use password prompts interactively.")
      }
      passwd <- getPass::getPass(sprintf("%s/%s Password: ", host, db))
    }
    if (is.null(authSource)) {
      authSource <- readline(sprintf("%s/%s Authorization Source (Leave Blank to Skip): ", host, db))
      if (authSource == "") authSource <- NULL
    }
    if (is.null(authMechanism)) {
      authMechanism <- readline(sprintf("%s/%s Authorization Mechanism (Leave Blank to Skip): ", host, db))
      if (authMechanism == "") authMechanism <- NULL
    }
    if (is.null(keyfile)) keyfile <- readline("Path/File to Store Keychain: ")
  } else {
    if (is.null(host)) {
      cat("No host specified in non-interactive mode, cannot add to keychain.\n")
      return(FALSE)
    }
    if (is.null(db)) {
      cat("No db specified in non-interactive mode, cannot add to keychain.\n")
      return(FALSE)
    }
    if (is.null(user)) {
      cat("No user specified in non-interactive mode, cannot add to keychain.\n")
      return(FALSE)
    }
    if (is.null(passwd)) {
      cat("No passwd specified in non-interactive mode, cannot add to keychain.\n")
      return(FALSE)
    }
    if (is.null(keyfile)) {
      cat("No keyfile specified in non-interactive mode, cannot add to keychain.\n")
      return(FALSE)
    }
  }

  # Load the keychain
  kc <- Keychain$new() #keyfile

  # Check if key exists
  if (kc$findKey(host, db)!=-1) {
    if (interactive) {
      cat(sprintf("%s already contains login key for %s/%s\n", keyfile, host, db))
      overwrite <- NULL
      while (is.null(overwrite)) {
        overwrite <- toupper(readline("Overwrite? Y/N: "))
        if (startsWith(overwrite, "Y")) {
          cat("Overwriting with new login info.\n")
        } else if (startsWith(overwrite, "N")) {
          cat("No overwrite, quitting without setting new login key.\n")
          return(FALSE)
        } else {
          overwrite <- NULL
        }
      }
    } else {
      cat(sprintf("Overwriting existing login key for %s/%s in %s\n", host, db, keyfile))
    }
  }

  # Add the new key and save
  kc$addKey(host, db, user, passwd, authSource, authMechanism)
  kc$save(keyfile)
  cat(sprintf("Wrote updated keychain to private file: %s\n", keyfile))
  return(TRUE)
}


#' getCredentials
#' This function replaces the Keychain class in lib/db/passwords.py
#' Rather than load entire keychains into memory, it just quickly scans ~/.mongopw and ~/.mngdb/passwd for relevant user,passwd combo
#' @param host (\emph{character})
#' @param db (\emph{mongo object})
#' @returns NULL or a list with members user,passwd (and host,db if specified)
#' @import jsonlite
#' @export getCredentials

getCredentials <- function(db_host=getOption("DB_HOST"), db_name=NULL) {
  # TO DO: if only one of db_host,db_name specified, generate a warning
  if(!is.null(db_host) & !is.null(db_name)) {
    # If both db_host and db_name were specified, then try searching ~/.mongopw
    # TO DO: Make the name of ~/.mongopw configurable
    #keyfile <- "~/.mongopw"
    mongopw_file <- Sys.getenv("MONGOPW_FILE")
    if (mongopw_file == "")
      keyfile <- "~/.mongopw"
    else
      keyfile <- mongopw_file
    if(file.exists(keyfile)) {
      # Use jsonlite package to parse the keychain json file
      # Read in the keyfile - if formatted properly this should become a dataframe with columns host, db, user, passwd
      keydata <- read_json(keyfile, simplifyVector = T)
      # Check for a row matching db_host and db_name (if multiple rows, take first one but throw a warning)
      matchRows <- which((keydata[,"host"] == db_host) & (keydata[,"db"] == db_name))
      #let's also try adding/removing the potentially appended port number
      if (length(matchRows) == 0){
        #if (grepl("27017", db_host) == TRUE)
        #  db_host=substr(db_host,0,nchar(db_host)-6)  
        #else
        #  db_host = paste0(db_host,":27017")
        db_host <- swapPort(db_host)
        matchRows <- which((keydata[,"host"] == db_host) & (keydata[,"db"] == db_name))
        if (length(matchRows) == 0){
          matchRows <- which((keydata[,"host"] == db_host) & (keydata[,"db"] == "*"))
          if (length(matchRows) == 0){
            db_host <- swapPort(db_host)
            matchRows <- which((keydata[,"host"] == db_host) & (keydata[,"db"] == "*"))
          }
        }
      }
      if(length(matchRows) > 1) {
        warning("Found multiple entries in ", keyfile, " with host=", db_host, " and db=", db_name, ", using first such entry.")
      }
      if(length(matchRows) > 0) {
        matchRows <- matchRows[1]
        # Drop missing values so they don't get passed as NA
        myCred <- as.list(keydata[matchRows,])
        return(myCred[!is.na(myCred)])
      }
      print("STILL NO MATCH")
    }
  }
  # At this point, either no db_host/db_name specified, keyfile didn't exist, or no matching entries
  # Check for /.mngdb/passwd file which should just have "user:passwd"
  # TO DO: Make this file name configurable as well
  pwfile <- "~/.mngdb/passwd"
  if(file.exists(pwfile)) {
    pwdata <- readLines(pwfile)
    pwdata <- sub("^[ \t]","",pwdata)
    pwdata <- sub("[ \t]$","",pwdata)
    pwdata <- strsplit(pwdata, split=":", fixed=T)[[1]]
    return(list(user=pwdata[1], passwd=pwdata[2])) #might need to add the authentication info here
  }
  # At this point, defer to global options httrDefaultUser and httrDefaultPasswd if defined
  if(!is.null(getOption("httrDefaultUser")) & !is.null(getOption("httrDefaultPasswd"))) {
    return(list(user=getOption("httrDefaultUser"), passwd=getOption("httrDefaultPasswd")))
  }
  # At this point, no user,passwd was found - throw warning and return NULL
  warning("Could not find any credential files.")
  return(NULL)
}


#' swapPort
#' This function adds or remove the port to/from the host name
#' @param db_host (\emph{character})
#' @return the given parameter augmented with the port if it was not there or without the port if it was there
#' @export swapPort

swapPort <- function(db_host){

  if (grepl("27017", db_host) == TRUE)
    db_host=substr(db_host,0,nchar(db_host)-6)  
  else
    db_host = paste0(db_host,":27017")
  return(db_host)
}


