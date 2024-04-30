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
}