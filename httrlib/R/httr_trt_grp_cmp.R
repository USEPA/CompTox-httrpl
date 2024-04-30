#' treatGroupFromSamples
#' Construct a document for httr_trt_grp_cmp collection.   
#' Given a list of treatment and control samples, construct an appropriate document and insert into httr_trt_grp_cmp
#'     
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (string) = name of database with httr_well and httr_trt_grp_cmp collections
#' @param  trt_wells (vector) = Sample IDs of all treated wells in the comparison group
#' @param  ctrl_wells (vector) = Sample IDs of all control wells in the the comparison group
#' @param  trt_prop_fields (vector) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate chem_id, conc, conc_unit, dose_level, and stype fields.
#' @param  both_prop_fields (vector) = Fields in httr_well collection that should match up for all trt_wells AND ctrl_wells, and the singular value should be propagated to to field of same name in httr_trt_grp_cmp collection. Default is to propagate media, timeh, pg_id, and block_id.
#' @param  ctrl_desc_field (str) = Field to propagate from ctrl_wells to ctrl field in httr_trt_grp_cmp document, defaults to "stype"
#' @param  grp_id_opts (vector) = Additional modifiers to determine trt_grp_id, "bl" = append block ID, "pg" = append plate group ID, "pl" = append plate ID, "vs" = use trt_name for trt_wells and ctrl_wells, applied in the order specified, defaults to simple use only trt_name from trt_wells
#' @param  well (str) = Name of collection with individual well treatment data, default is "httr_well"
#' @param  trt_grp (str) = Name of collection with treatment group data, default is "httr_trt_grp_cmp"
#' @param  rerun (bool) = If document with same trt_grp_id exists already, should it be replaced? Default is False
#' @param  db_insert (bool) = Whether to insert the new document into database at all, Default is True
#' @param  check_collection_present (bool) to check that httr_well is present, exit if not - defaults to FALSE cause check likely happened upstream already
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @return      (list) = Document that was inserted into trt_grp collection
#' @export treatGroupFromSamples
  
treatGroupFromSamples <- function(db_host = getOption("DB_HOST"),
                                  db_name = getOption("DB_NAME"),
                                  trt_wells, 
                                  ctrl_wells, 
                                  trt_prop_fields = c("chem_id", "conc", "conc_unit", "dose_level", "stype"),
                                  both_prop_fields = c("media", "timeh", "pg_id", "block_id"),
                                  ctrl_desc_field ="stype",
                                  grp_id_opts =  c(),
                                  well = "httr_well", 
                                  trt_grp = "httr_trt_grp_cmp",
                                  rerun = FALSE,
                                  db_insert = TRUE,
                                  check_collection_present = FALSE,
                                  output_dir="not_set", 
                                  ...)
{
  
  
  
  # Get the collection objects and build standard queries
  #check_collection_present is FALSE if treatGroupFromSamples caller has checked already

  httr_well  <- openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=check_collection_present, output_dir=output_dir)
  httr_trt_grp_cmp <- openMongo(db_host=db_host, db_name=db_name, collection= trt_grp, output_dir=output_dir)

  #if there is a cell_type field in the httr wells collection, add the cell_type field to output
  if (length(httr_well$distinct("cell_type")) != 0){
    both_prop_fields = c(both_prop_fields, "cell_type")
  }

  get_trt_wells <- mongoQuery(sample_id = trt_wells)
  get_ctrl_wells <- mongoQuery(sample_id = ctrl_wells)
  get_all_wells <- mongoQuery(sample_id = c(trt_wells,ctrl_wells))
  
  # All trt_wells should have matching trt_name
  trt_grp_id = httr_well$distinct(key= "trt_name", query=get_trt_wells)
  
  if (length(trt_grp_id) != 1){
    warning(sprintf("In treatGroupFromSamples, trt_wells contained list of %i sample_id values. \n Expected all to have matching trt_name but found %i distinct values in DB.", length(trt_wells), length(trt_grp_id)))
  }
  trt_grp_id=trt_grp_id[1]
  # Add optional suffixes to trt_grp_id - base on trt wells until after -vs-
  get_name_wells = get_trt_wells
  for (id_opt in grp_id_opts){
    if (id_opt %in% c("bl","pg","pl")){
      # block, pg, and plate suffixes:
      if (id_opt == "bl"){
        opt_field = "block_id"
      }
      else if (id_opt == "pg"){
        opt_field = "pg_id"
      }
      else if (id_opt == "pl"){
        opt_field = "plate_id"
      }
      #opt_val = httr_well.distinct(opt_field, query=get_name_wells)
      opt_val = httr_well$distinct(key = opt_field, query=get_name_wells)
      if (length(opt_val) != 1){
        #***not sure how to port this
        warning(sprintf("In treatGroupFromSamples, name_wells contained list of %i sample_id values. \n Expected all to have matching %s but found %i distinct values in DB. \n",length(get_name_wells['sample_id']['$in']), opt_field, length(opt_val)))
      }
      trt_grp_id =paste0(trt_grp_id, "_" , id_opt , opt_val[1])
    }
    if (id_opt == "vs"){
      # Add optional -vs-(ctrl.trt_name) suffix
      #ctrl_name = httr_well.distinct("trt_name", query=get_ctrl_wells)
      ctrl_name = httr_well$distinct(key= "trt_name", query=get_ctrl_wells)
      if (length(ctrl_name) != 1){
        warning(paste0(sprintf("In treatGroupFromSamples, ctrl_wells contained list of %i sample_id values. \n Expected all to have matching trt_name but found %i distinct values in DB. \n", length(ctrl_wells), length(ctrl_name)),trt_grp_id,"-vs-", ctrl_name[1]))
      }
      # switch name_wells to both trt_wells and ctrl_wells for any subsequent suffixes
      #trt_grp_id+="-vs-"+ctrl_name[0]
      trt_grp_id= paste0(trt_grp_id, "-vs-", ctrl_name[1])
      get_name_wells = get_all_wells
    }
  }
  # Check if this is in DB already (db_insert=True only)
  #    if db_insert and (httr_trt_grp_cmp.count_documents(dict(trt_grp_id=trt_grp_id)) > 0):
  if (db_insert == TRUE && httr_trt_grp_cmp$count(query = mongoQuery(trt_grp_id=trt_grp_id)) > 0){
    if (rerun == TRUE){
      # If rerun=True, just delete the existing entry
      #***
      #deleteByID(DB=DB, collection=trt_grp, ids=trt_grp_id, id_field="trt_grp_id")
    }
    else {
      # If rerun=False, just print a warning and return
      warning(sprintf("%s already contains document for trt_grp_id=%s, rerun=False - nothing new will be written. \n",trt_grp, trt_grp_id))
      
      #*** return commented 9/2/2020 for diagnostics
      #return()
    }
  }
  # Construct the document for httr_trt_grp_cmp
  trt_grp_doc = list(trt_grp_id=trt_grp_id)
  # Populate the dicts of trt_wells and ctrl_wells
  #***
  trt_grp_doc$ctrl_wells = findByID(DB=httr_well,  ids=ctrl_wells, fields= mongoQuery(list("_id"=0, "sample_id"=1, "plate_id"=1, "well_id"=1)),dump=TRUE)
  trt_grp_doc$trt_wells = findByID(DB=httr_well,  ids=trt_wells, fields= mongoQuery(list("_id"=0, "sample_id"=1, "plate_id"=1, "well_id"=1)),dump=TRUE)
  # Get list of all plates
  #trt_grp_doc['plates'] = httr_well.distinct("plate_id", query=get_all_wells)
  #***
  trt_grp_doc$plates <- unlist(httr_well$distinct("plate_id", query=get_all_wells))
  
  # Set ctrl = ctrl_desc_field from all ctrl_wells - expect these to be the same for all wells:
  #ctrl_type = httr_well.distinct(ctrl_desc_field, query=get_ctrl_wells)
  ctrl_type <- httr_well$distinct(ctrl_desc_field, query=get_ctrl_wells)
  if (length(ctrl_type) == 0){
    warning(sprintf("All %i ctrl_wells in %s for %s were missing %s field \n", length(ctrl_wells), well, trt_grp_id, ctrl_desc_field))
  }
  else{
    if (length(ctrl_type) > 1){
      warning(sprintf("%i ctrl_wells in %s for %s had %i %s values (expected 1) - ignoring: %s \n", length(ctrl_wells), well, trt_grp_id, length(ctrl_type), ctrl_desc_field, ctrl_type[-c(1)]))
    }
    trt_grp_doc$ctrl = ctrl_type[1]
    # Get chem_id, conc, conc_unit, dose_level, stype for all trt_wells - this should be a single value in each case
    
  }
  for (field in trt_prop_fields){
    #       field_val = httr_well.distinct(field, query=get_trt_wells)
    field_val = httr_well$distinct(key = field, query=get_trt_wells)
    if (length(field_val) == 0){
      message(sprintf("Treatment wells for %s do not contain %s field \n", trt_grp_id, field))
      
      #code below inserts a place holder for fields that are missing
      #trt_grp_doc[field] = "unspecified"
    }
    else{
      if (length(field_val) > 1){
        warning(sprintf("Sample IDs for %i trt_wells in %s for %s had %i different %s (expected 1) - ignoring: %s \n" ,length(trt_wells), well, trt_grp_id, length(field_val), field, field_val[-c(1)]))
      }
      trt_grp_doc[field] = field_val[1]
      
    }
  }
  # Get media, timeh, pg_id, block_id for all wells - this should be a single value in each case
  for (field in both_prop_fields){
    #        field_val = httr_well.distinct(field, query=get_all_wells)
    field_val = httr_well$distinct(key = field, query=get_all_wells)
    if (length(field_val) == 0){
      warning(sprintf("All wells for %s do not contain %s field \n", trt_grp_id, field))
      
      #code below inserts a place holder for fields that are missing
      #trt_grp_doc[field] = "unspecified"
    }
    else{
      if (length(field_val) > 1){
        warning(sprintf("Sample IDs for %i wells in %s for %s had %i different %s (expected 1) - ignoring: %s \n"  ,length(trt_wells+ctrl_wells), well, trt_grp_id, length(field_val), field, field_val[-c(1)]))
      }
      trt_grp_doc[field] = field_val[1]
    }
  }
  # Insert into the DB
  if (db_insert == TRUE){
    #***
    #mongoDB in R doesn't return the ID so i'm not sure how to replicate this behavior - do we need to query the database for the entry we just inserted to get the ID to return here?      
    #        trt_grp_doc['_id'] = httr_trt_grp_cmp.insert_one(trt_grp_doc).inserted_id

    tempDB = openMongo(db_host=db_host, db_name=db_name, collection= trt_grp, output_dir=output_dir)
    tempDB$insert(trt_grp_doc, auto_unbox=TRUE)
    
  }
  # Return the final document:
  return(trt_grp_doc)
}

#' chemTreatGroup
#' Function that generates the trt_grp_cmp for a specific test chemical treatment vs DMSO
#' Construct a document for httr_trt_grp_cmp collection corresponding to a specific chemical and dose level treatment.
#' 
#' Given a specific chemical and dose, plus optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. Note, if filtering reduces either trt or ctrl group to < min_reps samples this treatment group will be skipped.
#' 
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (string) = name of database with httr_well and httr_trt_grp_cmp collections collections
#' @param  trt_chem (str) = Match to chem_id in httr_well 
#' @param  trt_dose (int) = Match to dose_level in httr_well (set to None to exclude)
#' @param  trt_type (str) = Match to stype in httr_well, defaults to "test sample"
#' @param  ctrl_type (str) = Match to stype in httr_well
#' @param  ctrl_chem (str) = Match to chem_id in httr_well
#' @param  pg_id (str) = Filter both trt and ctrl wells, match to pg_id in httr_well, defaults to None
#' @param  media (str) = Filter both trt and ctrl wells, match to media in httr_well, defaults to None
#' @param  timeh (str) = Filter both trt and ctrl wells, match to timeh in httr_well, defaults to None
#' @param  trt_src (str) = Filter trt wells, match to rna_src field
#' @param  ctrl_src (str) = Filter ctrl wells, match to rna_src field
#' @param  qc_flags (vector) = Filter both trt and ctrl wells, match to qc_flag field, defaults to OK only
#' @param  min_reps (int) = Minimum number of replicates in trt and ctrl groups, respectively - if either is less, will generate an output message and return empty dict
#' @param  well (str) = Name of collection with individual well treatment data, default is "httr_well"
#' @param  check_collection_present (bool) to check that httr_well is present, exit if not - defaults to FALSE cause check likely happened upstream already
#' @param  ... = All additional args passed to treatGroupFromSamples
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @return (list) = Document that was inserted into trt_grp collection
#' Build the query for treatment and ctrl wells
#' @export chemTreatGroup
  

chemTreatGroup <- function(db_host = getOption("DB_HOST"),
                           db_name = getOption("DB_NAME"),
                           trt_chem, 
                           trt_dose, 
                           trt_type = "test sample", 
                           ctrl_type = "vehicle control", 
                           ctrl_chem = "DMSO",
                           cell_type = NULL,
                           pg_id = NULL, 
                           media = NULL, 
                           timeh = NULL,
                           trt_src = NULL,
                           ctrl_src = NULL,
                           qc_flags = c("OK"), 
                           min_reps = 2,
                           well = "httr_well",
                           check_collection_present = FALSE,
                           output_dir = "not_set",
                           ...){


  #require(tidyr)
  #check_collection_present is FALSE if chemTreatGroup caller has checked already

  httr_well = openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=check_collection_present, output_dir=output_dir)
  #ctrl_chem <- httr_well$distinct("chem_id", query = mongoQuery(stype = "vehicle control"))
  
  trt_well_query = list()
  ctrl_well_query = list()

  
  if (!is.null(trt_chem)){
    trt_well_query$chem_id = trt_chem}
  if (!is.null(trt_dose)){
    trt_well_query$dose_level = trt_dose}
  if (!is.null(trt_type)){
    trt_well_query$stype = trt_type}
  if (!is.null(ctrl_type)){
    ctrl_well_query$stype = ctrl_type}
  if (!is.null(ctrl_chem)){
    ctrl_well_query$chem_id = ctrl_chem}
  
  if (!is.null(cell_type)){
    trt_well_query$cell_type = cell_type
    ctrl_well_query$cell_type = cell_type}
  
  if (!is.null(pg_id)){
    trt_well_query$pg_id = pg_id
    ctrl_well_query$pg_id = pg_id}
  if (!is.null(media)){
    trt_well_query$media = media
    ctrl_well_query$media = media}
  if (!is.null(timeh)){
    trt_well_query$timeh = timeh
    ctrl_well_query$timeh = timeh}
  if (!is.null(trt_src)){
    trt_well_query$rna_src = trt_src}
  if (!is.null(ctrl_src)){
    ctrl_well_query$rna_src = ctrl_src} 
  if (!is.null(qc_flags)){
    #***
    #trt_well_query["qc_flag"] = { "$in" : qc_flags }
    trt_well_query$qc_flag = qc_flags
    #ctrl_well_query["qc_flag"] = { "$in" : qc_flags }
    ctrl_well_query$qc_flag = qc_flags
  }
  # Get sample IDs for both treatments and ctrl wells
  #***
  trt_wells = httr_well$distinct(key = "sample_id", query = mongoQuery(trt_well_query))
  ctrl_wells = httr_well$distinct("sample_id", query = mongoQuery(ctrl_well_query))

  
  # Make sure both trt_wells and ctrl_wells > min_reps long
  if ((length(trt_wells) < min_reps) | (length(ctrl_wells) < min_reps)){
    message(sprintf("SKIPPING COMPARISON GROUP: Query returned %i trt wells and %i ctrl wells with\n trt_well_query = %s\n ctr_well_query = %s", length(trt_wells), length(ctrl_wells), unite(data = data.frame(trt_well_query), col = "new")$new, unite(data = data.frame(ctrl_well_query), col = "new")$new))
    return()
  }
  # Otherwise, pass to treatGroupFromSamples
  return (treatGroupFromSamples(db_host=db_host,
                                db_name=db_name, 
                                trt_wells=trt_wells, 
                                ctrl_wells=ctrl_wells, 
                                well=well, 
                                check_collection_present=FALSE,
                                output_dir = output_dir,
                                ...))
}

#' allTestGroups
# Function to loop over all plate groups and call chemTreatGroup for each media, timeh, chem_id, dose_level
#'
#' Construct documents for httr_trt_grp_cmp collection corresponding to each test chemical and dose level treatment.
#' 
#' Loop over all plate groups, media, timeh, chem_id, and dose_level and create a httr_trt_grp_cmp document for each one.
#' 
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (string) = name of database with httr_well and httr_trt_grp_cmp collections collections collections
#' @param well (str) = Name of collection with individual well treatment data, default is "httr_well"
#' @param exp_doses (int) = Expected number of doses for each test chemical - this will suppress some debug messages and only warn if the number of doses does not match
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' ... = All additional args passed to chemTreatGroup
#'
#' @return (list) = List of documents that were inserted into trt_grp collection
#' Store all documents in a list, also track the number that were skipped due to insufficient replicates with passing QC flags:
#' @export allTestGroups
  
# TO DO: Parameters to limit to a subset of plate groups, etc.
allTestGroups <- function (db_host = getOption("DB_HOST"),
                           db_name = getOption("DB_NAME"), 
                           well = "httr_well",
                           exp_doses,
                           output_dir = "not_set",
                           ...){
  
  
  require(rlist)
  trt_grp_docs = list()
  skip_cnt = 0
  
  # Get list of pg_id values in httr_well
  #httr_well = DB[well]
  # we need to check for httr_well presence

  httr_well = openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=TRUE, output_dir=output_dir)  
  
  all_cell_type = httr_well$distinct("cell_type")
  if (length(all_cell_type) == 0){
    all_cell_type = c("unspecified")
  }
  for (cell_type in all_cell_type){
    if (cell_type == "unspecified"){
      cell_type = NULL
    }
    cell_pg = httr_well$distinct(key = "pg_id", query = mongoQuery(stype="test sample",
                                                                   cell_type = cell_type))
    #if length of cell_pg is 0, break out of current loop as the current cell type has no test samples
    if (length(cell_pg) == 0){
      next
    }
    
    
    for (pg_id in cell_pg){

      # Get list of all media on pg_id
      
      #pg_media = httr_well.distinct("media", dict(stype="test sample", pg_id=pg_id))
      
      #initialize an empty list to catch growing mongo queries for each iteration
      #query_list = list(stype="test sample", pg_id=pg_id)
      
      cpg_media = httr_well$distinct(key = "media", query = mongoQuery(stype="test sample", 
                                                                       cell_type = cell_type,
                                                                       pg_id=pg_id))
                                                                       
      if(length(cpg_media) == 0){
        cpg_media = c("unspecified")
      }

      for (media in cpg_media){
        
        #if there was no media field in the httr_well collection, remove media from the query
        if (media == "unspecified"){
          media <- NULL
        }
        # Get list of all timeh for pg_id, media
        cpgm_timeh = httr_well$distinct(key = "timeh", query = mongoQuery(stype="test sample", 
                                                                          cell_type = cell_type,
                                                                         pg_id=pg_id, 
                                                                         media = media))
                                                                         
        #check if this returns an empty list, as the media field is not always present
        if(length(cpgm_timeh) == 0){
          cpgm_timeh = c("unspecified")
        }
        
        for (timeh in cpgm_timeh){
          if (timeh == "unspecified"){
            timeh <- NULL
          }
          cpgmt_chems = httr_well$distinct(key ="chem_id", query = mongoQuery(stype="test sample", 
                                                                              cell_type = cell_type,
                                                                             pg_id=pg_id, 
                                                                             media = media,
                                                                             timeh = timeh)) 
          if(length(cpgmt_chems) == 0){
            cpgmt_chems = c("unspecified")
          }
          message(paste0("Generating treatment groups for ",length(cpgmt_chems)," chemicals on plate group ",pg_id, " media = ",media," timeh = ",timeh))
          if (!is.null(exp_doses)){
            message(sprintf(" Expecting each chemical to have %i dose levels.",  exp_doses))
          }
          for (chem_id in cpgmt_chems){
            if (chem_id == "unspecified"){
              chem_id <- NULL
            }
            doses = httr_well$distinct(key = "dose_level", query = mongoQuery(stype="test sample", 
                                                                              cell_type = cell_type,
                                                                              pg_id=pg_id, 
                                                                              media = media,
                                                                              timeh = timeh,
                                                                              chem_id = chem_id))
                                                                              
            if(length(doses) == 0){
              doses = c("unspecified")
            }
            # Report number of doses or warn if not matching expectations
            if (is.null(exp_doses))
              message(sprintf(" chem_id = %s => %i dose_levels", chem_id, length(doses)))
            else if(length(doses) != exp_doses){
              warning(sprintf("chem_id = %s had %i dose_levels, expected %i", chem_id, length(doses), exp_doses))
            }
            #for (dose_level in sort(doses)){
            for (dose_level in doses){
              #query_list$dose_level <- dose_level
              if (dose_level == "unspecified"){
                #query_list$dose_level <- NULL
                dose_level <- NULL
              }
              
              db_doc = chemTreatGroup(db_host=db_host,
                                      db_name=db_name, 
                                      trt_chem = chem_id, 
                                      trt_dose = dose_level, 
                                      pg_id = pg_id, 
                                      media=media, 
                                      timeh=timeh, 
                                      well=well,
                                      cell_type = cell_type,
                                      output_dir = output_dir,
                                      ...)
              if (length(db_doc) == 0){
                skip_cnt = skip_cnt + 1
              }
              else{
                trt_grp_docs <- list.append(trt_grp_docs, db_doc)
              }
            }
          }
        }
      }
    }
    
  }
  # Report how many were skipped
  if (skip_cnt > 0){
    message(sprintf("%i treatment comparison groups were skipped due to insufficient replicates.", skip_cnt))
  }
  message(sprintf("A total of %i treatment comparison groups were generated.", length(trt_grp_docs)))
  return (trt_grp_docs)
  
}

#' refChemGroup
#' Construct a document for httr_trt_grp_cmp collection corresponding to a specific reference chemical treatment.
#' 
#' Given a specific reference chemical and plate group (or media/timeh that specify plate group), plus any optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. This function is just a convenenience wrapper to chemTreatGroup with modified defaults.
#' 
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (string) = name of database with httr_well and httr_trt_grp_cmp collections collections
#' @param  trt_chem (str) = Match to chem_id in httr_well 
#' @param  pg_id (str) = Filter both trt and ctrl wells, match to pg_id in httr_well, defaults to None
#' @param  media (str) = Filter both trt and ctrl wells, match to media in httr_well, defaults to None
#' @param  timeh (str) = Filter both trt and ctrl wells, match to timeh in httr_well, defaults to None
#' @param  trt_dose (int) = Match to dose_level in httr_well - Excluded by default
#' @param  trt_type (str) = Match to stype in httr_well, defaults to "reference chemical"
#' @param  trt_prop_fields (vector) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate chem_id, conc, conc_unit, and stype fields (exclude dose_level here).
#' @param  check_collection_present (bool) to check that httr_well is present, exit if not - defaults to FALSE cause check likely happened upstream already
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' ... = All additional args passed to treatGroupFromSamples
#' 
#' @return (list) = Document that was inserted into trt_grp collection
#' Warn if neither pg_id nor media+timeh were specified:
#' @export refChemGroup


refChemGroup <- function(db_host = getOption("DB_HOST"),
                         db_name = getOption("DB_NAME"), 
                         trt_chem, 
                         pg_id = NULL,
                         media = NULL, 
                         timeh = NULL, 
                         trt_dose = NULL,
                         trt_type = "reference chemical",
                         trt_prop_fields = c("chem_id", "conc", "conc_unit", "stype"),
                         check_collection_present = FALSE,
                         output_dir = "not_set",
                         ...){

  
  
  
  if (is.null(pg_id) && is.null(media) || (is.null(timeh))){
    warning("Neither pg_id or media,timeh were specified in call to refChemGroup, this will probably not be able to generate a valid trt_grp_cmp document.")
  }
  # Pass to chemTreatGroup:
  return (chemTreatGroup(db_host=db_host, db_name=db_name, trt_chem=trt_chem, trt_dose=trt_dose, trt_type=trt_type, pg_id=pg_id, media=media, timeh=timeh, trt_prop_fields=trt_prop_fields, check_collection_present=check_collection_present, output_dir = output_dir, ...))
}


#' allRefGroups
#' Function to loop over all plate groups, media, and times and then call refChemGroup
#' Construct documents for httr_trt_grp_cmp collection corresponding to each reference chemical (single conc).
#' 
#' Loop over all plate groups, media, timeh, chem_id (reference chemicals only) and create a httr_trt_grp_cmp document for each one.
#' 
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (string) = name of database with httr_well and httr_trt_grp_cmp collections collections collections
#' @param trt_type (str) = Filter stype to get list of plate groups, media, timeh, and chem_id
#' @param well (str) = Name of collection with individual well treatment data, default is "httr_well"
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' ... = All additional args passed to chemTreatGroup
#' 
#' @return (list) = List of documents that were inserted into trt_grp collection
#' Store all documents in a list, also track the number that were skipped due to insufficient replicates with passing QC flags:
#' @export allRefGroups
  
  
allRefGroups <- function(db_host = getOption("DB_HOST"),
                         db_name = getOption("DB_NAME"),
                         trt_type = "reference chemical",
                         well = "httr_well",
                         output_dir = "not_set",
                         ...){
  
  require(rlist)
  trt_grp_docs = list()
  skip_cnt = 0
  # Get list of pg_id values in httr_well
  #need to check httr_Well present (not done upstream)
  httr_well = openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=TRUE, output_dir=output_dir)

  all_pg = httr_well$distinct("pg_id")
  for (pg_id in all_pg){
    # Get list of all media on pg_id
    pg_media = httr_well$distinct(key = "media", query = mongoQuery(stype=trt_type, pg_id=pg_id))
    if (length(pg_media) == 0){
      pg_media = c("unspecified")
    }
    for (media in pg_media){
      if (media == "unspecified"){
        media = NULL
      }
      # Get list of all timeh for pg_id, media
      pgm_timeh = httr_well$distinct(key = "timeh", query = mongoQuery(stype=trt_type, pg_id=pg_id, media=media))
      if (length(pgm_timeh) == 0){
        pgm_timeh = c("unspecified")
      }
      for (timeh in pgm_timeh){
        if (timeh == "unspecified"){
          timeh = NULL
        }
        pgmt_chems = httr_well$distinct(key = "chem_id", query = mongoQuery(stype=trt_type, pg_id=pg_id, media=media, timeh=timeh))
        message(sprintf("Generating treatment groups for %i reference chemicals on plate group %s, media = %s, timeh = %i", length(pgmt_chems), pg_id, media, timeh))
        for (chem_id in pgmt_chems){
          db_doc = refChemGroup(db_host=db_host, db_name=db_name, trt_chem=chem_id, trt_type=trt_type, pg_id=pg_id, media=media, timeh=timeh, well=well, output_dir = output_dir, ...)
          if (length(db_doc) == 0){
            skip_cnt = skip_cnt + 1
          }
          else {
            trt_grp_docs <- list.append(trt_grp_docs, db_doc)
          }
        }
      }
    }
  }
  # Report how many were skipped
  if (skip_cnt > 0){
    message(sprintf("%i treatment comparison groups were skipped due to insufficient replicates." , skip_cnt))
  }
  message(sprintf("A total of %i treatment comparison groups were generated.", length(trt_grp_docs)))
  return (trt_grp_docs)
}

#' bulkLysateGroup
#' Construct a document for httr_trt_grp_cmp collection corresponding to a specific bulk lysate chemical treatment.
#' 
#' Given a specific plate group, plus any optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. This function is just a convenenience wrapper to chemTreatGroup with modified defaults.
#' 
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (string) = name of database with httr_well and httr_trt_grp_cmp collections collections collections
#' @param pg_id (str) = Filter both trt and ctrl wells, match to pg_id in httr_well, required for bulk lysate groups
#' @param trt_chem (str) = Match to chem_id in httr_well, defaults to "TSA"
#' @param trt_dose (int) = Match to dose_level in httr_well - Excluded by default
#' @param trt_type (str) = Match to stype in httr_well, defaults to "QC sample"
#' @param ctrl_type (str) = Match to stype in httr_well, defaults to "QC sample"
#' @param ctrl_chem (str) = Match to chem_id in httr_well, defaults to "DMSO"
#' @param rna_src (str) = Match to rna_src in httr_well for both trt and ctrl wells, defaults to "Bulk Lysate"
#' @param trt_prop_fields (vector) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate chem_id, conc, conc_unit, and stype fields (exclude dose_level here).
#' @param both_prop_fields (vector) = Fields in httr_well collection that should match up for all trt_wells AND ctrl_wells, and the singular value should be propagated to to field of same name in httr_trt_grp_cmp collection. Default is to propagate pg_id and block_id only for bulk lysate comparisons
#' @param ctrl_desc_field (str) = Field to propagate from ctrl_wells to ctrl field in httr_trt_grp_cmp document, defaults to "chem_id" for bulk lysate comparisons
#' @param grp_id_opts (vector) = Options to modify trt_grp_id generation, "pg" is usually sufficient for bulk lysate to create distinct group IDs for each plate group
#' @param  check_collection_present (bool) to check that httr_well is present, exit if not - defaults to FALSE cause check likely happened upstream already
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository

#' ... = All additional args passed to treatGroupFromSamples
#' 
#' @return (list) = Document that was inserted into trt_grp collection
#' Pass to chemTreatGroup:
#' @export bulkLysateGroup
  
bulkLysateGroup <- function(db_host = getOption("DB_HOST"),
                            db_name = getOption("DB_NAME"), 
                            pg_id,
                            trt_chem = "TSA", 
                            trt_dose = NULL,
                            trt_type = "QC sample",
                            ctrl_type = "QC sample", 
                            ctrl_chem = "DMSO",
                            rna_src = "Bulk Lysate",
                            trt_prop_fields = c("chem_id", "conc", "conc_unit", "stype"),
                            both_prop_fields = c("pg_id", "block_id"),
                            ctrl_desc_field = "chem_id",
                            grp_id_opts = c("pg"),
                            check_collection_present = FALSE,
                            output_dir = "not_set",
                            ...){
  

  
  return (chemTreatGroup(db_host=db_host, db_name=db_name, trt_chem=trt_chem, trt_dose=trt_dose, trt_type=trt_type, ctrl_type=ctrl_type, pg_id=pg_id, trt_src=rna_src, ctrl_src=rna_src, trt_prop_fields=trt_prop_fields, both_prop_fields=both_prop_fields, ctrl_desc_field=ctrl_desc_field, grp_id_opts=grp_id_opts, check_collection_present=check_collection_present, output_dir = output_dir, ...))
}

#' allBulkLysateGroups
#' Construct documents for httr_trt_grp_cmp collection corresponding to each bulk lysate QC sample comparison (single conc treatment).
#' 
#' Loop over all plate groups, chem_id (bulk lysate chemicals only) and create a httr_trt_grp_cmp document for each one.
#' 
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (string) = name of database with httr_well and httr_trt_grp_cmp collections collections collections
#' @param trt_type (str) = Filter stype to get list of plate groups, chem_id
#' @param rna_src (str) = Filter rna_src to get list of plate groups, chem_id
#' @param ctrl_chem (str) = Skip this chemical when looping over chem_id
#' @param well (str) = Name of collection with individual well treatment data, default is "httr_well
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' ... = All additional args passed to chemTreatGroup
#' @return  (list) = List of documents that were inserted into trt_grp collection
#' Store all documents in a list, also track the number that were skipped due to insufficient replicates with passing QC flags:
#' @export allBulkLysateGroups
  
  
allBulkLysateGroups <- function(db_host = getOption("DB_HOST"),
                                db_name = getOption("DB_NAME"),
                                trt_type = "QC sample",
                                rna_src = "Bulk Lysate",
                                ctrl_chem = "DMSO",
                                well = "httr_well",
                                output_dir = "not_set",
                                ...){
  
  
  require(rlist)
  trt_grp_docs = list()
  skip_cnt = 0
  # Get list of pg_id values in httr_well
  #  httr_well = DB[well]
  # need to check httr_Well present cause most upstream function
  httr_well = openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=TRUE, output_dir=output_dir)

  # all_pg = httr_well.distinct("pg_id")
  all_pg = httr_well$distinct(key = "pg_id")
  for (pg_id in all_pg){
    # Get list of all chemicals with stype=trt_type and rna_src=rna_src, but leave out ctrl_chem
    pg_chems = httr_well$distinct(key = "chem_id", query = mongoQuery(stype=trt_type, pg_id=pg_id, rna_src=rna_src))
    #pg_chems = list(set(pg_chems) - set([ctrl_chem]))
    pg_chems = setdiff(pg_chems, ctrl_chem)
    message(sprintf("Generating treatment groups for %i bulk lysate chemical treatments on plate group %s" 
                    ,length(pg_chems), pg_id))
    for (chem_id in pg_chems){
      db_doc = bulkLysateGroup(db_host=db_host, db_name=db_name, trt_chem=chem_id, trt_type=trt_type, pg_id=pg_id, rna_src=rna_src, ctrl_chem=ctrl_chem,  well=well, output_dir = output_dir, ...)
      if (length(db_doc) == 0){
        skip_cnt = skip_cnt + 1
      }
      else {
        #trt_grp_docs.append(db_doc)
        trt_grp_docs <- list.append(trt_grp_docs, db_doc)
      }
    }
  }
  
  
  # Report how many were skipped
  if (skip_cnt > 0){
    message(sprintf("%i treatment comparison groups were skipped due to insufficient replicates.", skip_cnt))
  }
  message(sprintf("A total of %i treatment comparison groups were generated.", length(trt_grp_docs)))
  return (trt_grp_docs)
}

#' refRNAGroup 
#' Construct a document for httr_trt_grp_cmp collection corresponding to a specific reference RNA QC sample comparison.
#' 
#' Given a specific plate group, plus any optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. This function is just a convenenience wrapper to chemTreatGroup with modified defaults.
#' 
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (string) = name of database with httr_well and httr_trt_grp_cmp collections collections collections
#' @param pg_id (str) = Filter both trt and ctrl wells, match to pg_id in httr_well, required for ref RNA groups
#' @param trt_type (str) = Match to stype in httr_well, defaults to "QC sample"
#' @param ctrl_type (str) = Match to stype in httr_well, defaults to "QC sample"
#' @param trt_src (str) = Match to rna_src in httr_well, defaults to "HBRR"
#' @param ctrl_src (str) = Match to rna_src in httr_well, defaults to "UHRR"
#' @param trt_prop_fields (list) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate just the stype field (exclude all chemical and dose information).
#' @param both_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells AND ctrl_wells, and the singular value should be propagated to to field of same name in httr_trt_grp_cmp collection. Default is to propagate pg_id and block_id only for ref RNA comparisons
#' @param ctrl_desc_field (str) = Field to propagate from ctrl_wells to ctrl field in httr_trt_grp_cmp document, defaults to "trt_name" for ref RNA comparisons
#' @param grp_id_opts (list) = Options to modify trt_grp_id generation, "vs" will capture the pairwise comparison type, and "pg" is usually sufficient for ref RNA to create distinct group IDs for each plate group
#' @param  check_collection_present (bool) to check that httr_well is present, exit if not - defaults to FALSE cause check likely happened upstream already
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' ... = All additional args passed to treatGroupFromSamples
#' 
#' @return (list) = Document that was inserted into trt_grp collection
#' """
#' Pass to chemTreatGroup:
#' @export refRNAGroup
   
refRNAGroup <- function(db_host = getOption("DB_HOST"),
                        db_name = getOption("DB_NAME"), 
                        pg_id,
                        trt_type = "QC sample",
                        ctrl_type = "QC sample", 
                        trt_src = "HBRR",
                        ctrl_src = "UHRR",
                        trt_prop_fields = c("stype"),
                        both_prop_fields = c("pg_id", "block_id"),
                        ctrl_desc_field = "trt_name",
                        grp_id_opts = c("vs","pg"),
                        check_collection_present = FALSE,
                        output_dir = "not_set",
                        ...)
{
  
  
  return (chemTreatGroup(db_host=db_host, db_name=db_name, trt_chem=NULL, trt_dose=NULL, ctrl_chem=NULL, trt_type=trt_type, ctrl_type=ctrl_type, pg_id=pg_id, trt_src=trt_src, ctrl_src=ctrl_src, trt_prop_fields=trt_prop_fields, both_prop_fields=both_prop_fields, ctrl_desc_field=ctrl_desc_field, grp_id_opts=grp_id_opts, check_collection_present=check_collection_present, output_dir = output_dir, ...))
}

#' allRefRNAGroups
#' Construct documents for httr_trt_grp_cmp collection corresponding to each reference RNA QC sample comparison (single conc treatment).
#' 
#' Loop over all plate groups and create a httr_trt_grp_cmp document for each pair of reference RNAs.
#' 
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (string) = name of database with httr_well and httr_trt_grp_cmp collections collections collections
#' @param well (str) = Name of collection with individual well treatment data, default is "httr_well"
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' ... = All additional args passed to chemTreatGroup
#' 
#' @return (list) = List of documents that were inserted into trt_grp collection
#' Store all documents in a list, also track the number that were skipped due to insufficient replicates with passing QC flags:
#' @export allRefRNAGroups
  
allRefRNAGroups <- function(db_host = getOption("DB_HOST"),
                            db_name = getOption("DB_NAME"),
                            well = "httr_well",
                            output_dir = "not_set",
                            ...){
  
  
  trt_grp_docs = list()
  skip_cnt = 0
  # Get list of pg_id values in httr_well
  httr_well = openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=TRUE, output_dir=output_dir)
  all_pg = httr_well$distinct(key = "pg_id")
  message(sprintf("Generating treatment groups for reference RNA comparisons on %i plate groups." , length(all_pg)))        
  for (pg_id in all_pg){
    db_doc = refRNAGroup(db_host=db_host, db_name=db_name, pg_id=pg_id, well=well, output_dir = output_dir, ...)
    if (length(db_doc) == 0){
      skip_cnt = skip_cnt + 1
    }
    else{
      #trt_grp_docs.append(db_doc)
      trt_grp_docs <- list.append(trt_grp_docs,db_doc)
    }
  }
  # Report how many were skipped
  if (skip_cnt > 0){
    message(sprintf("%i treatment comparison groups were skipped due to insufficient replicates.", skip_cnt))
  }
  message(sprintf("A total of %i treatment comparison groups were generated.", length(trt_grp_docs)))
  return (trt_grp_docs)
}


#' allRefDoseGroups
#' Construct documents for httr_trt_grp_cmp collection corresponding to each reference chemical (single or multi conc).
#' 
#' Loop over all plate groups, media, timeh, chem_id, dose_level (reference chemicals only) and create a httr_trt_grp_cmp document for each one.
#' Alternate version of allRefGroups that also loops over and handles reference chemicals with multiple dose levels
#' Note that to use this version, all reference chemical wells in httr_well_trt MUST have dose_level defined (even single conc reference chemicals)
#' For studies with only single conc reference chemicals that do NOT have a dose_level field, use allRefGroups instead (e.g. for the original MCF-7 screens)
#' Also, unlike allRefGroups, this will automatically append plate group ID as part of the trt_name when creating docs
#' 
#' Parameters:
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (pymongo.MongoClient) = Open connection to database with httr_well and httr_trt_grp_cmp collections
#' @param  trt_type (str) = Filter stype to get list of plate groups, media, timeh, and chem_id
#' @param  trt_prop_fields (list of str) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate chem_id, conc, conc_unit, dose_level, and stype fields
#' @param  grp_id_opts (list) = Options to modify trt_grp_id generation, "pg" is usually sufficient for reference chemicals to create distinct group IDs for each plate group
#' @param  well (str) = Name of collection with individual well treatment data, default is "httr_well"
#' @param  exp_doses(list of str) = Expected number of doses for each reference chemical. This will suppress some debug messages and only warn if the number of doses does not match. For example, c("GEN" = 8, "SIRO" = 8, "TSA" = 1) 
#' @param  log (httrplcore.PipelineLogger) = Log handler
#' @param  **kwargs = All additional args passed to chemTreatGroup
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @return (list of dict) = List of documents that were inserted into trt_grp collection
#' @export allRefDoseGroups


allRefDoseGroups <- function(db_host = getOption("DB_HOST"),
                             db_name = getOption("DB_NAME"),
                             trt_type ="reference chemical",
                             trt_prop_fields = c("chem_id", "conc", "conc_unit", "dose_level", "stype"),
                             grp_id_opts=c("pg"),
                             well,
                             exp_doses = NULL,
                             output_dir = "not_set",
                             ...){

    

# Store all documents in a list, also track the number that were skipped due to insufficient replicates with passing QC flags:

  `%notin%` <- Negate(`%in%`)
  trt_grp_docs = list()
  skip_cnt = 0
  # Get list of pg_id values in httr_well
  httr_well = openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=TRUE, output_dir=output_dir)
  all_pg = httr_well$distinct(key = "pg_id")
  for (pg_id in all_pg){
    # Get list of all media on pg_id
    pg_media = httr_well$distinct(key = "media", query = mongoQuery(stype=trt_type, pg_id=pg_id))
    for (media in pg_media){
      # Get list of all timeh for pg_id, media
      pgm_timeh = httr_well$distinct("timeh", query = mongoQuery(stype=trt_type, pg_id=pg_id, media=media))
        for (timeh in pgm_timeh){
          pgmt_chems = httr_well$distinct("chem_id", query = mongoQuery(stype=trt_type, pg_id=pg_id, media=media, timeh=timeh))
          message(sprintf("Generating treatment groups for %i reference chemicals on plate group %s, media = %s, timeh = %i", 
                          length(pgmt_chems), pg_id, media, timeh))
          for (chem_id in pgmt_chems){
            doses = httr_well$distinct("dose_level", query = mongoQuery(stype=trt_type, pg_id=pg_id, media=media, timeh=timeh, chem_id=chem_id))
            # Report number of doses or warn if not matching expectations
            if (is.null(exp_doses)){
            warning(sprintf(" chem_id = %s => %i dose_levels", chem_id, length(doses)))
            }
            else if(chem_id %notin% names(exp_doses)){
              warning(sprintf(" chem_id = %s was not expected, found %i dose_levels",chem_id, length(doses)))
            }
            else if(length(doses) != exp_doses[chem_id]){
          warning(sprintf("chem_id = %s had %i dose_levels, expected %i", chem_id, length(doses), exp_doses))
          }
            for (dose_level in doses){
            db_doc = refChemGroup(db_host=db_host,
                                  db_name=db_name, 
                                  trt_chem=chem_id, 
                                  trt_type=trt_type, 
                                  pg_id=pg_id, 
                                  media=media, 
                                  timeh=timeh, 
                                  trt_dose=dose_level,
                                  trt_prop_fields=trt_prop_fields, 
                                  grp_id_opts=grp_id_opts, 
                                  well=well, 
                                  output_dir = output_dir,
                                  ...)

              if (length(db_doc) == 0){
               skip_cnt = skip_cnt + 1
              }
              else{
               trt_grp_docs <- list.append(trt_grp_docs, db_doc)
              }
            }
          }
        }
    }
  }
  # Report how many were skipped
  if (skip_cnt > 0){
    message(sprintf("%i treatment comparison groups were skipped due to insufficient replicates.", skip_cnt))
  }
  message(sprintf("A total of %i treatment comparison groups were generated.", length(trt_grp_docs)))
  return(trt_grp_docs)
}


#' treatPairGroup
#'   Construct a document for httr_trt_grp_cmp collection corresponding to a specific pair of treatment names.
#'    
#'    Given a specific pair of trt_name values, plus optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. This is initially designed for the pairs of BioSpyder QC reference samples now used in newer screens, but should work for other situations as well. Note, if filtering reduces either trt or ctrl group to < min_reps samples this treatment group will be skipped.
    
#' @param     db_name name of database with httr_well and httr_trt_grp_cmp collections
#' @param     trt_name (string) = Match to trt_name in httr_well
#' @param     ctrl_name (string) = Match to trt_name in httr_well, use as control group
#' @param     trt_type (string) = Match to stype in httr_well, defaults to "QC sample"
#' @param     ctrl_type (string) = Match to stype in httr_well, defaults to "QC sample"
#' @param     pg_id (string) = Filter both trt and ctrl wells, match to pg_id in httr_well, defaults to None
#' @param     media (string) = Filter both trt and ctrl wells, match to media in httr_well, defaults to None
#' @param     timeh (string) = Filter both trt and ctrl wells, match to timeh in httr_well, defaults to None
#' @param     trt_src (string) = Filter trt wells, match to rna_src field
#' @param     ctrl_src (string) = Filter ctrl wells, match to rna_src field
#' @param     qc_flags (vector) = Filter both trt and ctrl wells, match to qc_flag field, defaults to OK only
#' @param     min_reps (int) = Minimum number of replicates in trt and ctrl groups, respectively - if either is less, will generate an output message and return empty dict
#' @param     well (string) = Name of collection with individual well treatment data, default is "httr_well"
#' @param     check_collection_present (bool) to check that httr_well is present, exit if not - defaults to FALSE cause check likely happened upstream already
#' @param     output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @param     ... = All additional args passed to treatGroupFromSamples
#' @return   (list) = Document that was inserted into trt_grp collection
#' @export  treatPairGroup

treatPairGroup <- function(db_host = getOption("DB_HOST"),
                           db_name = getOption("DB_NAME"),
                           trt_name,
                           ctrl_name,
                           trt_type = "QC sample", 
                           ctrl_type = "QC sample", 
                           pg_id = "", 
                           media = "", 
                           timeh = 0, 
                           trt_src = "", 
                           ctrl_src = "", 
                           qc_flags = c("OK"), 
                           min_reps = 2,
                           well =  getOption("HTTR_WELL_NAME"),  
                           check_collection_present = FALSE,
                           output_dir = "not_set",
                           ...){
                   


# Build the query for treatment and ctrl wells
    trt_well_query = list() 
    ctrl_well_query = list() 
    
    
    if (!is.null(trt_type)){
        trt_well_query$stype = trt_type
    }
    if (!is.null(ctrl_type)){
      ctrl_well_query$stype = ctrl_type
    }
    if (!is.null(pg_id)){
      ctrl_well_query$pg_id = pg_id
      trt_well_query$pg_id = pg_id
    }
    if (!is.null(media)){
      ctrl_well_query$media = media
      trt_well_query$media = media
    }
    if (!is.null(timeh)){
      ctrl_well_query$timeh = timeh
      trt_well_query$timeh = timeh
    }
    if (!is.null(trt_src)){
      trt_well_query$rna_src = trt_src
    }
    if (!is.null(ctrl_src)){
      trt_well_query$rna_src = ctrl_src
    }
    if (!is.null(qc_flags)){
      trt_well_query$qc_flags = qc_flags
      ctrl_well_query$qc_flags = qc_flags
    }
    #check_collection_present is FALSE if treatPairGroup caller has checked already
    httr_well = openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=check_collection_present, output_dir=output_dir)
    # Get sample IDs for both treatments and ctrl wells
    trt_wells = httr_well$distinct(key = "sample_id", query = mongoQuery(trt_well_query))
    ctrl_wells = httr_well$distinct("sample_id", query = mongoQuery(ctrl_well_query))

    # Make sure both trt_wells and ctrl_wells > min_reps long
    if ((length(trt_wells) < min_reps) | (length(ctrl_wells) < min_reps)){
        message(sprintf("SKIPPING COMPARISON GROUP: Query returned %i trt wells and %i ctrl wells with\n trt_well_query = %s\n ctr_well_query = %s", length(trt_wells), length(ctrl_wells), unite(data = data.frame(trt_well_query), col = "new")$new, unite(data = data.frame(ctrl_well_query), col = "new")$new))
        return 
    }
    # Otherwise, pass to treatGroupFromSamples
    
    return (treatGroupFromSamples(db_host=db_host,
                                  db_name=db_name, 
                                  trt_wells=trt_wells, 
                                  ctrl_wells=ctrl_wells, 
                                  well=well, 
                                  check_collection_present=FALSE,
                                  output_dir = output_dir,
                                  ...))
}

#' bspRNAGroup
#' Construct a document for httr_trt_grp_cmp collection corresponding to the BioSpyder purified reference RNA QC sample comparison (A vsB).
#'   
#' Given a specific plate group, plus any optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. This function is just a convenenience wrapper to chemTreatGroup with modified defaults.
#' @param db_host (string) = name of host name where db name is located
#' @param db_name (db name) = Open connection to database with httr_well and httr_trt_grp_cmp collections
#' @param pg_id (string) = Filter both trt and ctrl wells, match to pg_id in httr_well, required for ref RNA groups
#' @param trt_type (string) = Match to stype in httr_well, defaults to "QC sample"
#' @param ctrl_type (string) = Match to stype in httr_well, defaults to "QC sample"
#' @param trt_src (string) = Match to rna_src in httr_well, defaults to "BSP_RNA_B"
#' @param ctrl_src (string) = Match to rna_src in httr_well, defaults to "BSP_RNA_A"
#' @param trt_prop_fields (vector) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate just the stype field (exclude all chemical and dose information).
#' @param both_prop_fields (vector) = Fields in httr_well collection that should match up for all trt_wells AND ctrl_wells, and the singular value should be propagated to to field of same name in#' @param httr_trt_grp_cmp collection. Default is to propagate pg_id and block_id only for ref RNA comparisons
#' @param ctrl_desc_field (string) = Field to propagate from ctrl_wells to ctrl field in httr_trt_grp_cmp document, defaults to "trt_name" for ref RNA comparisons
#' @param grp_id_opts (vector) = Options to modify trt_grp_id generation, "vs" will capture the pairwise comparison type, and "pg" is usually sufficient for ref RNA to create distinct group IDs for each plate group
#' @param  check_collection_present (bool) to check that httr_well is present, exit if not - defaults to FALSE cause check likely happened upstream already
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @return (list) = Document that was inserted into trt_grp collection
#' @export bspRNAGroup

bspRNAGroup <- function(db_host = getOption("DB_HOST"),
                        db_name = getOption("DB_NAME"), 
                        pg_id,
                        trt_name = "BSP_RNA_B",
                        ctrl_name = "BSP_RNA_A",
                        trt_type = "QC sample",
                        ctrl_type = "QC sample", 
                        trt_src = "Reference RNA",
                        ctrl_src = "Reference RNA",
                        trt_prop_fields = c(),
                        both_prop_fields = c("stype", "rna_src", "pg_id", "block_id"),
                        ctrl_desc_field = "trt_name",
                        grp_id_opts = c("vs","pg"),
                        check_collection_present = FALSE,
                        output_dir = "not_set", 
                        ...){


    
    # Pass to treatPairGroup:
    return (treatPairGroup(db_host=db_host, db_name=db_name, trt_name=trt_name, ctrl_name=ctrl_name, trt_type=trt_type, ctrl_type=ctrl_type, pg_id=pg_id, trt_src=trt_src, ctrl_src=ctrl_src, trt_prop_fields=trt_prop_fields, both_prop_fields=both_prop_fields, ctrl_desc_field=ctrl_desc_field, grp_id_opts=grp_id_opts, check_collection_present=check_collection_present, output_dir = output_dir))
}
    
#' allBspRNAGroups
#' Construct documents for httr_trt_grp_cmp collection corresponding to each BioSpyder Reference RNA QC sample comparison (single conc treatment).
#'    Loop over all plate groups and create a httr_trt_grp_cmp document for each pair of reference RNAs.
#' @param db_host (string) = name of host name where db name is located    
#' @param db_name (string) = database name with httr_well and httr_trt_grp_cmp collections
#' @param well (string) = Name of collection with individual well treatment data, default is "httr_well"
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @param ... = All additional args passed to chemTreatGroup
#' @return (list) = List of documents that were inserted into trt_grp collection
#' @export allBspRNAGroups

allBspRNAGroups <- function(db_host = getOption("DB_HOST"),
                            db_name = getOption("DB_NAME"),
                            well = getOption("HTTR_WELL_NAME"),
                            output_dir = "not_set",
                            ...){
    

# Store all documents in a list, also track the number that were skipped due to insufficient replicates with passing QC flags:
    trt_grp_docs <- list() #[]
    skip_cnt <- 0
    # Get list of pg_id values in httr_well

    httr_well = openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=TRUE, output_dir=output_dir)
    
    all_pg = httr_well$distinct(key = "pg_id")
    
    message(sprintf("Generating treatment groups for BioSpyder reference RNA comparisons on %i plate groups.", len(all_pg)))
         
    for (pg_id in sort(all_pg)){
        db_doc = bspRNAGroup(db_host=db_host, db_name=db_name, pg_id=pg_id, well=well, output_dir = output_dir, ...)
        if (length(db_doc) == 0){
            skip_cnt = skip_cnt + 1
        }
        else{
            trt_grp_docs.append(db_doc)
        }
    }
    # Report how many were skipped
    if (skip_cnt > 0){
        message(sprintf("%i treatment comparison groups were skipped due to insufficient replicates.", skip_cnt))
    }
    message(sprintf("A total of %i treatment comparison groups were generated.", len(trt_grp_docs)))
    return (trt_grp_docs)
} 

#' bspLysateGroup
#' Construct a document for httr_trt_grp_cmp collection corresponding to the BioSpyder reference QC lysate sample comparison (A vsB).
#' Given a specific plate group, plus any optional filter criteria, construct an appropriate document and insert into httr_trt_grp_cmp. This function is just a convenenience wrapper to chemTreatGroup with modified defaults. Note that unlike the original MCF7 bulk lysates, these samples are annotated more like the reference RNA samples.
    
#' @param  db_name (string) database name with httr_well and httr_trt_grp_cmp collections
#' @param pg_id (string) = Filter both trt and ctrl wells, match to pg_id in httr_well, required for ref RNA groups
#' @param trt_type (string) = Match to stype in httr_well, defaults to "QC sample"
#' @param ctrl_type (string) = Match to stype in httr_well, defaults to "QC sample"
#' @param trt_src (string) = Match to rna_src in httr_well, defaults to "BSP_RNA_B"
#' @param ctrl_src (string) = Match to rna_src in httr_well, defaults to "BSP_RNA_A"
#' @param trt_prop_fields (list) = Fields in httr_well collection that should match up for all trt_wells and the singular value should be propagated to field of same name in httr_trt_grp_cmp collection. Default is to propagate just the stype field (exclude all chemical and dose information).
#' @param both_prop_fields (list) = Fields in httr_well collection that should match up for all trt_wells AND ctrl_wells, and the singular value should be propagated to to field of same name in httr_trt_grp_cmp collection. Default is to propagate pg_id and block_id only for ref RNA comparisons
#' @param ctrl_desc_field (string) = Field to propagate from ctrl_wells to ctrl field in httr_trt_grp_cmp document, defaults to "trt_name" for ref RNA comparisons
#' @param grp_id_opts (list) = Options to modify trt_grp_id generation, "vs" will capture the pairwise comparison type, and "pg" is usually sufficient for ref RNA to create distinct group IDs for each plate group
#' @param  check_collection_present (bool) to check that httr_well is present, exit if not - defaults to FALSE cause check likely happened upstream already
#' @param ... All additional args passed to treatGroupFromSamples
#' @return  (list) = Document that was inserted into trt_grp collection
#' @export bspLysateGroup   
    
bspLysateGroup <- function(db_host = getOption("DB_HOST"),
                           db_name = getOption("DB_NAME"), 
                           pg_id,
                           trt_name = "BSP_LYSATE_B",
                           ctrl_name = "BSP_LYSATE_A",
                           trt_type = "QC sample",
                           ctrl_type = "QC sample", 
                           trt_src = "Bulk Lysate",
                           ctrl_src = "Bulk Lysate",
                           trt_prop_fields = c(), 
                           both_prop_fields = c("stype", "rna_src", "pg_id", "block_id"),
                           ctrl_desc_field = "trt_name",
                           grp_id_opts = c("vs","pg"),
                           check_collection_present = FALSE,
                           ...){
                   

    
    # Pass to treatPairGroup:
    return (treatPairGroup(db_host=db_host, db_name=db_name, trt_name=trt_name, ctrl_name=ctrl_name, trt_type=trt_type, ctrl_type=ctrl_type, pg_id=pg_id, trt_src=trt_src, ctrl_src=ctrl_src, trt_prop_fields=trt_prop_fields, both_prop_fields=both_prop_fields, ctrl_desc_field=ctrl_desc_field, grp_id_opts=grp_id_opts, check_collection_present=check_collection_present,...))
    
}

#' allBspLysateGroups
#' Construct documents for httr_trt_grp_cmp collection corresponding to each BioSpyder Reference QC Lysate sample comparison (single conc treatment).
#'    Loop over all plate groups and create a httr_trt_grp_cmp document for each pair of reference RNAs.
#' @param db_host (string) = name of host name where db name is located    
#' @param db_name (string) = database name with httr_well and httr_trt_grp_cmp collections
#' @param well (string) = Name of collection with individual well treatment data, default is "httr_well"
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @param ... = All additional args passed to chemTreatGroup
#' @return (list) = List of documents that were inserted into trt_grp collection
#' @export allBspLysateGroups
    
allBspLysateGroups <- function(db_host = getOption("DB_HOST"),
                               db_name = getOption("DB_NAME"),
                               well = getOption("HTTR_WELL_NAME"),
                               output_dir = "not_set",
                               ...){
    


# Store all documents in a list, also track the number that were skipped due to insufficient replicates with passing QC flags:
    trt_grp_docs <- list() #[]
    skip_cnt <- 0
    # Get list of pg_id values in httr_well

    httr_well = openMongo(db_host=db_host, db_name=db_name, collection= well, check_collection_present=TRUE, output_dir=output_dir)
    all_pg = httr_well$distinct(key = "pg_id")
    
    message(sprintf("Generating treatment groups for BioSpyder reference lysate comparisons on %i plate groups.", len(all_pg)))
         
    for (pg_id in sort(all_pg)){
        db_doc = bspLysateGroup(db_host=db_host, db_name=db_name, pg_id=pg_id, well=well, ...)
        if (length(db_doc) == 0){
            skip_cnt = skip_cnt + 1
        }
        else{
            trt_grp_docs.append(db_doc)
        }
    }
    # Report how many were skipped
    if (skip_cnt > 0){
        message(sprintf("%i treatment comparison groups were skipped due to insufficient replicates.", skip_cnt))
    }
    message(sprintf("A total of %i treatment comparison groups were generated.", len(trt_grp_docs)))
    return (trt_grp_docs)
}
    
    




