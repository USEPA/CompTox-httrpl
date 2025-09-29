
#' getGeoMetadata
#'
#' @param db_host character string: the address of the database being used
#' @param db_name character string: the name of the database being used
#' @param collections list: the well_trt, well, chemical and raw collections from the database
#' @param procFile character: the filename of the processed GEO count file used in GEO submission
#' @param vehicle_control character: the vehicle control used in the study; default is DMSO. Note column values for the vehicle as these may need to be changed.
#' @param cell_line character string: the cell line used in the study; default is NULL. If NULL, function will use the value in the httr_well collection.
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @param ... Additional query parameters to be passed to mongoQuery
#' @importFrom stringr str_detect
#'
#' @export getGeoMetadata
#'
#' @return GEOmetadata, a table

getGeoMetadata <- function(db_host = NULL, db_name = NULL, collections=character(0), procFile=' ', vehicle_control = "DMSO", cell_line = NULL, output_dir="", ...){

  collections <- check_collection_remapped(collections)

  #open DB connections
  DB_well <- openMongo(db_host=db_host, db_name=db_name, collection = collections["httr_well"], output_dir=output_dir)
  DB_chem <- openMongo(db_host=db_host, db_name=db_name, collection = collections["httr_chem"], output_dir=output_dir)
  DB_raw <- openMongo(db_host=db_host, db_name=db_name, collection = collections["httr_raw"], output_dir=output_dir)

  #Pass ... to query
  well_query <- mongoQuery(...)

  #setup field filter: exclude _id, probe_cnts, raw_id, counts_id, trt_id
  well_filter <- mongoQuery('_id'=0, probe_cnts=0, raw_id=0, counts_id=0, trt_id=0)

  well_iter <- DB_well$iterate(query = well_query, fields = well_filter)
  treatments <- list()
  rm_fields <- character(0)
  while(!is.null(well_data <- well_iter$one())) {
    # Put probe_cnts in one list, all other singular fields in another
    well_sid <- well_data$sample_id
    #counts[[well_sid]] <- well_data$probe_cnts
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
  # Sort by sample_id, use sample_id as row.names, then return
  treatments <- treatments[order(treatments[,"sample_id"], decreasing = F),]
  row.names(treatments) <- treatments[,"sample_id"]

  #Now that we have the treatments, we need to get the httr_chem collection loaded
  chemicals <- DB_chem$find()

  #Get the file paths
  raw <- data.table(DB_raw$find())

  raw <- raw[sample_id %in% treatments[, "sample_id"],]

  #Merge treatments (i.e. DB metadata) with chemicals
  db_meta <- merge(treatments, chemicals, by = "chem_id", all.x = TRUE)

  #Let's start building the GEO meta table

  #Define column names for GEO metadata table
  GEO_columns <- c("Sample name", "title", "source name", "organism", "characteristics: cell line",
                   "characteristics: media", "characteristics: treatment", "characteristics: chemical name", "characteristics: chemical sample ID",
                   "characteristics: chemical concentration", "characteristics: dose level", "characteristics: exposure time",
                   "characteristics: assay plate", "characteristics: assay plate well", "molecule", "description",
                   "processed data file", "raw data file")


  GEO_metadata <- data.table("Sample name", "title", "source name", "organism", "characteristics: cell line",
                             "characteristics: media", "characteristics: treatment", "characteristics: chemical name", "characteristics: chemical sample ID",
                             "characteristics: chemical concentration", "characteristics: dose level", "characteristics: exposure time",
                             "characteristics: assay plate", "characteristics: assay plate well", "molecule", "description",
                             "processed data file", "raw data file")
  colnames(GEO_metadata) <- GEO_columns

  GEO_metadata <- data.table(GEO_metadata, db_meta)

  #Fill in the metadata table

  #sample name is 'sample_id'
  GEO_metadata[, "Sample name" := sample_id]

  #title is 'trt_name' with sample_id appended to the end
  GEO_metadata[, "title" := paste(trt_name, sample_id, sep = "_")]

  #source name is 'stype' fields and/or 'MCF7 Cells', user can update as needed after running the function
  GEO_metadata[, `source name` := stype]

  if(!is.null(GEO_metadata$cell_type)){
    GEO_metadata[`source name` == "vehicle control" | `source name` == "untreated control" | `source name` == "test sample" | `source name` == "reference chemical" | str_detect(`title`, "BL_"), `source name` := paste(cell_type, "Cells", sep = " ")]
  } else{
    GEO_metadata[`source name` == "vehicle control" | `source name` == "untreated control" | `source name` == "test sample" | `source name` == "reference chemical" | str_detect(`title`, "BL_"), `source name` := paste(cell_line, "Cells", sep = " ")]
  }

  #organism is 'Homo sapiens'
  GEO_metadata[, organism := "Homo sapiens"]

  #char cell line is taken from the cell_type column of httr_well if it exists, else use the defined value
  if(!is.null(GEO_metadata$cell_type)){
    GEO_metadata[, "characteristics: cell line" := ifelse(rna_src == "Plated Cells" | rna_src == "Bulk Lysate", cell_type, NA)]
  } else{
    GEO_metadata[, "characteristics: cell line" := ifelse(rna_src == "Plated Cells" | rna_src == "Bulk Lysate", cell_line, NA)]
  }

  #char media is 'media' for cell lysate samples and blank for everything else and can be changed by user if more information is needed
  if(!is.null(GEO_metadata$media)){
    GEO_metadata[, "characteristics: media" := media]
  } else {
    GEO_metadata[, "characteristics: media" := NA]
    }

  #char treatment is '*timeh*h exposure of *conc* uM *chem_name*'
  # LJE - Line below is an error, does not follow the specs that were requested - Untrtd_... samples should be marked as "untreated" here
  # GEO_metadata[str_detect(`title`, "DMSO_") | str_detect(`title`, "Untrtd"), `characteristics: treatment` := "0.5% DMSO vehicle control"]
  if(is.null(GEO_metadata$timeh) & is.null(GEO_metadata$conc)){
    GEO_metadata[, "characteristics: treatment" := chem_name]
  } else if (is.null(GEO_metadata$timeh)){
    GEO_metadata[, "characteristics: treatment" := paste("exposure of", conc, " uM of ", chem_name, sep = "")]
  } else if (is.null(GEO_metadata$conc)){
    GEO_metadata[, "characteristics: treatment" := paste(timeh, "h exposure of", chem_name, sep = "")]
  } else{
    GEO_metadata[, "characteristics: treatment":= paste(timeh, "h exposure of ", conc, " uM of ", chem_name, sep = "")] #give everything the standard convention
  }

  GEO_metadata[stype == "vehicle control", "characteristics: treatment" := "0.5% DMSO vehicle control"] #default value, can be changed after running function
  GEO_metadata[str_detect(`title`, "Untrtd"), "characteristics: treatment" := "untreated"]
  GEO_metadata[is.na(`characteristics: media`), "characteristics: treatment" := "untreated"]
  GEO_metadata[rna_src == "Bulk Lysate", "characteristics: treatment" := "0.5% DMSO vehicle control"] #default for bulk lysate samples

  #char chemical name is 'chem_name' and RNA type, otherwise blank
  GEO_metadata[, `characteristics: chemical name` := chem_name]
  GEO_metadata[stype == "vehicle control", `characteristics: chemical name` := vehicle_control]

  #char chemical sample ID is 'chem_id' leave blank for everything not a test sample
  GEO_metadata[, `characteristics: chemical sample ID` := chem_id]
  GEO_metadata[stype != "test sample", `characteristics: chemical sample ID` := NA]
  # LJE - Added to this line to blank out any other rows
  GEO_metadata[is.na(chem_id), `characteristics: chemical sample ID` := NA]

  #char chemical concentration is 'conc conc_unit'
  GEO_metadata[, `characteristics: chemical concentration` := paste(conc, conc_unit, sep = " ")]
  GEO_metadata[str_detect(`characteristics: chemical concentration`, "NA"), `characteristics: chemical concentration` := NA]


  #make all fields equal to what's in dose_level
  GEO_metadata[, `characteristics: dose level` := dose_level]
  GEO_metadata[stype == "vehicle control", `characteristics: dose level` := 0]
  GEO_metadata[stype == "reference chemical", `characteristics: dose level` := dose_level]

  #char exposure time is '*timeh*h' except RNA or lysis buffer
  #GEO_metadata[, `characteristics: exposure time` := ifelse(`characteristics: cell line` == "MCF7", "6h", NA)]
  if(!is.null(GEO_metadata$timeh)){
    GEO_metadata[, `characteristics: exposure time` := ifelse(is.na(timeh), NA, paste0(timeh, "h"))]
  }
  GEO_metadata[rna_src != "Plated Cells" & rna_src != "Bulk Lysate", `characteristics: exposure time` := NA]


  #char assay plate is 'plate_id'
  GEO_metadata[, `characteristics: assay plate` := plate_id]

  #char assay plate well is 'well_id'
  GEO_metadata[, `characteristics: assay plate well` := well_id]

  #molecule is 'total RNA'
  # LJE - Fixed to match case of GEO schema as originally requested
  GEO_metadata[, molecule := "total RNA"]

  #descriptions is 'trt_name'
  GEO_metadata[, description := trt_name]

  #processed data file is procFile
  GEO_metadata[, `processed data file` := procFile]

  #raw data file is filname of fastq file (map by sample_id)
  GEO_metadata <- merge(GEO_metadata, raw[, 2:3], by = "sample_id")
  GEO_metadata[, `raw data file` := fastq]

  #Clean up metadata to have columns of interest only -- adding qc_flag as well
  GEO_metadata <- GEO_metadata[, which((names(GEO_metadata) %in% c(GEO_columns, "qc_flag"))==TRUE), with = FALSE]

  #Return the formatted data table
  return(GEO_metadata)
}


#' getSRAMetadata
#'
#' @param db_host character string: the address of the database being used
#' @param db_name character string: the name of the database being used
#' @param title character string: a short description of the dataset to be used on SRA public pages
#' @param collections list: the well and httr_raw collections from the database
#' @param library_strategy character string: Sequencing strategy used.  'RNA-seq' by default.
#' @param library_source character string: Source of RNA.  'TRANSCRIPTOMIC' by default.
#' @param library_selection character string: Selection method used during sequence library preparation.  'cDNA' by default.
#' @param library_layout character string: Single or paired-end.  'Single' by default.
#' @param platform character string: Sequencing platform.  'ILLUMINA' by default.
#' @param instrument_model character string: Sequencing instrument model (e.g. "Illumina HiSeq 2500" or "Illumina NovaSeq")
#' @param design_description character string: free-form description of the methods used to create the sequencing library. A brief 'materials and methods' section.
#' @param filetype: character string: typeof raw data file.  'fastq' by default.
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @param ... additional queries to be passed to mongoQuery
#' @importFrom stringr str_detect
#'
#' @export getSRAMetadata
#' @return csv containing BioSample metadata needed for an SRA submission.

getSRAMetadata <- function(db_host= NULL, db_name = NULL, title = "", collections=character(0), library_strategy = "RNA-seq", library_source = "TRANSCRIPTOMIC", library_selection="cDNA", library_layout="Single", platform="ILLUMINA", instrument_model= "", design_description= "",  filetype="fastq", output_dir="", ...){

  collections <- check_collection_remapped(collections)

  #open DB connections
  DB_well <- openMongo(db_host=db_host, db_name=db_name, collection = collections["httr_well"], output_dir=output_dir)
  DB_raw <- openMongo(db_host=db_host, db_name=db_name, collection = collections["httr_raw"], output_dir=output_dir)


  #Pass ... to query
  well_query <- mongoQuery(...)


  #setup field filter: exclude _id, probe_cnts, raw_id, counts_id, trt_id
  well_filter <- mongoQuery('_id'=0, probe_cnts=0, raw_id=0, counts_id=0, trt_id=0)

  well_iter <- DB_well$iterate(query = well_query, fields = well_filter)
  treatments <- list()
  rm_fields <- character(0)
  while(!is.null(well_data <- well_iter$one())) {
    # Put probe_cnts in one list, all other singular fields in anothern
    well_sid <- well_data$sample_id
    #counts[[well_sid]] <- well_data$probe_cnts
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
  # Sort by sample_id, use sample_id as row.names, then return
  treatments <- treatments[order(treatments[,"sample_id"], decreasing = F),]
  row.names(treatments) <- treatments[,"sample_id"]

  #Get the file paths
  raw <- data.table(DB_raw$find())
  raw <- raw[sample_id %in% treatments[, "sample_id"],]

  #Merge treatments (i.e. DB metadata) with chemicals
  db_meta <- merge(treatments, raw, by = "sample_id", all.x = TRUE)


  #Define column names for GEO metadata table
  SRA_columns <- c("sample_name", "library_ID", "title", "library_strategy", "library_source",
                   "library_selection", "library_layout", "platform", "instrument_model", "design_description","filetype", "filename")


  SRAMetadata <- data.table(`Sample name`="",`Library ID`="", Title=title, `Library strategy`=library_strategy, `Library source`=library_source, `Library selection`=library_selection, `Library layout`=library_layout, Platform=platform, `Instrument model`=instrument_model, `Design description`=design_description, Filetype=filetype, `File name`="")
  colnames(SRAMetadata) <- SRA_columns

  SRAMetadata <- data.table(SRAMetadata, db_meta)

  SRAMetadata[, sample_name := sample_id]

  SRAMetadata[, library_ID := sample_id]


  #title is defined by the user
  SRAMetadata[, title := title]


  #char cell line is 'MCF7' for cell lysates and blank for everything else
  SRAMetadata[, filename := fastq]

  #grab required columns
  SRAMetadata <- SRAMetadata[, which((names(SRAMetadata) %in% c(SRA_columns))==TRUE), with = FALSE]

  #Return the formatted data table
  return(SRAMetadata)
}

#' getSRABioSampleMetadata
#'
#' @param db_host character string: the address of the database being used
#' @param db_name character string: the name of the database being used
#' @param collections list: the well_trt, well, chemical and raw collections from the database
#' @param Organism character string: The most descriptive organism name for this sample (to the species, if possible), H. sapiens by default.
#' @param Isolate character string: identification or description of the specific individual from which this sample was obtained
#' @param Age character string: 	age at the time of sampling; relevant scale depends on species and study
#' @param biomaterial_provider character string: name and address of the lab or PI, or a culture collection identifier, 'US EPA,109 T.W. Alexander Drive, Durham, NC, 27709' by default
#' @param collection_date character string: the date on which the sample was collected
#' @param geo_loc_name character string: geographical origin of the sample,  'USA: Durham, North Carolina' by default
#' @param Sex character string: chromosomal sex of the sampled organism
#' @param Tissue character string: the type of tissue the sample was taken from
#' @param vehicle_control character: the vehicle control used in the study; default is DMSO. Note column values for the vehicle as these may need to be changed.
#' @param Cell_line character string: the cell line used in the study; default is NULL. If NULL, function will use the value in the httr_well collection.
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#' @param ... additional queries to be passed to mongoQuery
#' @importFrom stringr str_detect
#'
#' @export getSRABioSampleMetadata
#'
#' @return csv containing BioSample metadata needed for an SRA submission.

getSRABioSampleMetadata <- function(db_host = NULL, db_name = NULL, collections=character(0), Organism = 'Homo sapiens', Isolate = 'not applicable', Age = 'not applicable', biomaterial_provider = 'US EPA,109 T.W. Alexander Drive, Durham, NC, 27709', collection_date = 'not applicable', geo_loc_name = 'USA: Durham, North Carolina', Sex = 'not applicable', Tissue = 'not applicable', vehicle_control = 'DMSO', Cell_line = NULL, output_dir="", ...){

  collections <- check_collection_remapped(collections)

  #open DB connections
  DB_well <- openMongo(db_host=db_host, db_name=db_name, collection = collections["httr_well"], output_dir=output_dir)
  DB_chem <- openMongo(db_host=db_host, db_name=db_name, collection = collections["httr_chem"], output_dir=output_dir)
  DB_raw <- openMongo(db_host=db_host, db_name=db_name, collection = collections["httr_raw"], output_dir=output_dir)

  #Pass ... to query
  well_query <- mongoQuery(...)

  #setup field filter: exclude _id, probe_cnts, raw_id, counts_id, trt_id
  well_filter <- mongoQuery('_id'=0, probe_cnts=0, raw_id=0, counts_id=0, trt_id=0)

  well_iter <- DB_well$iterate(query = well_query, fields = well_filter)
  treatments <- list()
  rm_fields <- character(0)
  while(!is.null(well_data <- well_iter$one())) {
    # Put probe_cnts in one list, all other singular fields in another
    well_sid <- well_data$sample_id
    #counts[[well_sid]] <- well_data$probe_cnts
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
  # Sort by sample_id, use sample_id as row.names, then return
  treatments <- treatments[order(treatments[,"sample_id"], decreasing = F),]
  row.names(treatments) <- treatments[,"sample_id"]

  #Now that we have the treatments, we need to get the httr_chem collection loaded
  chemicals <- DB_chem$find()

  #Get the file paths
  raw <- data.table(DB_raw$find())

  raw <- raw[sample_id %in% treatments[, "sample_id"],]

  #Merge treatments (i.e. DB metadata) with chemicals
  db_meta <- merge(treatments, chemicals, by = "chem_id", all.x = TRUE)

  #Let's start building the GEO meta table

  #Define column names for GEO metadata table
  SRA_columns <- c("sample_name", "organism", "isolate",
                   "age", "biomaterial_provider", "collection_date", "geo_loc_name",
                   "sex", "tissue", "cell_line",
                   "sample_type", "treatment", "chemical_name", "chemical_ID",
                   "chemical_concentration")


  BioSample_metadata <- data.table(sample_name = "", organism = Organism, isolate = Isolate,
                                   age = Age, biomaterial_provider = biomaterial_provider, collection_date = collection_date, geo_loc_name = geo_loc_name,
                                   sex = Sex, tissue = Tissue, cell_line = "",
                                   sample_type = "", treatment = "", chemical_name = "", chemical_ID = "",
                                   chemical_concentration = "")
  colnames(BioSample_metadata) <- SRA_columns

  BioSample_metadata <- data.table(BioSample_metadata, db_meta)

  #Fill in the metadata table

  #sample_name is 'sample_id'
  BioSample_metadata[, sample_name := sample_id]

  #sample_type is stype
  BioSample_metadata[, sample_type := stype]


  #cell_line is taken from the cell_type column of httr_well if it exists, else use the defined value
  if(!is.null(BioSample_metadata$cell_type)){
    BioSample_metadata[, cell_line := cell_type] #since cell_type has already been formatted correctly
  } else{
    BioSample_metadata[, cell_line := Cell_line] #use provided value
  }
  BioSample_metadata[rna_src != "Plated Cells" & rna_src != "Bulk Lysate", cell_line := NA]

  #treatment is '*timeh*h exposure of *conc* *conc_unit* *chem_name*'
  if(is.null(BioSample_metadata$timeh) & is.null(BioSample_metadata$conc)){
    BioSample_metadata[, treatment := chem_name]
  } else if (is.null(BioSample_metadata$timeh)){
    BioSample_metadata[, treatment := paste("exposure of", conc, " uM of ", chem_name, sep = "")]
  } else if (is.null(BioSample_metadata$conc)){
    BioSample_metadata[, treatment := paste(timeh, "h exposure of", chem_name, sep = "")]
  } else{
    BioSample_metadata[, treatment:= paste(timeh, "h exposure of ", conc, " uM of ", chem_name, sep = "")] #give everything the standard convention
  }
  BioSample_metadata[stype == "vehicle control", treatment := "0.5% DMSO vehicle control"] #default value; can be changed after running function
  BioSample_metadata[stype == "QC sample" & rna_src != "Bulk Lysate", treatment := "untreated"]

  if(!is.null(BioSample_metadata$media)){
    BioSample_metadata[is.na(media), treatment := "untreated"]
    }
  BioSample_metadata[rna_src == "Bulk Lysate", treatment := "0.5% DMSO vehicle control"] #default for bulk lysate samples


  #chemical_name is 'chem_name' and RNA type, otherwise blank
  BioSample_metadata[, `chemical_name` := chem_name]
  BioSample_metadata[stype == "vehicle control", chemical_name := "DMSO"]

  # LJE - Added to this line to blank out any other rows
  BioSample_metadata[rna_src != "Plated Cells" & rna_src != "Bulk Lysate", chemical_name := NA]

  #chemical_ID is 'chem_id' leave blank for everything not a test sample
  BioSample_metadata[, chemical_ID := chem_id]
  BioSample_metadata[stype != "test sample", chemical_ID := NA]
  # LJE - Added to this line to blank out any other rows
  BioSample_metadata[is.na(chem_id), chemical_ID := NA]

  #chemical_concentration is 'conc conc_unit'
  BioSample_metadata[, chemical_concentration := paste(conc, conc_unit, sep = " ")]
  BioSample_metadata[str_detect(chemical_concentration, "NA"), chemical_concentration := NA]

  #make all fields equal to what's in dose_level
  BioSample_metadata[stype == "vehicle control", dose_level := 0]

  BioSample_metadata <- BioSample_metadata[, which((names(BioSample_metadata) %in% c(SRA_columns, "dose_level", "trt_name", "plate_id", "well_id"))==TRUE), with = FALSE] #add dose_level, trt_name, plate_id, and well_id

  #Return the formatted data table
  return(BioSample_metadata)
}



#' getRaw
#'
#' @param db_host character string: the address of the database being used
#' @param db_name character string: the name of the database being used
#' @param collapseReadGrps logical: if TRUE, the number of read_grps rows in substructure should be stored in n_read_grps, and the rest of the coliumns of read_grps discarded, 
#' if FALSE the columns in the substructure stored in read_grps should be flattened with the read_grps columns repeated accross all rows 
#'
#' @importFrom tibble as_tibble
#' @importFrom purrr map map_dfr
#' @importFrom dplyr bind_cols bind_rows
#' @export getRaw
#'
#' @return dataframe with the httr_raw data

getRaw <- function(db_host = NULL, db_name = NULL, collapseReadGrps = FALSE, output_dir="", ...){

  
  DB_raw <- openMongo(db_host=db_host, db_name=db_name, collection = "httr_raw", output_dir=output_dir)
  
  query <- mongoQuery(...)  

  documents <- DB_raw$find(query=query)  
  
  flatten_data <- function(documents){
  
    flat_list <- map(1:nrow(documents), function(i){
      row <- documents[i,]
      
      if (!is.null(row$read_grps[[1]])){
        aggregated_rows <- map_dfr(row$read_grps, ~bind_cols(row[setdiff(names(row), "read_grps")], .))
      } else{
        aggregated_rows <- as_tibble(row[setdiff(names(row), "read_grps")])
      }
      aggregated_rows  
  
    })
    result<- bind_rows(flat_list)
    return(result)

  }
  
  process_data <- function(documents){
    documents$n_read_grps <- sapply(documents$read_grps, function(x) if (is.null(x)) 0 else nrow(x))
    documents <- documents[ , !(names(documents) %in% "read_grps")]
    return(documents)
  
  }
  
  if (collapseReadGrps == FALSE){
    result <- flatten_data(documents)
  } else{
    result <- process_data(documents)
  }
  return(result)
}






