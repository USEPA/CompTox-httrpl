#' validate_httr_well_trt_schema
#'    this long function performs many tests on the data quality and integrity provided in the wellTrt list
#'    it delineates the tests that have failed
#'
#' @param wellTrt: (\emph{character vector}) a data.table, with columns: sample_id  plate_id well_id  chem_id chem_name  dtxsid  casrn  conc  conc_unit  dose_level block_id replicate_num pg_id  culture_id cell_type media stype trt_name rna_src qc_flag
#'  TC00001203_C07 TC00001203 C07 EPAPLT0593A13222 Glybenclamide DTXSID0037237 10238-21-8 100.00 uM  8 1 1 1 c2021-03-15 U-2 OS DMEM + 10% FBS test sample EPAPLT0593A13_8_100uM Plated Cells      OK
#'
#' @param skipped_tests: (\emph{character vector})  a list of skipped tests as defined in the tests.json file that are not used to test validate against
#' @param max_dose_level: (float) the highest dose level for any conc-response curve - knowing the intended number of points in each conc-response curve is useful
#' @param required_cols: (\emph{character vector}) a list of required columns to be found in the wellTrt data.table
#' @param extra_cols: (\emph{character vector}) a list of additional columns for which there may be a test
#' @param cell_types: (\emph{character vector}) list of all relevant type of cells
#' @param conc_units: (\emph{character vector}) list of all relevant concentration units
#' @export validate_httr_well_trt_schema
#' @import jsonlite
#' @import data.table
#' @import foreach
#' @return nothing explicit

 

# List of all tests with codes and descriptions:
tests <- fromJSON("inst/extdata/relevant_tests.json")

validate_httr_well_trt_schema <- function(wellTrt, skipped_tests = c(), max_dose_level=8, required_cols = c("sample_id", "plate_id", "well_id", "trt_name", "qc_flag"), extra_cols = c(), cell_types=c("MCF-7", "HepaRG_2D", "HepaRG_3D", "U-2 OS"), conc_units=c("uM", "ug/mL", "CF", "ppm", "mg/kg/day", "mg-kg/day", "mg.kg.day", "mg/kg/d", "mg-kg/d", "mg.kg.d" ), ...){



# Make sure all expected columns are present, no unexpected columns
  dbCols <- union(c("sample_id", "plate_id", "well_id", "trt_name", "qc_flag", "stype", "media", "timeh", "rna_src", "chem_id", "conc", "conc_unit", "dose_level", "block_id", "pg_id", "culture_id", "cell_type", "replicate_num", "conc_analytical", "conc_analytical_unit", "cell_cru", "exper_date", "replicate_id"), extra_cols)
  
  results <- list()
   
  if (!"CIT_0" %in% skipped_tests){
    print(tests$CIT_0)
    if(!all(required_cols %in% colnames(wellTrt))) {
      newelem <- paste("Missing the following columns: ", paste(setdiff(required_cols, colnames(wellTrt)), collapse=", "), "(CIT_0)")
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$CIT_0))

  if (!"UCT_0" %in% skipped_tests){
    print(tests$UCT_0)
    if(!all(colnames(wellTrt) %in% dbCols)) {
      newelem <- paste("wellTrt contains unexpected columns: ", paste(setdiff(colnames(wellTrt), dbCols), collapse=", "), "(UCT_0)")
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$UCT_0))
    

  if (!"NAS_0" %in% skipped_tests){
    print(tests$NAS_0)
    if (sum(is.na(wellTrt[,sample_id])) != 0){
      newelem <- "wellTrt contains NA sampleIds (NAS_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$NAS_0))
    
  if (!"DST_0" %in% skipped_tests){
    print(tests$DST_0)
    if (sum(duplicated(wellTrt[,sample_id])) != 0){
      newelem <- "wellTrt contains duplicated sampleIds (DST_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$DST_0))
  
  if (!"SIF_0" %in% skipped_tests){  
    print(tests$SIF_0)
    if (all(grepl("^TC[0-9]{7,8}_[A-Z][0-9]{2}$", wellTrt[,sample_id])) != TRUE){
      newelem <- "wellTrt contains sampleIds that don't follow the ^TC[0-9]{8}_[A-Z][0-9]{2} pattern (SIF_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$SIF_0))
  
  if (!"SPR_0" %in% skipped_tests && 'plate_id' %in% colnames(wellTrt) && 'well_id' %in% colnames(wellTrt)){
    print(tests$SPR_0)
    if (all(wellTrt[,sample_id] == paste(wellTrt[,plate_id], wellTrt[,well_id], sep="_")) != TRUE){
      newelem <- "wellTrt contains sampleIds that don't correspond to the plate_id_well_id (SPR)"
      results <- c(results, newelem)  
    }
  }
  else
    print(paste("skipping", tests$SPR_0))
  
  if (!"NAP_0" %in% skipped_tests && 'plate_id' %in% colnames(wellTrt)){
    print(tests$NAP_0)
    if (sum(is.na(wellTrt[,plate_id])) != 0){
      newelem <- "wellTrt contains plate_id that are NA (NAP_0)"
      results <- c(results, newelem)  
    }
  }
  else
    print(paste("skipping", tests$NAP_0))
  
  if (!"PIF_0" %in% skipped_tests && 'plate_id' %in% colnames(wellTrt)){
    print(tests$PIF_0)
    if (all(grepl("^TC[0-9]{7,8}$", wellTrt[,plate_id])) != TRUE){
      newelem <- "wellTrt contains plate_id that don't follow the TC[0-9]{8} format (PIF_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$PIF_0))
    
  if (!"NAW_0" %in% skipped_tests && 'well_id' %in% colnames(wellTrt)){
    print(tests$NAW_0)
    if (sum(is.na(wellTrt[,well_id])) != 0){
      newelem <- "wellTrt contains well_id that are NA (NAW_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$NAW_0))
  
  if (!"WIF_0" %in% skipped_tests && 'well_id' %in% colnames(wellTrt)){
    print(tests$WIF_0)
    if (all(grepl("^[A-Z][0-9]{2}$", wellTrt[,well_id])) != TRUE){
      newelem <- "wellTrt contains well_id that don't follow the TC[0-9]{2} format (WIF_0)"
      results <- c(results, newelem)
    } 
  }
  else
    print(paste("skipping", tests$WIF_0))
  
  if (!"WPC_0" %in% skipped_tests && 'plate_id' %in% colnames(wellTrt) && 'well_id' %in% colnames(wellTrt)){
    print(tests$WPC_0)
    if (all(table(wellTrt[,well_id]) <= length(unique(wellTrt[,plate_id]))) != TRUE){
      newelem <- "less well_ids found than plate_ids (WPC_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$WPC_0))
    #print(tests$SKIPPED_TESTS$WPC_0)
  
  if (!"NAT_0" %in% skipped_tests){
    print(tests$NAT_0)
    if (sum(is.na(wellTrt[,trt_name])) != 0){
      newelem <- "wellTrt contains trt_name that are NA (NAT_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$NAT_0))
  
  if (!"TRN_0" %in% skipped_tests){
    print(tests$TRN_0)
    if (all(grepl("^[A-Za-z0-9._]+$", wellTrt[rna_src != "Lysis Buffer", trt_name])) != TRUE){
      newelem <- "wellTrt contains trt_name that don't follow the [A-Za-z0-9._] format (TRN_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$TRN_0))
  
  if (!"TRF_0" %in% skipped_tests){
    print(tests$TRF_0)
    if (all(grepl("^[A-Za-z0-9._ -]+$", wellTrt[rna_src == "Lysis Buffer", trt_name])) != TRUE){
      newelem <- "wellTrt contains trt_name for rna_src == 'Lysis Buffer' that don't follow the [A-Za-z0-9._ -] format (TRF_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$TRF_0))
  
  if (!"TRR_0" %in% skipped_tests){
    print(tests$TRR_0)
    if (nrow(wellTrt[stype=="QC sample"])!=0 && length(unique(table(wellTrt[stype=="QC sample", trt_name]))) != 1){
      newelem <- "wellTrt contains QC samples for which there isn't a fixed number of replicates per trt_name (TRR_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$TRR_0))
    
  if (!"TRS_0" %in% skipped_tests){  
    print(tests$TRS_0)
  
    # Every trt_name should correspond to exactly one stype
    all_trt_name <- unique(wellTrt[,trt_name])
    all_trt_stype <- foreach(tn = all_trt_name, .combine='c') %do% {
      length(unique(wellTrt[trt_name == tn, stype]))
    }
    names(all_trt_stype) <- all_trt_name
  
    if (any(all_trt_stype != 1)){
      newelem <- "Each trt_name doesn't correspond to exactly one stype (TRS_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$TRS_0))
    
  if (!"TRP_0" %in% skipped_tests && 'plate_id' %in% colnames(wellTrt)){
    # For test samples, there should be 1 replicate of each trt_name on each plate
    print(tests$TRP_0)
    for(pid in unique(wellTrt[, plate_id])) {
      pid_test_wells <- wellTrt[(stype == "test sample") & (plate_id == pid), ]
      if (any(table(pid_test_wells[, trt_name]) != 1)){
        newelem <- "Some test samples have more than 1 replicate of each trt_name on each plate (TRP_0)"
        results <- c(results, newelem)
        break
      }
    }
  }
  else
    print(paste("skipping", tests$TRP_0))
  
  if (!"QCN_0" %in% skipped_tests){
    # qc_flag - No NAs, Should be in set {OK, CELL_VIABILITY}
    print(tests$QCN_0)
    if (sum(is.na(wellTrt[,qc_flag])) != 0){
      newelem <- "qc_flag should not be NA (QCN_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$QCN_0))
  
  if (!"QCV_0" %in% skipped_tests){
    print(tests$QCV_0)
    if (all(wellTrt[,qc_flag] %in% c("OK", "CELL_VIABILITY", "DOSEPLATE_FAIL", "DISPENSE_FAIL", "SINGLE_REP")) != TRUE){
      newelem <- "qc_flag must be one of those values: OK, CELL_VIABILITY, DOSEPLATE_FAIL, DISPENSE_FAIL or SINGLE_REP (QCV_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$QCV_0))
    
#  Starting testing optional column
  
  if (!"NAS_1" %in% skipped_tests && ('stype' %in% colnames(wellTrt))){
    print(tests$NAS_1)
    if (sum(is.na(wellTrt[,stype])) != 0){
      newelem <- "stype can't be NA (NAS_1)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$NAS_1))
  
  if (!"STV_0" %in% skipped_tests && ('stype' %in% colnames(wellTrt))){
    # stype - No NAs, should be one of: c("QC sample", "untreated control", "vehicle control", "reference chemical", "test sample", "viability control")
    print(tests$STV_0)
    if (all(wellTrt[,stype] %in% c("QC sample", "untreated control", "vehicle control", "reference chemical", "test sample", "viability control")) != TRUE){
      newelem <- "stype must be one of those values: QC sample, untreated control, vehicle control, reference chemical, test sample or viability control (STV_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$STV_0))
  
  if (!"NAR_0" %in% skipped_tests && ('rna_src' %in% colnames(wellTrt))){
    print(tests$NAR_0)
    if (sum(is.na(wellTrt[,rna_src]))!=0){
      newelem <- "rna_src can't ba NA NAR_0"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$NAR_0))
  
  if (!"RNV_0" %in% skipped_tests && ('rna_src' %in% colnames(wellTrt))){
    rna_src_vals <- c("Bulk Lysate", "HBRR", "UHRR", "Lysis Buffer", "Plated Cells", "Reference RNA", "Primary Tissue")
    print(tests$RNV_0)
    if (all(wellTrt[,rna_src] %in% rna_src_vals) != TRUE){
      newelem <- "rna_src must be one of:  Bulk Lysate, HBRR, UHRR, Lysis Buffer, Plated Cells, Reference RNA, Primary Tissue (RNV_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$RNV_0))
    
  if (!"MCH_0" %in% skipped_tests && ('media' %in% colnames(wellTrt))){
    # media - should be character type, should be defined for all Plated Cells, values should be DMEM when defined
    print(tests$MCH_0)
    if (!is.character(wellTrt[, media])){
      newelem <- "media should be of character type (MCH_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$MCH_0))
      
  if (!"MDP_0" %in% skipped_tests && 'rna_src' %in% colnames(wellTrt) && ('media' %in% colnames(wellTrt))){
    print(tests$MDP_0)
    if (sum(is.na(wellTrt[rna_src == "Plated Cells", media])) != 0){
      newelem <- "media should be defined for all plated cells (MDP_0)"
      results <- c(results, newelem)  
    }
  }
  else
    print(paste("skipping", tests$MDP_0))
     
  if (!"MVA_0" %in% skipped_tests && ('media' %in% colnames(wellTrt))){
  
    print(tests$MVA_0)
    if (all(wellTrt[!is.na(media), media] %in% c("DMEM", "DMEM + 10% FBS", "PRF.DMEM", "DMEM_PSG", "DMEM_G418", "MHPIT", "WilliamE-media")) != TRUE){
      newelem <- "media values should be DMEM when defined (MVA_0)"
      results <- c(results, newelem) 
    }
  }
  else
    print(paste("skipping", tests$MVA_0))
    
  if (!"TII_0" %in% skipped_tests && ('timeh' %in% colnames(wellTrt))){
    print(tests$TII_0)
    if (!is.integer(wellTrt[, timeh])){
      newelem <- "timeh should be of integer type (TII_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$TII_0))
  
  if (!"TID_0" %in% skipped_tests && 'dose_level' %in% colnames(wellTrt) && ('timeh' %in% colnames(wellTrt))){
    print(tests$TID_0)
    if (sum(is.na(wellTrt[dose_level > 0, timeh])) != 0){
      newelem <- "timeh should be defined for all samples with dose_level > 0 (TID_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$TID_0))

  
  if (!"CNA_0" %in% skipped_tests && 'stype' %in% colnames(wellTrt) && ('chem_id' %in% colnames(wellTrt))){
    print(tests$CNA_0)
    # chem_id - NA for stype == untreated control and rna_src = UHRR, HBRR, Lysis Buffer, No NAs in other cases
    if (all(is.na(wellTrt[(stype=="untreated control") | (trt_name %in% c("UHRR","HBRR","Lysis Buffer","BSP_LYSATE_A","BSP_LYSATE_B","BSP_RNA_A","BSP_RNA_B")),chem_id])) != TRUE){
      newelem <- "chem_id - NA for stype == untreated control (CNA_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$CNA_0))
    
  if (!"RNV_1" %in% skipped_tests && 'stype' %in% colnames(wellTrt) && 'chem_id' %in% colnames(wellTrt)){
    print(tests$RNV_1)
    if (sum(is.na(wellTrt[(stype!="untreated control") & !(trt_name %in% c("UHRR","HBRR","Lysis Buffer","BSP_LYSATE_A","BSP_LYSATE_B","BSP_RNA_A","BSP_RNA_B")),chem_id])) != 0){
      newelem <- "chem_id can't be NA when stype!=untreated control and trt_name not one of the following: UHRR,HBRR,Lysis Buffer,BSP_LYSATE_A,BSP_LYSATE_B,BSP_RNA_A,BSP_RNA_B (RNV_1)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$RNV_1))
  
  if (!"CMC_0" %in% skipped_tests && ('conc' %in% colnames(wellTrt))){
    print(tests$CMC_0)
    # conc - NA status should match chem_id, should be positive numeric
    if (all(is.na(wellTrt[,conc]) == is.na(wellTrt[,chem_id])) != TRUE){
      newelem <- "conc - NA status should match chem_id (CMC_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$CMC_0))

  if (!"CON_0" %in% skipped_tests && ('conc' %in% colnames(wellTrt))){
    print(tests$CON_0)
    if (!is.numeric(wellTrt[,conc])){
      newelem <- "conc should be numeric (CON_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$CON_0))
    
  if (!"COP_0" %in% skipped_tests && ('conc' %in% colnames(wellTrt))){
    print(tests$COP_0)
    if (nrow(wellTrt[conc < 0,]) != 0){
      newelem <- "conc should be positive (CPO_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$COP_0))
    
  if (!"COC_0" %in% skipped_tests && 'conc' %in% colnames(wellTrt) && 'conc_unit' %in% colnames(wellTrt)){
    print(tests$COC_0)
    # conc_unit - NA status should match conc, should be one of: uM
    if (all(is.na(wellTrt[,conc_unit]) == is.na(wellTrt[,conc])) != TRUE){
      newelem <- "conc_unit - NA status should match conc (COC_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$COC_0))
    
  if (!"CUV_0" %in% skipped_tests && 'conc_unit' %in% colnames(wellTrt)){
    print(tests$CUV_0)
    if (all(wellTrt[!is.na(conc_unit), conc_unit] %in% conc_units) != TRUE){  
      newelem <- paste("conc_unit should be one of: ", paste(conc_units, collapse = ", "), " (CUV_0)")
      results <- c(results, newelem)  
    }
  }
  else
    print(paste("skipping", tests$CUV_0))
    
  if (!"DMC_0" %in% skipped_tests && 'dose_level' %in% colnames(wellTrt) && 'conc' %in% colnames(wellTrt)){
  
    print(tests$DMC_0)
    # dose_level - NA status should match conc, should be integer in range 0:8, should be 0 for vehicle controls and BL DMSOs only
    if (all(is.na(wellTrt[,conc]) == is.na(wellTrt[,dose_level])) != TRUE){
      newelem <- "dose_level - NA status should match conc (DMC_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$DMC_0))
    
  if (!"DLI_0" %in% skipped_tests && 'dose_level' %in% colnames(wellTrt)){
    print(tests$DLI_0)
    if (!is.integer(wellTrt[,dose_level])){
      newelem <- "dose_level should be integer (DLI_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$DLI_0))
    
  if (!"DLR_0" %in% skipped_tests && 'dose_level' %in% colnames(wellTrt)){ 
    print(paste0(tests$DLR_0, max_dose_level, "]"))
    if (all(wellTrt[!is.na(dose_level), dose_level] %in% 0:max_dose_level) != TRUE){
      newelem <- paste0("dose_level should be in range [0:", max_dose_level, "] (DLR_0)")
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$DLR_0))
  
  if (!"DL0_0" %in% skipped_tests && 'stype' %in% colnames(wellTrt) && 'chem_id' %in% colnames(wellTrt) && 'dose_level' %in% colnames(wellTrt)){
    print(tests$DL0_0)
    if (all(wellTrt[(((stype == "vehicle control") | (chem_id == "DMSO")) & !is.na(dose_level)), dose_level] == 0) != TRUE){
      newelem <- "dose_level should be 0 for vehicle controls and BL DMSOs only (DL0_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$DL0_0))
  
  if (!"DCC_0" %in% skipped_tests && 'stype' %in% colnames(wellTrt) && 'chem_id' %in% colnames(wellTrt) && 'dose_level' %in% colnames(wellTrt)){
    print(tests$DCC_0)
    dose_warning=FALSE
    dose_conc_warning=FALSE
    conc_increasing_warning=FALSE
    # For each chem_id, each dose level should have the same conc and be monotonic increasing
    for(chem in unique(wellTrt[stype %in% c("test sample", "reference chemical"), chem_id])) {
      # Get the dose levels for this chem_id
      chem_levels <- sort(unique(wellTrt[!is.na(dose_level) & (chem_id == chem), dose_level]))
    
      if (length(chem_levels) <= 0 && dose_warning==FALSE){
        newelem <- "For each chem_id, each dose level don't have the same conc or are monotonic increasing (DCC_0)"
        results <- c(results, newelem)
        dose_warning=TRUE
      }
    
    
      if (max(chem_levels) != length(chem_levels) && dose_warning==FALSE){
        newelem <- "For each chem_id, each dose level don't have the same conc or are monotonic increasing (DCC_0)"
        results <- c(results, newelem)
        dose_warning=TRUE
      }
    
      # There should be a single conc per dose level, and should be in increasing order:
      chem_concs <- foreach(dose = chem_levels, .combine='c') %do% {
        dose_conc <- unique(wellTrt[(chem_id == chem) & (dose_level == dose), conc])
        if (length(dose_conc) != 1 && dose_conc_warning==FALSE){
          newelem <- "Some dose have more than 1 conc (DCC_0)"
          results <- c(results, newelem)
          dose_conc_warning=TRUE
        }
        return(dose_conc)
      }
      if (all(chem_concs == sort(chem_concs, decreasing = F)) != TRUE && conc_increasing_warning==FALSE){
        newelem <- "conc should be in increasing level (DCC_0)"
        results <- c(results, newelem)
        conc_increasing_warning=TRUE
      }
    }
  }
  else
    print(paste("skipping", tests$DCC_0))
    
  if (!"CPC_0" %in% skipped_tests && 'stype' %in% colnames(wellTrt) && 'chem_id' %in% colnames(wellTrt)){
    # For each test chemical - each chem_id should be on a single pg_id, and have no more than 8 * 3 samples
    chem_dose_rep_pb_detected = FALSE
    chem_id_pb_detected = FALSE
    chem_pgs_pb_detected = FALSE
  
    print(tests$CPC_0)
  
    for(chem in unique(wellTrt[stype == "test sample", chem_id])) {
      chem_pgs <- unique(wellTrt[chem_id == chem, pg_id])
      if (chem_pgs_pb_detected ==FALSE && length(chem_pgs) != 1){
        newelem <- "Some test chemical have chem_id that is not on a single pg_id (CPC_0)"
        results <- c(results, newelem)
        chem_pgs_pb_detected = TRUE      
      }

      chem_wells <- wellTrt[chem_id == chem, ]
      if (chem_id_pb_detected == FALSE && nrow(chem_wells) > (8*3)){
        newelem <- "Some test chemicals have more than 8 * 3 samples (CPC_0)"
        results <- c(results, newelem)
        chem_id_pb_detected = TRUE
      }
    
      # Every dose_level, replicate_num should be unique
      if ("replicate_num" %in% colnames(wellTrt)){
        chem_dose_rep <- paste(chem_wells$dose_level, chem_wells$replicate_num, "_")
        if (chem_dose_rep_pb_detected == FALSE && sum(duplicated(chem_dose_rep)) != 0){
          newelem <- "For some dose level, replicate_num is not unique (CPC_0)"
          results <- c(results, newelem)
          chem_dose_rep_pb_detected = TRUE
        }
        if (chem_id_pb_detected && chem_dose_rep_pb_detected && chem_pgs_pb_detected)
          break
      }
    
    }
  }
  else
    print(paste("skipping", tests$CPC_0))
  
  if (!"BIC_0" %in% skipped_tests && 'block_id' %in% colnames(wellTrt)){
    print(tests$BIC_0)
    # block_id - Character type, No NAs, should always be: 1
    if (!is.character(wellTrt[,block_id])){
      newelem <- "block_id should be character type (BIC_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$BIC_0))
  
  if (!"NAB_0" %in% skipped_tests && 'block_id' %in% colnames(wellTrt)){
    print(tests$NAB_0)
    if (sum(is.na(wellTrt[,block_id])) != 0){
      newelem <- "block_id should not be NA (NAB_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$NAB_0))
  
  if (!"PIC_0" %in% skipped_tests && 'pg_id' %in% colnames(wellTrt)){
    print(tests$PIC_0)
    # pg_id - Character type, No NAs, should always be one of: 1, 2
    if (!is.character(wellTrt[,pg_id])){
      newelem <- "pg_id should be of type Character (PIC_0)"
      results <- c(results, newelem)  
    }
  }
  else
    print(paste("skipping", tests$PIC_0))
  
  if (!"NAP_1" %in% skipped_tests && 'pg_id' %in% colnames(wellTrt)){
    print(tests$NAP_1)

    if (sum(is.na(wellTrt[,pg_id])) != 0){
      newelem <- "pg_id should not be NA (NAP_1)"
      results <- c(results, newelem)  
    }
  }
  else
    print(paste("skipping", tests$NAP_1))

  if (!"PGC_0" %in% skipped_tests && 'plate_id' %in% colnames(wellTrt) && 'pg_id' %in% colnames(wellTrt) && 'stype' %in% colnames(wellTrt)){
    print(tests$PGC_0)
    # Make sure plates are balanced within plate_group, and all contain the same chemicals
    plt_chem_set <- foreach(plt = unique(wellTrt[,plate_id]), .combine='c') %do% {
      plt_chems <- unique(wellTrt[(plate_id == plt) & (stype == "test sample"), chem_id])
      return(paste(sort(plt_chems), collapse=","))
    }
    names(plt_chem_set) <- unique(wellTrt[,plate_id])
    plates_balanced=FALSE
    plates_contain_chem=FALSE
    plates_contain_only=FALSE
    for(pg in unique(wellTrt[,pg_id])) {
      pg_plates <- wellTrt[pg_id == pg, plate_id]
      if (plates_balanced==FALSE && length(unique(table(pg_plates))) != 1){
        newelem <- "plates are not balanced within plate group (PGC_0)"
        results <- c(results, newelem)
        plates_balanced=TRUE
      }
    
      pg_plates <- unique(pg_plates)
      if (plates_contain_chem==FALSE && all(pg_plates %in% names(plt_chem_set)) != TRUE){
        newelem <- "some plates do not contain the same chemical (PGC_0)"
        results <- c(results, newelem)
        plates_contain_chem=TRUE
      }
    
      if (plates_contain_only==FALSE && length(unique(plt_chem_set[pg_plates])) != 1){
        newelem <- "some plates do not contain the same chemical (PGC_0)"
        results <- c(results, newelem)
        plates_contain_only=TRUE  
      }
    }
  }
  else
    print(paste("skipping", tests$PGC_0))
  
  if (!"NAC_0" %in% skipped_tests && 'rna_src' %in% colnames(wellTrt) && 'culture_id' %in% colnames(wellTrt)){
    print(tests$NAC_0)
    # culture_id - No NAs when rna_src == "Plated Cells", always NA for all others
    if (sum(is.na(wellTrt[rna_src == "Plated Cells", culture_id])) != 0){
      newelem <- "culture_id can't be NA for Plated Cells (NAC_0)"
      results <- c(results, newelem) 
    }  
  
    if (all(is.na(wellTrt[rna_src != "Plated Cells", culture_id])) != TRUE){
      newelem <- "culture_id must be NA for non Plated Cells (NAC_0)"
      results <- c(results, newelem) 
    }
  }
  else
    print(paste("skipping", tests$NAC_0))
  
  if (!"CIN_0" %in% skipped_tests && 'culture_id' %in% colnames(wellTrt)){
    print(tests$CIN_0)
    # There should only be 4 culture_id values, and they should all have the same number of samples 
    # (for larger screens this is true WITHIN each block)
    if('block_id' %in% colnames(wellTrt)) {
      study_blocks <- setdiff(unique(wellTrt$block_id), NA)
      for(blk in study_blocks) {
        print(paste("checking culture_ids in block", blk))
        if ((length(unique(wellTrt[(block_id == blk) & !is.na(culture_id), culture_id])) %in% c(3,4)) != TRUE){
          newelem <- paste0("there are a number of culture_ids other than 3 or 4 in block ", blk, ", there should be exactly 3 or 4 (CIN_0)")
          results <- c(results, newelem)
          print(paste0("CULTURE ID NB is: ",length(unique(wellTrt[!is.na(culture_id), culture_id]))))
        }
      }
    } else {
      print("checking culture_ids (whole sample key, no block_id column).")
      if ((length(unique(wellTrt[!is.na(culture_id), culture_id])) %in% c(3,4)) != TRUE){
        newelem <- "there are a number of culture_ids other than 3 or 4, there should be exactly 3 or 4 (CIN_0)"
        results <- c(results, newelem)
        print(paste0("CULTURE ID NB is: ",length(unique(wellTrt[!is.na(culture_id), culture_id]))))
      }
    }
  }
  else
    print(paste("skipping", tests$CIN_0))
  
  if (!"NAC_1" %in% skipped_tests && 'rna_src' %in% colnames(wellTrt) && 'cell_type' %in% colnames(wellTrt)){
    print(tests$NAC_1)
    # cell_type - No NAs when rna_src is Plated Cells, always NA for others, should always be "U-2 OS"
    # Note that in past studies this was defined for Bulk Lysate samples, which were MCF-7,
    # But the new BioSpyder bulk lysates are a mix of cell types and we don't have that info
    if (sum(is.na(wellTrt[rna_src %in% c("Plated Cells"), cell_type])) != 0){
      newelem <- "there are cell_type that are NAs when rna_src is Plated Cells (NAC_1)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$NAC_1))
  
  if (!"CU2_0" %in% skipped_tests && 'cell_type' %in% colnames(wellTrt)){
    print(tests$CU2_0)
    if (all(wellTrt[!is.na(cell_type), cell_type] %in% cell_types) != TRUE){
      newelem <- paste("there are cell_type that are not one of these values: ", paste(cell_types, collapse = ", ") , "(CU2_0)")
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$CU2_0))

  if (!"NAR_0" %in% skipped_tests && 'replicate_num' %in% colnames(wellTrt)){
    print(tests$NAR_0)
    # replicate_num - No NAs, should be between 1-3, should have same number of all replicates (these seem to be assigned per plate)
    #DEH Note ---- Because the U2 OS screen sent plates from all 4 replicate grous, the logic of line 363 does not hold true
    if (sum(is.na(wellTrt[, replicate_num])) != 0){
      newelem <- "replicate_num should not be NA (NAR_0)"
      results <- c(results, newelem)
    }
  }
  else
    print(paste("skipping", tests$NAR_0))
    
  if (!"TCT_0"  %in% skipped_tests && 'cell_type' %in% colnames(wellTrt)){
    print(tests$TCT_0)
    # Every trt_name should correspond to exactly one cell_type
    all_trt_name <- unique(wellTrt[,trt_name])
    all_trt_cell_type <- foreach(tn = all_trt_name, .combine='c') %do% {
      length(unique(wellTrt[trt_name == tn, cell_type]))
    }
    names(all_trt_cell_type) <- all_trt_name
    
    if (any(all_trt_cell_type != 1)){    
      newelem <- "Some values of trt_name correspond to multiple cell_type values - each trt_name must correspond to a single cell_type (TCT_0)"
      results <- c(results, newelem)
    }
  }    
  else
    print(paste("skipping", tests$TCT_0))
  
  return(results)

}

#' process_chem_columns
#' takes as input: (coming from prior manipulations)
#' @param wellTrt: (\emph{character}) data.table with data to check
#' @import data.table
#'
#' @return modified data table after removing rows with dup chems and removing chem_name, dtxsid, casrn and bottle_id columns 
#' @export process_chem_columns

process_chem_columns <- function(wellTrt){

  if(all(c('chem_id', 'chem_name', 'dtxsid') %in% colnames(wellTrt))) {
    
    # removing the extra info columns from wellTrt
    wellTrt[, chem_name := NULL]
    wellTrt[, dtxsid := NULL]
    if ('casrn' %in% colnames(wellTrt))
      wellTrt[, casrn := NULL]
    if ('bottle_id' %in% colnames(wellTrt))
      wellTrt[, bottle_id := NULL]
  }
  return(wellTrt)
}
  
#' process_chemInfo
#' @param wellTrt: (\emph{character})data.table with data to check
#'
#' @return chemInfo data table a subset of the inputted data.table to be stored into httr_chem by default
#' @export process_chemInfo

process_chemInfo <- function(wellTrt){

  chemInfo <- NULL

  if(all(c('chem_id', 'chem_name', 'dtxsid') %in% colnames(wellTrt))) {
    
    if ('casrn' %in% colnames(wellTrt) && 'bottle_id' %in% colnames(wellTrt))
      chemInfo <- copy(unique(wellTrt[!is.na(chem_id), list(chem_id, chem_name, dtxsid, casrn, bottle_id)]))
    else if ('casrn' %in% colnames(wellTrt))
      chemInfo <- copy(unique(wellTrt[!is.na(chem_id), list(chem_id, chem_name, dtxsid, casrn)]))
    else if ('bottle_id' %in% colnames(wellTrt))
      chemInfo <- copy(unique(wellTrt[!is.na(chem_id), list(chem_id, chem_name, dtxsid, bottle_id)]))
    else chemInfo <- copy(unique(wellTrt[!is.na(chem_id), list(chem_id, chem_name, dtxsid)]))
    
    if (sum(duplicated(chemInfo$chem_id)) != 0)
      cat("Pb: found duplicate chem_id in wellTrt")
    else
      cat("Extracted chemInfo table for", nrow(chemInfo), "distinct chem_ids.\n")
      
  }
  return (chemInfo)

}

#' readWellTrtFile
#' takes as input: (coming from prior manipulations)
#' @param sampleID_file: (\emph{character}) file that has such list in json format
#'
#' @return data table with json data with corrected column type
#' @export readWellTrtFile

readWellTrtFile <- function(sampleID_file){

    # Including na.strings = "" here is unnecessary and leads to a number of failed tests that should pass
    wellTrt <- read.csv(sampleID_file, stringsAsFactors = FALSE)
    # When loading directly from disk, pg_id and block_id columns are sometimes converted to integer automatically, which leads to failed test
    # Should automatically fix these column types in this case, but when wellTrt is provided then assume user has already handled column type conversions
    if(("pg_id" %in% colnames(wellTrt)) && (class(wellTrt$pg_id) == "integer"))
      wellTrt$pg_id <- as.character(wellTrt$pg_id)
    if(("block_id" %in% colnames(wellTrt) && (class(wellTrt$block_id) == "integer")))
      wellTrt$block_id <- as.character(wellTrt$block_id)
    wellTrt$fastq <- NULL
    wellTrt$fq_file <- NULL

    orig_columns <- colnames(wellTrt)
    
    return(wellTrt)
    
}


#' sampleID_wrapper
#' takes either as input: (coming from prior manipulations)
#' @param wellTrt: (\emph{character vector}) a data.table with following columns: 
#'     sample_id  plate_id well_id  chem_id chem_name  dtxsid  casrn  conc  conc_unit  dose_level block_id replicate_num pg_id  culture_id cell_type media stype trt_name rna_src qc_flag
#'     TC00001203_C07 TC00001203 C07 EPAPLT0593A13222 Glybenclamide DTXSID0037237 10238-21-8 100.00 uM  8 1 1 1 c2021-03-15 U-2 OS DMEM + 10% FBS test sample EPAPLT0593A13_8_100uM Plated Cells      OK
#'  OR
#' @param sampleID_file: (\emph{character}) file that has such list in json format
#' @param chemInfo: (\emph{character}) data.table that if not provided, built from wellTrt, contains some of these columns chem_id, chem_name, dtxsid, casrn, bottle_id
#' @param Target collection: (\emph{character}) targetWellTrtCol defaulting to httr_well_trt
#' @param Target Chem Collection: (\emph{character}) targetChem defaulting to httr_chem
#' @param status: (\emph{numeric}) whether of not we keep prior data if found in target collection (status="KEEP"), erase all items in collection (status="RERUN"), or just replace the sampleIDs found (status="REPLACE")
#'
#' @param skipped_tests: (\emph{character vector}) list of tests that are excluded
#' @param db_host: (\emph{character}) mongo url with or without port
#' @param db_name: (\emph{character}) mongo database or sandbox
#' @param validate: (\emph{character}) boolean if TRUE (default) will run the validate function on wellTrt
#' @param max_dose_level (float) the highest dose level for any conc-response curve - knowing the intended number of points in each conc-response curve is useful
#' @param required_cols: (\emph{character vector}) a list of required columns to be found in the wellTrt data.table
#' @param extra_cols: (\emph{character vector}) a list of additional columns for which there may be a test
#' @import data.table
#' @return nothing explicit
#'
#' the function will process each sample ids, first making sure they pass the validation tests
#' if they don't pass any of the validation tests declared in the relevant_test list, the offended test will be printed out and the 
#' the process is terminated
#' if the validation tests pass, the process continues on to store (by erasing first all or some or none of potentially already present sampleIDs in the 
#' target collections) the data into two collections: the target collection and the target Chem collection as specified earlier
#' finally, each sampleID that ended up being replaced in the target collection is removed from the httr_well collection, so that 
#' another downstream process may detect the discrepencies between httr_well_trt and httr_well and rebuild the missing sampleIDs that
#' were found in httr_well_trt but missing in httr_well
#' @export sampleID_wrapper
    

sampleID_wrapper <- function(wellTrt=NULL, chemInfo=NULL, sampleID_file=NULL, targetWellTrtCol="httr_well_trt", targetChem = "httr_chem", status="KEEP", skipped_tests=c(), db_host=NULL, db_name=NULL, validate=TRUE, max_dose_level = 8, required_cols = c("sample_id", "plate_id", "well_id", "trt_name", "qc_flag"), extra_cols = c(), output_dir = "", ...){


  if (missing(wellTrt)){
    ### read the data
    if (missing(sampleID_file))
      # For use in other projects, there should be no default here, just an error
      stop("Must specify either wellTrt or sampleID_file.\n")

      
    wellTrt <- readWellTrtFile(sampleID_file)
    orig_columns <- colnames(wellTrt)
    wellTrt <- as.data.table(wellTrt)
    #wellTrt <- process_chem_columns(wellTrt)
    #chemInfo <- process_chemInfo(wellTrt)
  }
  if(missing(chemInfo)){
    cat("No chemInfo was provided. Will generate chemInfo from wellTrt or sampleID_file. \n")
    chemInfo <- process_chemInfo(wellTrt)
    wellTrt <- process_chem_columns(wellTrt)
  }

  results <- list()
  if (validate == TRUE)
    results = validate_httr_well_trt_schema(wellTrt, skipped_tests, required_cols = required_cols, extra_cols = extra_cols, ...)

  if (length(results) != 0)
    print(results)
  else{ #we should probably not insert in db at this point, until the data is formatted properly
    #### Store in DB
    httr_well_trt <- openMongo(db_host = db_host, db_name = db_name, collection = targetWellTrtCol, check_collection_present=(status=="RERUN" || status=="REPLACE"), output_dir = output_dir)

    existing_count <- httr_well_trt$count()
    cat(targetWellTrtCol, "contains", existing_count, "documents before inserting wellTrt data.\n")
    if(existing_count > 0) {
      if(status=="RERUN") {
        cat("Dropping all existing documents in", targetWellTrtCol, "before insert.\n")
        httr_well_trt$drop()
      }
      else {
          if (status=="REPLACE"){
            my_query = list(list('$in'=wellTrt$sample_id))
            names(my_query)[1] <- 'sample_id'
            my_query = toJSON(my_query)
            cat("Removing ", wellTrt$sample_id, " from httr_well_trt \n")
            httr_well_trt$remove(query=my_query)
          }
          else
            stop("Collection already exists in database and status = KEEP.\n")
      }
    }
    httr_well_trt$insert(wellTrt)

    cat(targetWellTrtCol, "contains", httr_well_trt$count(), "documents after inserting wellTrt data.\n")

    if(is.null(chemInfo) == FALSE){
      httr_chem <- openMongo(db_host = db_host, db_name = db_name, collection = targetChem, check_collection_present=(status=="RERUN" || status=="REPLACE"), output_dir = output_dir)

      existing_count <- httr_chem$count()
      cat(targetChem, "contains", existing_count, "documents before inserting chemInfo data.\n")
      if(existing_count > 0) {
        if(status=="RERUN") {
          cat("Dropping all existing documents in", targetChem, "before insert.\n")
          httr_chem$drop()
        }
        else {
          if (status=="REPLACE"){
            my_query = list(list('$in'=chemInfo$chem_id))
            names(my_query)[1] <- 'chem_id'
            my_query = toJSON(my_query)
            #cat("Removing ", chemInfo$chem_id, " from httr_chem \n")
            httr_chem$remove(query=my_query)
          }
          else
            stop("Collection already exists in database and status = KEEP.\n")
        }
      }
      httr_chem$insert(chemInfo)
    }

    #cat(targetChem, "contains", httr_chem$count(), "documents after inserting chemInfo data.\n")
    
    #finally we need to remove those sample_ids from the httr_well collection so that the python buildAllWells script is able to figure out ikt has to reprocess them
    
    httr_well <- openMongo(db_host = db_host, db_name = db_name, collection = "httr_well", check_collection_present=TRUE, output_dir = output_dir)
    my_query = list(list('$in'=wellTrt$sample_id))
    names(my_query)[1] <- 'sample_id'
    my_query = toJSON(my_query)
    #cat("Removing ", wellTrt$sample_id, " from httr_well \n")
    httr_well$remove(query=my_query)
    
       
  }
}


#' sampleID_scan_and_update 
#' This function takes two files and scan them for differences
#'   
#' @param orig_sampleID_file: (\emph{character}) file that has already been processed prior
#' @param sampleID_file: (\emph{character}) new vs of that files with some edits - regardless where the edits took place
#' @param targetWellTrtCol: (\emph{character}) target collection where the updates will be written to - by default httr_well_trt
#' @param targetChem: (\emph{character}) the target Chem collection where some updates will be written to - by default httr_chem
#' @param skipped_tests: (\emph{character vector}) the list of tests to skip when validating the resulting set of data
#' @param db_host: (\emph{character}) mongo db server +- port
#' @param db_name: (\emph{character}) mongo db database
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository
#'
#' process:
#'     The function does a diff on the two files and calls sampleID_wrapper() on that resulting list of modified sampleIds
#'     if the list validates against a list of provided tests, the function continues to replace found sampleId documents
#'     in target collections. 
#'
#' @import data.table
#' @return nothing explicit
#'   print the test that fail if any. If validation passes, nothing returned
#' @export sampleID_scan_and_update


sampleID_scan_and_update <- function(orig_sampleID_file=NULL, sampleID_file=NULL, targetWellTrtCol="httr_well_trt", targetChem = "httr_chem", skipped_tests=skipped_tests, db_host, db_name, output_dir = "", ...){

  wellTrt <- readWellTrtFile(sampleID_file)
  wellTrt <- as.data.table(wellTrt)
  #wellTrt <- process_chem_columns(wellTrt)
  
  results <- list()
  
  results = validate_httr_well_trt_schema(wellTrt, skipped_tests, ...)
  if (length(results) != 0){
      print(results)
  }
  else{  
    orig_wellTrt <- readWellTrtFile(orig_sampleID_file)
    orig_wellTrt <- as.data.table(orig_wellTrt)
    #orig_wellTrt <- process_chem_columns(orig_wellTrt)
    #find lines in sampleID_file that are somewhat different than those in orig_sampleID_file
  
    # Set keys for merging
    setkey(orig_wellTrt,sample_id)

    # Merge, remove duplicates
    merged <- rbind(orig_wellTrt, wellTrt)
    setkey(merged,sample_id)
    diff_rows <-merged[!(duplicated(merged) | duplicated(merged, fromLast = TRUE)), ]
    diff_rows.new = diff_rows[seq(2, nrow(diff_rows), 2), ]
    print(diff_rows.new)
    chemInfo <- process_chemInfo(diff_rows.new)
    diff_rows.new <- process_chem_columns(diff_rows.new)
  
    sampleID_wrapper(diff_rows.new, chemInfo = chemInfo, status="REPLACE", skipped_tests=skipped_tests, db_host = db_host, db_name = db_name, validate = FALSE, output_dir = output_dir, ...)

  }
}

#' insert_into_httr_study
#' This function reads all inputs, validates them to create a document into httr_study


#' @param study_id (str) = short string with study ID (all collections specific to this study should have name starting with this ID, so no spaces, punctuation or special characters)
#' @param study_name (str) = Name of the study in human readable format (spaces, punctuation, etc. allowed) – replaces Imran’s “name” field in prev version of this collection.
#' @param study_desc (str) = longer text briefly describing the study
#' @param study_probe (str) = name of collection that catalogs all probes in the version of probe set used for this study
#' @param study_well (str) = name of collection with the well-level data for this study
#' @param study_trt_grp_cmp (str) = name of collection with the treatment groups for comparison for this study
#' @param study_degs (str) = name of collection with the DEG data for this study
#' @param src (str) = Source of data, e.g. “BioSpyder”
#' @param tech (str) = Technology type, e.g. “TempoSeq”
#' @param times_hr (list of int) = List of exposure times used (does not include Bulk Lysates or other QC samples)
#' @param max_dose_level (float) = Highest dose level for any conc-response curve - this replaces Imran's concs_um field because not all concs are um or will match up across chemicals, but knowing the intended number of points in each conc-response curve is useful.
#' @param media (list of str) = List of media tested (does not include Bulk Lysates or other QC samples
#' @param cell_types (list of str) = List of cell-types tested (was just "cell" in previous schema; does not include Bulk Lysates or other QC samples)
#' @param chem_ids (list of str) = List of chemical IDs tested (whatever IDs are used in chem_id field of httr_well_trt collection).
#' @param nreps (int) = Number of replicates for primary treatment groups of interest (for screening studies, this is per concentration of test samples, also per media/time/cell-type in other study types)
#' @param bs_assay_id (str) = Unique bar code or other identifier for the actual tube/batch of assay mix that BioSpyder used - we will need to get this information from them for tracking
#' @param assay_name (str) = Unique identifier for this probe set version (including the combination of attenuation factors, e.g. human_wt_1.2_mcf7 to denote Human Whole Transcriptome v1.2 with MCF-7 attenuation)
#' @param platform_name (str) = Just the general platform, without version number or attenuation modifiers, e.g. human_wt for human whole transcriptome, or human_s1500
#' @param platform_ver (str) = Version number for the platform, e.g. 1.2 or 2.0
#' @param atten_type (str) = Type of attenuation used, e.g. "none" if no attenuation, "mcf7", "heparg_super", or "broad"
#' @param probe_source_file (str) = Name of the manifest file sent from BioSpyder that was used to process this data
#' @param probe_source_date (date) = When did BioSpyder last update the file?
#' @param probe_source_rcv (date) = When did we actually receive the manifest from BioSpyder?
#' @param probe_ncct_file (str) = If NCCT performed additional clean-up of the manifest, name of the final clean file that was loaded into DB. Otherwise can be blank or same as source_file.
#' @param probe_ncct_date (date) = When did NCCT finalize the probe set manifest (after validating/modifying the manifest received from BioSpyder)
#' @param fasta_file (str) = Full path to fasta file used to build HISAT2 index and align fastq files
#' @param anno_type (str) = What was the primary annotation used for this probe set (e.g. earlier versions are refseq, newer versions are ensembl)
#' @param anno_date (date) = When was the annotation for this probe set last updated (we may update the annotations periodically to keep up with current transcriptome annotations)
#' @param output_dir (\emph{character}) = used to overwrite the global of same name to indicate mongo or Json file used as data repository                    
#'
#' process:
#'     The function does validation checks on an entry into the httr_study collection and if successfull inserts it
#'
#' @return nothing explicit
#'   print the test that fail if any. If validation passes, nothing returned
#' @export insert_into_httr_study

insert_into_httr_study <- function(study_id="httr_mcf7_pilot", study_name="HTTr MCF-7 Pilot Study", 
study_desc="Pilot of HTTr platform on MCF-7 cells testing multiple media types and exposure durations", study_probe="httr_probe", study_well="httr_well", study_trt_grp_cmp="httr_trt_grp_cmp", study_degs="httr_deg", src="BioSpyder", tech="TempoSeq", times_hr= list(6,12,24), max_dose_level=8,   media=list("DMEM", "PRF.DMEM"), cell_types=list("MCF-7"), chem_ids=list("TP0001651A01","TP0001651A02","TP0001651A03","TP0001651A04","TP0001651A05","TP0001651A06",  "TP0001651B01", "TP0001651B02", "TP0001651B03", "TP0001651B04", "TP0001651B05", "TP0001651B06", "TP0001651C01", "TP0001651C02","TP0001651C03","TP0001651C04",  "TP0001651C05", "TP0001651C06"), nreps=3, bs_assay_id="", assay_name="human_wt_1.2_mcf7", platform_name="human_wt", platform_ver="1.2", atten_type="mcf7", probe_source_file="180905 Human Whole Transcriptome 1.2 Manifest.xlsx", probe_source_date="2018-09-05T00:00:00Z", probe_source_rcv="2019-07-15T13:24:12Z", probe_ncct_file="httr_mcf7_pilot_probe.csv", probe_ncct_date="2019-09-23T13:44:50.419Z" , fasta_file="/share/projects/HTTr/HTTr_pipeline_dev/HumanWT_v1/AlignCount/genome/humanWT_v1.2.fa", anno_type="refseq", anno_date="2018-09-05T00:00:00Z", db_host=NULL, db_name=NULL, output_dir = ""){
  
  #validate each field/parameter
  
  stopifnot(is.character(study_id))  
  stopifnot(is.character(study_name))  
  stopifnot(is.character(study_desc))  
  stopifnot(is.character(study_probe))  
  stopifnot(is.character(study_well))  
  stopifnot(is.character(study_trt_grp_cmp))
  stopifnot(is.character(study_degs))
  stopifnot(is.character(src))
  stopifnot(is.character(tech))
  stopifnot(all(sapply(times_hr, function(x) is.integer(x) || (is.numeric(x) && identical(round(x), x)))))
  stopifnot(is.numeric(max_dose_level))
  stopifnot(all(sapply(media, function(x) is.character(x))))
  stopifnot(all(sapply(cell_types, function(x) is.character(x))))
  stopifnot(all(sapply(chem_ids, function(x) is.character(x))))
  stopifnot(is.numeric(nreps))
  stopifnot(is.character(bs_assay_id))
  stopifnot(is.character(assay_name))
  stopifnot(is.character(platform_name))
  stopifnot(is.character(platform_ver))
  stopifnot(is.character(atten_type))
  stopifnot(is.character(probe_source_file))
  stopifnot(is.character(probe_source_date))
  stopifnot(is.character(probe_source_rcv))
  stopifnot(is.character(probe_ncct_file))
  stopifnot(is.character(probe_ncct_date))
  stopifnot(is.character(fasta_file))
  stopifnot(is.character(anno_type))
  stopifnot(is.character(anno_date))  

  #convert all dates to ISODate ojects
  probe_source_date2<-strptime(probe_source_date, format = "%Y-%m-%d")
  probe_source_rcv2<-strptime(probe_source_rcv, format = "%Y-%m-%d")
  probe_ncct_date2<-strptime(probe_ncct_date, format = "%Y-%m-%d")
  anno_date2<-strptime(anno_date, format = "%Y-%m-%d") 
  
  httr_study <- openMongo(db_host = host, db_name = db, collection = "httr_study", output_dir = output_dir)

  my_query = list(list('$in'=study_id))
  names(my_query)[1] <- 'study_id'
  my_query = toJSON(my_query)
  cat("Removing ", study_id, " from httr_study \n")
  httr_study$remove(query=my_query)
   
   doc <- data.frame(study_id = study_id, study_name = study_name, study_desc = study_desc, study_probe = study_probe, study_well = study_well, study_trt_grp_cmp = study_trt_grp_cmp, study_degs = study_degs, src = src, tech = tech,  max_dose_level = max_dose_level, nreps = nreps, bs_assay_id = bs_assay_id, assay_name = assay_name, platform_name = platform_name, platform_ver = platform_ver, atten_type = atten_type, probe_source_file = probe_source_file, probe_source_date = probe_source_date2, probe_source_rcv = probe_source_rcv2, probe_ncct_file = probe_ncct_file, probe_ncct_date = probe_ncct_date2, fasta_file = fasta_file, anno_type = anno_type, anno_date = anno_date2)
   
   #insert all data except list - ISOdates works only with data.frame and list only with vector
   httr_study$insert(doc, auto_unbox=T)  
   
    #updating with the lists
   
   m_upd = paste0('{"$set":{"chem_ids":', toJSON(chem_ids, auto_unbox=T), ',"times_hr":', toJSON(times_hr, auto_unbox=T) , ',"media":', toJSON(media, auto_unbox=T),',"cell_types":', toJSON(cell_types, auto_unbox=T),'}}')

   httr_study$update(query = my_query, update = m_upd)
   
}


#' fill_dose_for_well_trt 
#' This function takes a data.table and fills the dose column with calculated values
#'   
#' @param sampleKey: (\emph{data.table}) the data.table conforming with the normal welltrt columns
#'
#' process:
#'     for each conc found ordered by pg_id/chem_id assign a corresponding dose_level
#'
#' @import data.table
#' @return the modified data.table
#' @export fill_dose_for_well_trt

fill_dose_for_well_trt <- function(sampleKey){

  if (class(sampleKey$dose_level) != "integer")
      sampleKey$dose_level <- as.integer(sampleKey$dose_level)

  for(pg in unique(sampleKey[stype == "test sample" | stype == "reference chemical", pg_id])) {
   for(chem in unique(sampleKey[(stype == "test sample" | stype == "reference chemical") & (pg_id == pg), chem_id])) {
      chem_concs <- sort(unique(sampleKey[(stype == "test sample" | stype == "reference chemical") & (pg_id == pg) & (chem_id == chem), conc]), decreasing = F)
      for(lvl in 1:length(chem_concs)) {
        sampleKey[(stype == "test sample" | stype == 'reference chemical') & (pg_id == pg) & (chem_id == chem) & (conc == chem_concs[lvl]),
                dose_level := lvl]
      }
    }
  }
  sampleKey[stype == "vehicle control", dose_level := 0]
  sampleKey[trt_name == "BL_DMSO", dose_level := 0]
  sampleKey[trt_name == "BL_TSA", dose_level := 1]
  return(sampleKey)
}

#' fill_dose_for_well_trt_wrapper
#' This function takes a sample_id file and converts it to a data.table and does some checks then calls fill_dose_for_well_trt
#'   
#' @param sampleKey: (\emph{file}) the file conforming with the normal welltrt columns
#' @import data.table
#' @return the dat.frame resulted from  fill_dose_for_well_trt
#' @export fill_dose_for_well_trt_wrapper

fill_dose_for_well_trt_wrapper <- function(sampleID_file){

  wellTrt <- read.csv(sampleID_file, na.strings = "", stringsAsFactors = FALSE) 
  wellTrt <- as.data.table(wellTrt)
  results <- fill_dose_for_well_trt(wellTrt)
  print(paste("results for fill_dose_for_well_trt_wrapper are list of " ,length(results), " elements"))
  return(results)
}
