#' get_global_option
#' Function to find the value of the passed global_var
#' first checks if old name for global var exists and returns that and a warning
#' if not, then just simply return the value corresponding to the passed global var
#'
#' @param global_var (\emph{character}) = may be any of the following values: min_mapd_frac, min_n_reads_mapd, min_n_sig80, min_n_cov5, 
#' max_top10_prop, max_gini_coef, qc_flags
#' @return numeric
#' @export get_global_option

get_global_option <- function(global_var){

  new_to_old_map <- list(min_mapd_frac='httrMinMapdFrac',min_n_reads_mapd='httrMinMapdN',min_n_sig80='httrMinNsig80',min_n_cov5='httrMinNcov5',max_top10_prop='httrMaxTop10Prop',max_gini_coef='httrMaxGini',qc_flags='httrQCflags')

# if related but old global_var exists, use it but issue obscolete warning
  old_var <- getOption(new_to_old_map[[global_var]])
  if (!is.null(old_var)){
    message(paste(new_to_old_map[[global_var]]), " is now obsolete. Instead use ", global_var)
    return(old_var)
  }
  else{
    return(getOption(global_var))
  }
}