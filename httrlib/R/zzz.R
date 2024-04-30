#' options set upon package loading

.onLoad <- function(lib, pkg) {
#' Options used for the QC flag calls - to skip any of these remove them from httrQCflags
#' httrMinMapdFrac = Flag wells with mapd_frac < this value, default 0.5
  if(is.null(getOption("min_mapd_frac"))) {
    options(min_mapd_frac = 0.5)
  }
#' httrMinMapdN = Flag wells with n_reads_mapd < this value, default 300k
  if(is.null(getOption("min_n_reads_mapd"))) {
    options(min_n_reads_mapd = 3*(10**5))
  }
#' httrMinNsig80 = Flag wells with n_sig80 < this value, default 1000
  if(is.null(getOption("min_n_sig80"))) {
    options(min_n_sig80 = 1000)
  }
#' httrMinNcov5 = Flag wells with n_cov5 < this value, default 5000
  if(is.null(getOption("min_n_cov5"))) {
    options(min_n_cov5 = 5000)
  }
#' httrMaxTop10Prop = Flag wells with top10_prop > this value, default 0.1 (but NOT used)
  if(is.null(getOption("max_top10_prop"))) {
    options(max_top10_prop = 0.1)
  }
#' httrMaxGini = Flag wells with gini_coef > this value, default 0.95
  if(is.null(getOption("max_gini_coef"))) {
    options(max_gini_coef = 0.95)
  }
#' httrQCflags = Vector of QC flags, order defines flag priority (first flag that hits will be used) - default is to skip HIGH_TOP10 flag
  if(is.null(getOption("qc_flags"))) {
    options(qc_flags = c("LOW_MAPD_FRAC","LOW_READS","LOW_NSIG80","LOW_NCOV5","HIGH_GINI"))
  }
#' min_colsum = floor value under which sample_id are removed 
  if(is.null(getOption("min_colsum"))) {
    options(min_colsum = 100000)
  }
  
#' mean_cnt (\emph{integer}) = floor value to exclude samples with colsum below this value
  if(is.null(getOption("mean_cnt"))) {
    options(mean_cnt = 5)
  }
}

