#' countStats
#' Function to compute count-based QC statistics for a single sample
#'
#' @param counts (\emph{integer}) = a vector of counts for a single sample, w/ or w/out 0 counts
#'  All of these statistics should be invariant to the probe labels, ordering of the counts, or whether 0 counts are missing
#' @param ncov (\emph{integer}) = Minimum read count(s) for Ncov statistic (number of probes covered at a minimum read count level)
#' @param nsig (\emph{numeric}) = Proportion of signal for Nsig statistic (minimum number of probes capturing top X% of signal)
#' @param topn (\emph{integer}) = Number of probes to use for TopN statistic (proportion of signal captured by top probe)
#' @export countStats
#' @import foreach
#' @return (\emph{named list}) = each of the requested statistics, member names now match DB schema

countStats <- function(
  counts, 
  ncov=getOption("httrStatNCov", default=5), 
  nsig=getOption("httrStatNsig", default=0.8), 
  topn=getOption("httrStatTopN", default=10)
) {

  
  # Load required pkgs
	# Store results in a list
	st <- list()
	# Compute the number of probes with reads >= ncov
	if(length(ncov) > 0) {
		nc <- foreach(minrd=ncov, .combine='c') %do% {sum(counts >= minrd)}
		names(nc) <- paste0("n_cov",ncov)
		st <- append(st, nc)
	}
	# Drop probes w/ no reads (this is for efficiency, should not impact final Nsig values)
	counts_nz <- counts[counts > 0]
	# Sort in decreasing order
	counts_nz <- sort(counts_nz, decreasing = T)
	# Compute cumulative sum of counts starting from highest count probe
	cumCounts <- cumsum(counts_nz)
	# Convert cumulative sums to cumulative proportions
	cumProp <- cumCounts / sum(counts_nz)
	# Compute the minimum number of probes to capture X% of total reads
	if(length(nsig) > 0) {
		ns <- foreach(prop=nsig, .combine='c') %do% {min(which(cumProp > prop))}
		names(ns) <- paste0("n_sig",nsig*100)
		st <- append(st, ns)
	}
	# Compute the total proportion captured by top N reads
	if(length(topn) > 0) {
		tn <- cumProp[topn]
		names(tn) <- paste0("top",topn,"_prop")
		st <- append(st, tn)
	}
	return(st)
}


#' countGini
#' Computing Gini coefficient
#' Use reldist::gini(counts) where counts = probe counts for a single sample
#' This metric DOES depend on whether 0 counts are present or missing from the vector
#' If zero counts are missing, should specify proben to fill in missing 0s
#' 
#' @param counts (\emph{integer}) = a vector of counts for a single sample, w/ or w/out 0 counts
#' @param proben (\emph{integer}) = Total number of probes used for Gini coefficients across samples, ignored if NULL
#' @param debug (\emph{logical}) = Whether or not to report extra debug messages
#' @export countGini
#' @return (\emph{numeric}) = Gini coefficient returned from reldist::gini


countGini = function(counts, proben=getOption("httrProbeN"), debug=getOption("debug",default=FALSE)) {
  if (!requireNamespace("reldist", quietly = TRUE)) {
    stop(
      "Package \"reldist\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if(!is.null(proben)) {
    if(proben < length(counts)) {
      # Warn if proben < number of probe counts provided
      warning("proben < length(counts), no zero counts added to counts vector.\n")
    } else if(proben > length(counts)) {
      # Warn if there are zero counts in the counts vector already
      if(any(counts == 0)) {
        warning("proben > length(counts), but counts already includes some 0 values.\n")
      }
      zero_counts <- rep(as.integer(0), times=proben-length(counts))
      counts <- c(counts, zero_counts)
      if(debug) {
        cat("Inferring", length(zero_counts), "missing probes with count = 0, total probes =", length(counts), "\n")
      }
    }
  }
  # Pass counts to reldist::gini and return the result
  return(reldist::gini(counts))
}

#' countQCflag
#' Given a list structure with all the quantitative fields in the httr_counts_qc schema, determine the qc_flag field
#' Flags are checked in priority order according to qc_flags parameter
#' NOTE: If any required fields are missing from qc_stats, it will lead to an error
#'
#' @param qc_stats (\emph{list}) = list of all QC stats computed by countStats and countGini above
#' @param min_mapd_frac (\emph{numeric}) = minimum mapd_frac, flag with LOW_MAPD_FRAC below this cutoff
#' @param min_n_reads_mapd (\emph{integer}) = minimum n_reads_mapd, flag with LOW_READS below this cutoff
#' @param min_n_sig80 (\emph{integer}) = minimum n_sig80, flag with LOW_NSIG80 below this cutoff
#' @param min_n_cov5 (\emph{integer}) = minimum n_cov5, flag with LOW_NCOV5 below this cutoff
#' @param max_top10_prop (\emph{numeric}) = maximum top10_prop, flag with HIGH_TOP10 above this cutoff
#' @param max_gini_coef (\emph{numeric}) = maximum gini_coef, flag with HIGH_GINI above this cutoff
#' @param qc_flags (\emph{character vector}) = list of flags to apply in priority order
#' @param debug (\emph{logical}) = Whether to report some debug info
#' @export countQCflag 
#' @return (\emph{character}) = a single QC flag for the well, "OK" if not thresholds were hit

countQCflag <- function(
  qc_stats,
  min_mapd_frac = get_global_option("min_mapd_frac"),
  min_n_reads_mapd = get_global_option("min_n_reads_mapd"),
  min_n_sig80 = get_global_option("min_n_sig80"),
  min_n_cov5 = get_global_option("min_n_cov5"),
  max_top10_prop = get_global_option("max_top10_prop"),
  max_gini_coef = get_global_option("max_gini_coef"),
  qc_flags = get_global_option("qc_flags"),
  debug=getOption("debug", default=FALSE)
) {

  if(debug) {
    cat("Checking for flags in the following order: ", paste(qc_flags, collapse=", "), "\n")
  }
  # Loop over qc_flags, apply each threshold and return the first flag that fails
  # TO DO: For loop here may be slow - there may be faster ways to do while still allowing flexible order
  for(flag in qc_flags) {
    if(flag == "LOW_MAPD_FRAC") {
      if(qc_stats$mapd_frac < min_mapd_frac) return(flag)
    } else if(flag == "LOW_READS") {
      if(qc_stats$n_reads_mapd < min_n_reads_mapd) return(flag)
    } else if(flag == "LOW_NSIG80") {
      if(qc_stats$n_sig80 < min_n_sig80) return(flag)
    } else if(flag == "LOW_NCOV5") {
      if(qc_stats$n_cov5 < min_n_cov5) return(flag)
    } else if(flag == "HIGH_TOP10") {
      if(qc_stats$top10_prop > max_top10_prop) return(flag)
    } else if(flag == "HIGH_GINI") {
      if(qc_stats$gini_coef > max_gini_coef) return(flag)
    } else {
      # Unknown flag - generate a warning
      warning("Unknown QC flag requested: ", flag, "\n")
    }
  }
  # No flags hit, return "OK"
  return("OK")
}


#' countQC
#' Given a document from httr_counts collection, compute all QC stats and flags and format according to httr_counts_qc schema
#' option() function should be used to change defaults for underlying QC functions
#' 
#' @param counts_doc (\emph{list}) = Follows DB schema for httr_counts, msut include the _id field
#' @param bad_probes (\emph{character vector}) = List of probe IDs that should be masked
#' @param proben (\emph{integer}) = Total number of probes on the platform (length bad_probes will be subtracted before passing to gini function)
#' @param calc_gini (\emph{logical}) = Whether or not to calculate gini, default True but can override if no reldist package installed
#' @param calc_flag (\emph{logical}) = Whether or not to calculate qc_flag, default True but when set to False all flags will be set to "OK"
#' @param debug (\emph{logical}) = Whether to report additional debug info
#' @export countQC

countQC <- function(
  counts_doc, 
  bad_probes=c(), 
  proben=getOption("httrProbeN"), 
  calc_gini=getOption("httrStatGini", default=TRUE),
  calc_flag=TRUE,
  min_mapd_frac = get_global_option("min_mapd_frac"),
  min_n_reads_mapd = get_global_option("min_n_reads_mapd"),
  min_n_sig80 = get_global_option("min_n_sig80"),
  min_n_cov5 = get_global_option("min_n_cov5"),
  max_top10_prop = get_global_option("max_top10_prop"),
  max_gini_coef = get_global_option("max_gini_coef"),
  qc_flags = get_global_option("qc_flags"),
  debug=getOption("debug",default=FALSE)) {

  # Do some sanity checks
  if(!is.null(proben)) {
    if(proben <= length(bad_probes)) {
      warning("proben is <= the number of bad_probes.\n")
    }
  }
  req_fields <- c("sample_id", "_id", "n_reads", "n_reads_mapd", "probe_cnts")
  if(!all(req_fields %in% names(counts_doc))) {
    warning("counts_doc missing required field(s): ", paste(setdiff(req_fields, names(counts_doc)), collapse=", "), "\n")
  }
  
  # Start building document for httr_counts_qc
  qc_doc <- list(sample_id=counts_doc$sample_id, count_id=counts_doc[["_id"]], n_reads=counts_doc$n_reads)
  
  # Remove bad_probes and compute total reads removed
  probe_cnts <- counts_doc$probe_cnts
  if(length(bad_probes) > 0 && any(bad_probes %in% names(probe_cnts))) {
    qc_doc$bad_probe_count <- sum(probe_cnts[bad_probes])
    keep_probes <- !(names(probe_cnts) %in% bad_probes)
    probe_cnts <- probe_cnts[keep_probes]
  } else {
    qc_doc$bad_probe_count <- as.integer(0)
    if(debug) {
      if(length(bad_probes) == 0) {
        cat("No bad_probes specified.\n")
      } else if(length(intersect(bad_probes,names(probe_cnts)))==0) {
        cat("probe_cnts does not contain any bad_probes.\n")
      }
    }
  }
  
  # Recompute n_reads_mapd and mapd_frac after removal of bad_probes
  qc_doc$n_reads_mapd <- counts_doc$n_reads_mapd - qc_doc$bad_probe_count
  if(sum(probe_cnts) != qc_doc$n_reads_mapd) {
    warning("sum(probe_cnts) != n_reads_mapd after removal of bad_probes - counts_doc$n_reads_mapd may be incorrect?\n")
  }
  qc_doc$mapd_frac <- qc_doc$n_reads_mapd / qc_doc$n_reads
  
  # Compute count-based QC statistics:
  qc_doc <- append(qc_doc, countStats(probe_cnts))
  
  # (Optional) Compute Gini coefficient:
  if(calc_gini) {
    qc_doc$gini_coef <- countGini(probe_cnts, proben=proben-length(bad_probes), debug=debug)
  }

  # Assign qc_flag
  if(calc_flag) {
    qc_doc$qc_flag <- countQCflag(qc_doc, min_mapd_frac, min_n_reads_mapd, min_n_sig80, min_n_cov5, max_top10_prop, max_gini_coef, qc_flags, debug=debug)
  } else {
    # When calc_flag == FALSE, just assign "OK"
    qc_doc$qc_flag <- "OK"
  }
  
  # Return the final qc_doc object
  return(qc_doc)
}

#' outerFences
#' Function to compute "Tukey's Outer Fence" for a set of values
#' Computes both lower and upper thresholds, which are 3*IQR beyond the lower and upper quartile respectively
#' 
#' @param x (\emph{numeric vector}) = Set of values to compute fences for
#' @param iqr.factor (\emph{numeric}) = Multiplier for IQR in the fence computation, default is 3
#'
#' @return (\emph{numeric}, length 2) = Lower and upper fence values
#' @export outerFences
outerFences <- function(x, iqr.factor = 3) {
  x_iqr <- IQR(x)
  x_q1 <- quantile(x, 0.25)
  x_q3 <- quantile(x, 0.75)
  x_lower <- x_q1 - (iqr.factor * x_iqr)
  x_upper <- x_q3 + (iqr.factor * x_iqr)
  return(c(x_lower, x_upper))
}

#' getQCdefault
#' Function to determine appropriate threshold for a QC metric when not specified:
#' NOTE: When the relevant flag is not in httrQCflags, this will always return NULL
#'
#' @param qc_metric (\emph{character}) = Name of the QC metric to get default for
#' @export getQCdefault
#' @return (\emph{numeric}) = Value of default threshold, or NULL if not thresholding on this metric
#'
getQCdefault <- function(qc_metric) {
  if((qc_metric == "mapd_frac") && ("LOW_MAPD_FRAC" %in% get_global_option("qc_flags"))) {
    return(get_global_option("min_mapd_frac"))
  }
  # Return read depth threshold for both total reads and mapd, +/- log10 scale conversion
  if((qc_metric %in% c("n_reads", "n_reads_mapd", "log10_n_reads", "log10_n_reads_mapd")) && ("LOW_READS" %in% get_global_option("qc_flags"))) {
    cutoff <- getOption("min_n_reads_mapd")
    if((qc_metric %in% c("log10_n_reads", "log10_n_reads_mapd")) && !is.null(cutoff)) {
      return(log10(cutoff))
    } else {
      return(cutoff)
    }
  }
    
  if((qc_metric == "n_sig80") && ("LOW_NSIG80" %in% get_global_option("qc_flags"))) {
    return(get_global_option("min_n_sig80"))
  }
  if((qc_metric == "n_cov5") && ("LOW_NCOV5" %in% get_global_option("qc_flags"))) {
    return(get_global_option("min_n_cov5"))
  }
  if((qc_metric == "top10_prop") && ("HIGH_TOP10" %in% get_global_option("qc_flags"))) {
    return(get_global_option("max_top10_prop"))
  }
  if((qc_metric == "gini_coef") && ("HIGH_GINI" %in% get_global_option("qc_flags"))) {
    return(get_global_option("max_gini_coef"))
  }
  return(NULL)
}

#' addQCplotCols
#' Function to adjust well treatment data in a way that is better for QC plotting
#' Reclassifies stype/rna_src column to "scat" column, and converts to a factor
#' and creates log10 scale columns from n_reads and n_reads_mapd, and bad_probe_prop if necessary cols present
#' NOTE: Requires data.table package
#'
#' @param data (\emph{data.frame} or \emph{data.table}) = Table containing all well treatment and QC info, e.g. as returned by getWellInfo()
#' @import data.table
#'
#' @return  (\emph{data.table}) = data with additional columns
#' @export addQCplotCols

addQCplotCols <- function(data) {

  
  # Convert data to data.table
  if(!("data.table" %in% class(data))) {
    data <- as.data.table(data)
  }
  
  # Create scat column if not present yet, and all other req'd columns present
  if(all(c("stype", "rna_src", "qc_flag") %in% colnames(data)) && !("scat" %in% colnames(data))) {
    data[, scat := stype]
    # Split stype="QC sample" into "Lysis Buffer", "Bulk Lysate", and "reference RNA" for these plots
    data[rna_src %in% c("Lysis Buffer", "Bulk Lysate"), scat := rna_src]
    data[rna_src %in% c("UHRR", "HBRR"), scat := "reference RNA"]
    data[(qc_flag == "CELL_VIABILITY") & (stype == "test sample"), scat := "dead cells"]
    # Next convert stype to factors with shorter names for plot labels
    map_scat <- c(
      "blank"="Lysis Buffer", 
      "ref_rna"="reference RNA", 
      "bulk_ctrl"="Bulk Lysate", 
      "untrtd"="untreated control", 
      "veh_ctrl"="vehicle control", 
      "viab_ctrl"="viability control",
      "ref_chem"="reference chemical", 
      "test_samp"="test sample",
      "test_dead"="dead cells"
    )
    if(!all(data$scat %in% map_scat)) {
      warning("Some samples could not be categorized in scat column.\n")
    }
    data[, scat := factor(scat, levels=map_scat, labels=names(map_scat))]
  }
  
  # Convert n_reads and n_reads_mapd to log10 scale (Any cases with values <= 0 will be mapped to 0 and a warning will be generated)
  if("n_reads" %in% colnames(data)) {
    data[, log10_n_reads := log10(n_reads)]
    if(any(data$n_reads <= 0)) {
      warning("Some n_reads values are <= 0, mapped to 0 on log10 scale.\n")
      data[n_reads <= 0, log10_n_reads := 0]
    }
  }
  if("n_reads_mapd" %in% colnames(data)) {
    data[, log10_n_reads_mapd := log10(n_reads_mapd)]
    if(any(data$n_reads_mapd <= 0)) {
      warning("Some n_reads_mapd values are <= 0, mapped to 0 on log10 scale.\n")
      data[n_reads_mapd <= 0, log10_n_reads_mapd := 0]
    }
  }
  # Compute bad_probe_prop = bad_probe_count / (bad_probe_count + n_reads_mapd)
  if(all(c("bad_probe_count", "n_reads_mapd") %in% colnames(data))) {
    data[, bad_probe_prop := bad_probe_count / (bad_probe_count + n_reads_mapd)]
  }
  
  # Return the final data object
  return(data)
}


#' plotQCviolin
#' Function for drawing violin plot of a QC metric grouped by an experimental design factor
#'
#' @param qc_metric (\emph{character}) = Name of the QC metric column in data to plot
#' @param design_fac (\emph{character}) = Name of the design factor column in data to group by
#' @param data (\emph{data.frame}) = Table of all QC/meta-data for plotting
#' @param cutoff (\emph{numeric}) = Single value for drawing a horizontal dashed line, denoting current threshold - if NULL, will attempt to fill in with appropriate default
#' @param alt_cutoff (\emph{numeric}) = Single value for drawing a horizontal dotted line, denoting alternate threshold - if NULL, not drawn
#' @param title (\emph{character}) = Title for the plot, optional
#' @param plot.ylim (\emph{numeric}, length 2) = Set lower and upper limits for Y axis for consistent plotting, if NULL plots will auto-decide
#' @return (\emph{ggplot2}) object that can be further modified or drawn with print
#' @export plotQCviolin

plotQCviolin <- function(
  qc_metric,
  design_fac,
  data,
  cutoff=getQCdefault(qc_metric),
  alt_cutoff=NULL,
  title=NULL,
  plot.ylim=NULL
) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  # Setup initial violin plot
  vPlot <- ggplot(data, aes_string(x=design_fac, y=qc_metric, color=design_fac)) + geom_violin(trim = T)
  # Add additional markings showing median and IQR
  vPlot <- vPlot + stat_summary(fun.data=median_hilow, fun.args=list(conf.int=0.5), geom="pointrange", shape=18)
  # Rotate x-axis labels 45 degrees IF they are longer than a few chars
  if(any(nchar(levels(data[[design_fac]])) > 3)) {
    vPlot <- vPlot + theme(axis.text.x=element_text(angle=45, hjust=1))
  }
  # Add cut-off line(s) if not NULL
  if(!is.null(cutoff)) {
    vPlot <- vPlot + geom_hline(yintercept = cutoff, linetype=2)
  }
  if(!is.null(alt_cutoff)) {
    vPlot <- vPlot + geom_hline(yintercept = alt_cutoff, linetype=3)
  }
  # Add title if not NULL
  if(!is.null(title)) {
    vPlot <- vPlot + ggtitle(title)
  }
  # Add ylim if not NULL
  if(!is.null(plot.ylim)) {
    vPlot <- vPlot + ylim(plot.ylim[1], plot.ylim[2])
  }
  return(vPlot)
}


#' plotQCjitter
#' Function for drawing jitter plot of a QC metric grouped by an experimental design factor
#'
#' @param qc_metric (\emph{character}) = Name of the QC metric column in data to plot
#' @param design_fac (\emph{character}) = Name of the design factor column in data to group by
#' @param data (\emph{data.frame}) = Table of all QC/meta-data for plotting
#' @param cutoff (\emph{numeric}) = Single value for drawing a horizontal dashed line, denoting current threshold - if NULL, will attempt to fill in with appropriate default
#' @param alt_cutoff (\emph{numeric}) = Single value for drawing a horizontal dotted line, denoting alternate threshold - if NULL, not drawn
#' @param title (\emph{character}) = Title for the plot, optional
#' @param plot.ylim (\emph{numeric}, length 2) = Set lower and upper limits for Y axis for consistent plotting, if NULL plots will auto-decide
#' @return (\emph{ggplot2}) object that can be further modified or drawn with print
#' @export plotQCjitter

plotQCjitter <- function(
  qc_metric, 
  design_fac, 
  data, 
  cutoff=getQCdefault(qc_metric),
  alt_cutoff=NULL,
  title=NULL,
  plot.ylim=NULL
) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  
  # Setup initial jitter plot
  jPlot <- ggplot(data, aes_string(x=design_fac, y=qc_metric, color=design_fac)) + geom_jitter(position=position_jitterdodge(jitter.width=1, seed=516821), size=1)
  # Add additional markings showing median and IQR
  jPlot <- jPlot + stat_summary(fun.data=median_hilow, fun.args=list(conf.int=0.5), geom="pointrange", shape=18, colour="black")
  # Rotate x-axis labels 45 degrees IF they are longer than a few chars
  if(any(nchar(levels(data[[design_fac]])) > 3)) {
    jPlot <- jPlot + theme(axis.text.x=element_text(angle=45, hjust=1))
  }
  # Add cut-off line(s) if not NULL
  if(!is.null(cutoff)) {
    jPlot <- jPlot + geom_hline(yintercept = cutoff, linetype=2)
  }
  if(!is.null(alt_cutoff)) {
    jPlot <- jPlot + geom_hline(yintercept = alt_cutoff, linetype=3)
  }
  # Add title if not NULL
  if(!is.null(title)) {
    jPlot <- jPlot + ggtitle(title)
  }
  # Add ylim if not NULL
  if(!is.null(plot.ylim)) {
    jPlot <- jPlot + ylim(plot.ylim[1], plot.ylim[2])
  }
  return(jPlot)
}

