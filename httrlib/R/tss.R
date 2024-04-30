#' tss_deg_count: Function to simply count the number or fraction of probes with |l2fc| >= cutoff (with optional pvalue cutoff)
#' @param degs (\emph{data.frame}) = Results from httr_deg for a single trt_grp_id and anl_name, required.
#' @param abs_l2fc (\emph{numeric}) = Count probes with |l2fc| >= this cutoff, required.
#' @param pval (\emph{numeric}) = Additional threshold for p-value, ignore by default.
#' @param l2fc_col (\emph{character}) = Name of l2fc column, defaults to "log2FoldChange" to match DESeq2 output format
#' @param pval_col (\emph{character}) = Name of p-value column, defaults to "pvalue" to match DESeq2 output format
#' @param as_frac (\emph{logical}) = Whether or not to return result as a fraction of total probes in table, defaults to FALSE
#' @export tss_deg_count
#' @return (\emph{numeric or integer}) = Either the total number or fraction of probes passing the threshold criteria

# TO DO: Future improvement would be to calculate at gene level - when computing total numbers, map probes to genes and collapse duplicates

tss_deg_count <- function(
  degs, abs_l2fc, pval=NULL,
  l2fc_col="log2FoldChange", pval_col="pvalue", as_frac=FALSE
) {
  # Filter degs to just those passing the criteria:
  pass_degs <- degs[abs(degs[,l2fc_col]) >= abs_l2fc, ]
  # Optional p-value filter:
  if(!is.null(pval)) {
    pass_degs <- pass_degs[pass_degs[,pval_col] <= pval, ]
  }
  # TO DO: Score depends just on total number of rows for now, but could collapse to unique probes and/or gene IDs first
  # Return value depends on as_frac
  if(as_frac) {
    # Return proportion of degs in pass_degs
    return(nrow(pass_degs) / nrow(degs))
  } else {
    # Return the number of probes in pass_degs
    return(nrow(pass_degs))
  }
}


#' tss_l2_norm: Function to compute Euclidean (L2) norm for top/bottom N probes by l2fc
#'
#' @param degs (\emph{data.frame}) = Results from httr_deg for a single trt_grp_id and anl_name, required.
#' @param n (\emph{integer}) = Use the top and bottom n probes by l2fc
#' @param pval (\emph{numeric}) = If specified, change all l2fc to 0 for probes with p-values above this threshold before ranking, defaults to NULL
#' @param l2fc_col (\emph{character}) = Name of l2fc column, defaults to "log2FoldChange" to match DESeq2 output format
#' @param pval_col (\emph{character}) = Name of pvalue column, defaults to "pvalue" to match DESeq2 output format, only matters if pval is specified
#' @param rank_col (\emph{character}) = Which column to use for ranking the top/bottom N probes, defaults to l2fc_col but can rank by e.g. pval or SNR metric
#' @param rank_dir (\emph{character}) = Whether to use the top N, bottom N, or both top/bottom N when computing L2 Norm, defaults to both
#' @param debug (\emph{logical}) = Whether to print debug messages
#' @export tss_l2_norm
#' @return (\emph{numeric}) = The euclidean L2 norm of the top/bottom n l2fc values

# TO DO: Future improvement would be to calculate at gene level - when computing total numbers, map probes to genes and collapse duplicates
tss_l2_norm <- function(
  degs, n, pval=NULL,
  l2fc_col="log2FoldChange", pval_col="pvalue",
  rank_col=l2fc_col, rank_dir="both", 
  debug=getOption("debug", default = FALSE)
) {
  # rank_dir must be "both", "top", or "bottom"
  stopifnot(rank_dir %in% c("both","top","bottom"))
  # If pval cutoff defined, change all non-signif probes to l2fc=0
  if(!is.null(pval)) {
    ns_probes <- which(degs[, pval_col] > pval)
    if(debug) {
      cat("Setting", l2fc_col, "= 0 for", length(ns_probes), "probes with", pval_col, ">", pval, "\n")
    }
    degs[ns_probes, l2fc_col] <- 0
    # Warn if this 0'd out more than half of the signature probes
    if( (nrow(degs) - length(ns_probes)) < n ) {
      warning("After setting", l2fc_col, "= 0 when", pval_col, ">", pval, "fewer than n =", n, "probes remain, TSS values will be severely truncated.\n")
    } 
  }
  # Get the row numbers for top n probes by rank_col:
  top_n <- order(degs[,rank_col], decreasing = TRUE)[1:n]
  # Get the row numbers for bottom n probes by rank_col:
  bottom_n <- order(degs[,rank_col], decreasing = FALSE)[1:n]
  # Optional debug output
  if(debug) {
    if(rank_dir %in% c("top","both")) {
      cat("Top", n, "probes by", rank_col, "have", l2fc_col, "ranging from", min(degs[top_n, l2fc_col]), "to", max(degs[top_n,l2fc_col]), "\n")
    }
    if(rank_dir %in% c("bottom","both")) {
      cat("Bottom", n, "probes by", rank_col, "have", l2fc_col, "ranging from", max(degs[bottom_n, l2fc_col]), "to", min(degs[bottom_n, l2fc_col]), "\n")
    }
  }
  # Make sure top_n and bottom_n sets don't overlap - warn if so
  if((rank_dir == "both") && (length(intersect(top_n, bottom_n)) > 0)) {
    # Most likely cause is nrows(degs) < 2*n
    warning("Top and Bottom probe sets overlap when computing L2 norm on top+bottom ", n, "probes for degs table with ", nrow(degs), "probes.\n")
  }
  # Now compute L2 Euclidean Norm:
  use_l2fc <- degs[union(top_n, bottom_n), l2fc_col]
  # Note: can also use base R function 'norm' here
  l2_norm <- sqrt(sum(abs(use_l2fc) ^ 2))
  return(l2_norm)
}
