#!/bin/bash

# Exit immediately on error
set -e

# Pipes should exit if there is an error, too
set -o pipefail

# Define the base directory for built collections
if [[ -z "${HTTRPL_DATA_DIR}" ]]; then
    export HTTRPL_DATA_DIR="/var/lib/httrpl"
fi

COLLECTION_DIR="${HTTRPL_DATA_DIR}"

# Define expected collection output paths
QC_COLLECTION="${COLLECTION_DIR}/httr_counts_qc"
WELL_COLLECTION="${COLLECTION_DIR}/httr_well"
TRT_GRP_COLLECTION="${COLLECTION_DIR}/httr_trt_grp_cmp"

timestamp=$(date +"%Y-%m-%d_%H-%M-%S")
LOGS_DIR="${COLLECTION_DIR}/logs/${timestamp}"
mkdir -p "$LOGS_DIR"

current_dir=$(pwd)
# change to the scripts directory
cd /workspace/httr/scripts/

# Run the steps

# Run QC Batch
if [ ! -f "$QC_COLLECTION" ]; then
    echo "Collection 'httr_counts_qc' not found. Running QC Batch..."
    Rscript qc_batch.r 2>&1 | tee "${LOGS_DIR}/qc_batch.log"
else
    echo "Collection 'httr_counts_qc' already exists. Skipping QC Batch."
fi

# Build all wells
if [ ! -f "$WELL_COLLECTION" ]; then
    echo "Collection 'httr_well' not found. Building httr_well collection..."
    python build_wells.py 2>&1 | tee "${LOGS_DIR}/build_wells.log"
else
    echo "Collection 'httr_well' already exists. Skipping well building."
fi

# Identify treatment group comparisons
if [ ! -f "$TRT_GRP_COLLECTION" ]; then
    echo "Collection 'httr_trt_grp_cmp' not found. Adding treatment groups..."
    Rscript build_treatment_groups.r 2>&1 | tee "${LOGS_DIR}/build_treatment_groups.log"
else
    echo "Collection 'httr_trt_grp_cmp' already exists. Skipping treatment group setup."
fi

# Run DESeq2
echo "Running DESeq2..."
mkdir -p "${COLLECTION_DIR}/deseq2_results"

Rscript run_deseq2.r 2>&1 | tee "${LOGS_DIR}/run_deseq2.log"

cd $current_dir