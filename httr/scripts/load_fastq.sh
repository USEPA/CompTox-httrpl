#!/bin/bash

if [[ "$LOAD_FROM" == "Zenodo" ]]; then
  ./load_fastq_fr_zenodo.sh
else
  ./load_fastq_fr_clowder.sh
fi