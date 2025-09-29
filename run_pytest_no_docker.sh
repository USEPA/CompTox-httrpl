
. ~/.bashrc

. venv_httr_app/bin/activate

export PYTEST=Y
export RUN_TEST_WITH_NO_MONGO=N  # set to Y to run DB-free unit tests only, if N will run both MongoDB and DB-free unit tests
export CALIBRATE=N
export FORCE_FASTQ_TEST_DOWNLOAD=N
export FASTQ_DATA_LOCATION=data/httrpl_automationTestData/lionel_fastq/
export R_DATA_LOCATION=RTesting/DESeq2/
export DESeq2_unit_test_object_Rdata=6616b45fe4b063812d70f997
export PREFIX=https://clowder.edap-cluster.com/api/files/

#change as needed for MongoDB environment
export TEST_HOST=
export TEST_DB=sbox_${USER}

#MongoDB password keychain file location
export MONGOPW_FILE=~/.mongopw

export PARALLEL_CORES=3 #0 or absent means don't run in parallel

export LOAD_FROM=Clowder

cd httr/scripts
./load_fastq.sh

cd ..
pytest

cd $R_DATA_LOCATION
if [ ! -e DESeq2_unit_test_object.Rdata ] || [ $FORCE_FASTQ_TEST_DOWNLOAD == "Y" ]
then
  if [ ! -e DESeq2_unit_test_object.Rdata ]; then
    echo "R testing data not found" 
  else
    rm DESeq2_unit_test_object.Rdata
  fi
  wget -nv -O DESeq2_unit_test_object.Rdata $PREFIX$DESeq2_unit_test_object_Rdata
fi
cd ../..

Rscript RTesting/DESeq2/DESeq2_compartmentalized_unit_test.R 
Rscript RTesting/validateProbeManifest/unit_validateProbeManifest.R
