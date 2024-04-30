. ~/.bashrc
#date '+%a %b %d %H:%M:%S %z %Y' > /.dockerrun  #no_docker

cd /httr/scripts

echo $PYTEST 

./load_fastq_fr_clowder.sh

if [ $PYTEST == "Y" ]
then
  cd ..
  pytest -v -s
  
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
  
else
  cd ..  
  s3fs $S3_BUCKET_NAME $S3_MOUNT_DIRECTORY
  python3 bin/align_and_count.py data/httrpl_automationTestData/config/test_httrpl_userConfig.json
fi