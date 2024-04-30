
declare -A clowderVals
while IFS='|' read -r key value; do
    clowderVals[$key]=$value
done < "clowderValues"

cd ..

cd $FASTQ_DATA_LOCATION
  declare -a fastqs=("TC00283209_K19" "TC00283209_F02" "TC00283209_E07" "TC00283209_C07" "TC00283171_L10" "TC00283171_J02" "TC00283171_H13" "TC00283171_C09" "TC00283151_D21" "TC00283151_C16" )
 
  declare -a ext=(".fastq_0" ".fastq_1" ".fastq_2")
  for val in ${fastqs[@]}; do
    for ex in ${ext[@]};do
      echo $val$ex
      if [ ! -e $val$ex ] || [ $FORCE_FASTQ_TEST_DOWNLOAD == "Y" ]
      then  
        if  [ ! -e $val$ex ]; then 
          echo "not found" 
        else
          rm $val$ex
        fi
        echo ${clowderVals[$val$ex]}
        wget -nv -O $val$ex $PREFIX${clowderVals[$val$ex]}
        
      fi
    done
  done
  
  cd ../../../..