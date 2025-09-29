echo "running zenodo download"
declare -A zenodoVals
while IFS='|' read -r key value; do
    zenodoVals[$key]=$value
done < "zenodoValues"

mkdir zenodo_temp
cd zenodo_temp
wget -nv -O allfiles https://zenodo.org/api/records/15085866/files-archive
unzip allfiles

cd ../..

declare -a fastqs=("TC00283209_K19" "TC00283209_F02" "TC00283209_E07" "TC00283209_C07" "TC00283171_L10" "TC00283171_J02" "TC00283171_H13" "TC00283171_C09" "TC00283151_D21" "TC00283151_C16" )
 
  declare -a ext=(".fastq_0" ".fastq_1" ".fastq_2")
  for val in ${fastqs[@]}; do
    for ex in ${ext[@]};do
      echo $val$ex
      if [ ! -e $FASTQ_DATA_LOCATION$val$ex ] || [ $FORCE_FASTQ_TEST_DOWNLOAD == "Y" ]
      then  
        if  [ ! -e $FASTQ_DATA_LOCATION$val$ex ]; then 
          echo "not found" 
        else
          rm $FASTQ_DATA_LOCATION$val$ex
        fi
        
        actual=$(md5sum "scripts/zenodo_temp/${val}${ex}" | awk '{print $1 }' | xargs)
        
        key="${val}${ex}"
        value="${zenodoVals[$key]}"
        expected=$(echo "$value" | xargs)
          
        if [[ "$actual" != "$expected" ]]; then
          echo "Checksum mismatch: expected $expected but got $actual"
          exit 1
        fi        
        mv scripts/zenodo_temp/$val$ex $FASTQ_DATA_LOCATION$val$ex
        
      fi
    done
  done

  rm -r scripts/zenodo_temp

