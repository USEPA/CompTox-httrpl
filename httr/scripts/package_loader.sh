if [ ! -e RpackagesDir ]
then
  mkdir RpackagesDir
fi

cd RpackagesDir

while read -r line; do
  line=$(echo "$line" | tr -d '\r')  
  original_url="http://cran.r-project.org/src/contrib/$line.tar.gz"
  filename=$(basename "$original_url")
  product=$(echo "$filename" | cut -d'_' -f1)
  wget -nv -O pkg "$original_url" 
 
  if [ $? -eq 0 ]; then 
    echo "Download successful!"
  else
    echo "Download failed. Trying alternate URL..." 
    alt_url="https://cran.r-project.org/src/contrib/Archive/$product/$filename"
    echo ${alt_url}
    wget  -nv -O pkg "$alt_url" 
    if [ $? -eq 0 ]; then 
      echo "Alternate download successful!" 
    else
      echo "Alternate download failed." 
    fi 
  fi
  R CMD INSTALL --build pkg
done < "../httr/scripts/Rpackages"
