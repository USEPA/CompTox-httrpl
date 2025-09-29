#. ~/.bashrc
export TEST_HOST=  # Set to MongoDB server URL
export TEST_DB=sbox_${USER}
#mongo password keychain file location
export MONGOPW_FILE=~/.mongopw

#cd httr/Rlib
Rscript sampleID_wrapper.R
