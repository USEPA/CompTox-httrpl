. ~/.bashrc

. venv_httr_app/bin/activate

# Set TEST_HOST to the MongoDB server and TEST_DB to the DB you want to validate
export TEST_HOST=
export TEST_DB=sbox_${USER}
# Set HTTR_CELERY_MP=Y for multi-threading (requires write permission to TEST_DB)
export HTTR_CELERY_MP=N

#mongo password keychain file location
export MONGOPW_FILE=~/.mongopw

cd httr

python bin/schema_check.py
