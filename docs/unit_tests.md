----------

Unit Tests
----------

The httrpl package contains a variety of unit tests to check for proper installation, that required dependencies are present,
and to confirm that code improvements don't impact reproducibility of the previous pipeline versions.

### Running the standard unit tests

#### For Linux

1. Make sure you have a sandbox MongoDB database named: sbox_{UID} where {UID} is your login id and where you have both read and write permissions

2. Make sure you have the file `.mongopw` in your home directory. *(See below for skipping all MongoDB-based unit tests if you only plan to use the pipeline in DB-free mode)*
  + This file stores credential information that is required for connecting to MongoDB.
  + The file must be JSON format where each object has the following fields: `[{"host":<>, "db":"sbox_<>", "user":<>, "passwd":<>,"authMechanism":"SCRAM-SHA-256", "authSource":"admin"}]`.
  + This file must contain the hostname and user credentials for the sandbox database.
  + You can update this file using a text editor, or it can be created with the `bin/addKey.py` script:
```bash
python3 (localpath)/httrpl/httr/bin/addKey.py
```

3. Unit tests are executed by the `run_pytest_no_docker.sh` shell script. Please edit the exported environment variables and adjust them as needed. Additionally, if using MongoDB, `TEST_HOST` and `TEST_DB` should be changed to reflect your MongoDB environment. After making any needed changes, you can then run the unit tests using:
```bash
(localpath)/httrpl/run_pytest_no_docker.sh
```

*NOTES:* 

+ The unit test script will download test fastq files from Clowder. You need to make sure the fastq files are located in (test_httrpl_userConfig.json): `"fq_path": "data/httrpl_automationTestData/lionel_fastq/"
  + This path is relative to the path containing the test_httrpl_userConfig.json file
+ OR, change that variable to the folder containing the following files: `TC00283151_C16.fastq` and `TC00283171_J02.fastq`


#### For Docker

Edit the `httr_settings.env` file and set `PYTEST=Y`. Additionally, if using MongoDB, `TEST_HOST` and `TEST_DB` should be changed to reflect your MongoDB environment.

You also will need to set the `FASTQ_TEST_LOCATION` variable in that file to point to the folder on Clowder that contains the fastq files needed to run the test

You can potentially force the download from clowder of all the fastq file by setting the environment variable to Y:
  `FORCE_FASTQ_TEST_DOWNLOAD=Y`

This will be used for the `start.cmd` script to know to run the tests and not the pipeline

Then:
```bash
. venv_httr_app
docker-compose --file local-docker-compose.yml --file docker-compose.yml --env-file httr_settings.env up
```
or simply: 
```bash
./up.sh
```


### Running DB schema and data integrity unit tests

1.  Run the standard unit test script: `build_pytest_env_no_docker.sh` -- this will install redis and celery, necessary to provide multi-processing capabilities when HTTR_CELERY_MP=Y
redis is securily started in the background using a password
and celery runs in the background connecting to redis using password

2. Look at the file `run_db_validation_tests.sh` and edit the `TEST_DB` and `TEST_HOST` variables if necessary to your MongoDB environment. Also edit the HTTR_CELERY_MP variable if desired (if `HTTR_CELERY_MP=Y`, then validation tests run in parallel using celery over redis; if `HTTR_CELERY_MP=N` then validation tests run sequentially and take about 8 times the time it takes to run them in parallel).

3. Then run the DB validation script after making any necessary changes to exported environment variables, including MongoDB information if needed:
```bash
(localpath)/httrpl/run_db_validation_tests.sh
```


### Additional Notes:

#### Calibration:

Some of the tests compare running the pipeline and observing the db results with pre-recorded results. 
When a new test is added that needs to make that comparison, it is easier to write out to a file what the data looks like and use that file going forward for comparison than fetching the data from the db and build a file from there.
Also, when functionality changes and affects the output to the db, it may be necessary to take a snapshot of what will become the new reference for comparisons going forward.
Use the `CALIBRATE` parameter to the unit test scripts in the `run_pytest_no_docker.sh` (for Linux)  or `httr_settings.env` (for Docker) to update the reference data for unit tests.
`CALIBRATE=True` will force the tests to capture data from multiple unit tests and save it to a file.
`CALIBRATE=False` will simply run the tests.

#### Parallel Processing

You can specify the number of cores to use for unit tests *(e.g. for `getFCMatrix()` function)* by setting:

+ For Docker: `PARALLEL_CORES=<nb of cores>` in the `httr_settings.env`
+ Without Docker: `PARALLEL_CORES=<nb of cores>` in `run_pytest_no_docker.sh`

If this parameter is set to 1 or `NULL`, then all R functions run sequentially.
If this parameter is absent or set to 0, then `getFCMatrix()` will pick the number of cores in the system (Ncores-1) and run in parallel.

#### Running in DB-Free Mode

By default, the unit tests cover ***both*** MongoDB and DB-free functionality.
If you do not have access to a MongoDB server and only plan to use the pipeline in DB-free mode, change the parameter `RUN_TEST_WITH_NO_MONGO` to `Y` in the appropriate file:

+ For Docker, change in `httr_settings.env`
+ Without Docker, change in `run_pytest_no_docker.sh`

