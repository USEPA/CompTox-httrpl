HTTr Pipeline Code (httrpl)
===========================


System Requirements
-------------------

This software pipeline provides scripts and APIs in both python 3 and R.
To access pipeline outputs from existing databases, only R is required, and can be used on either Windows or Linux.
For pipelining new data, both python 3 and R are required on a Linux-based system, and MongoDB is used as the (optional) database backend.


### R

All R code in this pipeline has been encapsulated in the R package `httrlib` contained within this repository.
All R code has been tested on R 3.6.0, but any version of R 3.x or 4.x should be compatible,
***except*** with regards to controlling the version of `DESeq2` noted below.
However, differences between versions of other R package dependencies may impact certain aspects of the pipeline due to changes in functionality or API.

The following R packages are required to pull existing HTTr data from MongoDB:

+ [mongolite](https://cran.r-project.org/web/packages/mongolite/index.html) - Required for DB access in R, code has been tested with v2.1.0 of this package.
+ [jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html) - Required for DB access and handling JSON files, code has been tested with v1.6 of this package.

Additional R packages are required to support pipelining of new HTTr data:

+ [reldist](https://cran.r-project.org/web/packages/reldist/index.html) - Required for computing Gini coefficients when applying QC checks on Level 1 count data, code has been tested with v1.6-6 of this package.
+ [DESeq2](https://bioconductor.org/packages/3.9/bioc/html/DESeq2.html) - Required for differential expression analysis, code has been tested and all published analysis run with v1.24.0 of this package, which is tied to R v3.6 and BioConductor v3.9.


### Python

All code in this package has been tested on Python 3.6 - compatibility with newer and older versions has not been tested.

Once Python 3 and associated pip tool is installed, the following python libraries should be installed for the individual user:
```bash
pip3 install --user pandas pymongo mongoengine
```


### Additional Tools

The alignment step relies on two command-line tools that need be installed and available in the user's `PATH` environment variable:

+ [HISAT2](http://daehwankimlab.github.io/hisat2/download/) - Current version of the pipeline requires [v2.1.0](http://daehwankimlab.github.io/hisat2/download/#version-hisat2-210) to ensure reproducibility of the results.
+ [Samtools](http://www.htslib.org/) - Current version of the pipeline has been tested with [v1.9](https://github.com/samtools/samtools/releases/tag/1.9) only, other versions have not been tested for compatibility. 

The default behavior is to use [MongoDB](https://www.mongodb.com/try/download/community) as the database back end, which is highly recommended for data sets with >1,000 samples.
However, there is also an option to store and access similar data structures as JSON files stored in a directory, with no database manager system.



------------------------

Full httrpl Installation
------------------------

Follow these instructions to install the complete python and R environment with all required tools for pipelining new HTTr data.

The full httrpl environment can either be built as a python virtual environment, or as a Docker environment, as described below.

*Note:* All paths in this README assume a user has cloned the repository to `(localpath)/httrpl`. This path may be different depending on where the repo is cloned to and should be adjusted accordingly.  


#### For Linux (with full R package builder)

For initial installation on Linux systems, we recommend using the full build script as this will install the pipeline along with all the required R packages and the `httrlib` R package (see below for details on this R package).

Simply run at the command line:
```bash
cd (localpath)/httrpl/
./build_pytest_env_no_docker.sh
```

Additionally, if R is already installed with the required R packages you can run:
```bash
cd (localpath)/httrpl/
./build_pytest_env_no_docker_no_R.sh
```
***NOTE:*** `build_pytest_env_no_docker_no_R.sh` does not install the `httrlib` R package.

#### For Docker

This package currently includes an experimental script for building a docker container with all tools and dependencies needed to pipeline HTTr data.
See [docs/docker_notes.md](docs/docker_notes.md) for more information.

#### Unit Tests

This package contains a variety of unit tests that can help verify proper installation of the complete pipelining environment.
See [docs/unit_tests.md](docs/unit_tests.md) for more information.



----------------------------------

Stand-Alone R Package Installation
----------------------------------

All R functions for pulling HTTr data from the various MongoDB collections that are populated by the pipeline, functions for running differential expression analysis, useful QC functions, etc. have been compiled into an R package named `httrlib`.

The `httrlib` R package is installed after following the full installation procedures for Linux and/or Docker using the `build_pytest_env_no_docker.sh` script (see above).
*However,* there are additional ways to build and install the `httrlib` R package for both Linux and Windows users if a user only wants/needs to install this R package.

### For Linux (or Docker)

If a user already has all the required R packages and wants to just build and install the `httrlib` package, simply run:
```bash
cd (localpath)/httrpl/
./httrlib_builder.sh
```
### For Windows

Instructions to install the `httrlib` package on Windows-based machine are shown below. 
***NOTE:*** These instructions assume a Windows machine with R v3.6.x installed. 
However, these instructions also apply to users with newer R installations *except* that the version of RTools needed will be different and the R path in `mininstall.bat` needs to reflect the correct R version, if applicable.

To install the `httrlib` package on Windows-based machines:

1. Install Rtools version 3.5 at https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe
2. Ensure write access to the `<R installation folder>/library` folder
3. Open cmd and "run as administrator" or if you can't, open cmd normally and do the following:
  + Add an environment variable for the user logged in called BINPREF:
    ```bash
    set BINPREF=<RTools installation folder>\mingw_32\bin\
    ```
    + ***NOTE:*** Make sure you include the last `\` in the above cmd code
    + `<RTools installation folder>` is likely to be `C:\Users\<userId>\Rtools`
    + This steps allows R to know where to find the RTools binaries
  + Add the 'bin' folders to your PATH variable:
    ```bash 
    path %PATH%;<R installation folder>\bin;<RTools installation folder>\bin;
    ```
    + This is likely something similar to: `path %PATH%;C:\Users\<user_id>\R\R-3.6.0\bin;C:\Users\<userId>\Rtools\bin;`
4.  If you want to install all R package dependencies (~250 packages), including DESeq2, and build the `httrlib` package, run:
  ```bash
  cd (localpath)/httrpl/windows-install/
  ./build_httrlib_and_requisites.bat
  ```
  + If, on the other hand, you don't need to install DESeq2 and R package dependencies, just replace the `<userid>` variable in `httrpl/windows-install/mininstall.bat` with your user ID and run:
  ```bash
  cd (localpath)/httrpl/windows-install/
  ./mininstall.bat
  ./httrlib_builder.bat
  ```
5. To test that the `httrlib` package was installed and can be loaded, run the following command in R/RStudio:
  ```r
  library(httrlib)
  ```

***NOTE:*** Windows installation of the `httrlib` R package has been tested using R v3.6 (Rtools v3.5) and R v4.2 (Rtools v4.0).

------------------------------------------------------

Running the primary alignment and count step of httrpl
------------------------------------------------------

A brief manual/vignette that covers the main steps of `httrpl` can be found here: [docs/httrpl_manual.md](docs/httrpl_manual.md).

+ Executable scripts are in: `httrpl/httr/bin/`
+ Python modules are in: `httrpl/httr/lib/`
+ R modules are in: `httrpl/httrlib/`
+ An example config file can be found in: `httrpl/httr/data/httrpl_automationTestData/config/originals/test_httrpl_userConfig.json`

For running the alignment step, make sure your `PATH` environment variable points to the hisat2 and samtools libraries. For example, you can add the following lines to your `.bashrc`:
```bash
export PATH=/share/projects/HTTr/Tools/hisat2-2.1.0/:$PATH
export PATH=/share/projects/HTTr/Tools/samtools-1.9/bin/:$PATH
```
To run the pipeline, you must run the `align_and_count.py` command from the `httrpl/httr/bin/` directory and specify the config file that you create.

Here is an example command to run the `align_and_count.py`script using an example config file and creating a log of the alignment run:
```bash
cd (localpath)/httrpl/httr/bin/
python3 align_and_count.py (localpath)/path/to/example_config.json &> (localpath)/path/to/logs/example.log &
```



---------------

Version History
---------------

**v0.7.5-public (4/30/24)**

*This is an extensive update to the [previous pilot code released in 2020](https://github.com/USEPA/httrpl_pilot)*

+ Updated documentation and user manual
+ Streamlined steps for pipelining data sets, with increased flexibility for alternate study designs.
+ Encapsulated all R functions into the `httrlib` package, with improved handling of MongoDB connections and standardized queries on both Windows and Linux
+ Added functionality to pipeline smaller data sets to disk without MongoDB back-end
+ Added functionality to handle DESeq2 differential expression analysis of multi-conc reference chemicals
+ Added functionality to validate DB schema and verify fastq paths
+ Added extensive unit tests for both Python functions and R/DESeq2 functions to ensure reproducible results.
+ Added scripts to build and test complete environment for pipelining, using either `venv` or `Docker`.




Contributors
------------

+ **[Logan J. Everett](mailto:everett.logan@epa.gov)**
+ **[Derik E. Haggard](mailto:haggard.derik@epa.gov)**
+ **Lionel Girardin**
+ **Imran Shah**
+ **Joseph Bundy**
+ **Beena Vallanat**
+ **Joshua Witten**


Disclaimer
----------

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
