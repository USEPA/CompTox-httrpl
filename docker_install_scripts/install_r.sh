#!/bin/bash
# change to the workspace directory
current_dir=$(pwd)
cd /workspace/

# install the initial R packages
echo "Installing R packages"
Rscript -e "install.packages(c('roxygen2', 'rversions', 'Hmisc', 'remotes'), repos='https://cran.r-project.org')"

# run the install scripts in order
echo "Running package_loader.sh"
chmod 755 package_loader.sh
./package_loader.sh

echo "Running requirements.r"
Rscript requirements.r

echo "Installing devtools and reldist..."
Rscript -e "remotes::install_version('devtools', version='2.4.3', repos='https://cran.r-project.org')"
Rscript -e "remotes::install_version('reldist', version='1.6-6', repos='https://cran.r-project.org')"

echo "Running package_fixer.sh"
chmod 755 package_fixer.sh
./package_fixer.sh

echo "Running httrlib_builder_no_pdf.sh"
chmod 755 httrlib_builder_no_pdf.sh
./httrlib_builder_no_pdf.sh

# switch back to the original working directory
cd $current_dir