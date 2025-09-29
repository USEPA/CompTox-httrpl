wget -nv -O RcppEigen_0.3.3.7.0.tar.gz https://cran.r-project.org/src/contrib/Archive/RcppEigen/RcppEigen_0.3.3.7.0.tar.gz
R CMD INSTALL --build RcppEigen_0.3.3.7.0.tar.gz 
wget -nv -O  interp_1.1-3.tar.gz https://cran.r-project.org/src/contrib/Archive/interp/interp_1.1-3.tar.gz
R CMD INSTALL --build interp_1.1-3.tar.gz 
wget -nv -O  devtools_2.4.3.tar.gz https://cran.r-project.org/src/contrib/Archive/devtools/devtools_2.4.3.tar.gz
R CMD INSTALL --build devtools_2.4.3.tar.gz 
wget -nv -O  reldist_1.6-6.tar.gz https://cran.r-project.org/src/contrib/Archive/reldist/reldist_1.6-6.tar.gz
R CMD INSTALL --build reldist_1.6-6.tar.gz 