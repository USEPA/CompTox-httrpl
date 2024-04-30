@echo off

call package_loader.bat
cd RpackagesDir
Rscript ../requirements.r
cd ..
call package_fixer.bat
call httrlib_builder.bat

