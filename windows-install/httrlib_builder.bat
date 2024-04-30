@echo off

cd ..

if exist "%USERPROFILE%\R\httrlib" (
  rmdir /s /q "%USERPROFILE%\R\httrlib"
)

REM building httr doxigen comments (namespace and .rd files)
cd httrlib
Rscript doxygen.r
cd ..

Rscript windows_installer_scripts\rd2cmd

REM building httr pdf documentation
REM R CMD Rd2pdf httrlib
REM timeout /t 5 /nobreak
REM move httrlib.pdf httrlib\

REM building httrlib
R CMD build httrlib
for /r %%F in (httrlib_*.tar.gz) do set "f=%%F"
move "%f%" "httrlib.tar.gz"
R CMD INSTALL --build httrlib.tar.gz
