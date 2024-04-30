@echo off
setlocal enabledelayedexpansion

set "install_dir=%~dp0RpackagesDir"
set "packages_file=..\\RPackages"
set "installer_file=installer.R"

if not exist "%install_dir%" (
    mkdir "%install_dir%"
)

pushd "%install_dir%"

REM Delete existing installer file if it exists
if exist "%installer_file%" (
    del "%installer_file%"
)

for /f "usebackq delims=" %%i in ("%packages_file%") do (
    set "package=%%i"

    echo Downloading package: !package!
    curl -o "!package!.zip" "https://cran-archive.r-project.org/bin/windows/contrib/3.6/!package!.zip"

    if not errorlevel 1 (
        echo install.packages("!package!.zip",repos=NULL,type="win.library"^) >> "%installer_file%"
    )  else (
        echo Failed to download package: !package!
    )
)
curl -o httr2_0.2.3.zip https://cran.r-project.org/bin/windows/contrib/4.2/httr2_0.2.3.zip
echo install.packages("httr2_0.2.3.zip",repos=NULL,type="win.library"^) >> "%installer_file%"

REM Run the installer script
Rscript "%installer_file%"

popd

endlocal


